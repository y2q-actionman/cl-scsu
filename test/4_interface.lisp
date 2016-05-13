(in-package :cl-scsu.test)

(defun make-test-string (&key (str-buffer (make-array 16 :element-type 'character :fill-pointer t))
			   (start 0) (end (length str-buffer))
			 &aux (test-chars (list (char-code #\a) #x3041 #x30A1
						#xFF65 #x10000 #x10001 #x10FFFF)))
  (declare (type string str-buffer)
	   (type fixnum start end)
	   (type list test-chars)
	   (dynamic-extent test-chars))
  (fill str-buffer #\rubout :start 0 :end start)
  (loop with i = start
     for c in test-chars
     while (< i end)
     if (>= c char-code-limit)		; UTF-16 check
     do (multiple-value-bind (h l)
	    (cl-scsu::encode-to-surrogate-pair c)
	  (setf (char str-buffer i) (code-char h))
	  (incf i)
	  (setf (char str-buffer i) (code-char l))
	  (incf i))
     else do
       (setf (char str-buffer i) (code-char c))
       (incf i)
     finally
       (fill str-buffer #\rubout :start i :end end)
       (when (array-has-fill-pointer-p str-buffer)
	 (setf (fill-pointer str-buffer) i))
       (return
	 (values str-buffer i))))

;;; Buffer arguments

(defun test-buffer-args* (make-array-args)
  (let ((src-string (make-test-string))
	(dst-string (apply #'make-array 20 :element-type 'character make-array-args))
	(tmp-bytes (apply #'make-array 30 :element-type '(unsigned-byte 8) make-array-args)))
    (declare (type string src-string dst-string)
	     (type (array (unsigned-byte 8) (*)) tmp-bytes)
	     (dynamic-extent dst-string tmp-bytes))
    (fill tmp-bytes -1)
    (fill dst-string #\rubout)
    ;; encode
    (multiple-value-bind (ret-bytes encoded-len encoder-used-len state)
	(encode-from-string src-string :bytes tmp-bytes)
      (assert (eq ret-bytes tmp-bytes))
      (assert (eq encoder-used-len (length src-string)))
      ;; decode
      (scsu-state-reset state)		; reuse state object
      (multiple-value-bind (ret-string decoded-len decoder-used-len ret-state)
	  (decode-to-string tmp-bytes :end1 encoded-len :string dst-string
			    :state state)
	(assert (eq ret-string dst-string))
	(assert (= encoded-len decoder-used-len))
	(assert (eq state ret-state))
	(assert (string= src-string dst-string :end2 decoded-len)))))
  t)

(defun test-buffer-args ()
  (and
   (test-buffer-args* '())
   (test-buffer-args* '(:fill-pointer 0	:adjustable nil))
   t))

(defun test-ignored-write-buffer-args ()
  (let* ((str (make-test-string))
	 (encoded (encode-from-string str
				      :start2 5 :end2 2)) ; these are ignored
	 (decoded (decode-to-string encoded
				    :start2 9 :end2 1))) ; these are ignored
    (assert (string= str decoded)))
  t)

(defun test-range-args ()
  (let* ((src-string (make-array 20 :element-type 'character))
	 (dst-string (make-array 20 :element-type 'character))
	 (tmp-bytes (make-array 30 :element-type '(unsigned-byte 8)))
	 (string-start 1)
	 (string-end -1)
	 (bytes-start 5))
    (declare (type simple-string src-string dst-string)
	     (type (simple-array (unsigned-byte 8) (*)) tmp-bytes)
	     (type fixnum string-start string-end bytes-start)
	     (dynamic-extent src-string dst-string tmp-bytes))
    (fill tmp-bytes -1)
    (fill dst-string #\rubout)
    ;; make string
    (multiple-value-bind (_ s-end)
	(make-test-string :str-buffer src-string :start string-start)
      (declare (ignore _))
      (setf string-end s-end))
    ;; encode
    (multiple-value-bind (ret-bytes encoded-pos encoder-reached-pos)
	(encode-from-string src-string :start1 string-start :end1 string-end
			    :bytes tmp-bytes :start2 bytes-start)
      (assert (eq ret-bytes tmp-bytes))
      (assert (= encoder-reached-pos string-end))
      ;; decode
      (multiple-value-bind (ret-string decoded-pos decoder-reached-pos)
	  (decode-to-string tmp-bytes :start1 bytes-start :end1 encoded-pos
			    :string dst-string :start2 string-start :end2 string-end)
	(assert (eq ret-string dst-string))
	(assert (= decoder-reached-pos encoded-pos))
	(assert (string= src-string dst-string
			 :start1 string-start :end1 string-end
			 :start2 string-start :end2 decoded-pos)))))
  t)

;;; Encoder options
				
(defun encode-decode (&rest args)
  (let* ((str (make-test-string))
	 (encoded (apply #'encode-from-string str args))
	 (decoded-rets (multiple-value-list (decode-to-string encoded))))
    (assert (string= str (first decoded-rets)))
    (apply #'values decoded-rets)))

(defun test-initial-priority-arg ()
  (encode-decode :initial-priority :lookahead)
  (encode-decode :initial-priority :random)
  (encode-decode :initial-priority #(1 2 3 4 5 6 7 8))
  t)

(defun test-fixed-mode-arg ()
  (macrolet ((check-no-defwin (form)
	       `(let ((decoder-state (nth-value 3 ,form)))
		  (assert (null (cl-scsu::scsu-state-dynamic-window decoder-state))))))
    (let ((*scsu-state-default-fix-dynamic-window* nil))
      (encode-decode)
      (check-no-defwin (encode-decode :fix-dynamic-window t))
      (encode-decode :fix-dynamic-window nil))
    (let ((*scsu-state-default-fix-dynamic-window* t))
      (check-no-defwin (encode-decode))
      (check-no-defwin (encode-decode :fix-dynamic-window t))
      (encode-decode :fix-dynamic-window nil)))
  t)

(defun test-continuous-encoding ()
  (multiple-value-bind (src-string src-string-len)
      (make-test-string)
    (declare (type string src-string)
	     (type fixnum src-string-len))
    (let* ((tmp-bytes (make-array 70 :element-type '(unsigned-byte 8)))
	   (bytes-end 0))
      (declare (type (simple-array (unsigned-byte 8) (*)) tmp-bytes)
	       (type fixnum bytes-end)
	       (dynamic-extent tmp-bytes))
      (fill tmp-bytes 0)
      ;; encode
      (multiple-value-bind (_bytes encoded-pos __ state)
	  (encode-from-string src-string :bytes tmp-bytes)
	(declare (ignore __))
	(assert (eq _bytes tmp-bytes))
	(multiple-value-bind (_bytes encoded-pos2 __ _state)
	    (encode-from-string src-string :bytes tmp-bytes :start2 encoded-pos :state state) ; reuse state
	  (declare (ignore __))
	  (assert (eq _bytes tmp-bytes))
	  (assert (eq _state state))
	  (multiple-value-bind (_bytes encoded-pos3 _state)
	      (encode-reset-sequence state :bytes tmp-bytes :start encoded-pos2) ; reset state
	    (assert (eq _bytes tmp-bytes))
	    (assert (eq _state state))
	    (multiple-value-bind (_bytes encoded-pos4)
		(encode-from-string src-string :bytes tmp-bytes :start2 encoded-pos3) ; use a new state
	      (assert (eq _bytes tmp-bytes))
	      (setf bytes-end encoded-pos4)))))
      ;; decode
      (let ((ret-string (decode-to-string tmp-bytes :end1 bytes-end)))
	(assert (= (length ret-string) (* 3 src-string-len)))
	(loop for i of-type fixnum from 0 below 2
	   do (assert (string= src-string ret-string
			       :start2 (* i src-string-len) :end2 (* (1+ i) src-string-len)))))))
  t)


;;; Restart

(defmacro with-restoring-state-test (&body body)
  (alexandria:with-gensyms (raised?)
    `(let ((,raised? nil))
       (multiple-value-prog1 
	   (handler-bind
	       ((scsu-error (lambda (c)
			      (setf ,raised? c)
			      (invoke-restart 'restore-state))))
	     (progn ,@body))
	 (assert ,raised?)))))

(defun test-write-exhaust ()
  (let ((src-string "0123456")
	(tmp-bytes (make-array 10 :element-type '(unsigned-byte 8)))
	(dst-string (make-array 10 :element-type 'character)))
    (declare (type (simple-array (unsigned-byte 8) *) tmp-bytes)
	     (type simple-string dst-string)
	     (dynamic-extent tmp-bytes dst-string))
    (multiple-value-bind (_ bytes-used src-used)
	(with-restoring-state-test
	  (encode-from-string src-string :bytes tmp-bytes :end2 5))
      (declare (ignore _))
      (assert (= src-used 5))
      (assert (= bytes-used 5)))
    (multiple-value-bind (_ dst-used bytes-used-2)
	(with-restoring-state-test
	  (decode-to-string tmp-bytes :end1 5 :string dst-string :end2 4))
      (declare (ignore _))
      (assert (= bytes-used-2 4))
      (assert (= dst-used 4)))
    (assert (string= src-string dst-string :end1 4 :end2 4)))
  t)
    
(defun test-bad-surrogate ()
  (let ((src-string (make-array 2 :element-type 'character)))
    (declare (type simple-string src-string)
	     (dynamic-extent src-string))
    (setf (schar src-string 0) #\a)
    (flet ((test-encode (bad-code)
	     (setf (schar src-string 1) (code-char bad-code))
	     (multiple-value-bind (bytes bytes-len src-used)
		 (with-restoring-state-test (encode-from-string src-string))
	       (assert (= (aref bytes 0) (char-code #\a)))
	       (assert (= bytes-len 1))
	       (assert (= src-used 1)))))
      (test-encode #xD800)
      (test-encode #xDFFF)))
  (let ((src-bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) src-bytes)
	     (dynamic-extent src-bytes))
    (setf (aref src-bytes 0) (char-code #\a))
    (setf (aref src-bytes 1) cl-scsu::+SQU+)
    (flet ((test-decode (bad-code)
	     (setf (aref src-bytes 2) (ldb (byte 8 8) bad-code))
	     (setf (aref src-bytes 3) (ldb (byte 8 0) bad-code))
	     (multiple-value-bind (string string-len bytes-used)
		 (with-restoring-state-test (decode-to-string src-bytes))
	       (assert (= string-len 1))
	       (assert (char= (char string 0) #\a))
	       (assert (= bytes-used 1)))))
      (test-decode #xD800)
      (test-decode #xDFFF)))
  t)
    
(defun test-decode-bad-tag ()
  (let ((src-bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) src-bytes)
	     (dynamic-extent src-bytes))
    (setf (aref src-bytes 0) (char-code #\a))
    (setf (aref src-bytes 1) cl-scsu::+SC1+)
    (flet ((test-tag (bad-tag args)
	     (setf (aref src-bytes 2) bad-tag)
	     (loop for i from 0 below (1- args)
		do (setf (aref src-bytes (+ 3 i)) i)
		  (multiple-value-bind (string string-len bytes-used)
		      (with-restoring-state-test (decode-to-string src-bytes :end1 (+ 3 i)))
		    (assert (= string-len 1))
		    (assert (char= (char string 0) #\a))
		    (assert (= bytes-used 2))))))
      ;; single-byte mode
      (setf (aref src-bytes 1) cl-scsu::+SC1+)
      (test-tag cl-scsu::+SQU+ 2)
      (loop for tag from cl-scsu::+SQ0+ to cl-scsu::+SQ7+
	 do (test-tag tag 1))
      (loop for tag from cl-scsu::+SD0+ to cl-scsu::+SD7+
	 do (test-tag tag 1))
      (test-tag cl-scsu::+SDX+ 2)
      ;; unicode mode
      (setf (aref src-bytes 1) cl-scsu::+SCU+)
      (test-tag cl-scsu::+UQU+ 2)
      (loop for tag from cl-scsu::+UD0+ to cl-scsu::+UD7+
	 do (test-tag tag 1))
      (test-tag cl-scsu::+UDX+ 2)))
  t)
	
(defun test-decode-reserved-bytes ()
  (let ((src-bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) src-bytes)
	     (dynamic-extent src-bytes))
    (setf (aref src-bytes 0) (char-code #\a))
    (flet ((test-bad-bytes (bytes good-bytes-len)
	     (loop for i from 1
		for b in bytes
		do (setf (aref src-bytes i) b)
		finally
		  (multiple-value-bind (string string-len bytes-used)
		      (with-restoring-state-test (decode-to-string src-bytes :end1 i))
		    (assert (= string-len 1))
		    (assert (char= (char string 0) #\a))
		    (assert (= bytes-used (+ 1 good-bytes-len)))))))
      ;; single-byte mode reserved byte
      (test-bad-bytes '(#xC) 0)
      ;; unicode mode reserved byte
      (test-bad-bytes `(,cl-scsu::+SCU+ #xF2) 1)
      ;; window offset table reserved byte
      (test-bad-bytes `(,cl-scsu::+SD0+ #x0) 0)
      (test-bad-bytes `(,cl-scsu::+SD0+ #xA8) 0)
      (test-bad-bytes `(,cl-scsu::+SD0+ #xF8) 0)))
  t)
  
;;; Main
(defun test-interface ()
  (and (test-buffer-args)
       (test-ignored-write-buffer-args)
       (test-range-args)
       (test-initial-priority-arg)
       (test-fixed-mode-arg)
       (test-continuous-encoding)
       
       (test-write-exhaust)
       (test-bad-surrogate)
       (test-decode-bad-tag)
       (test-decode-reserved-bytes)
       t))
