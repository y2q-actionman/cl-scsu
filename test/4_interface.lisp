(in-package :cl-scsu.test)

(declaim (type list +test-chars+))
(define-constant +test-chars+
  (list (char-code #\a) #x3041 #x30A1 #xFF65 #x10000 #x10001 #x10FFFF)
  :test 'equal)

(defun make-test-string (&key (str-buffer (make-array 16 :element-type 'character :fill-pointer t))
			   (start 0) (end (length str-buffer)))
  (declare (type string str-buffer)
	   (type fixnum start end))
  (fill str-buffer #\rubout :start 0 :end start)
  (loop with i = start
     for c in +test-chars+
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
	     (type (array (unsigned-byte 8) (*)) tmp-bytes))
    (fill tmp-bytes #xFF)
    (fill dst-string #\rubout)
    ;; encode
    (multiple-value-bind (ret-bytes encoded-len encoder-used-len state)
	(encode-from-string src-string :bytes tmp-bytes)
      (is (eq ret-bytes tmp-bytes))
      (is (= encoder-used-len (length src-string)))
      ;; decode
      (encode-reset-sequence state)	; reuse state object
      (multiple-value-bind (ret-string decoded-len decoder-used-len ret-state)
	  (decode-to-string tmp-bytes :end1 encoded-len :string dst-string
			    :state state)
	(is (eq ret-string dst-string))
	(is (= encoded-len decoder-used-len))
	(is (eq state ret-state))
	(is (string= src-string dst-string :end2 decoded-len)))))
  t)

(test test-buffer-args
  (test-buffer-args* '())
  (test-buffer-args* '(:fill-pointer 0	:adjustable nil)))

(test test-ignored-write-buffer-args
  (let* ((str (make-test-string))
	 (encoded (encode-from-string str
				      :start2 5 :end2 2)) ; these are ignored
	 (decoded (decode-to-string encoded
				    :start2 9 :end2 1))) ; these are ignored
    (is (string= str decoded))))

(test test-range-args
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
    (fill tmp-bytes #xFF)
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
      (is (eq ret-bytes tmp-bytes))
      (is (= encoder-reached-pos string-end))
      ;; decode
      (multiple-value-bind (ret-string decoded-pos decoder-reached-pos)
	  (decode-to-string tmp-bytes :start1 bytes-start :end1 encoded-pos
			    :string dst-string :start2 string-start :end2 string-end)
	(is (eq ret-string dst-string))
	(is (= decoder-reached-pos encoded-pos))
	(is (string= src-string dst-string
		     :start1 string-start :end1 string-end
		     :start2 string-start :end2 decoded-pos))))))

;;; Encoder options
				
(defun encode-decode (&rest args)
  (let* ((str (make-test-string))
	 (encoded (apply #'encode-from-string str args))
	 (decoded-rets (multiple-value-list (decode-to-string encoded))))
    (is (string= str (first decoded-rets)))
    (apply #'values decoded-rets)))

(test test-initial-priority-arg
  (encode-decode :initial-priority :lookahead)
  (encode-decode :initial-priority :random)
  (encode-decode :initial-priority #(1 2 3 4 5 6 7 8))
  (let ((decoder-state (nth-value 3 (encode-decode :initial-priority :fixed))))
    (is (null (cl-scsu::scsu-state-dynamic-window decoder-state)))))

(test test-continuous-encoding
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
	(is (eq _bytes tmp-bytes))
	(multiple-value-bind (_bytes encoded-pos2 __ _state)
	    (encode-from-string src-string :bytes tmp-bytes :start2 encoded-pos :state state) ; reuse state
	  (declare (ignore __))
	  (is (eq _bytes tmp-bytes))
	  (is (eq _state state))
	  (multiple-value-bind (_bytes encoded-pos3 _state)
	      (encode-reset-sequence state :bytes tmp-bytes :start encoded-pos2) ; reset state
	    (is (eq _bytes tmp-bytes))
	    (is (eq _state state))
	    (multiple-value-bind (_bytes encoded-pos4)
		(encode-from-string src-string :bytes tmp-bytes :start2 encoded-pos3) ; use a new state
	      (is (eq _bytes tmp-bytes))
	      (setf bytes-end encoded-pos4)))))
      ;; decode
      (let ((ret-string (decode-to-string tmp-bytes :end1 bytes-end)))
	(is (= (length ret-string) (* 3 src-string-len)))
	(loop for i of-type fixnum from 0 below 2
	   do (is (string= src-string ret-string
			   :start2 (* i src-string-len) :end2 (* (1+ i) src-string-len))))))))

(test test-empty-reset
  (is (zerop (length (encode-reset-sequence (make-instance 'scsu-state)))))
  (multiple-value-bind (bytes len _ state)
      (encode-from-string "a")
    (declare (ignore _))
    (is (= (aref bytes 0) (char-code #\a)))
    (is (= len 1))
    (is (zerop (length (encode-reset-sequence state))))))
  
;; TODO: test (encode-reset-sequence <initial-state>) == 0 bytes.


;;; Restart

(defmacro with-restoring-state-test (&body body)
  (with-gensyms (raised?)
    `(let ((,raised? nil))
       (multiple-value-prog1 
	   (handler-bind
	       ((scsu-error (lambda (c)
			      (setf ,raised? c)
			      (invoke-restart 'restore-state))))
	     (progn ,@body))
	 (is ,raised?)))))

(test test-write-exhaust
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
      (is (= src-used 5))
      (is (= bytes-used 5)))
    (multiple-value-bind (_ dst-used bytes-used-2)
	(with-restoring-state-test
	  (decode-to-string tmp-bytes :end1 5 :string dst-string :end2 4))
      (declare (ignore _))
      (is (= bytes-used-2 4))
      (is (= dst-used 4)))
    (is (string= src-string dst-string :end1 4 :end2 4))))
    
(test test-bad-surrogate
  (let ((src-string (make-array 2 :element-type 'character)))
    (declare (type simple-string src-string)
	     (dynamic-extent src-string))
    (setf (schar src-string 0) #\a)
    (flet ((test-encode (bad-code)
	     (setf (schar src-string 1) (code-char bad-code))
	     (multiple-value-bind (bytes bytes-len src-used)
		 (with-restoring-state-test (encode-from-string src-string))
	       (is (= (aref bytes 0) (char-code #\a)))
	       (is (= bytes-len 1))
	       (is (= src-used 1)))))
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
	       (is (= string-len 1))
	       (is (char= (char string 0) #\a))
	       (is (= bytes-used 1)))))
      (test-decode #xD800)
      (test-decode #xDFFF))))
    
(test test-decode-bad-tag
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
		    (is (= string-len 1))
		    (is (char= (char string 0) #\a))
		    (is (= bytes-used 2))))))
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
      (test-tag cl-scsu::+UDX+ 2))))
	
(test test-decode-reserved-bytes
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
		    (is (= string-len 1))
		    (is (char= (char string 0) #\a))
		    (is (= bytes-used (+ 1 good-bytes-len)))))))
      ;; single-byte mode reserved byte
      (test-bad-bytes '(#xC) 0)
      ;; unicode mode reserved byte
      (test-bad-bytes `(,cl-scsu::+SCU+ #xF2) 1)
      ;; window offset table reserved byte
      (test-bad-bytes `(,cl-scsu::+SD0+ #x0) 0)
      (test-bad-bytes `(,cl-scsu::+SD0+ #xA8) 0)
      (test-bad-bytes `(,cl-scsu::+SD0+ #xF8) 0))))
