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

(defun test-buffer-args ()
  (let ((src-string (make-test-string))
	(dst-string (make-array 20 :element-type 'character))
	(tmp-bytes (make-array 30 :element-type '(unsigned-byte 8))))
    (declare (type string src-string)
	     (type simple-string dst-string)
	     (type (simple-array (unsigned-byte 8) (*)) tmp-bytes)
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
;; -- use simple-test


;;; Main
(defun test-interface ()
  (warn "restart test is under implementation")
  (and (test-buffer-args)
       (test-ignored-write-buffer-args)
       (test-range-args)
       (test-initial-priority-arg)
       (test-fixed-mode-arg)
       (test-continuous-encoding)
       t))
