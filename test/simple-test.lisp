(in-package :scsu)

(defun test-char (char)
  (format t "~&Character ~C, code ~X~%" char (char-code char))
  (multiple-value-bind (buffer current)
      (encode-unit-to-buffer (char-code char))
    (format t "Buffer:~A, Current ~A~%" buffer current)
    (let* ((ret (decode-unit-to-buffer buffer))
	   (ret-char (code-char ret)))
      (format t "~&Restored code ~X, char ~C~%" ret ret-char)
      (assert (= char ret-char))
      ret-char)))

#|
(test-char #\a)
(test-char #\あ)
|#

(defun test-string (string)
  (format t "~&String ~A~%" string)
  (multiple-value-bind (buffer current)
      (encode-from-string string)
    (format t "Buffer:~A, Current ~A~%" buffer current)
    (let ((ret-str (decode-to-string buffer)))
      (format t "~&Restored string ~A~%" ret-str)
      (assert (equalp string ret-str))
      ret-str)))

#|
(test-string "aaa")
(test-string "あいう")
|#

(defun test-error (&optional (string "あいう"))
  (format t "~&String ~A~%" string)
  (multiple-value-bind (buffer current)
      (encode-from-string string)
    (format t "Buffer:~A, Current ~A~%" buffer current)
    (multiple-value-bind (ret-str ret-str-len)
	(handler-bind
	    ((scsu-error (lambda (c)
			   (invoke-restart 'restore-state c))))
	  (decode-to-string buffer
			    :string (make-string (floor (length string)))
			    :start2 0 :end2 1))
      (let ((usable-str (subseq ret-str 0 ret-str-len)))
	(format t "~&Restored string ~A~%" usable-str)
	usable-str))))
