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

(defun test-error (string)
  (format t "~&String ~A~%" string)
  (multiple-value-bind (buffer current)
      (encode-from-string string)
    (format t "Buffer:~A, Current ~A~%" buffer current)
    (let ((ret-str (decode-to-string buffer
				     :string (make-string (floor (length string)))
				     :start2 0 :end2 1)))
      (format t "~&Restored string ~A~%" ret-str)
      ret-str)))
