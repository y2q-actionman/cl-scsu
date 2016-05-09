(in-package :cl-scsu.test)

(defun test-window-offset-table ()
  (loop for i of-type (unsigned-byte 8) from 0 below #xFF
     as offset of-type (or null (unsigned-byte 16))
       = (ignore-errors (cl-scsu::lookup-window-offset-table i))
     when offset
     do (assert (= (cl-scsu::find-window-offset-table-index offset) i)))
  t)

(defun test-extended-window-tag ()
  (loop for window of-type fixnum from 0 below cl-scsu::+window-count+
     do (loop for offset of-type fixnum from #x10000 to #x10FFFF by #x80
	   do (multiple-value-bind (high low)
		  (cl-scsu::encode-extended-window-tag window offset)
		(multiple-value-bind (w o)
		    (cl-scsu::decode-extended-window-tag high low)
		  (assert (and (= window w) (= offset o)))))))
  (loop for high of-type fixnum from 0 to #x7F
     do (loop for low of-type fixnum from #x0 to #x7F
	   do (multiple-value-bind (w o)
		  (cl-scsu::decode-extended-window-tag high low)
		(multiple-value-bind (h l)
		    (cl-scsu::encode-extended-window-tag w o)
		  (assert (and (= high h) (= low l)))))))
  t)

(defun test-surrogate-pair ()
  (loop for i of-type fixnum from #x10000 to #x10FFFF
     do (multiple-value-bind (h l)
	    (cl-scsu::encode-to-surrogate-pair i)
	  (assert (= i (cl-scsu::decode-from-surrogate-pair h l)))))
  (loop for high of-type fixnum from #xD800 to #xDBFF
     do (loop for low of-type fixnum from #xDC00 to #xDFFF
	   do (let ((cp (cl-scsu::decode-from-surrogate-pair high low)))
		(multiple-value-bind (h l)
		    (cl-scsu::encode-to-surrogate-pair cp)
		  (assert (and (= high h) (= low l)))))))
  t)
