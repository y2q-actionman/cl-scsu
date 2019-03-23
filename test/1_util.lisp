(in-package :cl-scsu.test)

;;; Exhaustive tests for some conversions.
;;; (I once wrote `is' in deeply nested loop, but it causes endless
;;; compilation (on old fiveam) or too many output (on 1am). So, I
;;; rewrote them with `ALWAYS' clause.)

(test test-window-offset-table
  (is
   (loop for i of-type (unsigned-byte 8) from 0 below #xFF
      as offset = (ignore-errors (cl-scsu::lookup-window-offset-table i))
      always (if offset
		 (= (cl-scsu::window-offset-to-table-index offset) i)
		 t)))
  (is
   (loop for cp of-type fixnum from 0 below #x20000
      as candidates = (cl-scsu::list-offset-candidates cp)
      always
	(loop for offset in candidates
	   as table-index = (cl-scsu::window-offset-to-table-index offset)
	   always (if (>= offset #x10000)
		      (null table-index)
		      table-index)))))

(test test-extended-window-tag
  (is
   (loop for window of-type fixnum from 0 below cl-scsu::+window-count+
      always
	(loop for offset of-type fixnum from #x10000 to #x10FFFF by #x80
	   always (multiple-value-bind (high low)
		      (cl-scsu::encode-extended-window-tag window offset)
		    (multiple-value-bind (w o)
			(cl-scsu::decode-extended-window-tag high low)
		      (and (= window w) (= offset o)))))))
  (is
   (loop for high of-type fixnum from 0 to #x7F
      always
	(loop for low of-type fixnum from #x0 to #x7F
	   always (multiple-value-bind (w o)
		      (cl-scsu::decode-extended-window-tag high low)
		    (multiple-value-bind (h l)
			(cl-scsu::encode-extended-window-tag w o)
		      (and (= high h) (= low l))))))))

(test test-surrogate-pair
  (is
   (loop for i of-type fixnum from #x10000 to #x10FFFF
      always (multiple-value-bind (h l)
		 (cl-scsu::encode-to-surrogate-pair i)
	       (= i (cl-scsu::decode-from-surrogate-pair h l)))))
  (is
   (loop for high of-type fixnum from #xD800 to #xDBFF
      always
	(loop for low of-type fixnum from #xDC00 to #xDFFF
	   as cp = (cl-scsu::decode-from-surrogate-pair high low)
	   always (multiple-value-bind (h l)
		      (cl-scsu::encode-to-surrogate-pair cp)
		    (and (= high h) (= low l)))))))
