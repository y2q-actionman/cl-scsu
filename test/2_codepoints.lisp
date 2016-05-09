(in-package :cl-scsu.test)

(defun test-codepoints* (str)
  (let* ((compressed (encode-from-string str))
	 (decompressed (decode-to-string compressed)))
    (assert-equalp str decompressed)
    t))
  
(defun test-bmp-codepoints ()
  (loop with str = (make-array '(#x10000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from 0 to #xFFFF
     unless (<= #xD800 i #xDFFF)
     do (vector-push (code-char i) str)
     finally (return (test-codepoints* str))))

(defun test-smp-codepoints ()
  (loop with str = (make-array '(#x100000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from #x10000 to #x10FFFF
     do (vector-push (code-char i) str)
     finally (return (test-codepoints* str))))

(defun test-smp-codepoints-as-surrogate ()
  (loop with str = (make-array '(#x200000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from #x10000 to #x10FFFF
     do (multiple-value-bind (high low)
	    (cl-scsu::encode-to-surrogate-pair i)
	  (vector-push (code-char high) str)
	  (vector-push (code-char low) str))
     finally (return (test-codepoints* str))))

(defun test-2-codepoints ()
  (and (test-bmp-codepoints)
       (if (> char-code-limit #xFFFF)
	   (test-smp-codepoints)
	   t)
       (test-smp-codepoints-as-surrogate)
       t))
