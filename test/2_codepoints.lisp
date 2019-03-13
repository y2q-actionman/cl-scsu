(in-package :cl-scsu.test)

(defun codepoints-equalp* (str)
  (let* ((compressed (encode-from-string str))
	 (decompressed (decode-to-string compressed)))
    (equalp str decompressed)))
  
(test test-bmp-codepoints
  (loop with str = (make-array '(#x10000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from 0 to #xFFFF
     unless (<= #xD800 i #xDFFF)
     do (vector-push (code-char i) str)
     finally (is (codepoints-equalp* str))))

(defun test-smp-codepoints-as-32bit ()
  (loop with str = (make-array '(#x100000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from #x10000 to #x10FFFF by 3
     do (vector-push (code-char i) str)
     finally (is (codepoints-equalp* str))))

(defun test-smp-codepoints-as-surrogate ()
  (loop with str = (make-array '(#x200000) :element-type 'character
			       :fill-pointer 0)
     for i fixnum from #x10000 to #x10FFFF by 3
     do (multiple-value-bind (high low)
	    (cl-scsu::encode-to-surrogate-pair i)
	  (vector-push (code-char high) str)
	  (vector-push (code-char low) str))
     finally (is (codepoints-equalp* str))))

(test test-smp-codepoints
  (if (> char-code-limit #xFFFF)
      (test-smp-codepoints-as-32bit)
      (test-smp-codepoints-as-surrogate)))
