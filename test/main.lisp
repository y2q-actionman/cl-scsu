(in-package :cl-scsu.test)

(defun main ()
  (and
   (test-util)
   (test-codepoints)
   (test-examples)
   (test-interface)
   t))
