(in-package :cl-scsu.test)

(defun main ()
  (warn "under implementation")
  (and
   (test-1-util)
   (test-2-codepoints)
   (test-3-examples)
   ;; (test-4-interface)
   t))
