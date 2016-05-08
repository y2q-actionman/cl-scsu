(in-package :cl-scsu.test)

(defun main ()
  (warn "under implementation")
  (and
   (test-1-examples)
   ;; (test-2-codepoints)
   ;; (test-3-coverage)
   ;; (test-4-interface)
   t))
