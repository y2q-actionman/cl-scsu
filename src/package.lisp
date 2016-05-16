(in-package :cl-user)

(defpackage :cl-scsu
  (:use :cl)
  (:export
   ;; scsu-state object
   #:scsu-state
   #:scsu-state-reset
   ;; errors
   #:scsu-error
   #:restore-state
   ;; main entry point
   #:decode-to-string
   #:encode-from-string
   #:encode-reset-sequence
   ))
