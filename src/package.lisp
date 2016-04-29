(in-package :cl-user)

(defpackage :cl-scsu
  (:use :cl)
  (:export
   ;; scsu-state object
   #:*scsu-state-default-fix-dynamic-window*
   #:scsu-state
   #:scsu-state-reset
   ;; errors
   #:scsu-error
   ;; main encoder
   #:decode-to-string
   #:decode-unit-from-bytes
   #:encode-from-string
   #:encode-unit-to-bytes
   ))
