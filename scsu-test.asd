;; Copyright (c) 2016 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :cl-scsu.test
  :description "Tests for cl-scsu."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-scsu)
  :components
  ((:module "test"
    :serial t    
    :components
    ((:file "package")
     ;; (:file "simple-test")
     (:file "1_util")
     (:file "2_codepoints")
     (:file "3_examples")
     (:file "4_interface")
     (:file "main"))))
  :perform (asdf:test-op (o s)
  			 (uiop:symbol-call '#:cl-scsu.test '#:main)))

