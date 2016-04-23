;; Copyright (c) 2016 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :cl-scsu
  :description "An implementation of 'Standard Compression Scheme for Unicode'."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:alexandria)  ; :babel :flexi-stream
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "constant")
     (:file "scsu"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-scsu.test)))) 
