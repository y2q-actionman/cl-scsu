(defsystem :cl-scsu
  :description "An implementation of 'Standard Compression Scheme for Unicode'."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:alexandria)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "constant")
     (:file "scsu"))))
  :in-order-to ((test-op (test-op #:cl-scsu-test))))
