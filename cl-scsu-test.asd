(defsystem :cl-scsu-test
  :description "Tests for cl-scsu."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-scsu)
  :components
  ((:module "test"
    :serial t    
    :components
    ((:file "package")
     (:file "util")
     (:file "1_util")
     (:file "2_codepoints")
     (:file "3_examples")
     (:file "4_interface")
     (:file "main"))))
  :perform (test-op (o s)
  		    (symbol-call '#:cl-scsu.test '#:main)))
