(defsystem :cl-scsu-test
  :description "Tests for cl-scsu."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-scsu :alexandria :1am)
  :components
  ((:module "test"
    :serial t    
    :components
    ((:file "package")
     (:file "1_util")
     (:file "2_codepoints")
     (:file "3_examples")
     (:file "4_interface"))))
  :perform (prepare-op :before (o c)
		       (set (find-symbol* :*tests* :1am) '()))
  :perform (test-op (o s)
		    (symbol-call '#:1am '#:run)))
