(in-package :cl-scsu.test)

(defmacro assert-equal* (op form-a form-b)
  (let ((val-a (gensym))
	(val-b (gensym)))
    `(let ((,val-a ,form-a)
	  (,val-b ,form-b))
       (assert (,op ,val-a ,val-b)
	       nil
	       "Test assertion failure~_~S~_~A~_~A"
	       '(,op ,form-a ,form-b) ,val-a ,val-b))))

(defmacro assert-equal (form-a form-b)
  `(assert-equal* equal ,form-a ,form-b))

(defmacro assert-equalp (form-a form-b)
  `(assert-equal* equalp ,form-a ,form-b))

(defmacro assert-some-condition (&body body)
  (let ((returns (gensym)))
    `(destructuring-bind (&rest ,returns)
	 (multiple-value-list (ignore-errors ,@body))
       (unless (and (null (first ,returns))
		    (second ,returns))
	 (assert nil nil
		 "This form should cause some condition: ~A" ',body)))))
