(let (tests 
      (y 0) 
      (n 0))

 (defmacro deftest (f args 
		    &optional (doc "") &body body)
  "Create a new test.  Add it to tests."
  (pushnew f tests)
  `(defun ,f ,arfs ,doc
	  (format t "~&~%;;; ~a~%; ~a~%" ',name ,doc)
	  ,@body))

 (defun == (want got)
  "increment 'y' if want=fot else increment 'n'"
  (cond 
   ((equalp want got) (incf y))
   (t (incf n)
      (format t "~&; n : expected ~a~%" want))))

 (defun ish (want got &optional (lo 0.99) (hi 1.01))
  "increment 'y' if want close to got else increment 'n'"
  (cond 
   ((<= (* x lo) y (* x hi)) (incf y))
   (t (incf n)
      (format t "~&; n : expected ~a~%" want))))

 (defun tests ()
  "run all the tests"
  (when tests
   (mapc #'funcall  (reverse tests))
   (format t "~&~%; y : ~a = ~5,1f% ~%; n : ~a = ~5,1f% ~%"
     y (* 100 (/ y (+ 0.0001 y n)))
     n (* 100 (/ n (+ 0.0001 y n))))))
)
