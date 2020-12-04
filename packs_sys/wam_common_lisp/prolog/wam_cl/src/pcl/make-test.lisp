(in-package :pcl)

(defun top-level-form-form (form)
  #+cmu
  (if (and (consp form) (eq (car form) 'eval-when))
      (third form)
      form)
  #+kcl
  (fourth (third form))
  #+lcl3.0
  (third (third form)))

(defun make-test ()
  (let ((table (make-hash-table :test 'eq))
	(count 0))
    (labels ((fixup (form)
	       (if (consp form)
		   (cons (fixup (car form)) (fixup (cdr form)))
		   (if (and (symbolp form) (null (symbol-package form)))
		       (or (gethash form table)
			   (setf (gethash form table)
				 (intern (format nil "~A-%-~D" (symbol-name form)
						 (incf count))
					 *the-pcl-package*)))
		       form))))
      (with-open-file (out "test.lisp"
			   :direction :output :if-exists :supersede)
	(declare (type stream out))
	(let ((*print-case* :downcase)
	      (*print-pretty* t)
	      (*package* *the-pcl-package*))
	  (format out "~S~%" '(in-package :pcl))
	  (let ((i 0)
		(f (macroexpand '(PRECOMPILE-FUNCTION-GENERATORS PCL))))
	    (dolist (form (cdr (top-level-form-form f)))
	      (let ((name (intern (format nil "FGEN-~D" (incf i)))))
		(format out "~S~%" `(defun ,name () ,(fixup form))))))
	  (let ((i 0)
		(f (macroexpand '(PRECOMPILE-DFUN-CONSTRUCTORS PCL))))
	    (dolist (form (cdr f))
	      (let ((name (intern (format nil "DFUN-CONSTR-~D" (incf i))))
		    (form (top-level-form-form form)))
		(format out "~S~%" `(defun ,name () 
				      (list ,(second form)
				            ,(third form)
				            ,(fixup (macroexpand (fifth form))))))))))))))

