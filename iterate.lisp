(defpackage :cover/iterate
  (:use :cl :cover :interate/internal)
  (:import-from :cover :c-or :c-and :c-if :c-when
		:c-unless :c-case :c-typecase)
  (:import-from :iterate/internal :walk :walk-cdr :walk-list-nconcing
		:special-form? :return-code-modifying-body))

(in-package :cover/iterate)

;;; Extra methods to make COVER work with ITERATE

(defmethod special-form? ((symbol (eql 'c-or)))
  '(c-or . walk-cdr))

(defmethod special-form? ((symbol (eql 'c-and))
  '(c-and . walk-cdr))

(defmethod special-form? ((symbol (eql 'c-if)))
  '(c-if . walk-cdr))

(defmethod special-form? ((symbol (eql 'c-when)))
  '(c-when . walk-cdr))

(defmethod special-form? ((symbol (eql 'c-unless)))
  '(c-unless . walk-cdr))

(defmethod special-form? ((symbol (eql 'c-case)))
  '(c-case . walk-case))

(defmethod special-form? ((symbol (eql 'c-typecase)))
  '(c-typecase . walk-case))

(defmethod special-form? ((symbol (eql 'c-cond)))
  `(c-cond . walk-cond))

(defun walk-case (case expr &rest body)
  (multiple-value-bind (bod decs init step final final-p)
      (walk expr)
    (multiple-value-bind (abod adecs ainit astep afinal afinal-p)
        (walk-case-body body)
      (values (list (list* case (cons 'progn bod) abod))
              (nconc decs adecs)
              (nconc init ainit)
              (nconc step astep)
              (nconc final afinal)
              (nconc final-p afinal-p)))))

(defun walk-case-body (case-body)
  (walk-list-nconcing case-body #'walk-case-clause))

(defun walk-case-clause (case-clause)
  (apply #'walk-cdr case-clause))

(defun walk-cond (cond &rest cond-clauses)
  (return-code-modifying-body #'walk-cond-clauses cond-clauses
			      (lambda (result) (list (cons cond result)))))

(defun walk-cond-clauses (cond-clauses)
  (walk-list-nconcing cond-clauses (lambda (x) (list (walk-arglist x)))))
