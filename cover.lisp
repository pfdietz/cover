;-*-syntax:COMMON-LISP; Mode: LISP-*-

(cl:in-package "COVER")

#|----------------------------------------------------------------------------|
 | Copyright 1991 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
|----------------------------------------------------------------------------|#

;;; Additional changes Copyright 2018 by Paul F. Dietz
;;; Permission granted to use, copy, modify and distribute these changes
;;; for any purpose, without restriction.

(provide "COVER")

(shadow '(defun defmacro defmethod defgeneric))

(export '(annotate report reset forget
	  forget-all *line-limit* *report-readable-branches*))

(defstruct (point (:conc-name nil)
		  (:type list))
  (hit 0)
  (id nil)
  (status :show)
  (name nil)
  (subs nil))

;;; Data structure for the set of all points
;;; head -> a cons cell before the actual list
;;; tail -> the last cons cell in the points list, or head if empty
;;; map -> an equal hash table from fn names of points
;;;    to their cell in the points list

(defstruct points-set
  head
  tail
  map)

;; Swallowed the globals into a single global state,
;; for simplicity.  Replace their references with symbol macros.
(defstruct cgs
  (count 0 :type integer)
  (hit 1 :type integer)
  (points-head nil :type list)
  (points-tail nil :type list)
  (points-map (make-hash-table :test #'equal) :type hash-table)
  (annotating nil)
  (testing nil))

(defvar *cgs* (make-cgs))
(define-symbol-macro *count* (cgs-count *cgs*))
(define-symbol-macro *hit* (cgs-hit *cgs*))
;; (define-symbol-macro *points* (cgs-points *cgs*))
(define-symbol-macro *points* (identity (cdr (cgs-points-head *cgs*))))
(define-symbol-macro *annotating* (cgs-annotating *cgs*))
(define-symbol-macro *testing* (cgs-testing *cgs*))

(cl:defun forget (&rest ids)
  (forget1 ids *points*)
  t)

(cl:defun forget1 (names ps)
  (dolist (p ps)
    (when (member (id p) names)
      (setf (status p) :forgotten))
    (forget1 names (subs p))))

(cl:defun forget-all ()
  (let ((cgs *cgs*)
	(head (list nil)))
    (setf (cgs-points-head cgs) head
	  (cgs-points-tail cgs) head)
    (clrhash (cgs-points-map cgs)))
  (setf *hit* 1
	*count* 0)
  t)

(cl:defun reset () (incf *hit*) t)

(cl:defun add-top-point (p)
  (setq p (copy-tree p))
  (let* ((cgs *cgs*)
	 (map (cgs-points-map cgs))
	 (fn (fn-name p))
	 (old-cell (gethash fn map)))
    ;; Make sure the list is initialized
    (unless (cgs-points-head cgs)
      (let ((head (list nil)))
	(setf (cgs-points-head cgs) head
	      (cgs-points-tail cgs) head)))
    (cond (old-cell
	   (setf (id p) (id (car old-cell)))
	   (setf (car old-cell) p))
	  (t (setf (id p) (incf *count*))
	     (let ((new-cell (list p)))
	       (setf (gethash fn map) new-cell
		     (cdr (cgs-points-tail cgs)) new-cell)
	       (setf (cgs-points-tail cgs) new-cell))))
    (cdr (cgs-points-head cgs))))

(cl:defun record-hit (p)
  (let ((h *hit*))
    (unless (= (hit p) h)
      (setf (hit p) h)
      (let ((old (locate (name p))))
	(if old
	    (setf (hit old) h)
	    (add-point p))))))

(cl:defun locate (name)
  (find name
	(if (not (cdr name))
	    *points*
            (let ((p (locate (cdr name))))
	      (if p (subs p))))
	:key #'name :test #'equal))

(cl:defun find-point (fn)
  ;; (find fn *points* :key #'fn-name :test #'equal)
  (car (gethash fn (cgs-points-map *cgs*))))

(cl:defun add-point (p)
  (let ((sup (locate (cdr (name p)))))
    (when sup
      (setq p (copy-tree p))
      (setf (subs sup)
	    (nconc (subs sup) (list p)))
      (setf (id p) (incf *count*))
      (dolist (p (subs p))
	(setf (id p) (incf *count*))))))

(defvar *line-limit* 75)
(defvar *report-readable-branches* nil
  "When true, causes COVER:REPORT to print the branch information readably.
This can be annoying when the symbols are not in the current package.")

(declaim (special *depth* *all*
		  *out* *done*))
(declaim (special *subs* *sup*))

(cl:defun report
            (&key (fn nil)
		  (out *standard-output*)
		  (all nil))
 (let (p)
  (cond
   ((not (streamp out))
    (with-open-file
	(s out :direction :output)
      (report :fn fn :all all :out s)))
    ((null *points*)
     (format out
       "~%No definitions annotated."))
    ((not fn)
     (report1 *points* all out))
    ((setq p (find-point fn))
     (report1 (list p) all out))
    (t (format out "~%~A is not annotated."
	       fn))))
  (values))

(cl:defun fn-name (p)
  (let ((form (cadr (car (name p)))))
    (and (consp form)
	 (consp (cdr form))
	 (cadr form))))

(cl:defun report1 (ps *all* *out*)
  (let ((*depth* 0) (*done* t))
    (mapc #'report2 ps)
    (when *done*
      (format *out*
	"~%;All points exercised."))))

(cl:defun report2 (p)
  (case (status p)
    (:forgotten nil)
    (:hidden (mapc #'report2 (subs p)))
    (:show
     (cond ((reportable-subs p)
	    (report3 p)
	    (let ((*depth* (1+ *depth*)))
	      (mapc #'report2 (subs p))))
	   ((reportable p)
	    (report3 p))))))

(cl:defun reportable (p)
  (and (eq (status p) :show)
       (or *all*
	   (not (= (hit p) *hit*)))))

(cl:defun reportable-subs (p)
  (and (not (eq (status p) :forgotten))
       (or *all* (not (reportable p)))
       (some #'(lambda (s)
		 (or (reportable s)
		     (reportable-subs s)))
	     (subs p))))

(cl:defun report3 (p)
  (setq *done* nil)
  (let* ((*print-pretty* nil)
	 (*print-level* 3)
	 (*print-length* nil)
	 (ffn (if *report-readable-branches*
		  (formatter ";~V@T~:[-~;+~]~{ ~S~}")
		  (formatter ";~V@T~:[-~;+~]~{ ~A~}")))
	 (m (format nil
		    ffn
		    *depth*
		    (= (hit p) *hit*)
		    (car (name p))))
	 (limit (- *line-limit* 8)))
    (when (> (length m) limit)
      (setq m (subseq m 0 limit)))
    (format *out* "~%~A  <~S>" m (id p))))

(cl:defmacro annotate (t-or-nil)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (annotate1 ,t-or-nil)))

;;; We shadow IN-PACKAGE so these forms themselves
;;; can instrument a package, rather than having to
;;; add ANNOTATE forms to each Lisp file.

(cl:defmacro in-package (name)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (let ((package (cl:in-package ,name)))
       (when *annotating*
	 (do-shadowing-import package))
       package)))

;;; COVER permanently shadows certain defining macro symbols.
;;; When *ANNOTATING* is false, these default back to the
;;; standard behavior.  However, the symbols are still the
;;; shadowed ones, not the originals.  IN-PACKAGE itself
;;; becomes shadowed so that subsequence IN-PACKAGE forms
;;; in a file can themselves instrument things.

(cl:defun annotate1 (flag)
  (do-shadowing-import *package*)
  (when (and flag (not *testing*))
    (warn "Coverage annotation applied."))
  (setq *annotating* (not (null flag))))

(cl:defun do-shadowing-import (package)
  "Add the shadowed symbols provided by COVER to the indicated
package."
  (shadowing-import
   (set-difference '(defun defmacro defmethod defgeneric in-package)
     (package-shadowing-symbols package))
   package))

(cl:defun cover-compile-hook (thunk)
  "Used in ASDF to enable coverage in a package being compiled"
  (let ((*package* (find-package '#:cover))
	(cgs (copy-cgs *cgs*)))
    (setf (cgs-annotating cgs) t)
    (let ((*cgs* cgs))
      (funcall thunk))))

(cl:defmacro defun (n argl &body b)
  (process 'defun 'cl:defun n argl b))

(cl:defmacro defmacro (n a &body b)
  (process 'defmacro 'cl:defmacro n a b))

(cl:defmacro defmethod (n &rest method-body)
  (let* ((method-qualifiers (loop while (not (listp (car method-body)))
			       collect (pop method-body)))
	 (specialized-lambda-list (pop method-body))
	 (name (cons n (append method-qualifiers
			       (name-part-from-specialized-lambda-list specialized-lambda-list))))
	 )
    ;; method-body now is docstrings + declarations + actual body
    (process1 'defmethod 'cl:defmethod (list n) name
	      (append method-qualifiers (list specialized-lambda-list))
	      method-body)))

(cl:defmacro defgeneric (&whole whole n &rest defgeneric-body)
  (flet ((%assert (p) (unless p (error "Not a valid defgeneric form: ~a" whole))))
    (%assert (consp defgeneric-body))
    (%assert (listp (car defgeneric-body)))
    (%assert (null (cdr (last defgeneric-body))))
    (let ((options-and-methods (cdr defgeneric-body)))
      (%assert (every #'consp options-and-methods))
      (let* ((point-forms nil)
	     (processed-options-and-methods
	      (loop for om in options-and-methods
		 collect (if (eql (car om) :method)
			     (multiple-value-bind (point-form def-form)
				 (process-defgeneric-method n om)
			       (push point-form point-forms)
			       def-form)
			     om))))
	(if point-forms
	    `(eval-when (:compile-toplevel :load-toplevel :execute)
	       ,@(reverse point-forms)
	       (cl:defgeneric ,n ,(car defgeneric-body)
		 ,@processed-options-and-methods))
	    `(cl:defgeneric ,n ,@defgeneric-body))))))

(cl:defun process-defgeneric-method (n om)
  "Process a :method option from a defgeneric form for function named N.
   Returns the form for defining the point and the revised method description"
  (assert (eql (pop om) :method))
  (let* ((method-qualifiers (loop while (not (listp (car om)))
			       collect (pop om)))
	 (specialized-lambda-list (pop om))
	 (name (cons n (append method-qualifiers
			       (name-part-from-specialized-lambda-list specialized-lambda-list)))))
    (if (not (or *annotating* (find-point name)))
	(values nil om)
	(process2 :method :method nil name
		  (append method-qualifiers (list specialized-lambda-list))
		  om))))

(cl:defun name-part-from-specialized-lambda-list (specl)
  "List of the specializer parts from a specialized lambda list"
  (loop for spec in specl
     until (member spec lambda-list-keywords)
     collect (typecase spec
	       (symbol t)
	       ((cons symbol (cons symbol null))
		(cadr spec))
	       ((cons symbol (cons class null))
		(class-name (cadr spec)))
	       ((cons symbol (cons (cons (eql 'eql) (cons t null)) null))
		(cadr spec))
	       (t (error "Invalid parameter specializer: ~A" spec)))))

(cl:defun parse-body (body)
  (let ((decls nil))
    (when (stringp (car body))
      (push (pop body) decls))
    (loop (unless (and (consp (car body))
		       (eq (caar body)
			   'declare))
	    (return nil))
	  (push (pop body) decls))
    (values (nreverse decls) body)))

(defvar *check*
  '((or . c-or) (and . c-and)
    (if . c-if) (when . c-when)
    (unless . c-unless)
    (cond . c-cond) (case . c-case)
    (typecase . c-typecase)))

(cl:defun process (cdef def fn argl b)
  (process1 cdef def (list fn) fn (list argl) b))

(cl:defun process1 (cdef def fnl fn-extra argll b)
  ;; FN is the bare function name
  ;; FN-EXTRA is that name, plus (in the case of methods)
  ;;   extra information to distinguish this method from
  ;;   others (method qualifiers and method arg qualifiers)
  (if (not (or *annotating* (find-point fn-extra)))
      `(,def ,@fnl ,@argll ., b)
      (multiple-value-bind (point-form def-form)
	  (process2 cdef def fnl fn-extra argll b)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   ,point-form
	   ,def-form))))

(cl:defun instrument-forms (forms)
  "Function that replaces various symbols with their instrumented
versions.  This will be expanded later because some macros need
to customize it.  For example, ITERATE depends on certain symbols
being present and breaks if they are blindly replaced."
  (sublis *check* forms))

(cl:defun process2 (cdef def fnl fn-extra argll b)
  (multiple-value-bind (decls b)
      (parse-body b)
    (setq b (instrument-forms b))
    (let ((name
	   `((:reach
	      (,cdef ,fn-extra ,.argll)))))
      (values
       `(add-top-point ',(make-point :name name))
       `(,def ,@fnl ,@argll ,@ decls
	      ,(c0 (make-point :name
			       name)
		   name b))))))

(defvar *fix*
  '((c-or . or) (c-and . and) (c-if . if)
    (c-when . when) (c-unless . unless)
    (c-cond . cond) (c-case . case)
    (c-typecase . typecase)))

(cl:defmacro sup-mac () nil)

(cl:defmacro def (name args form)
  `(cl:defmacro ,name (&whole w ,@ args
			 &environment env)
    (let* ((*subs* nil)
	   (*sup*
	    `((:reach ,(sublis *fix* w))
	      .,(macroexpand-1
		 (list 'sup-mac) env)))
	   (p (make-point :name *sup*))
	   (form ,form))
      (setf (subs p) (nreverse *subs*))
      (c0 p *sup* (list form)))))

(cl:defmacro c (body &rest msg)
  (c1 `(list ,body) msg :show))

(cl:defmacro c-hide (b)
  (c1 `(list ,b) (list :reach b) :hidden))
	    
(eval-when (:compile-toplevel :load-toplevel :execute)

(cl:defun c1 (b m s)
  `(let ((n (cons (sublis *fix*
			  (list .,m))
		  *sup*)))
    (push (make-point :name n :status ,s)
          *subs*)
    (c0 (make-point :name n :status ,s)
        n ,b)))

(cl:defun c0 (p sup b)
  `(macrolet ((sup-mac () ',sup))
     (record-hit ',p)
     .,b)) )

(def c-case (key &rest cs)
  `(case ,(c-hide key)
     .,(c-case0 cs)))

(def c-typecase (key &rest cs)
  `(typecase ,(c-hide key)
     .,(c-case0 cs)))

(cl:defun c-case0 (cs)
  (let ((stuff (mapcar #'c-case1 cs)))
    (when (not (member (caar (last cs))
		       '(t otherwise)))
      (setq stuff
	(nconc stuff
	  `((t ,(c nil :select-none))))))
    stuff))

(cl:defun c-case1 (clause)
  `(,(car clause)
    ,(c `(progn ., (cdr clause)) :select
         (car clause))))

(def c-if (pred then &optional (else nil))
  `(if ,(c-hide pred)
       ,(c then :non-null pred)
       ,(c else :null pred)))

(def c-when (pred &rest actions)
  `(if ,(c-hide pred)
       ,(c `(progn ., actions)
	    :non-null pred)
       ,(c nil :null pred)))

(def c-unless (pred &rest actions)
  `(if (not ,(c-hide pred))
       ,(c `(progn ., actions) :null pred)
       ,(c nil :non-null pred)))

(def c-cond (&rest cs)
  (c-cond0 (gensym) cs))

(cl:defun c-cond0 (var cs)
  (cond ((null cs) (c nil :all-null))
	((eq (caar cs) t)
	 (c (if (cdar cs)
		`(progn .,(cdar cs))
		t)
	    :first-non-null t))
	((cdar cs)
	 `(if ,(c-hide (caar cs))
	   ,(c `(progn .,(cdar cs))
	       :first-non-null
	       (caar cs))
	   ,(c-cond0 var (cdr cs))))
	(t `(let ((,var
		   ,(c-hide (caar cs))))
	     (if ,var
		 ,(c var :first-non-null
		     (caar cs))
		 ,(c-cond0 var
			   (cdr cs)))))))

(def c-or (&rest ps) (c-or0 ps))

(cl:defun c-or0 (ps)
  (if (null (cdr ps))
      (c (car ps) :eval-all (car ps))
      (let ((var (gensym)))
	`(let ((,var ,(c-hide (car ps))))
	  (if ,var
	      ,(c var :first-non-null
		  (car ps))
	      ,(c-or0 (cdr ps)))))))

(def c-and (&rest ps)
  `(cond .,(maplist #'c-and0
	            (or ps (list t)))))

(cl:defun c-and0 (ps)
  (if (null (cdr ps))
      `(t ,(c (car ps) :eval-all (car ps)))
      `((not ,(c-hide (car ps)))
	,(c nil :first-null (car ps)))))

(deftype c-and (&rest b) `(and ., b))

(deftype c-or (&rest b) `(or ., b))

#|----------------------------------------------------------------------------|
 | Copyright 1991 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#
