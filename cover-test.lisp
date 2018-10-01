;-*-syntax:COMMON-LISP; Mode: LISP-*-

(in-package "COMMON-LISP-USER")

;This is the September 15, 1991 version of
;the testing file for Richard Waters' test case coverage checker

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

(setq cover::*testing* t)
(cover:annotate nil)

;The first few pages are a copy of Richard Waters RT testing program
;see R.C. Waters, "Supporting the Regression Testing of Lisp Programs",
;       ACM Lisp Pointers, 4(2):47--53, June 1991.

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar test-file nil "temp file for testing")

(defstruct (entry (:conc-name nil)
		  (:type list))
  pend name form)

(defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l)) 
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~@:(~S~)"
        (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

(defun get-temporary-file ()
  (uiop:with-temporary-file (:pathname pn :keep t)
    pn))

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun do-entry (entry &optional
			 (s *standard-output*))
  (unless test-file (setf test-file (get-temporary-file)))
;;  (loop (if test-file (return nil))
;;    (format T "~%Type a string representing a pathname of a scratch disk file: ")
;;    (setq test-file (read))
;;    (if (not (stringp test-file)) (setq test-file nil)))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   ;; (*break-on-warnings* t)
	   (r (multiple-value-list
		(eval (form entry)))))
      (setf (pend entry)
	    (not (equal r (vals entry))))
      (when (pend entry)
	(format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~
                   ~%Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
		*test* (form entry)
		(length (vals entry))
		(vals entry)
		(length r) r))))
      (when (not (pend entry)) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
		   (out *standard-output*))
  (unless test-file (setf test-file (get-temporary-file)))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file 
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (cdr *entries*)
		 :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (pend entry)
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests)))
    (if (null pending)
	(format s "~&No tests failed.")
	(format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		(length pending)
		(length (cdr *entries*))
		pending))
    (null pending)))

;The following are tests of the coverage checker itself.

(rem-all-tests)

;These tests check that the coverage checking of individual forms does not
;alter the values they return.

(defmacro ck (form)
  `(do-ck ',form))

(defun maybe-compile (fn)
  (unless (compiled-function-p (symbol-function fn))
    (compile fn)))

(defun do-ck (form)
  (setq form `(multiple-value-list ,form))
  (eval '(cover:annotate nil))
  (let* ((def `(defun interp-fn () ,(copy-tree form)))
	 (interp-value (progn (cover:forget-all) (eval '(cover:annotate t))
			      (eval def) (cover:annotate nil)
			      (eval '(interp-fn))))
	 (interp-report (cdr (condensed-report :all t)))
	 (def2 `(defun compiled-fn () ,(copy-tree form)))
	 (compiled-value (progn (cover:forget-all) (eval '(cover:annotate t))
				(eval def2) (cover:annotate nil) 
                                (maybe-compile 'compiled-fn)
				(eval '(compiled-fn))))
	 (compiled-report (cdr (condensed-report :all t))))
    (dolist (l interp-report)
      (decf (car l)))
    (dolist (l compiled-report)
      (decf (car l)))
    (when (not (equal interp-report compiled-report))
      (setq interp-report `("interpreted and compiled reports disagree"
			    ,interp-report ,compiled-report)))
    (when (not (equal interp-value compiled-value))
      (setq interp-value
	    `("interpreted, and compiled values fail to agree"
	      ,interp-value ,compiled-value)))
    (values-list (cons interp-report interp-value))))	 

(defun condensed-report (&rest args)
  (let* ((cover:*line-limit* 100)
	 (report (with-output-to-string (s)
		  (apply #'cover:report :out s args)))
	(items nil))
    (with-input-from-string (s report)
      (read-line s nil nil)
      (loop (let* ((line (read-line s nil nil)))
	      (if (null line) (return nil))
	      (let ((pos (position-if #'(lambda (c)
					  (or (char= c #\-) (char= c #\+)))
				      line)))
		(with-input-from-string (s line :start pos)
		  (let ((+- (read s nil nil))
			(type (read s nil nil))
			(c (peek-char t s nil nil))
			(code nil))
		    (when (and c (not (char= c #\<)))
		      (when (char= c  #\()
			(read-char s nil nil))
		      (setq code (read s nil nil)))
		    (push (list pos +- type code) items)))))))
    (nreverse items)))

(deftest if1 (ck (if (plusp 2) (values 1 2) (values 3 4)))
  ((1 + :REACH IF) (2 + :NON-NULL PLUSP) (2 - :NULL PLUSP))
  1 2)
(deftest if2 (ck (if (plusp -2) (values 1 2) (values 3 4)))
  ((1 + :REACH IF) (2 - :NON-NULL PLUSP) (2 + :NULL PLUSP))
  3 4)
(deftest if3 (ck (if (identity (and (plusp -2) t)) (values 1 2) (values 3 4)))
  ((1 + :REACH IF)
   (2 + :REACH AND)
   (3 + :FIRST-NULL PLUSP)
   (3 - :EVAL-ALL T)
   (2 - :NON-NULL IDENTITY)
   (2 + :NULL IDENTITY))
  3
  4)
(deftest if4 (ck (if (if (plusp -2) t) (values 1 2) (values 3 4)))
  ((1 + :REACH IF)
   (2 + :REACH IF)
   (3 - :NON-NULL PLUSP)
   (3 + :NULL PLUSP)
   (2 - :NON-NULL IF)
   (2 + :NULL IF))
  3
  4)

(deftest when1 (ck (when (plusp 2) (values 1 2) (values 3 4)))
  ((1 + :REACH WHEN) (2 + :NON-NULL PLUSP) (2 - :NULL PLUSP))
  3 4)
(deftest when2 (ck (when (plusp -2) (values 1 2) (values 3 4)))
  ((1 + :REACH WHEN) (2 - :NON-NULL PLUSP) (2 + :NULL PLUSP))
  nil)
(deftest when3 (ck (when (and t (plusp -2)) (values 1 2) (values 3 4)))
  ((1 + :REACH WHEN)
   (2 + :REACH AND)
   (3 - :FIRST-NULL T)
   (3 + :EVAL-ALL PLUSP)
   (2 - :NON-NULL AND)
   (2 + :NULL AND))
  nil)

(deftest unless1 (ck (unless (plusp 2) (values 1 2) (values 3 4)))
  ((1 + :REACH UNLESS) (2 - :NULL PLUSP) (2 + :NON-NULL PLUSP))
  nil)
(deftest unless2 (ck (unless (plusp -2) (values 1 2) (values 3 4)))
  ((1 + :REACH UNLESS) (2 + :NULL PLUSP) (2 - :NON-NULL PLUSP))
  3 4)
(deftest unless3 (ck (unless (or (plusp -2) nil) (values 1 2) (values 3 4)))
  ((1 + :REACH UNLESS)
   (2 + :REACH OR)
   (3 - :FIRST-NON-NULL PLUSP)
   (3 + :EVAL-ALL NIL)
   (2 + :NULL OR)
   (2 - :NON-NULL OR))
  3 4)

(deftest cond0 (ck (cond))
  ((1 + :REACH COND) (2 + :ALL-NULL NIL))
  NIL)
(deftest cond1 (ck (cond ((plusp 2) (values 1 2)) ((values 3 4))))
  ((1 + :REACH COND) (2 + :FIRST-NON-NULL PLUSP)
   (2 - :FIRST-NON-NULL VALUES) (2 - :ALL-NULL NIL))
  1 2)
(deftest cond2 (ck (cond ((plusp -2) (values 1 2)) ((values 3 4))))
  ((1 + :REACH COND) (2 - :FIRST-NON-NULL PLUSP)
   (2 + :FIRST-NON-NULL VALUES) (2 - :ALL-NULL NIL))
  3)
(deftest cond3 (ck (cond ((plusp -2) (values 1 2)) ((values nil 4))))
  ((1 + :REACH COND) (2 - :FIRST-NON-NULL PLUSP)
   (2 - :FIRST-NON-NULL VALUES) (2 + :ALL-NULL NIL))
  nil)
(deftest cond4 (ck (cond ((plusp -2) (values 1 2)) ((values nil 4)) (T nil)))
  ((1 + :REACH COND) (2 - :FIRST-NON-NULL PLUSP)
   (2 - :FIRST-NON-NULL VALUES) (2 + :FIRST-NON-NULL T))
  nil)
(deftest cond5 (ck (cond ((plusp -2) (values 1 2)) ((values nil 4)) (T)))
  ((1 + :REACH COND) (2 - :FIRST-NON-NULL PLUSP)
   (2 - :FIRST-NON-NULL VALUES) (2 + :FIRST-NON-NULL T))
  t)
(deftest cond6 (ck (cond ((and (plusp 2) t) (values 1 2)) ((values 3 4))))
  ((1 + :REACH COND)
   (2 + :REACH AND)
   (3 - :FIRST-NULL PLUSP)
   (3 + :EVAL-ALL T)
   (2 + :FIRST-NON-NULL AND)
   (2 - :FIRST-NON-NULL VALUES)
   (2 - :ALL-NULL NIL))
  1 2)
(deftest cond7 (ck (cond ((plusp -2) 1) ((and (plusp 3) t) 2)))
  ((1 + :REACH COND)
   (2 - :FIRST-NON-NULL PLUSP)
   (2 + :REACH AND)
   (3 - :FIRST-NULL PLUSP)
   (3 + :EVAL-ALL T)
   (2 + :FIRST-NON-NULL AND)
   (2 - :ALL-NULL NIL))
  2)
(deftest cond8 (ck (cond ((plusp -2) 1) (nil) ((and (plusp 3) t))))
  ((1 + :REACH COND)
   (2 - :FIRST-NON-NULL PLUSP)
   (2 - :FIRST-NON-NULL NIL)
   (2 + :REACH AND)
   (3 - :FIRST-NULL PLUSP)
   (3 + :EVAL-ALL T)
   (2 + :FIRST-NON-NULL AND)
   (2 - :ALL-NULL NIL))
  t)
(deftest cond9 (ck (cond ((plusp -2) 1) ((and (plusp 3) t))))
  ((1 + :REACH COND)
   (2 - :FIRST-NON-NULL PLUSP)
   (2 + :REACH AND)
   (3 - :FIRST-NULL PLUSP)
   (3 + :EVAL-ALL T)
   (2 + :FIRST-NON-NULL AND)
   (2 - :ALL-NULL NIL))
  t)

(deftest case1 (ck (case 1 (1 (values 10 11)) (2)))
  ((1 + :REACH CASE) (2 + :SELECT 1)
   (2 - :SELECT 2) (2 - :SELECT-NONE nil))
  10 11)
(deftest case2 (ck (case 2 (1 (values 10 11)) (2)))
  ((1 + :REACH CASE) (2 - :SELECT 1)
   (2 + :SELECT 2) (2 - :SELECT-NONE nil))
  nil)
(deftest case3 (ck (case 3 (1 (values 10 11)) (2)))
  ((1 + :REACH CASE) (2 - :SELECT 1)
   (2 - :SELECT 2) (2 + :SELECT-NONE nil))
  nil)
(deftest case4 (ck (case 3 (1 (values 10 11)) (2) (T T)))
  ((1 + :REACH CASE) (2 - :SELECT 1)
   (2 - :SELECT 2) (2 + :SELECT T))
  T)
(deftest case5 (ck (case (when t 1) (1 (values 10 11)) (2)))
  ((1 + :REACH CASE)
   (2 + :REACH WHEN)
   (3 + :NON-NULL T)
   (3 - :NULL T)
   (2 + :SELECT 1)
   (2 - :SELECT 2)
   (2 - :SELECT-NONE NIL))
  10 11)

(deftest typecase1 (ck (typecase 1 (integer (values 10 11)) (string)))
  ((1 + :REACH TYPECASE) (2 + :SELECT INTEGER)
   (2 - :SELECT STRING) (2 - :SELECT-NONE NIL))
  10 11)
(deftest typecase2 (ck (typecase "foo" (integer (values 10 11)) (string)))
  ((1 + :REACH TYPECASE) (2 - :SELECT INTEGER)
   (2 + :SELECT STRING) (2 - :SELECT-NONE NIL))
  nil)
(deftest typecase3 (ck (typecase #\F (integer (values 10 11)) (string)))
  ((1 + :REACH TYPECASE) (2 - :SELECT INTEGER)
   (2 - :SELECT STRING) (2 + :SELECT-NONE NIL))
  nil)
(deftest typecase4 (ck (typecase #\F
			  (integer (values 10 11))
			  (string)
			  (otherwise t)))
  ((1 + :REACH TYPECASE) (2 - :SELECT INTEGER)

   (2 - :SELECT STRING) (2 + :SELECT OTHERWISE))
  T)
(deftest typecase5 (ck (typecase (unless nil 1)
			  (integer (values 10 11)) (string)))
  ((1 + :REACH TYPECASE)
   (2 + :REACH UNLESS)
   (3 + :NULL NIL)
   (3 - :NON-NULL NIL)
   (2 + :SELECT INTEGER)
   (2 - :SELECT STRING)
   (2 - :SELECT-NONE NIL))
  10 11)

(deftest and0 (ck (and))
  ((1 + :REACH AND) (2 + :EVAL-ALL T))
  T)
(deftest and1 (ck (and (plusp 1) (values 2 3)))
  ((1 + :REACH AND) (2 - :FIRST-NULL PLUSP) (2 + :EVAL-ALL VALUES))
  2 3)
(deftest and2 (ck (and (plusp -1) (values 2 3)))
  ((1 + :REACH AND) (2 + :FIRST-NULL PLUSP) (2 - :EVAL-ALL VALUES))
  nil)
(deftest and3 (ck (and (plusp 1) (minusp 1)))
  ((1 + :REACH AND) (2 - :FIRST-NULL PLUSP) (2 + :EVAL-ALL MINUSP))
  nil)
(deftest and4 (ck (and (or (plusp 1) (plusp 2)) 3))
  ((1 + :REACH AND)
   (2 + :REACH OR)
   (3 + :FIRST-NON-NULL PLUSP)
   (3 - :EVAL-ALL PLUSP)
   (2 - :FIRST-NULL OR)
   (2 + :EVAL-ALL 3))
  3)
(deftest and5 (ck (and 3 (or (plusp 1) (plusp 2))))
  ((1 + :REACH AND)
   (2 - :FIRST-NULL 3)
   (2 + :EVAL-ALL OR)
   (3 + :REACH OR)
   (4 + :FIRST-NON-NULL PLUSP)
   (4 - :EVAL-ALL PLUSP))
  t)

(deftest or0 (ck (or))
  ((1 + :REACH OR) (2 + :EVAL-ALL NIL))
  NIL)
(deftest or1 (ck (or (plusp 1) (values 2 3)))
  ((1 + :REACH OR) (2 + :FIRST-NON-NULL PLUSP) (2 - :EVAL-ALL VALUES))
  t)
(deftest or2 (ck (or (plusp -1) (values 2 3)))
  ((1 + :REACH OR) (2 - :FIRST-NON-NULL PLUSP) (2 + :EVAL-ALL VALUES))
  2 3)
(deftest or3 (ck (or (plusp -1) (minusp 1)))
  ((1 + :REACH OR) (2 - :FIRST-NON-NULL PLUSP) (2 + :EVAL-ALL MINUSP))
  nil)
(deftest or4 (ck (or (plusp -1) (and (plusp 2) 3)))
  ((1 + :REACH OR)
   (2 - :FIRST-NON-NULL PLUSP)
   (2 + :EVAL-ALL AND)
   (3 + :REACH AND)
   (4 - :FIRST-NULL PLUSP)
   (4 + :EVAL-ALL 3))
  3)

;The next group of tests check the basic interaction of the tracking of cases.

(defmacro tr (&body body)
  `(progn (cover:forget-all)
          (eval '(cover:annotate t))
          (defun eval-exp (form)
	    "a test function"
	    (cond ((numberp form) form)
		  ((symbolp form) (symbol-value form))
		  ((consp form) (eval-non-atomic form))
		  (T form)))
          (defun eval-non-atomic (form)
	    "another test function"
	    (declare (type list form))
	    (case (car form)
	      (+ (+ (eval-exp (cadr form)) (eval-exp (caddr form))))
	      (* (* (eval-exp (cadr form)) (eval-exp (caddr form))))
	      (otherwise (apply (symbol-function (car form))
				(mapcar #'eval-exp (cdr form))))))
          (eval-exp 1)
    (let ((*print-case* :upcase) (*print-base* 10.) (*print-radix* nil))
      .,body)))

(deftest hit1 (tr (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL SYMBOLP)
   (3 - :FIRST-NON-NULL CONSP)
   (3 - :FIRST-NON-NULL T)
   (1 - :REACH DEFUN)))
(deftest hit2 (tr (let ((x 3)) (declare (special x)) (eval-exp 'x))
		  (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL CONSP)
   (3 - :FIRST-NON-NULL T)
   (1 - :REACH DEFUN)))
(deftest hit3 (tr (eval-exp "foo") (eval-exp '(+ 1 2)) (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL SYMBOLP)
   (1 + :REACH DEFUN)
   (2 + :REACH CASE)
   (3 - :SELECT *)
   (3 - :SELECT OTHERWISE)))
(deftest hit4 (tr (eval-exp '(list "foo" (* 1 2))) (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL SYMBOLP)
   (1 + :REACH DEFUN)
   (2 + :REACH CASE)
   (3 - :SELECT +)))
(deftest hit5 (tr (eval-exp '(list "foo" (+ 1 1) (* 1 2))) (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL SYMBOLP)))
(deftest hit6 (tr (defun eval-exp (x) (when (plusp x) (list x)))
		  (condensed-report))
  ((1 - :REACH DEFUN) (1 - :REACH DEFUN)))
(deftest hit7 (tr (defun eval-exp (x) (when (plusp x) (list x)))
		  (eval-exp 1)
		  (condensed-report))
  ((1 + :REACH DEFUN) (2 + :REACH WHEN) (3 - :NULL PLUSP) (1 - :REACH DEFUN)))
(deftest hit8 (tr (defun eval-exp (form)
		    "a test function"
		    (cond ((numberp form) form)
			  ((symbolp form) (symbol-value form))
			  ((consp form) (eval-non-atomic form))
			  (T form)))
		  (condensed-report))
  ((1 - :REACH DEFUN) (1 - :REACH DEFUN)))
(deftest hit9 (tr (defun eval-exp (form)
		    "a test function"
		    (cond ((numberp form) form)
			  ((symbolp form) (symbol-value form))
			  ((consp form) (eval-non-atomic form))
			  (T form)))
		  (eval-exp "foo")
		  (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 - :FIRST-NON-NULL NUMBERP)
   (3 - :FIRST-NON-NULL SYMBOLP)
   (3 - :FIRST-NON-NULL CONSP)
   (1 - :REACH DEFUN)))

(deftest reset1 (tr (values (cover:reset) (condensed-report)))
  t ((1 - :REACH DEFUN) (1 - :REACH DEFUN)))

(deftest forget-all1 (tr (values (cover:forget-all)
				 (defun f1 (x) x) (condensed-report)))
  T F1 ((1 - :REACH DEFUN)))
(deftest forget-all2 (tr (values (cover:forget-all)
				 (defun f1 (x) x)
				 (eval-exp 3)
				 (condensed-report)))
  T F1 3 ((1 - :REACH DEFUN)))
(deftest forget-all3 (tr (values (cover:forget-all) (cover:reset) (eval-exp 1)
				 (defun f1 (x) x) (eval-exp '(+ 1 2))
				 (condensed-report)))
  T T 1 F1 3 ((1 - :REACH DEFUN)))

(deftest forget1 (tr (values (cover:forget) (condensed-report)))
  t ((1 + :REACH DEFUN)
     (2 + :REACH COND)
     (3 - :FIRST-NON-NULL SYMBOLP)
     (3 - :FIRST-NON-NULL CONSP)
     (3 - :FIRST-NON-NULL T)
     (1 - :REACH DEFUN)))
(deftest forget2 (tr (values (cover:forget 1) (condensed-report)))
  t ((1 - :REACH DEFUN)))
(deftest forget3 (tr (values (cover:forget 1)
			     (defun eval-exp (form)
			       "a test function"
			       (cond ((numberp form) form)
				     ((symbolp form) (symbol-value form))
				     ((consp form) (eval-non-atomic form))
				     (T form)))
			     (condensed-report)))
  t eval-exp ((1 - :REACH DEFUN)(1 - :REACH DEFUN)))

(deftest forget4 (tr (values (cover:forget 7 9) (condensed-report)))
  t ((1 + :REACH DEFUN)
     (2 + :REACH COND)
     (3 - :FIRST-NON-NULL T)
     (1 - :REACH DEFUN)))
(deftest forget5 (tr (values (cover:forget 7 9 10) (condensed-report)))
  t ((1 - :REACH DEFUN)))
(deftest forget6 (tr (values (cover:forget 3 44) (condensed-report)))
  t ((1 - :REACH DEFUN)))

(deftest report1 (tr (let ((cover:*line-limit* 50))
		       (with-output-to-string (s) (cover:report :out s)))) "
;+ :REACH (DEFUN EVAL-EXP (FORM))  <1>
; + :REACH (COND ((NUMBERP FORM) FORM) ((S  <3>
;  - :FIRST-NON-NULL (SYMBOLP FORM)  <7>
;  - :FIRST-NON-NULL (CONSP FORM)  <9>
;  - :FIRST-NON-NULL T  <10>
;- :REACH (DEFUN EVAL-NON-ATOMIC (FORM))  <2>")
(deftest report2 (tr (let ((cover:*line-limit* 30))
		       (with-output-to-string (s) (cover:report :out s)))) "
;+ :REACH (DEFUN EVAL-  <1>
; + :REACH (COND ((NUM  <3>
;  - :FIRST-NON-NULL (  <7>
;  - :FIRST-NON-NULL (  <9>
;  - :FIRST-NON-NULL T  <10>
;- :REACH (DEFUN EVAL-  <2>")
(deftest report3 (tr (let ((cover:*line-limit* 100))
		       (with-output-to-string (s)
			 (cover:report :fn 'eval-non-atomic :out s)))) "
;- :REACH (DEFUN EVAL-NON-ATOMIC (FORM))  <2>")
(deftest report4 (tr (let ((cover:*line-limit* 100))
		       (with-output-to-string (s)
			 (cover:report :fn 'not-there :out s)))) "
NOT-THERE is not annotated.")
(deftest report5 (tr (eval-exp '(list "foo" (+ 1 1) (* 1 2)))
		     (let ((cover:*line-limit* 100))
		       (with-output-to-string (s)
			 (cover:report :fn 'eval-non-atomic :out s)))) "
;All points exercised.")
(deftest report6 (tr (cover:forget-all)
		     (let ((cover:*line-limit* 100))
		       (with-output-to-string (s)
			 (cover:report :fn 'eval-non-atomic :out s)))) "
No definitions annotated.")
(deftest report7 (tr (let ((cover:*line-limit* 50) (stuff nil))
		       (if (probe-file test-file) (delete-file test-file))
		       (cover:report :out test-file)
		       (with-open-file (s test-file :direction :input)
			 (loop (let ((line (read-line s nil nil)))
				 (when (null line) (return nil))
				 (push line stuff)
				 (push "
" stuff))))
		       (if (probe-file test-file) (delete-file test-file))
		       (apply #'concatenate 'string (nreverse (cdr stuff))))) "
;+ :REACH (DEFUN EVAL-EXP (FORM))  <1>
; + :REACH (COND ((NUMBERP FORM) FORM) ((S  <3>
;  - :FIRST-NON-NULL (SYMBOLP FORM)  <7>
;  - :FIRST-NON-NULL (CONSP FORM)  <9>
;  - :FIRST-NON-NULL T  <10>
;- :REACH (DEFUN EVAL-NON-ATOMIC (FORM))  <2>")
(deftest report8 (tr (defun eval-exp (x) (when (plusp x) (list x)))
		     (let ((cover:*line-limit* 100))
		       (with-output-to-string (s)
			 (cover:report :out s)))) "
;- :REACH (DEFUN EVAL-EXP (X))  <1>
;- :REACH (DEFUN EVAL-NON-ATOMIC (FORM))  <2>")

;The next group of tests check various fine points

(cl:defmacro tx (&body body) ;so will work no matter what cover does!
  `(progn (cover:forget-all)
	  (eval '(cover:annotate nil))
	  (defmacro maybe+ (x y)
	    `(if (numberp ,x) (+ ,x ,y)))
	  (eval '(cover:annotate t))
	  (defmacro maybe- (x y)
	    `(if (numberp ,x) (- ,x ,y)))
	  (defun g (x y)
	    (cond ((and (null x) y) y)
		  (y (case y
		       (1 (maybe- x y))
		       (2 (maybe+ x y))))))
	  (eval '(cover:annotate nil))
	  (defun h (x y) (list x y))
    .,body))

(deftest macro1 (tx (g 1 1) (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 + :REACH AND)
   (4 - :EVAL-ALL Y)
   (3 - :FIRST-NON-NULL AND)
   (3 + :FIRST-NON-NULL Y)
   (4 + :REACH CASE)
   (5 + :SELECT 1)
   (6 + :REACH IF)
   (7 - :NULL NUMBERP)
   (5 - :SELECT 2)
   (5 - :SELECT-NONE NIL)
   (3 - :ALL-NULL NIL)))
(deftest macro2 (tx (g nil 4) (g 1 1) (g 2 2) (condensed-report))
  ((1 + :REACH DEFUN)
   (2 + :REACH COND)
   (3 + :FIRST-NON-NULL Y)
   (4 + :REACH CASE)
   (5 + :SELECT 1)
   (6 + :REACH IF)
   (7 - :NULL NUMBERP)
   (5 - :SELECT-NONE NIL)
   (3 - :ALL-NULL NIL)))

;This tests that the interpreted checking works right with multiple
;expansions.  (or at least is does on common lisps that do multiple
;expansions.) and that we do not get choice points for conditions
;generated by system macros.

(deftest looping1 (ck (do ((i -1 (1+ i))) (nil) (if (plusp i) (return i))))
  ((1 + :REACH IF)
   (2 + :NON-NULL PLUSP)
   (2 + :NULL PLUSP))
  1)

;this tests the lexical scoping nature of cover.

(deftest lexical1 (ck (let ((i 1))
			(declare (special i))
			(when (plusp i) (incf i))
		        (if (evenp i) (list i))))
  ((1 + :REACH WHEN)
   (2 + :NON-NULL PLUSP)
   (2 - :NULL PLUSP)
   (1 + :REACH IF)
   (2 + :NON-NULL EVENP)
   (2 - :NULL EVENP))
  (2))
(deftest lexical2 (ck (let ((i 1))
			(declare (special i))
			(when (plusp i) (incf i))
			(eval '(if (evenp i) (list i)))))
  ((1 + :REACH WHEN)
   (2 + :NON-NULL PLUSP)
   (2 - :NULL PLUSP))
  (2))

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
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#
