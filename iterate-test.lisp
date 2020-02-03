(defpackage :cover-iterate-test
  (:use :cl :cover-rt :iterate)
  (:import-from :cover-test :ck)
  (:export :test))

(in-package :cover-iterate-test)

(setq cover::*testing* t)
(cover:annotate nil)

(rem-all-tests)

(deftest when1 (ck (iter (for x in '(1 nil 2)) (when x (sum x))))
  ((1 + :REACH WHEN) (2 + :NON-NULL X) (2 + :NULL X))
  3)

(deftest when2 (ck (iter (for x in '(1 2)) (when x (sum x))))
  ((1 + :REACH WHEN) (2 + :NON-NULL X) (2 - :NULL X))
  3)

(deftest when3 (ck (iter (for x in '(nil)) (when x (sum x))))
  ((1 + :REACH WHEN) (2 - :NON-NULL X) (2 + :NULL X))
  0)

(deftest unless1 (ck (iter (for x in '(1 nil 2)) (unless (null x) (sum x))))
  ((1 + :REACH UNLESS) (2 + :NULL NULL) (2 + :NON-NULL NULL))
  3)

(deftest if1 (ck (iter (for x in '(1 2 3 4 5))
		       (if (evenp x)
			   (collecting x into a)
			   (collecting x into b))
		       (finally (return (values a b)))))
  ((1 + :REACH IF) (2 + :NON-NULL EVENP) (2 + :NULL EVENP))
  (2 4)
  (1 3 5))

(deftest or1 (ck (iter (for x in '(1 2 3 4))
		       (or (eql x 3)
			   (collecting x))))
  ((1 + :REACH OR) (2 + :FIRST-NON-NULL EQL) (2 + :EVAL-ALL PROGN))
  (1 2 4))

(deftest and1 (ck (iter (for x in '(1 2 nil 4))
			(and x (collecting x))))
  ((1 + :REACH AND) (2 + :FIRST-NULL X) (2 + :EVAL-ALL PROGN))
  (1 2 4))

(deftest cond1 (ck (iter (for x in '(1 2 3 4))
			 (cond
			   ((eql x 2) (collecting :a))
			   ((eql x 4) (collecting :b))
			   (t (collecting x)))))
  ((1 + :REACH COND) (2 + :FIRST-NON-NULL EQL) (2 + :FIRST-NON-NULL EQL)
   (2 + :FIRST-NON-NULL T))
  (1 :A 3 :B))

(deftest case1 (ck (iter (for x in '(1 2 3 4))
			 (collecting (case x
				       ((0) :bad)
				       ((1) :a)
				       ((2 3) :b)
				       (t :c)))))
  ((1 + :REACH CASE) (2 - :SELECT 0) (2 + :SELECT 1) (2 + :SELECT 2) (2 + :SELECT T))
  (:A :B :B :C))

(deftest typecase1 (ck (iter (for x in '(1 2 3 4))
			     (collecting
			      (typecase x
				((integer * 0) :bad)
				((eql 1) :a)
				((member 2 3) :b)))))
  ((1 + :REACH TYPECASE) (2 - :SELECT INTEGER) (2 + :SELECT EQL) (2 + :SELECT MEMBER)
   (2 + :SELECT-NONE NIL))
  (:A :B :B NIL))

  
