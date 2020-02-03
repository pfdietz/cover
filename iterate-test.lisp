(defpackage :cover-iterate-test
  (:use :cl :cover-rt :iterate)
  (:import-from :cover-tests :ck)
  (:export :test))

(in-package :cover-iterate-test)

(setq cover::*testing* t)
(cover:annotate nil)

(rem-all-tests)

(deftest when1 (ck (iter (for x in '(1 nil 2)) (when x (sum x))))
  ((1 + REACH WHEN) (2 + NON-NULL X) (2 + NULL X))
  3)

(deftest when2 (ck (iter (for x in '(1 2)) (when x (sum x))))
  ((1 + REACH WHEN) (2 + NON-NULL X) (2 - NULL X))
  3)

(deftest when3 (ck (iter (for x in '(nil)) (when x (sum x))))
  ((1 + REACH WHEN) (2 - NON-NULL X) (2 + NULL X))
  0)

(deftest unless1 (ck (iter (for x in '(1 nil 2)) (unless (null x) (sum x))))
  ((1 + REACH UNLESS) (2 + NULL NULL) (2 + NON-NULL NULL))
  3)

(deftest if1 (ck (iter (for x in '(1 2 3 4 5))
		       (if (evenp x)
			   (collecting x into a)
			   (collecting x into b))
		       (finally (return (values a b)))))
  ((1 + REACH IF) (2 + NON-NULL EVENP) (2 + NULL EVENP))
  (2 4)
  (1 3 5))


