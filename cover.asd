(asdf:defsystem "cover"
    :author "Richard C. Waters"
    :description "Code coverage utility for Common Lisp"
    :long-description "The COVER Common Lisp test case coverage determination tool can help assess the coverage of a suite of test cases. When a suite of test cases for a program is run in conjunction with COVER, statistics are kept on which conditions in the code for the program are exercised and which are not. Based on this information, COVER can print a report of what has been missed. By devising tests that exercise these conditions, a programmer can extend the test suite so that it has more complete coverage."
    ;; :described-in "http://www.merl.com/publications/docs/TR91-04.pdf"
    :license "MIT"
    :components
    ((:file "cover-package")
     (:file "cover" :depends-on ("cover-package")))
    :in-order-to ((asdf:test-op (asdf:test-op "cover-tests"))))

(asdf:defsystem "cover-rt"
  :description "Copy of RT test system"
  :components ((:file "cover-rt")))

(asdf:defsystem "cover-tests"
  :author "Richard C. Waters"
  :description "Test suite for COVER"
  :license "MIT"
  :depends-on ("cover" "uiop" "cover-rt")
  :components ((:file "cover-test"))
  :perform (asdf:test-op (operation component)
			 (let ((*package* (find-package "COVER-TEST")))
			   (or (uiop:symbol-call '#:cover-rt '#:do-tests)
			       (error "TEST-OP failed for COVER-TESTS")))))
  
