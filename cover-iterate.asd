(asdf:defsystem "cover/iterate"
    :author "Paul F. Dietz"
    :description "Methods to augment the code walker in a modified
version of ITERATE to make it work with COVER."
    :long-description "The ITERATE package, without modification, would not
work with COVER, because it cannot handle the macrolet forms introduced by COVER.
Instead, ITERATE's code walker must be informed about the macros used by COVER
so they can be walked without expansion.   This system adds methods that cause
ITERATE to do the right thing on those macros."
    :depends-on ("cover" "iterate")
    :components
    ((:file "iterate")
     :in-order-to ((asdf:test-op (asdf:test-op "cover/iterate/tests")))))

(asdf:defsystem "cover/iterate/tests"
    :author "Paul F. Dietz"
    :description "Tests demonstrating that COVER, augmented with
COVER/ITERATE, works on forms from ITERATE."
    :depends-on ("cover" "cover/iterate" "iterate" "uiop")
    :components ((:file "iterate-test"))
    :perform (asdf:test-op (operation components)
			   (or (uiop:symbol-call '#:cover-iterate-test '#:test)
			       (error "TEST-OP failed for COVER-ITERATE-TESTS"))))