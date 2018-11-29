(defpackage #:cover
  (:use #:cl)
  (:shadow #:defun #:defmacro #:defmethod #:defgeneric)
  (:export #:annotate report reset forget forget-all *line-limit*))
