(defpackage #:cover
  (:use #:cl)
  (:shadow #:defun #:defmacro #:defmethod)
  (:export #:annotate report reset forget forget-all *line-limit*))
