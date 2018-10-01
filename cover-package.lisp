(defpackage #:cover
  (:use #:cl)
  (:shadow #:defun #:defmacro)
  (:export #:annotate report reset forget forget-all *line-limit*))
