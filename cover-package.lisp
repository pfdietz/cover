(defpackage #:cover
  (:use #:cl)
  (:shadow #:defun #:defmacro #:defmethod #:defgeneric #:in-package)
  (:export #:annotate #:report #:reset #:forget #:forget-all #:*line-limit*
	   #:cover-compile-hook))
