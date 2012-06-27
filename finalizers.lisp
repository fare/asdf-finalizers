#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf)

(defvar *finalizers* '())

(defmacro finalize ()
  `(progn
     ,(loop :while *finalizers* :do (funcall (pop *finalizers*)))))

(defun register-finalizer (f)
  (push f *finalizers*))

(defun no-finalizer-left-behind-p ()
  (null *finalizers*))

(defun assert-no-finalizer-left-behind ()
  (assert (no-finalizer-left-behind-p)))
