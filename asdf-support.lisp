#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf)

(defun compile-check-finalizers (&rest args)
  (declare (ignore args))
  (no-finalizer-left-behind-p))

(defun check-finalizers-around-compile (fun)
  (funcall fun :compile-check 'compile-check-finalizers))

(defclass finalized-cl-source-file (cl-source-file)
  ((around-compile :initargs 'check-finalizers-around-compile)))
