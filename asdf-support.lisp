#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

(defun compile-check-finalizers (&rest args)
  (declare (ignore args))
  (no-finalizer-left-behind-p))

(defun check-finalizers-around-compile (fun)
  (with-finalizers ()
    (unwind-protect
	 (funcall fun :compile-check 'compile-check-finalizers)
      (setf *finalizers* nil))))

(defclass finalized-cl-source-file (cl-source-file)
  ((around-compile :initargs 'check-finalizers-around-compile)))
