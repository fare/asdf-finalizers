#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

(defun compile-check-finalizers (input-file &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (let ((foo (no-finalizer-left-behind-p)))
    (unless foo
      (warn "Source file ~A uses finalizers but fails to finalize" input-file))
    foo))

(defun check-finalizers-around-compile (fun)
  (with-finalizers ()
    (unwind-protect
	 (funcall fun :compile-check 'compile-check-finalizers)
      (reset-finalizers))))

(defclass asdf::finalized-cl-source-file (cl-source-file)
  ((around-compile :initargs 'check-finalizers-around-compile)))
