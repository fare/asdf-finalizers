#+xcvb (module (:depends-on ("finalizers")))

(in-package :asdf-finalizers)

(defun compile-check-finalizers (input-file &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (let ((okp (no-finalizer-left-behind-p)))
    (unless okp
      (warn 'missing-final-forms
	    :format-control "Source file ~A uses finalizers but fails to ~
             include ~S between the last finalizer and the end of file"
	    :format-arguments `(,input-file (final-forms))))
    okp))

(defun check-finalizers-around-compile (fun)
  (with-finalizers ()
    (funcall fun :compile-check 'compile-check-finalizers)))

(defclass asdf::finalized-cl-source-file (cl-source-file)
  ((around-compile :initargs 'check-finalizers-around-compile)))
