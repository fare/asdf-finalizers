#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

;; UNBOUND by default: catch people using them outside of a proper with-finalizers form!
(defvar *finalizers*)

(defun using-finalizers-p ()
  (boundp '*finalizers*))

(defmacro finalize ()
  `(progn
     ,(loop :while *finalizers* :do (funcall (pop *finalizers*)))))

(defun register-finalizer (f)
  (push f *finalizers*))

(defun register-final-code (code)
  (register-finalizer (constantly code)))

(defun no-finalizer-left-behind-p ()
  (null *finalizers*))

(defun assert-no-finalizer-left-behind ()
  (assert (no-finalizer-left-behind-p)))

(defmacro with-finalizers ((&key finalize) &body body)
  `(call-with-finalizers #'(lambda () ,@body) :finalize ,finalize))

(defun call-with-finalizers (thunk &key finalize)
  (let ((*finalizers* '()))
    (unwind-protect
	 (funcall thunk)
      (when finalize (eval '(finalize)))
      (assert-no-finalizer-left-behind))))
