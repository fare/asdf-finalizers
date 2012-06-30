#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

;; UNBOUND by default: catch people using them outside of a proper with-finalizers form!
(defvar *finalizers*)
(defvar *finalizers-data* nil)

(defun using-finalizers-p ()
  (boundp '*finalizers*))

(defun reset-finalizers ()
  (setf *finalizers* nil
	*finalizers-data* (make-hash-table :test 'equal))
  (values))

(defun disable-finalizers ()
  (makunbound '*finalizers*)
  (makunbound '*finalizers-data*)
  (values))

(defmacro final-forms ()
  "Evaluate registered finalization thunks."
  `(progn
     ,(loop :while *finalizers* :do (funcall (pop *finalizers*)))))

(define-condition finalizers-off () ())
(define-condition finalizers-off-error (finalizers-off error) ())
(define-condition finalizers-off-simple-error (finalizers-off-error simple-error) ())
(define-condition finalizers-off-warning (finalizers-off warning) ())
(define-condition finalizers-off-simple-warning (finalizers-off-warning simple-warning) ())


(defun register-finalizer (thunk)
  "Register a THUNK to be called during finalization.
Any dependencies must be enforced by calling thunk dependencies.
Any form returned by the THUNK will be included in the finalized code;
if its effects are to be available at compile-time,
it will probably enclose these effects in a
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  (unless (using-finalizers-p)
    (error 'finalizers-off-simple-error
	   :format-control "Trying to use finalizers outside of a (~S ...) form. ~
       You probably need to use ~
       :around-compile \"asdf-finalizers:check-finalizers-around-compile\" ~
       in your asdf defsystem"
	   :format-arguments '(with-finalizers)))
  (push thunk *finalizers*))

(defun register-final-form (form)
  "Register a constant piece of code to the evaluated at toplevel
at the end of the current code fragment (e.g. file).
If its effects are to be available at compile-time,
it will probably enclose these effects in a
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  (register-finalizer (constantly form)))

(defun no-finalizer-left-behind-p ()
  (null *finalizers*))

(defun assert-no-finalizer-left-behind ()
  (assert (no-finalizer-left-behind-p)))

(defmacro with-finalizers ((&key finalize) &body body)
  "Evaluate BODY in a context where finalizers are enabled.
By default, don't finalize, because we want to catch code
that fails to finalize in the same file that requires code finalization.
For convenience, to test code at the REPL, if FINALIZE is provided and true,
evaluate finalization forms."
  `(call-with-finalizers #'(lambda () ,@body) :finalize ,finalize))

(defun call-with-finalizers (thunk &key finalize)
  (let ((*finalizers* '())
	(*finalizers-data* (make-hash-table :test 'equal)))
    (unwind-protect
	 (funcall thunk)
      (when finalize (eval '(finalize)))
      (assert-no-finalizer-left-behind))))

(defun eval-at-toplevel (form &optional already-done-p-form warning &rest warning-arguments)
  "This function, to be used within a macro, deftype, reader-macro, etc.,
will evaluate toplevel FORM now during the macroexpansion phase, but also
register it to be evaluated at the toplevel before the end of current file,
so it is available to whoever load the associated FASL or CFASL.
If the FORM has already been registered, it is skipped.
Either now or when loading the (C?)FASL, the evaluation of FORM will be skipped
when ALREADY-DONE-P-FORM evaluates to a true value.
When finalizers are not enabled, warn with given warning and arguments or
with a default warning, unless ALREADY-DONE-P-FORM evaluated to a true value,
at which point we trust the user to somehow have done the right thing,
and a build from clean will hopefully catch him if he didn't."
  (let ((whole `(eval-at-toplevel ,form ,already-done-p-form))
	(already-done-p (eval already-done-p-form)))
    (unless already-done-p
      (eval form))
    (cond
      ((using-finalizers-p)
       (unless (gethash whole *finalizers-data*)
	 (setf (gethash whole *finalizers-data*) t)
	 (register-final-form
	  `(eval-when (:compile-toplevel :load-toplevel :execute)
	     (unless ,already-done-p-form ,form)))))
      (already-done-p) ;; don't warn if it has already been done; it could be by design.
      ((stringp warning)
       (warn 'finalizers-off-simple-warning :format-control warning :format-arguments warning-arguments))
      ((and warning (symbolp warning))
       (apply 'warn warning warning-arguments))
      (t
       (warn 'finalizers-off-simple-warning
	     :format-control "trying to ~S form ~S without finalizers enabled~@[ while not ~S~]"
	     :format-arguments whole))))
  nil)
