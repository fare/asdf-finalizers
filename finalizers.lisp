#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

;;#+sbcl (declaim (sb-ext:muffle-conditions style-warning))

;; UNBOUND by default: catch people using them outside of a proper with-finalizers form!
(defvar *finalizers*)

(defun using-finalizers-p ()
  (boundp '*finalizers*))

(defmacro finalize ()
  "Evaluate registered finalization thunks."
  `(progn
     ,(loop :while *finalizers* :do (funcall (pop *finalizers*)))))

(defun register-finalizer (thunk)
  "Register a THUNK to be called during finalization.
Any dependencies must be enforced by calling thunk dependencies.
Any form returned by the THUNK will be included in the finalized code;
if its effects are to be available at compile-time,
it will probably enclose these effects in a
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  (push thunk *finalizers*))

(defun register-final-code (code)
  "Register a constant piece of code to the evaluated at toplevel
at the end of the current code fragment (e.g. file).
If its effects are to be available at compile-time,
it will probably enclose these effects in a
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  (register-finalizer (constantly code)))

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
  (let ((*finalizers* '()))
    (unwind-protect
	 (funcall thunk)
      (when finalize (eval '(finalize)))
      (assert-no-finalizer-left-behind))))

(defun eval-at-toplevel (form &optional already-done-p-form warning &rest warning-arguments)
  "This function, to be called during macroexpansion time, wiil
evaluate toplevel FORM now during the macroexpansion phase, but also
register it to be evaluated at the toplevel before the end of current file,
so it is available to whoever load the associated FASL or CFASL.
Either now or when loading the (C?)FASL, skip evaluation of FORM
when ALREADY-DONE-P-FORM evaluates to a true result.
When finalizers are not enabled, warn with given warning and arguments or
with a default warning, unless ALREADY-DONE-P-FORM evaluated to a true result,
at which point we trust the user to somehow have done the right thing,
and a build from clean will hopefully catch him if he didn't."
  (let ((already-done-p (eval already-done-p-form)))
    (unless already-done-p
      (eval form))
    (cond
      ((using-finalizers-p)
       (register-final-code
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (unless ,already-done-p-form ,form))))
      (already-done-p) ;; don't warn if it has already been done; it could be by design.
      (warning
       (apply 'warn warning warning-arguments))
      (t
       (warn "trying to ~S form ~S without finalizers enabled~@[ while not ~S~]"
	     'eval-at-toplevel form already-done-p-form))))
  nil)
