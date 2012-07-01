#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-finalizers)

(defvar *warn-when-finalizers-off* t
  "Flag to enable or disable the raising warnings
when finalizers are used outside of context.
Typically, you want that flag to be on while compiling your application, but
off when your application is done compiled and you're at runtime.")

(defvar *debug-finalizers* nil
  "Flag to enable debugging output for finalizers.")


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
  (when *finalizers*
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;;(eval-when (:compile-toplevel :execute) *finalizers*) ; trying to debug on CCL.
       ,(expand-final-forms))))

(defun expand-final-forms ()
  (let ((forms (reverse
		(loop :while *finalizers*
		      :collect (let ((f (pop *finalizers*)))
                                 (etypecase f
                                   (function (funcall f))
                                   (cons f)))))))
    (when *debug-finalizers*
      (with-standard-io-syntax
	(let ((*package* (find-package :cl))
              (*print-readably* nil)
	      (*print-pretty* t))
	  (format *trace-output* "~&Final forms:~%~{  ~S~%~}~%" forms))))
    `(progn ,@forms)))

(define-condition finalizers-off () ())
(define-condition finalizers-off-error (finalizers-off error) ())
(define-condition finalizers-off-simple-error (finalizers-off-error simple-error) ())
(define-condition finalizers-off-warning (finalizers-off warning) ())
(define-condition finalizers-off-simple-warning (finalizers-off-warning simple-warning) ())

(define-condition missing-final-forms (simple-warning) ())


(defun register-finalizer (finalizer)
  "Register a thunk to be called during finalization (if a function)
or a constant form to be included (if a cons).
Any dependencies must be enforced by calling thunk dependencies.
Any form returned by the thunk will be included in the finalized code.
It will be wrapped inside an
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)
but you can override that with your own explicit eval-when."
  (check-type finalizer (or function cons))
  (unless (using-finalizers-p)
    (error 'finalizers-off-simple-error
	   :format-control "Trying to use finalizers outside of a (~S ...) form. ~
       You probably need to use ~
       :around-compile \"asdf-finalizers:check-finalizers-around-compile\" ~
       in your asdf defsystem"
	   :format-arguments '(with-finalizers)))
  (push finalizer *finalizers*))

(defun register-final-form (form)
  "Register a constant piece of code to the evaluated at toplevel
at the end of the current code fragment (e.g. file).
It will be wrapped inside an
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)
but you can override that with your own explicit eval-when."
  (check-type form cons)
  (register-finalizer form))

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
          (if already-done-p-form
            `(unless ,already-done-p-form ,form)
            form))))
      (already-done-p) ;; don't warn if it has already been done; it could be by design.
      ((not *warn-when-finalizers-off*)) ;; don't warn if warnings are off - e.g. at runtime.
      ((stringp warning)
       (warn 'finalizers-off-simple-warning
             :format-control warning :format-arguments warning-arguments))
      ((and warning (symbolp warning))
       (apply 'warn warning warning-arguments))
      (t
       (warn 'finalizers-off-simple-warning
	     :format-control "trying to ~S form ~S without finalizers enabled~@[ while not ~S~]"
	     :format-arguments whole))))
  nil)
