#+xcvb (module ())

(in-package :cl)

(defpackage :asdf-finalizers
  (:use :cl :asdf)
  (:export
   #:eval-at-toplevel
   #:final-forms
   #:register-finalizer
   #:register-final-form
   #:no-finalizer-left-behind-p
   #:assert-no-finalizer-left-behind
   #:compile-check-finalizers
   #:check-finalizers-around-compile
   #:finalized-cl-source-file
   #:call-with-finalizers
   #:with-finalizers
   #:using-finalizers-p
   ))
