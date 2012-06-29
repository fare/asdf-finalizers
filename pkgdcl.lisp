#+xcvb (module ())

(in-package :cl)

(defpackage :asdf-finalizers
  (:use :cl :asdf)
  (:export
   #:finalize
   #:register-finalizer
   #:register-final-code
   #:no-finalizer-left-behind-p
   #:assert-no-finalizer-left-behind
   #:compile-check-finalizers
   #:check-finalizers-around-compile
   #:finalized-cl-source-file
   #:call-with-finalizers
   #:with-finalizers
   #:using-finalizers-p
   #:eval-at-toplevel
   ))
