;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :asdf-finalizers
  :description "Enforced calling of finalizers for Lisp code"
  :defsystem-depends-on (:asdf)
  :depends-on ((:version "asdf" "2.22.3")) ;; we require a working :compile-check feature
  :components
  ((:file "pkgdcl")
   (:file "finalizers" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("finalizers"))
   (:file "initialization" :depends-on ("pkgdcl"))))

(defmethod perform ((op test-op) (system (eql (find-system :asdf-finalizers))))
  (asdf:load-system :asdf-finalizers-test)
  (funcall (asdf:find-symbol* :test-suite :asdf-finalizers-test)))
