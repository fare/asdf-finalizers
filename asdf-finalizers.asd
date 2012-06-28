;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :asdf-finalizers
  :description "Enforced calling of finalizers for Lisp code"
  :defsystem-depends-on (:asdf)
  :depends-on ((:version "asdf" "2.22.1")) ;; we require the :compile-check feature
  :components
  ((:file "pkgdcl")
   (:file "finalizers" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("pkgdcl"))
   (:file "initialization" :depends-on ("pkgdcl"))))

(defmethod perform ((op test-op) (system (eql (find-system :asdf-finalizers))))
  (asdf:load-system :asdf-finalizers-test)
  (funcall (asdf:find-symbol* :test-suite :asdf-finalizers-test)))
