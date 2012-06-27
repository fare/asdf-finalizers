;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

;; Ensure upgrade, so #+asdf-unicode below has better chances of working.
(asdf:oos 'asdf:load-op :asdf)

(asdf:defsystem :asdf-finalizers
  :description "Enforced calling of finalizers for Lisp code"
  :depends-on ((:version "asdf" "2.22.1"))
  #+asdf-unicode #+asdf-unicode ;; disabled in implementations with no unicode support.
  :components
  ((:file "pkgdcl")
   (:file "finalizers" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("pkgdcl"))
   (:file "initialization" :depends-on ("pkgdcl"))))

(defmethod perform ((op test-op) (system (eql (find-system :asdf-finalizers))))
  (asdf:load-system :asdf-finalizers-test)
  (funcall (asdf:find-symbol* :test-suite :asdf-finalizers-test)))
