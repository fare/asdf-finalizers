;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :asdf-finalizers-test
  :defsystem-depends-on (:asdf-finalizers)
  :depends-on (:list-of :fare-utils :hu.dwim.stefil)
  :around-compile "asdf-finalizers:check-finalizers-around-compile"
  :components ((:file "asdf-finalizers-test")))
