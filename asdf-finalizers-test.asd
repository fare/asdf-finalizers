;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :asdf-finalizers-test
  :defsystem-depends-on (:asdf-finalizers)
  :around-compile "asdf-finalizers:check-finalizers-around-compile"
  :depends-on (:list-of :fare-utils :hu.dwim.stefil)
  :components ((:file "asdf-finalizers-test")))
