;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :asdf-finalizers-test
  :depends-on (:asdf-finalizers :list-of :fare-utils :hu.dwim.stefil)
  :components
  ((:file "asdf-finalizers-test")))
