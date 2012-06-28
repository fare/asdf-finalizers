;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :list-of
  :description "magic list-of deftype"
  :depends-on (:asdf-finalizers)
  :components ((:file "list-of")))
