#+xcvb (module (:depends-on ("initialization")))

(defpackage :list-of
  (:use :cl :asdf-finalizers)
  (:export
   #:list-of))

(in-package :list-of)

(defun list-of-predicate-for (type)
  (with-standard-io-syntax
    (let ((*package* (find-package :cl)))
      (intern (format nil "LIST-OF-~S-P" type) :list-of))))

(defun list-of-type-predicate (type)
  #'(lambda (x)
      (loop :for c = x :then (cdr c) :while (consp c) :always (typep (car c) type)
	    :finally (return (null c)))))

(defun ensure-list-of-predicate (type &optional predicate)
  (unless predicate
    (setf predicate (list-of-predicate-for type)))
  (check-type predicate symbol)
  (unless (fboundp predicate)
    (setf (symbol-function predicate) (list-of-type-predicate type)))
  nil)

(defmacro declare-list-of (type &optional predicate)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-list-of-predicate ',type ',predicate)))

(defun ensure-final-list-of-predicate (type &optional predicate)
  (register-final-form `(declare-list-of ,type ,predicate)))

(deftype list-of (type)
  (case type
    ((t) 'list)
    ((nil) 'null)
    (otherwise
     (let ((predicate (list-of-predicate-for type)))
       (eval-at-toplevel
	`(setf (symbol-function ',predicate) (list-of-type-predicate ',type))
	`(fboundp ',predicate)
	"Defining ~S outside of finalized Lisp code" `(list-of ,type))
       `(and list (satisfies ,predicate))))))
