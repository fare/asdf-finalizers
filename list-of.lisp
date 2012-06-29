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

(defun is-list-of-type-p (x type)
  (typecase x
    (null
     t)
    (cons
     (and (typep (car x) type)
	  (is-list-of-type-p (cdr x) type)))
    (t
     nil)))

(defun ensure-list-of-predicate (type &optional predicate)
  (unless predicate
    (setf predicate (list-of-predicate-for type)))
  (check-type predicate symbol)
  (unless (fboundp predicate)
    (setf (symbol-function predicate)
	  #'(lambda (x) (is-list-of-type-p x type)))))

(defmacro declare-list-of (type &optional predicate)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-list-of-predicate ',type ',predicate)))

(defun ensure-final-list-of-predicate (type &optional predicate)
  (register-final-code `(declare-list-of ,type ,predicate)))

(deftype list-of (type)
  (case type
    ((t) 'list)
    ((nil) 'null)
    (otherwise
     (let ((predicate (list-of-predicate-for type)))
       (ensure-list-of-predicate type predicate)
       (when (using-finalizers-p)
	 (ensure-final-list-of-predicate type predicate))
       `(and list (satisfies ,predicate))))))
