#+xcvb (module (:depends-on ("initialization")))

(defpackage :list-of
  (:use :cl :asdf-finalizers)
  (:export
   #:list-of))

(in-package :list-of)

(defun list-of-predicate-for (type)
  (with-standard-io-syntax
    (let ((*package* (find-package :list-of)))
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

(deftype list-of (type)
  (case type
    ((t) 'list)
    ((nil) 'null)
    (otherwise
     (let ((predicate (list-of-predicate-for type)))
       (eval-at-toplevel
	`(ensure-list-of-predicate ',type ',predicate)
	`(fboundp ',predicate) ;; hush unnecessary eval-at-toplevel warnings
	"Defining ~S outside of finalized Lisp code" `(list-of ,type))
       `(and list (satisfies ,predicate))))))

;; This is available in case you prefer to explicitly call declare-list-of
;; in your code-base rather than rely on finalizers.
;; It is not exported because we do not encourage it, but you can import it.
(defmacro declare-list-of (type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-list-of-predicate ',type)))
