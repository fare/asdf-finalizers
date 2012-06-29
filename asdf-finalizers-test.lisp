#+xcvb (module (:depends-on ("asdf-finalizers" (:asdf "hu.dwim.stefil"))))

(defpackage :asdf-finalizers-test
  (:use :cl :fare-utils :asdf-finalizers :hu.dwim.stefil :list-of))

(in-package :asdf-finalizers-test)

;;; Testing the asdf-finalizers library.

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing asdf-finalizers"))

(deftest test-list-of ()
  (is (typep '(nil t t nil) '(list-of boolean)))
  (is (not (typep '(nil t 1 nil) '(list-of boolean))))
  (is (not (typep '(nil t t nil . 1) '(list-of boolean))))
  (is (typep '(1 2 3 4) '(list-of integer)))
  (is (not (typep '(1 2 3 4) '(list-of nil))))
  (is (typep nil '(list-of nil))))

(final-forms)
