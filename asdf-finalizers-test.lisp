#+xcvb (module (:depends-on ("asdf-finalizers" (:asdf "hu.dwim.stefil"))))

(defpackage :asdf-finalizers-test
  (:use :cl :fare-utils :asdf-finalizers :hu.dwim.stefil))

(in-package :asdf-finalizers-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

;;; Testing the asdf-finalizers library.

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing asdf-finalizers"))

;;; TODO: actually test stuff
