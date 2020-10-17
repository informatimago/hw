(defpackage "HELLO-WORLD"
  (:use "COMMON-LISP")
  (:export "HW" "MAIN")
  (:documentation "Implements the Hello, World! hw function, and main function.
Copyright 2020 Pascal J. Bourguignon <pjb@informatimago.com>"))
(in-package "HELLO-WORLD")

(defun hw (&optional (out *standard-output*))
  "Prints out a Hello, World! message."
  (format out "~&Hello, World!~%")
  (finish-output out)
  (values))

(defun main (pname &rest arguments)
  "The main function for a hw executable."
  (declare (ignore pname arguments))
  (hw))

