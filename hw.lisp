(defpackage "HELLO-WORLD"
  (:use "COMMON-LISP")
  (:export "HW" "MAIN"))
(in-package "HELLO-WORLD")

(defun hw (&optional (out *standard-output*))
  (format out "~&Hello, World!~%")
  (finish-output out)
  (values))

(defun main (pname &rest arguments)
  (declare (ignore pname arguments))
  (hw))

