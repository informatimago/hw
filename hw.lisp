(defpackage "HELLO-WORLD"
  (:use "COMMON-LISP")
  (:export "HW"))
(in-package "HELLO-WORLD")

(defun hw (pname &rest arguments)
  (declare (ignore pname arguments))
  (write-line "Hello World!"))
