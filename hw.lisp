(defpackage "HELLO-WORLD"
  (:use "COMMON-LISP")
  (:export "HW"))
(in-package "HELLO-WORLD")

(defun hw ()
  (write-line "Hello World!"))
