(defpackage "Hello World"
  (:use "COMMON-LISP"))
(in-package "Hello World")

(defun main (&optional arguments)
  (declare (ignorable arguments))
  (when arguments
    (format t "Arguments are: ~{~%    ~A~}~%" arguments))
  (princ "Hello World!")
  (terpri))
