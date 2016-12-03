(in-package "COMMON-LISP-USER")

(defun main (&optional arguments)
  (declare (ignorable arguments))
  (when arguments
    (format t "Arguments are: ~{~%    ~A~}~%" arguments))
  (princ "Hello World!")
  (terpri))

