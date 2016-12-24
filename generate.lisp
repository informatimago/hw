(in-package "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Load quicklisp.
;;;

(format t "~%;;; Loading quicklisp.~%")
(finish-output)
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)

;;; --------------------------------------------------------------------
;;; HOME
;;;

#-(and)
(unless (logical-pathname-translations "HOME")
  (setf (logical-pathname-translations "HOME")
        (list (list "HOME:**;*.*"
                    (merge-pathnames "**/*.*"
                                     (user-homedir-pathname))))))

;;; --------------------------------------------------------------------
;;; Load the program.
;;;
(defparameter *source-directory*  (or *load-pathname* (truename #P"./")))
(defparameter *program-name*      "hw" "Generated executable name (when implementation allows it)")
(defparameter *system-name*       "hw" "ASDF system name (when implementation needs it.")
(defparameter *init-file*         "~/.hw.lisp")
(defparameter *release-directory* *source-directory* #|#P"HOME:bin;"|#)
(defparameter *version*           "1.0.0")
(defparameter *copyright*
  "Copyright Pascal J. Bourguignon 2015 - 2016
License: AGPL3")

#-(and)(defparameter *root-directory* *source-directory*)
#-(and)(setf asdf:*central-registry*
             (union (delete-duplicates (mapcar (lambda (path)
                                                 (make-pathname :name nil :type nil :version nil
                                                                :defaults path))
                                               (directory (merge-pathnames "**/*.asd" *root-directory*)))
                                       :test (function equalp))
                    asdf:*central-registry*
                    :test (function equal)))

(pushnew *source-directory* asdf:*central-registry*
         :test (function equal))

#-ecl (ql:quickload *system-name*)

(format t "Generating ~A~%" (merge-pathnames *program-name* *release-directory*))


#-(and)
(defun copy-files (files dest-dir)
  (ensure-directories-exist (make-pathname :name "test"
                                           :type "test"
                                           :defaults dest-dir))
  (dolist (file (directory files))
    (let ((dest-file (make-pathname
                      :name (pathname-name file)
                      :type (pathname-type file)
                      :defaults dest-dir)))
      (format t "Copying ~A~%" dest-file)
      (com.informatimago.common-lisp.cesarum.file:copy-file
       file dest-file
       :element-type '(unsigned-byte 8)
       :if-exists :supersede))))




(defun generate-program (program-name main-function
                         &key release-directory init-file system-name)
  (declare (ignorable release-directory init-file system-name))
  (finish-output)

  ;; This doesn't return.
  #+ccl (ccl::save-application
         (merge-pathnames program-name release-directory  nil)
         :toplevel-function (lambda ()
                              (handler-case
                                  (progn
                                    (load init-file :if-does-not-exist nil)
                                    (funcall main-function
                                             (rest ccl:*command-line-argument-list*)))
                                (error (err)
                                  (finish-output *standard-output*)
                                  (finish-output *trace-output*)
                                  (format *error-output* "~%~A~%" err)
                                  (finish-output *error-output*)
                                  (ccl:quit 1)))
                              (finish-output *standard-output*)
                              (finish-output *trace-output*)
                              (ccl:quit 0))
         :init-file init-file
         :mode #o755
         :prepend-kernel t
         :error-handler t)

  #+ecl (progn
          (asdf:make-build system-name
                           :type :program
                           :monolithic t
                           :ld-flags '()
                           :prologue-code ""
                           :epilogue-code `(progn
                                             (handler-case
                                                 (progn

                                                   (load ',init-file :if-does-not-exist nil)
                                                   (funcall ',main-function
                                                            (rest (si::command-args))))
                                               (error (err)
                                                 (finish-output *standard-output*)
                                                 (finish-output *trace-output*)
                                                 (format *error-output* "~%~A~%" err)
                                                 (finish-output *error-output*)
                                                 (si:quit 1)))
                                             (finish-output *standard-output*)
                                             (finish-output *trace-output*)
                                             (si:quit 0)))
          (rename-file
           (make-pathname
            :directory (append (pathname-directory
                                (uiop/configuration::compute-user-cache))
                               (rest (pathname-directory *source-directory*)))
            :name (string-downcase system-name)
            :type nil)
           (merge-pathnames program-name release-directory nil)))

  #+(and nil ecl) (c:build-program (merge-pathnames program-name release-directory nil)
                                   :lisp-files '()
                                   :ld-flags '()
                                   :prologue-code ""
                                   :epilogue-code `(funcall ',main-function (si::command-args)))
  #+ecl (quit)

  #-(or ccl ecl)
  (error "~S is not implemented yet for ~A"
         'generate-program
         (lisp-implementation-type)))


(generate-program *program-name*
                  'cl-user::main
                  :system-name *system-name*
                  :release-directory *release-directory*
                  :init-file *init-file*)


;;;; THE END ;;;;
