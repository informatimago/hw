(in-package "COMMON-LISP-USER")

;; Remove used package from CL-USER to avoid conflicts.
;; Note: this doesn't remove directly imported symbols.
(mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
      (set-difference
       (copy-seq (package-use-list "COMMON-LISP-USER"))
       (delete nil (list ;; A list of all the "CL" packages possible:
                         (find-package "COMMON-LISP")))))

#+ecl (require 'cmp)

;;; --------------------------------------------------------------------
;;; Configuration
;;;
(defparameter *program-name*      "hw" "Generated executable name (when implementation allows it)")
(defparameter *version*           "1.0.0")
(defparameter *copyright*         "Copyright Pascal J. Bourguignon 2015 - 2018
License: AGPL3")
(defparameter *system-name*       "hw" "The main ASDF system name (when implementation needs it.")
(defparameter *system-list*        '() "List of  ASDF systems that must be loaded before *SYSTEM-NAME*.")
(defparameter *main-function*     "HELLO-WORLD:HW" "This string will be read to get the name of the main function.
The main function must be a (lambda (&rest command-line-arguments) …).")
(defparameter *init-file*         "~/.hw.lisp" "Can be NIL or a namestring to the rc file for this program.")
(defparameter *source-directory*  (or *load-pathname* (truename #P"./")))
(defparameter *release-directory* *source-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )
(defparameter *root-directory*    *source-directory*)
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *root-directory* nil))
                                                  (list *source-directory*))))

;;; --------------------------------------------------------------------
;;; Load quicklisp.
;;;

(defun say (format-control &rest arguments)
  (format t "~&;;; ~?~%" format-control arguments)
  (finish-output))

(say "Loading quicklisp.")
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)

(defun configure-asdf-directories (directories &key append)
  #-adsf3 (setf asdf:*central-registry*
                (remove-duplicates (if append
                                       (append directories asdf:*central-registry*)
                                       directories)
                                   :test (function equalp)))
  #+asdf3 (asdf:initialize-source-registry
           `(:source-registry
             :ignore-inherited-configuration
             ,@(mapcar (lambda (dir) `(:directory ,dir))
                       (remove-duplicates directories :test (function equalp)))
             ,@(when append `(:default-registry)))))

(configure-asdf-directories *asdf-directories*)

;;; --------------------------------------------------------------------
;;; Utilities
;;;

(defun not-implemented-yet (what)
  (error "~S is not implemented yet on ~A, please provide a patch!"
         what (lisp-implementation-type)))

#-(and)
(defun program-path ()
  (let* ((argv  (ext:argv))
         (largv (length argv))
         (args  ext:*args*)
         (largs (length args))
         (index (- largv largs 1))
         (path  (and (<= 0 index largv) (elt argv index))))
    (cond
      (path
       path)
      ((and *load-truename*
            (string/= (file-namestring *load-truename*) "script.lisp"))
       (namestring *load-truename*))
      (t
       *default-program-name*))))

(defun argv ()
  #+ccl   ccl:*command-line-argument-list*
  #+clisp (cons (elt (ext:argv) 0) ext:*args*)
  #+ecl   (si::command-args)
  #+sbcl  sb-ext:*posix-argv*
  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'exit))

(defun exit (status)
  #+ccl   (ccl:quit status)
  #+clisp (ext:quit status)
  #+ecl   (ext:quit status)
  #+sbcl  (sb-ext:exit :code status)
  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'exit))

(defun make-toplevel-function (main-function-name init-file)
  (let ((form `(lambda ()
                 (handler-case
                     (progn
                       ,@(when init-file
                           `((load ,init-file :if-does-not-exist nil)))
                       (apply (read-from-string ,main-function-name)
                              #+ecl (si::command-args) #-ecl (argv)))
                   (error (err)
                     (finish-output *standard-output*)
                     (finish-output *trace-output*)
                     (format *error-output* "~%~A~%" err)
                     (finish-output *error-output*)
                     #+ecl (ext:quit 1) #-ecl (exit 1)))
                 (finish-output *standard-output*)
                 (finish-output *trace-output*)
                 #+ecl (ext:quit 0) #-ecl (exit 0))))
    #+ecl (list form)
    #-ecl (coerce form 'function)))

(defun system-cl-source-files (system)
  (let ((system (asdf:find-system system)))
    (remove-duplicates
     (append
      (mapcar (function asdf:component-pathname)
              (remove-if-not (lambda (component) (typep component 'asdf:cl-source-file))
                             (asdf:component-children system)))
      (mapcan (function system-cl-source-files)
              (delete-duplicates
               (mapcan (lambda (depend) (copy-list (funcall depend system)))
                       '(asdf:system-defsystem-depends-on
                         asdf:system-depends-on
                         asdf:system-weakly-depends-on))
               :test (function equal))))
     :test (function equal))))

(defun system-object-files (system)
  (let ((system (asdf:find-system system)))
    (remove-duplicates
     (append
      (mapcan (lambda (component) (copy-list (asdf:output-files 'asdf:compile-op component)))
              (asdf:component-children system))
      (mapcan (function system-object-files)
              (delete-duplicates
               (mapcan (lambda (depend) (copy-list (funcall depend system)))
                       '(asdf:system-defsystem-depends-on
                         asdf:system-depends-on
                         asdf:system-weakly-depends-on))
               :test (function equal))))
     :test (function equal))))


(defun generate-program (program-name main-function
                         &key release-directory init-file system-name system-list
                           version copyright source-directory)
  (declare (ignorable release-directory init-file
                      system-name system-list
                      version copyright source-directory))

  (when system-list
    (say "Load systems ~A" system-list)
    (ql:quickload system-list))
  #-ecl
  (progn
    (say "Load system ~A" system-name)
    (ql:quickload system-name))

  (say "Generating program ~A" (merge-pathnames program-name release-directory))

  #+ccl (progn
          ;; This doesn't return.
          (ccl::save-application
           (merge-pathnames program-name release-directory  nil)
           :toplevel-function (make-toplevel-function main-function nil)
           :init-file init-file
           :mode #o755
           :prepend-kernel t
           :error-handler t))

  #+clisp (progn
            (ext::saveinitmem
             (merge-pathnames program-name release-directory  nil)
             :executable t
             :norc t
             :quiet t
             :verbose t
             :script t
             :documentation (format nil "~A version ~A~%~A~%" program-name version copyright)
             :start-package (find-package "COMMON-LISP-USER")
             ;; locked-packages
             ;; :keep-global-handlers
             :init-function (make-toplevel-function main-function init-file))
            (ext:quit 0))

  #+ecl (progn
          (handler-bind
              ((error (lambda (condition)
                        (invoke-debugger condition))))
            #-(and) (load "hw.asd")
            #-(and) (asdf:oos 'asdf:program-op system-name)
            (asdf:make-build system-name
                             :type :program
                             :monolithic t
                             :ld-flags '()
                             :prologue-code ""
                             :epilogue-code #-(and) '(progn (print 'hello) (finish-output)) ; doesn't work either.
                                            (make-toplevel-function main-function init-file))
            #-(and)
            (c:build-program (merge-pathnames program-name release-directory nil)
                             :lisp-files (system-object-files system-name)
                             :ld-flags '()
                             :prologue-code ""
                             :epilogue-code (make-toplevel-function main-function init-file)))

          (rename-file
           (make-pathname
            :directory (append (pathname-directory
                                (uiop/configuration::compute-user-cache))
                               (rest (pathname-directory source-directory)))
            :name (string-downcase system-name)
            :type nil)
           (merge-pathnames program-name release-directory nil))

          (ext:quit 0))

  #+sbcl (sb-ext:save-lisp-and-die "hw"
                                   :executable t
                                   :compression 9
                                   :toplevel (make-toplevel-function main-function init-file))

  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'generate-program))

;;; --------------------------------------------------------------------
;;; generate the program
;;;

(generate-program *program-name*
                  *main-function*
                  :system-name *system-name*
                  :system-list *system-list*
                  :release-directory *release-directory*
                  :init-file *init-file*
                  :version *version*
                  :copyright *copyright*
                  :source-directory *source-directory*)

;;;; THE END ;;;;
