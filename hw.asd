(asdf:defsystem "hw"
  ;; system attributes:
  :description "Hello World"
  :long-description "

This program prints \"Hello World!\" on the standard output.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2016")
               ((#:albert #:output-dir)          . "../documentation/hw/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ()
  :components ((:file "hw" :depends-on ()))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

