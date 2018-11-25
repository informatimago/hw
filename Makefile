 ALL_PROGRAMS=   \
	hw-c         \
	hw-pascal     \
	hw-lisp-ccl    \
	hw-lisp-ecl     \
	hw-lisp-sbcl     \
	hw-lisp-clisp     \
	hw-lisp-clisp-fas

all:$(ALL_PROGRAMS)

FPC=/opt/local/lib/fpc/bin/fpc
CLISP=clisp
CCL=ccl
ECL=ecl
SBCL=sbcl
CC=cc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)

.PHONY: all clean test

hw-c:hw.c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	@$(CC) -o hw-c hw.c > hw-c.log 2>&1

hw-pascal:hw.pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	@if [ -x $(FPC) ] ; then $(FPC) -ohw-pascal hw.pas > hw-pascal.log 2>&1 ; else printf 'fpc not installed\n' 1>&2 ; fi

hw-lisp-ccl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	@$(CCL) -n < generate-hw.lisp > hw-lisp-ccl.log 2>&1
	-@mv hw hw-lisp-ccl

hw-lisp-clisp:generate-hw.lisp generate.lisp hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	@$(CLISP) -norc < generate-hw.lisp > hw-lisp-clisp.log 2>&1
	-@mv hw hw-lisp-clisp

hw-lisp-clisp-fas:Makefile hw.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@(echo '#!/usr/local/bin/clisp -ansi -q -E utf-8' ;\
	  cat hw.fas  ;\
	  echo '(defun argv () (cons (elt (ext:argv) 0) ext:*args*))' ;\
	  echo '(defun start () ' ;\
	  echo '   (handler-case' ;\
	  echo '       (progn' ;\
	  echo '         (load #P"~/.hw.lisp" :if-does-not-exist nil)' ;\
	  echo '         (apply (function hello-world:hw) (argv)))' ;\
	  echo '     (error (err)' ;\
	  echo '       (finish-output *standard-output*)' ;\
	  echo '       (finish-output *trace-output*)' ;\
	  echo '       (format *error-output* "~%~A~%" err)' ;\
	  echo '       (finish-output *error-output*)' ;\
	  echo '       (ext:quit 1)))' ;\
	  echo '   (finish-output *standard-output*)' ;\
	  echo '   (finish-output *trace-output*)' ;\
	  echo '   (finish-output *error-output*)' ;\
	  echo '   (ext:quit 0))' ;\
	  echo '(start)' ) > $@
	@chmod 755 $@

hw.fas:hw.lisp
	@printf "// Compiling: %s\n" $@
	@clisp -ansi -q -E utf-8 -norc -c $^ -o $@ > hw-lisp-clisp-fas.log 2>&1

hw-lisp-ecl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	@$(ECL) -norc < generate-hw.lisp > hw-lisp-ecl.log 2>&1
	-@mv hw hw-lisp-ecl

hw-lisp-sbcl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	@$(SBCL) --no-userinit < generate-hw.lisp > hw-lisp-sbcl.log 2>&1
	-@mv hw hw-lisp-sbcl

test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(./$$p)" ; done
	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f $(ALL_PROGRAMS) *.o *.fas *.lib *.log
