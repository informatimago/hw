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

hw-lisp-ccl:generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	@$(CCL) -n < generate.lisp > hw-lisp-ccl.log 2>&1
	-@mv hw hw-lisp-ccl

hw-lisp-clisp:generate.lisp hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	@$(CLISP) -norc < generate.lisp > hw-lisp-clisp.log 2>&1
	-@mv hw hw-lisp-clisp

hw-lisp-clisp-fas:Makefile hw.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@( echo '#!/usr/local/bin/clisp -ansi -q -E utf-8' ;\
	  cat hw.fas ;\
	  echo '(hello-world:hw #|ext:*args*|#)' ) > $@
	@chmod 755 $@

hw.fas:hw.lisp
	@printf "// Compiling: %s\n" $@
	@clisp -ansi -q -E utf-8 -norc -c $^ -o $@ > hw-lisp-clisp-fas.log 2>&1

hw-lisp-ecl:generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	@$(ECL) -norc < generate.lisp > hw-lisp-ecl.log 2>&1
	-@mv hw hw-lisp-ecl

hw-lisp-sbcl:generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	@$(SBCL) --no-userinit < generate.lisp > hw-lisp-sbcl.log 2>&1
	-@mv hw hw-lisp-sbcl

test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(./$$p)" ; done
	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f $(ALL_PROGRAMS) *.o *.fas *.lib *.log
