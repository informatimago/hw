unexport JAVA_TOOL_OPTIONS

ALL_PROGRAMS=  \
	hw-c        \
	hw-ecl       \
	hw-java       \
	hw-pascal      \
	hw-haskell      \
	hw-lisp-ccl      \
	hw-lisp-ecl       \
	hw-lisp-sbcl       \
	hw-lisp-clisp       \
	hw-lisp-clisp-fas

all:$(ALL_PROGRAMS)

ECL_INCS=-I/opt/local/include
ECL_LIBS=-L/opt/local/lib -lecl
ECL_RUN=DYLD_LIBRARY_PATH=/opt/local/lib:$(DYLD_LIBRARY_PATH) LD_LIBRARY_PATH=/opt/local/lib:$(LD_LIBRARY_PATH)

FPC=fpc
CLISP=clisp
CCL=ccl
ECL=ecl
SBCL=sbcl
CC=cc
HASKELL=ghc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)

define compile
	@echo "$2" >  $1
	@$2        >> $1 2>&1 || cat $1
endef

.PHONY: all clean test

hw-c:hw.c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	$(call compile,hw-c.log,$(CC) -o $@ hw.c)

hw-c-static:hw.c
	@printf "// Generating Static Executable from %s source: %s\n" "C" $@
	$(call compile,hw-c-static.log,$(CC) -static -o $@ hw.c)

hw-java:hw.java
	@printf "// Generating Executable from %s source: %s\n" "Java" $@
	$(call compile,hw-java.log,javac hw.java && jar cf hw.jar HelloWorld.class && printf '#!/bin/sh\nexec java HelloWorld hw.jar\n' > $@ && chmod 755 $@)

hw-pascal:hw.pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	$(call compile,hw-pascal.log,$(FPC) -ohw-pascal hw.pas)

hw-lisp-ccl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	$(call compile,hw-lisp-ccl.log,$(CCL) -n < generate-hw.lisp)
	-@mv hw hw-lisp-ccl

hw-lisp-clisp:generate-hw.lisp generate.lisp hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	$(call compile,hw-lisp-clisp.log,$(CLISP) -norc < generate-hw.lisp)
	-@mv hw hw-lisp-clisp

hw-lisp-clisp-fas:Makefile hw.fas hw-clisp-fas-rt.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@(echo '#!/usr/local/bin/clisp -norc -ansi -q -E utf-8' ;\
	  cat hw.fas hw-clisp-fas-rt.fas ) > $@
	@chmod 755 $@

hw-clisp-fas-rt.fas:hw-clisp-fas-rt.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,hw-lisp-clisp-fas-rt.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

hw.fas:hw.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,hw-lisp-clisp-fas.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

hw-lisp-sbcl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	$(call compile,hw-lisp-sbcl.log,$(SBCL) --no-userinit < generate-hw.lisp)
	-@mv hw hw-lisp-sbcl

hw-lisp-ecl:generate-hw.lisp generate.lisp hw.asd hw.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,hw-lisp-ecl.log,$(ECL) -norc < generate-hw.lisp)
	-@mv hw hw-lisp-ecl

hw-ecl-lisp:hw-ecl-lisp.c hw.lisp
	@printf "// Generating Object from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,hw-ecl-lisp-lisp.log,$(ECL) --norc --eval '(compile-file "hw.lisp")' --eval '(quit)')
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,hw-ecl-lisp-c.log,$(CC) -o hw-ecl-lisp hw-ecl-lisp.c hw.fas $(ECL_INCS) $(ECL_LIBS))

# ecl-hello-r-lisp: ecl-hello-r-lisp.c ecl-hello-r-lisp.a
# 		$(CC) `ecl-config --cflags` -o $@ ecl-hello-r-lisp.c ecl-hello-r-lisp.a `ecl-config --ldflags` -lecl
#
# ecl-hello-r-lisp.a: constants.h ecl-hello-r-lisp.lisp
# 		# HACK: Force recompilation of ecl-hello-r-lisp.lisp
# 		# when header file changes. There's likely a better
# 		# way to do this via asdf:make-build ...
# 		touch ecl-hello-r-lisp.lisp
# 		ecl -norc \
# 		-eval '(require :asdf)' \
# 		-eval '(push "./" asdf:*central-registry*)' \
# 		-eval '(asdf:make-build :ecl-hello-r-lisp :type :static-library :move-here "./" :init-name "init_lib_ECL_HELLO_R_LISP")' \
# 		-eval '(quit)'


hw-ecl:hw-ecl.c
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,hw-ecl-c.log,$(CC) -o hw-ecl hw-ecl.c $(ECL_INCS) $(ECL_LIBS))

hw-haskell:hw.hs
	@printf "// Generating Executable from %s source: %s\n" "Haskell" $@
	-@rm -f *.o
	$(call compile,hw-haskell.log,$(HASKELL) $^)
	-@mv hw hw-haskell

test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(case $$p in (*ecl*) $(ECL_RUN) ./$$p ;; (*) ./$$p ;; esac)" ; done
	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f *.o *.fas *.lib *.log *.hi *.jar *.class
	-rm -f $(ALL_PROGRAMS)
