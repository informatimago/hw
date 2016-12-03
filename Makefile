ALL_PROGRAMS= \
	hw-c \
	hw-pascal \
	hw-lisp-ccl \
	hw-lisp-ecl

all:$(ALL_PROGRAMS)

FPC=/opt/local/lib/fpc/bin/fpc
CCL=ccl
ECL=ecl
CC=cc
LINE="//----------------------------------------------------------------------"

.PHONY: all clean test

hw-c:hw.c
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "C" $@
	@printf "%s\n" "$(LINE)"
	$(CC) -o hw-c hw.c

hw-pascal:hw.pas
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "Pascal" $@
	@printf "%s\n" "$(LINE)"
	$(FPC) -ohw-pascal hw.pas

hw-lisp-ccl:generate.lisp hw.asd hw.lisp
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "Lisp" $@
	@printf "%s\n" "$(LINE)"
	$(CCL) -n < generate.lisp
	-mv hw hw-lisp-ccl

hw-lisp-ecl:generate.lisp hw.asd hw.lisp
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "Lisp" $@
	@printf "%s\n" "$(LINE)"
	-rm -f ~/.cache/common-lisp/ecl-16.0.0-unknown-macosx-x64/Users/pjb/src/hw/hw
	$(ECL) -norc < generate.lisp
	-mv hw hw-lisp-ecl

test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(./$$p)" ; done

clean:
	-rm -f $(ALL_PROGRAMS) *.o
