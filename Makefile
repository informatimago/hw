all:hw-c hw-lisp-ccl hw-lisp-ecl

.PHONY: hw-c hw-lisp-ccl hw-lisp-ecl
LINE="//----------------------------------------------------------------------"
hw-c:hw.c
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "C" $@
	@printf "%s\n" "$(LINE)"
	cc -o hw-c hw.c

hw-lisp-ccl:generate.lisp hw.asd hw.lisp
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "Lisp" $@
	@printf "%s\n" "$(LINE)"
	ccl -n < generate.lisp
	-mv hw hw-lisp-ccl

hw-lisp-ecl:generate.lisp hw.asd hw.lisp
	@printf "%s\n" "$(LINE)"
	@printf "// Generating Executable from %s source:  %s\n" "Lisp" $@
	@printf "%s\n" "$(LINE)"
	-rm -f ~/.cache/common-lisp/ecl-16.0.0-unknown-macosx-x64/Users/pjb/src/hw/hw
	ecl -norc < generate.lisp
	-mv hw hw-lisp-ecl

clean:
	-rm -f hw-c hw-lisp-ccl hw-lisp-ecl

