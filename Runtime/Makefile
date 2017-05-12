E4BUILDDIR=cmake-build-debug
E4EMU=${E4BUILDDIR}/e4emu

${E4EMU}: compile

.PHONY: gdb
gdb: compile
	gdb ${E4EMU}

.PHONY: compile
compile:
	cd ${E4BUILDDIR} && $(MAKE) -j; cd ..

.PHONY: lic-add
lic-add:
	rm -rf TMP && \
	copyright-header --license-file license.snippet \
		--add-path src:include \
		--output-dir TMP
.PHONY: lic-rm
lic-rm:
	rm -rf TMP && \
	copyright-header --license-file license.snippet \
		--remove-path src:include \
		--output-dir TMP

# Convert and print PVS-studio results if it wasn't done automatically
.PHONY: plog
plog:
	plog-converter -t tasklist -a "GA:1,2;64:1;CS" \
		${E4BUILDDIR}/PVS-Studio/src/main.cpp.plog

.PHONY: valgrind
valgrind: compile
	valgrind --log-file=valgrind.log \
		${E4EMU}

#
# Invoke nm and size tools to print section sizes of the executable
#
.PHONY: sizes
sizes: ${E4EMU}
	nm --print-size --size-sort --radix=d ${E4EMU} | sort -k 2 -r > size_report_1.txt && \
	size -A -d ${E4EMU} > size_report_2.txt
