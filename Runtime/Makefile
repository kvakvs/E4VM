DEBUG_EMU=cmake-build-debug/e4emu

.PHONY: gdb
gdb: compile
	gdb ${DEBUG_EMU}

.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j; cd ..

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
		cmake-build-debug/PVS-Studio/src/main.cpp.plog

.PHONY: valgrind
valgrind: compile
	valgrind --log-file=valgrind.log \
		cmake-build-debug/gleam_d
