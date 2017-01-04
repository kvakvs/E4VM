.PHONY: gdb
gdb: compile
	gdb cmake-build-debug/gleam_d

.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j; cd ..
