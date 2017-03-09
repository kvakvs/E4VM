MYCMD=./cmake-build-debug/compiler
MYARGS=priv/gb_trees.P

.PHONY: debug run

run: compile
	$(MYCMD) $(MYARGS)

debug: compile
	gdb -r --args $(MYCMD) $(MYARGS)

.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j8