MYCMD=./cmake-build-debug/compiler
MYARGS=../e4compile/priv/test1.erl

.PHONY: debug run

run:
	$(MYCMD) $(MYARGS)

debug:
	gdb -r --args $(MYCMD) $(MYARGS)
