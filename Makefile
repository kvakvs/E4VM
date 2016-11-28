.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j

.PHONY: test
test:
	./3eamc test/test1.erl