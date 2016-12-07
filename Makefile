.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j

.PHONY: test
test:
	cd 3eamc && rebar3 compile && cd .. && \
	./3eamc.sh test/gb_trees.erl
