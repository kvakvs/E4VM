TESTFILE=gb_trees
TESTFILE2=test1

.PHONY: run1
run1:
	rebar3 compile && ./e4c.sh priv/$(TESTFILE).erl
.PHONY: run2
run2:
	rebar3 compile && ./e4c.sh priv/$(TESTFILE2).erl

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: test
test:
	rebar3 ct
