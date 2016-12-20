#TESTFILE=test1
TESTFILE=gb_trees

.PHONY: run
run:
	rebar3 compile && ./e4c.sh priv/$(TESTFILE).erl

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: test
test:
	rebar3 ct
