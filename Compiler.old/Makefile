TESTFILE=gb_trees
TESTFILE2=test1
TESTFILE3=mochijson
TESTFILE4=mochijson2

.PHONY: run1 run2 run3 run4
run1:
	rebar3 compile && ./uerlc.sh priv/$(TESTFILE).erl
run2:
	rebar3 compile && ./uerlc.sh priv/$(TESTFILE2).erl
run3:
	rebar3 compile && ./uerlc.sh priv/$(TESTFILE3).erl
run4:
	rebar3 compile && ./uerlc.sh priv/$(TESTFILE4).erl

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: test
test:
	rebar3 ct
