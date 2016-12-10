.PHONY: run
run:
	rebar3 compile && ./e4c.sh priv/test1.erl

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: test
test:
	rebar3 ct
