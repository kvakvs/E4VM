STACKOPT=--profile
RUNOPT=+RTS -xc

.PHONY: compile
compile:
	stack build $(STACKOPT)

TESTDATA=testData/

INP1=$(TESTDATA)/gb_trees.S
INP2=$(TESTDATA)/mochijson.S
INP3=$(TESTDATA)/mochijson2.S

%.S: %.erl
	erlc -S -o $(@D) $^

.PHONY: run1 run2 run3
run1: compile $(INP1)
	stack exec -- uerlc-exe $(INP1) $(RUNOPT) 2>&1 | less -r
run2: compile $(INP2)
	stack exec -- uerlc-exe $(INP2) $(RUNOPT) 2>&1 | less -r
run3: compile $(INP3)
	stack exec -- uerlc-exe $(INP3) $(RUNOPT) 2>&1 | less -r
