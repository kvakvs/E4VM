STACKOPT=--profile
RUNOPT=+RTS -xc

.PHONY: compile
compile:
	stack build $(STACKOPT)

PRIV=../Compiler/priv

INP1=$(PRIV)/gb_trees.S
INP2=$(PRIV)/mochijson.S
INP3=$(PRIV)/mochijson2.S

%.S: %.erl
	erlc -S -o $(@D) $^

.PHONY: run1 run2 run3
run1: compile $(INP1)
	stack exec -- uerlc-exe $(INP1) $(RUNOPT)
run2: compile $(INP2)
	stack exec -- uerlc-exe $(INP2) $(RUNOPT)
run3: compile $(INP3)
	stack exec -- uerlc-exe $(INP3) $(RUNOPT)
