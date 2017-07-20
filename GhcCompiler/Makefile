.PHONY: compile
compile:
	stack build
#	ghc -O2 --make src/uerlc.hs src/tokenizer.hs -threaded -rtsopts \
#	    -o uerlc

PRIV=../Compiler/priv

INP1=$(PRIV)/gb_trees.S
INP2=$(PRIV)/mochijson.S
INP3=$(PRIV)/mochijson2.S

ERLFILE1=$(INP1:.S=.erl)
ERLFILE2=$(INP2:.S=.erl)
ERLFILE3=$(INP3:.S=.erl)


$(INP1): $(ERLFILE1)
	erlc -S -o $(PRIV) $(ERLFILE1)
$(INP2): $(ERLFILE2)
	erlc -S -o $(PRIV) $(ERLFILE2)
$(INP3): $(ERLFILE3)
	erlc -S -o $(PRIV) $(ERLFILE3)

.PHONY: run1 run2 run3
run1: compile $(INP1)
	stack exec uerlc-exe $(INP1)
run2: compile $(INP2)
	stack exec uerlc-exe $(INP2)
run3: compile $(INP3)
	stack exec uerlc-exe $(INP3)
