.PHONY: compile
compile:
	stack build
#	ghc -O2 --make src/uerlc.hs src/tokenizer.hs -threaded -rtsopts \
#	    -o uerlc

PRIV=../Compiler/priv
INP1=$(PRIV)/gb_trees.S

.PHONY: run
run: compile
	./uerlc $(INP1)
