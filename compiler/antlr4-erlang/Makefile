all: compile

compile:
	antlr4 -visitor -listener -Dlanguage=Cpp Erlang.g4 -o ../src/antlr4

debug: compile
	java org.antlr.v4.runtime.misc.TestRig  Erlang forms -gui -encoding utf8

check: compile
	./check.sh test/

clean:
	rm -f *.cpp *.h *.tokens
