#!/bin/sh
erlc gcompile.erl && erl -noshell -s gcompile main $@ -s init stop