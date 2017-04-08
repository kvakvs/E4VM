#!/bin/sh
erl -pa _build/default/lib/*/ebin -noshell -s e4c -extra $@
