#!/bin/sh
erl -pa _build/default/lib/*/ebin -noshell -s uerlc -extra $@
