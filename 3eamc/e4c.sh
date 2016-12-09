#!/bin/sh
erl -pa _build/default/lib/*/ebin -noshell -s 3eamc -extra $@
