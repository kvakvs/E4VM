#!/bin/sh
erl -pa 3eamc/_build/default/lib/*/ebin \
    -noshell -s 3eamc start_3eamc $@ -s init stop