#!/bin/sh

BASE=`pwd`
#for dir in src src/bif src/miniz src/platf include include/bif include/struct test; do
for F in `find . -name \*.h -print -o -name \*.cpp -print -name \*.c -print`; do
    echo "===> $F"
    clang-format-3.8 -i $F
done