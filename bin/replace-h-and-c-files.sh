#!/bin/sh

find '(' -name "*.c" -o -name "*.h" ')' -exec perl -pi -e $@ {} \;
