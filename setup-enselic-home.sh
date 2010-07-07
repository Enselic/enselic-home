#!/bin/sh

if [ ! -f ~/.gdbinit ]; then
    cat > ~/.gdbinit <<EOF
# Created with `basename $0`, do not edit!
source ~/enselic-home/gdbinit.txt
EOF
fi
