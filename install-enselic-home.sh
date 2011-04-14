#!/bin/sh

if [ ! -f ~/.gdbinit ]; then
    echo "installing ~/.gdbinit"
    cat > ~/.gdbinit <<EOF
# Created with `basename $0`, do not edit!
source ~/enselic-home/gdbinit.txt
EOF
else
    echo "~/.gdbinit already installed"
fi

if [ -f ~/.bashrc ]; then
    if grep "enselic-home/bashrc.sh" ~/.bashrc &> /dev/null; then
        echo "enselic-home already installed"
    else
        echo "installing enselic-home"
        echo "source $HOME/enselic-home/bashrc.sh" >> ~/.bashrc
    fi
else
    echo "Did not find ~/.bashrc, not installing enselic-home"
fi
