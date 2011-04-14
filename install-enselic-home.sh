#!/bin/sh

if [ ! -f $HOME/.gdbinit ]; then
    echo "installing $HOME/.gdbinit"
    cat > $HOME/.gdbinit <<EOF
# Created with `basename $0`, do not edit!
source $HOME/enselic-home/gdbinit.txt
EOF
else
    echo "$HOME/.gdbinit already installed"
fi

if [ ! -f $HOME/.emacs ]; then
    echo "installing $HOME/.emacs"
    cat > $HOME/.emacs <<EOF
(setq user-init-file "$HOME/enselic-home/elisp/init.el")
(load user-init-file)
EOF
else
    echo "$HOME/.emacs already installed"
fi

if [ -f $HOME/.bashrc ]; then
    if grep "$HOME/enselic-home/bashrc.sh" $HOME/.bashrc &> /dev/null; then
        echo "$HOME/enselic-home/bashrc.sh already installed"
    else
        echo "installing enselic-home"
        echo "source $HOME/enselic-home/bashrc.sh" >> $HOME/.bashrc
    fi
else
    echo "Did not find $HOME/.bashrc, not installing enselic-home"
fi

git config --global alias.st status
git config --global alias.br branch
git config --global alias.rb rebase
git config --global alias.up "pull --rebase"
git config --global alias.ch checkout
