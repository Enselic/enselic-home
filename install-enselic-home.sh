#!/bin/bash

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

touch $HOME/.bash_profile
if [ -f $HOME/.bash_profile ]; then
    if grep "$HOME/enselic-home/bashrc.sh" $HOME/.bash_profile &> /dev/null; then
        echo "$HOME/enselic-home/bashrc.sh already installed"
    else
        echo "installing enselic-home"
        echo "source $HOME/enselic-home/bashrc.sh" >> $HOME/.bash_profile
    fi
else
    echo "Did not find $HOME/.bash_profile, not installing enselic-home"
fi

# Install stuff
if [ "Darwin" != `uname` ]; then
sudo apt-get build-dep linux
sudo apt-get install \
    emacs \
    gitk \
    git-gui \
    yakuake \
    build-essential \
    gdb \
    libncurses5-dev \
    id-utils \
    exuberant-ctags \
    strace \
    tree \
    apt-file \
    git-email \
    initramfs-tools \
    ocaml \
    python-dev \
    apt-file \
    libxml2-dev \
    llvm-dev \
    xclip
fi    

curl -O http://www.levien.com/type/myfonts/Inconsolata.otf
if [ "Darwin" != `uname` ]; then
gnome-open Inconsolata.otf
else
open Inconsolata.otf
fi

git config --global alias.st status
git config --global alias.br branch
git config --global alias.rb rebase
git config --global alias.up "pull --rebase"
git config --global alias.ch checkout

git config --global sendemail.from enselic@gmail.com
git config --global sendemail.to enselic@gmail.com

git config --global sendemail.smtpencryption tls
git config --global sendemail.smtpserver smtp.gmail.com
git config --global sendemail.smtpuser enselic@gmail.com
git config --global sendemail.smtpserverport 587

git config --global user.name "Martin Nordholts"
git config --global user.email enselic@gmail.com

git config --global core.excludesfile ~/.gitignore
git config --global push.default matching
