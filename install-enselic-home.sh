#!/bin/bash

# Install into ~/.bash_profile
install_file="$HOME/.bash_profile"
if [ ! -f "$install_file" ]; then
    touch "$install_file"
fi
enselic_home_root="$(cd $(dirname $0) && pwd)"
enselic_home_bashrc="$enselic_home_root/bashrc.sh"
if grep "$enselic_home_bashrc" "$install_file" &> /dev/null; then
    echo "$enselic_home_bashrc already installed in $install_file"
else
    echo "Installing $enselic_home_bashrc into $install_file"
    echo "source $enselic_home_bashrc" >> "$install_file"
fi

# Install ~/.gdbinit
if [ ! -f $HOME/.gdbinit ]; then
    echo "installing $HOME/.gdbinit"
    cat > $HOME/.gdbinit <<EOF
# Created with `basename $0`, do not edit!
source $HOME/enselic-home/gdbinit.txt
EOF
else
    echo "$HOME/.gdbinit already installed"
fi

git config --global alias.cp cherry-pick
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
