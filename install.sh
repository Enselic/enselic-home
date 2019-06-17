#!/bin/bash

add_to_file() {
    content="$1"
    target="$2"

    echo "Adding '$content' to '$target'"

    touch "$target"
    grep --quiet --fixed-strings "$content" "$target" ||
            echo "$content" >> "$target"
}

add_to_file "source $(dirname $0)/shell/bashrc.sh" $HOME/.bash_aliases
add_to_file "source $(dirname $0)/shell/zshrc.sh" $HOME/.zshrc
add_to_file "$(grep '^export PATH=' $(dirname $0)/shell/common.sh)" ~/.profile

git config --global alias.cp "cherry-pick"
git config --global alias.st "status"
git config --global alias.br "branch --sort=-committerdate"
git config --global alias.rb "rebase"
git config --global alias.up "pull --rebase"
git config --global alias.ch "checkout"

git config --global sendemail.from enselic@gmail.com
git config --global sendemail.to enselic@gmail.com

git config --global sendemail.smtpencryption tls
git config --global sendemail.smtpserver smtp.gmail.com
git config --global sendemail.smtpuser enselic@gmail.com
git config --global sendemail.smtpserverport 587

git config --global user.name "Martin Nordholts"
git config --global user.email enselic@gmail.com

git config --global core.editor "/usr/bin/code -n -w"
git config --global core.excludesfile ~/.gitignore
git config --global log.decorate short
git config --global push.default matching
git config --global merge.conflictstyle diff3
git config --global rebase.autosquash true

// From Dethariel here https://stackoverflow.com/a/48999882/287761
git config --global alias.amend-to '!f() { SHA=`git rev-parse "$1"`; git commit --fixup "$SHA" && GIT_SEQUENCE_EDITOR=true git rebase --autostash --interactive --autosquash "$SHA^"; }; f'
