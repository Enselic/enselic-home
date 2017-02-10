source ~/enselic-home/shell/common.sh

export PS1="
%n@%M
%B%d%b%{$fg[green]%}\$(__git_ps1)%{$reset_color%}
$ "

# __git_ps1
source /etc/bash_completion.d/git-prompt

# TODO: Needed?
# zstyle ':completion:::git:*' script /etc/bash_completion.d/git-prompt

# To not correct tree to tee when tree exists
unsetopt correct

# Don't complain when we use HEAD^ instead of 'HEAD^'
unsetopt nomatch

# Don't cycle between many matches when tabbing files and directories
setopt no_auto_menu

# i case insensitive search
# d dump
# q quit (don't beep)
# M long prompt
# F quit if one screen
unset LESS

# Word boundary on /
autoload -Uz select-word-style
select-word-style bash

# Colored prompt
autoload -U colors && colors

# __git_ps1 in PS1
setopt PROMPT_SUBST

# Comments in shell
setopt interactivecomments

# Add commands to history
setopt inc_append_history

# Load commands form history
setopt share_history

# History size
export SAVEHIST=10000

# For auto-completion
# http://stackoverflow.com/a/27853970
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source /etc/bash_completion

t() {
    if [ -z "$1" ]; then
        first="$(basename $(pwd))"
    else
        first="$1"
    fi
    # Partly from http://superuser.com/a/633952
    printf "\033];%s\07\n" "$first $2 $3 $4 $3"
}
