source ~/enselic-home/shell/common.sh

 # From http://unix.stackexchange.com/a/48113
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Generated with help of http://bashrcgenerator.com/
export PS1="
\[$(tput dim)\]\u@\h\[$(tput sgr0)\]
\[$(tput bold )\]\w\[$(tput sgr0)\] \[$(tput setf 2)\]\$(__git_ps1)\[$(tput sgr0)\] \[$(tput bold )\]
\$\[$(tput sgr0)\] "

# Mac OS X
if [ `uname` = Darwin -a -f /opt/local/etc/bash_completion ]; then
    source /opt/local/etc/bash_completion
    source /opt/local/share/git-core/contrib/completion/git-completion.bash
fi

# From http://unix.stackexchange.com/a/186167
t() {
  if [[ -z "$ORIG" ]]; then
    ORIG=$PS1
  fi
  TITLE="\[\e]2;$*\a\]"
  PS1=${ORIG}${TITLE}
}
