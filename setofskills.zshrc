# INSTALATTION INSTRUCTIONS
# =========================
#
# Run this command:
#
#   name=setofskills.zshrc ; curl -L setofskills.com/$name -o ~/$name ; echo "source ~/$name" >> ~/.zshrc



# HISTORY
# =======

HISTFILE=~/.zsh_history # [1]
HISTSIZE=100000 # [1]
SAVEHIST=50000 # [1]
setopt sharehistory histignoredups histexpiredupsfirst # [2]



# OTHER OPTIONS
# =============

setopt interactivecomments # [2]
unsetopt appendcreate autocd automenu clobber correct extendedglob listbeep menucomplete # [2]



# BINDINGS
# ========

bindkey -e # [3]

# Ctrl-Up goes up a dir
# Press Ctrl-Up after running 'cat' to see if the code is still correct
bindkey -s '^[[A' 'cd ..\n'



# COMPLETION
# ==========

#
autoload -Uz compinit && compinit -i # [7]



# GIT PROMPT
# ==========

setopt promptsubst # [2]
autoload -Uz vcs_info # [4]

# Colors [5]
# ------
# 0 = black
# 1 = red
# 2 = green
# 3 = yellow
# 4 = blue
# 5 = magenta
# 6 = cyan
# 7 = white
#
# %F{n} = Foreground color change
# %f    = stop foreground color change
# %K{n} = bacKground color change
# %k    = stop background color change
#
#                      First 10 SHA1 chars in yellow fg like in git log
#                      |              Green bacKground, black Foreground, bold (inspired by gitk branch name style)
#                      |              |                      Red 'U' if there are unstaged changes
#                      |              |                      |         Green 'S' if there are staged changes
#                      |              |                      |         |
#                      |              |                    vvvvvvvvv   |
#                  vvvvvvvvvvvvvv vvvvvvvvvvvvvvvvvvvvvvvv          vvvvvvvvv
baseformatstring=" %F{3}%10.10i%f %K{2}%F{0} %%B%b%%b %f%k %F{1}%u%f%F{2}%c%f"
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' formats "$baseformatstring"
zstyle ':vcs_info:*' actionformats "$baseformatstring %F{5}%a%f"
precmd() { vcs_info }

PROMPT="
%n @ %M
%B%d%b\${vcs_info_msg_0_}
%# " # [6]

if [[ "$(uname)" = "Darwin" ]]; then
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
fi



# GIT CONFIG
# ==========

if [[ "$(git config --global alias.ch)" != "checkout" ]]; then
    echo "Setting up personal git config with e.g. ch = checkout alias"

    git config --global alias.br "branch --sort=-committerdate"
    git config --global alias.ch "checkout"
    git config --global alias.cp "cherry-pick"
    git config --global alias.rb "rebase"
    git config --global alias.st "status"
    git config --global alias.up "pull --rebase"

    git config --global sendemail.from enselic@gmail.com
    git config --global sendemail.smtpencryption tls
    git config --global sendemail.smtpserver smtp.gmail.com
    git config --global sendemail.smtpserverport 587
    git config --global sendemail.smtpuser enselic@gmail.com
    git config --global sendemail.to enselic@gmail.com

    git config --global core.editor "code -n -w"
    git config --global core.excludesfile ~/.gitignore
    git config --global log.decorate short
    git config --global merge.conflictstyle diff3
    git config --global push.default nothing
    git config --global rebase.autosquash true
    git config --global user.email enselic@gmail.com
    git config --global user.name "Martin Nordholts"

    # From Dethariel here https://stackoverflow.com/a/48999882/287761
    git config --global alias.amend-to '!f() { SHA=`git rev-parse "$1"`; git commit --fixup "$SHA" && GIT_SEQUENCE_EDITOR=true git rebase --autostash --interactive --autosquash "$SHA^"; }; f'
fi



# ALIASES AND FUNCTIONS
# =====================

# Aliases
alias check="git diff --check HEAD^..HEAD"
alias now="date +%Y-%m-%d_%H%M%S.%N"
alias gitkk="gitk --all"
alias clipboard='xclip -selection clipboard'

# Functions
blame() {
    file_path="$1"
    cd $(dirname "$file_path")
    git gui blame "$file_path"
}

old() {
    mv -v $1 $1-$(now)
}

dumpcores() {
    ulimit -c unlimited
}

f() {
    find . -name "$1"
}

cf() {
    find . -name "$1" -exec cat {} \;
}

log-1() {
    git log -1
}

justpush() {
    git add . && git commit -m "commit msg not relevant" && git push
}

wip() {
    git add . && git commit -m "wip $@"
}

amend() {
    git commit --amend
}

aamend() {
    git commit -a --amend
}

hardamend() {
    git reset HEAD^
    git commit -a --amend
}

glog() {
    file_path="$1"
    if [ "${file_path:0:1}" != "/" ]; then
        file_path="$(pwd)/$file_path"
    fi
    cd $(dirname "$file_path")
    git log "$file_path"
}

# Convert a path relative to the working directory to an absolute path
p()
{
    echo "$(pwd)"/"$1"
}

# Change to first ancestor dir with a .git subfolder, while avoiding
# to mess up "cd -"
groot()
{
    previous_dir=`cd -`
    original_dir=`pwd`
    while [ ! -d ".git" ] && [ ! `pwd` = "/" ]; do
        cd ..
    done

    if [ ! -d ".git" ]; then
        echo "No .git found in current or ancestor dirs"
        cd "$previous_dir"
        cd "$original_dir"
    else
        new_dir=`pwd`
        # So 'cd -' works
        cd "$original_dir"
        cd "$new_dir"
    fi
}

# Change to first ancestor dir with a .repo subfolder, while avoiding
# to mess up "cd -"
rroot()
{
    previous_dir=`cd -`
    original_dir=`pwd`
    while [ ! -d ".repo" ] && [ ! `pwd` = "/" ]; do
        cd ..
    done

    if [ ! -d ".repo" ]; then
        echo "No .repo found in current or ancestor dirs"
        cd "$previous_dir"
        cd "$original_dir"
    else
        new_dir=`pwd`
        # So 'cd -' works
        cd "$original_dir"
        cd "$new_dir"
    fi
}



# REFERENCES
# ==========
#
# [1] http://zsh.sourceforge.net/Doc/Release/Parameters.html#Parameters-Used-By-The-Shell
# [2] http://zsh.sourceforge.net/Doc/Release/Options.html#Options
# [3] http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Keymaps
# [4] http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Version-Control-Information
# [5] https://wiki.archlinux.org/index.php/zsh#Colors
# [6] http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Prompt-Expansion
# [7] http://zsh.sourceforge.net/Doc/Release/Completion-System.html#Use-of-compinit
