# INSTALLATION
# ============

# One of
# curl -L https://setofskills.com/setofskills.zshrc -o ~/setofskills.zshrc ; echo "source ~/setofskills.zshrc" >> ~/.zshrc
# curl -L https://raw.githubusercontent.com/Enselic/enselic-home/master/setofskills.zshrc -o ~/setofskills.zshrc ; echo "source ~/setofskills.zshrc" >> ~/.zshrc


# HISTORY
# =======

HISTFILE=~/.zsh_history # [1]
HISTSIZE=100000 # [1]
SAVEHIST=50000 # [1]
setopt sharehistory histignoredups histexpiredupsfirst # [2]



# OTHER OPTIONS
# =============

setopt interactivecomments # [2]
unsetopt appendcreate autocd automenu clobber correct extendedglob listbeep menucomplete nomatch # [2]



# PATH
# ====

if [[ "$(uname)" = "Darwin" ]]; then
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
    export PATH="$PATH:/Users/$USER/Library/Android/sdk/platform-tools"
    export PATH="$PATH:/Users/$USER/.cargo/bin"
fi



# BINDINGS
# ========

bindkey -e # [3]

type-git-branch () {
    br="$(git branch --show-current)"
    LBUFFER=${LBUFFER}${br}
}
zle -N type-git-branch
bindkey '^G^B' type-git-branch

type-git-hash () {
    h="$(git log -1 --format=%h)"
    LBUFFER=${LBUFFER}${h}
}
zle -N type-git-hash
bindkey '^G^H' type-git-hash
bindkey -r '^G' # Disable so that being slow above does not timeout

# https://superuser.com/a/1603845/173759
bindkey "^R" history-incremental-pattern-search-backward

# COMPLETION
# ==========

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
#                      Green bacKground, black Foreground, bold (inspired by gitk branch name style)
#                      |                      Red 'U' if there are unstaged changes
#                      |                      |         Green 'S' if there are staged changes
#                      |                      |         |
#                      |                    vvvvvvvvv   |
#                  vvvvvvvvvvvvvvvvvvvvvvvv          vvvvvvvvv
baseformatstring=" %K{2}%F{0} %%B%b%%b %f%k %F{1}%u%f%F{2}%c%f"
zstyle ':vcs_info:*' check-for-changes true # [4]
zstyle ':vcs_info:*' get-revision true # [4]
zstyle ':vcs_info:*' formats "$baseformatstring" # [4]
zstyle ':vcs_info:*' actionformats "$baseformatstring %F{5}%a%f" # [4]
precmd() {
    if [ -d .git ]; then
        vcs_info # [4]
        commit_info=$(git log --color '--pretty=format:%C(yellow)%h%Creset %s%n' -1 | cut -c 1-50)
        ref_info=$(git log --color --format=short --decorate-refs=refs/remotes --decorate-refs=refs/tags | head -n 1 | sed $'s/\x1B[^\x1B]*\x1B\\[m//')
        git_line="
${commit_info}${vcs_info_msg_0_}${ref_info}"
    else
        git_line=""
    fi;
}

PROMPT="



%n @ %M \$(date '+%Y-%m-%d %H:%M:%S')
%B%d%b\${git_line}
%# " # [4] [6]



# GIT CONFIG
# ==========

if [[ "$(git config --global alias.ch)" != "checkout" ]]; then
    echo "Setting up personal git config with e.g. ch = checkout alias"

    git config --global alias.br "branch --sort=-committerdate"
    git config --global alias.ch "checkout"
    git config --global alias.cp "cherry-pick"
    git config --global alias.cm "commit -m"
    git config --global alias.diffcw "diff --color-words=."
    git config --global alias.diffs "diff --staged"
    git config --global alias.diffscw "diff --staged --color-words=."
    git config --global alias.pop "reset --hard HEAD^"
    git config --global alias.showcw "show --color-words=."
    git config --global alias.rb "rebase"
    git config --global alias.restores "restore --staged"
    git config --global alias.st "status"
    git config --global alias.up "pull --rebase"

    git config --global sendemail.from enselic@gmail.com
    git config --global sendemail.smtpencryption tls
    git config --global sendemail.smtpserver smtp.gmail.com
    git config --global sendemail.smtpserverport 587
    git config --global sendemail.smtpuser enselic@gmail.com
    git config --global sendemail.to enselic@gmail.com

    git config --global branch.sort -committerdate
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
alias graph="git log --graph --oneline"
alias clipboard='xclip -selection clipboard'
if [ "$(uname -s)" == Linux ]; then
    alias open=xdg-open
fi

# Functions
blame() {
    file_path="$1"
    cd $(dirname "$file_path")
    git gui blame "$file_path"
}

# Inspired by https://medium.com/@mrWinston/smarter-git-checkout-using-fzf-to-supercharge-your-commandline-7507db600996
gch() {
    git checkout $(git branch --sort=-committerdate | fzf --no-sort  | tr -d '[:space:]')
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
    git add . && git commit -m "wip $*"
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

# [1] http://zsh.sourceforge.net/Doc/Release/Parameters.html#Parameters-Used-By-The-Shell
# [2] http://zsh.sourceforge.net/Doc/Release/Options.html#Options
# [3] http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Keymaps
# [4] http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Version-Control-Information
# [5] https://wiki.archlinux.org/index.php/zsh#Colors
# [6] http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Prompt-Expansion
# [7] http://zsh.sourceforge.net/Doc/Release/Completion-System.html#Use-of-compinit
