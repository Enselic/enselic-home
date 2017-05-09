# Aliases
alias cd..='cd ..'
alias check="git diff --check HEAD^..HEAD"
alias now="date +%Y-%m-%d_%H%M%S.%N"
alias gitkk="gitk --all"
alias gcp=gerrit_cherry_pick

# PATH
# add to ~/.profile for Alt + F2 Gnome support
export PATH="$HOME/enselic-home/bin:$PATH"

# Functions
blame() {
    file_path="$1"
    cd $(dirname "$file_path")
    git gui blame "$file_path"
}

dumpcores() {
    ulimit -c unlimited
}

f() {
    find . -name $1
}

log-1() {
    git log -1
}

justpush() {
    git add . && git commit -m "commit msg not relevant" && git push
}

wip() {
    git add . && git commit -m "wip"
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
    echo `pwd`/$1
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

gerrit_cherry_pick() {
    if [ -z "$1" ]; then
        echo "Usage: gerrit_cherry_pick nnnnnnn [pp]"
        return 1
    fi
    # http://foo.bar.com/r/123456/ -> 123456
    change="$(echo $1 | sed 's/[^0-9]*\([0-9]\+\)[^0-9]*/\1/g')"
    if [ -z "$2"]; then
        patchSet="latest"
    else
        patchSet="$2"
    fi

    git fetch origin "refs/changes/${change: -2}/${change}/${patchSet}" && \
    git cherry-pick --no-commit FETCH_HEAD && \
    ! git diff --exit-code --cached --quiet && \
    git commit --no-verify --no-edit
}
