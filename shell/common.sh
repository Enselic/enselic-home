# Aliases
alias cd..='cd ..'
alias check="git diff --check HEAD^..HEAD"
alias now="date +%Y-%m-%d_%H%M%S"
alias gitkk="gitk --all"
alias gcp=gerrit_cherry_pick

# PATH
export PATH="$HOME/enselic-home/bin:$PATH"

# Functions
blame() {
    file_path="$1"
    cd $(dirname "$file_path")
    git gui blame "$file_path"
}

justpush() {
    git add . && git commit -m "commit msg not relevant" && git push
}

glog() {
    file_path="$1"
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
