# Aliases
alias cd..='cd ..'
alias check="git diff --check HEAD^..HEAD"
alias now="date +%Y-%m-%d_%H%M%S"
alias gitkk="gitk --all"

# PATH
export PATH="$HOME/enselic-home/bin:$PATH"

# Functions
blame() {
    file_path="$1"
    cd $(dirname "$file_path")
    git gui blame "$file_path"
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
