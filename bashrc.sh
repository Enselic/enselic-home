alias gitka="gitk &"
alias gitc="git rebase --continue"
alias gits="git rebase --skip"
alias acat="adb shell cat"
alias als="adb shell ls -l"
alias arm="adb shell rm"
alias pud="pushd ."
alias pod="popd"
alias cd..='cd ..'
alias clip="xclip -selection clipboard"
alias clip="xclip -selection clipboard"
alias check="git diff --check HEAD^..HEAD"
alias ms="git br -r | grep m/"




# From http://unix.stackexchange.com/a/48113

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"




export PATH="$HOME/bin:$HOME/enselic-home/bin:$PATH"
export EDITOR="gedit"
export PS1='\[\033[1m\]\u@\h:\w\$ \[\033[0m\]'

export ANDROID_SDK=~/android/android-sdk-linux
if [ -n "$ANDROID_SDK" ]; then
    export PATH="$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools:$PATH"
fi

if [ `uname` = Darwin -a -f /opt/local/etc/bash_completion ]; then
    source /opt/local/etc/bash_completion
    source /opt/local/share/git-core/contrib/completion/git-completion.bash
fi


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

function installtarball()
{
    if [ $# -lt 2 ]; then
        echo "Usage: installtarball PREFIX URL"
        return;
    fi

    prefix=$1
    url=$2

    tarballname=${url##*/}
    dirname=${tarballname%.tar*}

    curl --remote-name ${url}
    if [[ ${tarballname} =~ .*\.tar\.xz$ ]]; then
        unxz ${tarballname}
        tar -xf ${dirname}.tar
    else
        tar -xf ${tarballname}
    fi
    cd ${dirname}
    . ${prefix}/share/config.site
    ./configure --prefix=${prefix} && \
    make -j5 && \
    make install
    cd ..
}
