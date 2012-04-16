alias gitka="gitk &"
alias gitc="git rebase --continue"
alias gits="git rebase --skip"
alias acat="adb shell cat"
alias als="adb shell ls -l"
alias arm="adb shell rm"
alias pud="pushd ."
alias pod="popd"
alias cd..='cd ..'

export PATH="$HOME/bin:$HOME/enselic-home/bin:$PATH"
export EDITOR="gedit"
export PS1='\[\033[1m\]\u@\h:\w\$ \[\033[0m\]'
export HISTSIZE=10000
export PROMPT_COMMAND="history -n; history -a"

export ANDROID_SDK=~/android/android-sdk-linux
if [ -n "$ANDROID_SDK" ]; then
    export PATH="$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools:$PATH"
fi

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
