export PATH="$HOME/bin:$HOME/enselic-home/bin:$PATH"
export EDITOR="gedit"
export PS1='\[\033[1m\]\u@\h:\w\$ \[\033[0m\]'
alias cd..='cd ..'
export HISTSIZE=10000
source ~/enselic-home/gdbinit.txt

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

installtarball()
{
    if [ $# -lt 2 ]; then
        echo "Usage: installtarball PREFIX URL"
        return;
    fi

    prefix=$1
    url=$2

    tarballname=${url##*/}
    dirname=${tarballname%.tar.*}

    wget ${url}
    tar -xvf ${tarballname}
    cd ${dirname}
    ./configure --prefix=${prefix}
    make -j5
    make install
    cd ..
}
