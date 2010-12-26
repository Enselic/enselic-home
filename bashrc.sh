export PYTHONPATH="/home/martin/prefixes/buildbot-master/lib/python2.6/site-packages"
export PATH="/home/martin/prefixes/buildbot-master/bin:/home/martin/source/gimp-buildbot-utils/shellscripts:/home/martin/source/pastebinit-1.1:/home/martin/bin:/home/martin/enselic-home/bin:$PATH"
export EDITOR="gedit"
export PS1='\[\033[1m\]\u@\h:\w\$ \[\033[0m\]'
alias cd..='cd ..'
export HISTSIZE=10000

# Change to first ancestor dir with a .git subfolder, while avoiding
# to mess up "cd -"
function groot()
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
