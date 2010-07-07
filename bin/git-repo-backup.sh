#!/bin/sh
# git-repo-backup.sh

if [ -z "$1" ]; then
    name=`git branch | grep '*' | sed 's/* //'`
    [ -z "$name" ] && exit -1
else
    name=$1
fi

dir=`pwd`
prefix=`basename $dir`
filename="$prefix-$name-`date +%F_%H%M`.tar.gz"

git format-patch origin/master -o $name || exit -1
tar -c $name | gzip > $filename || exit -1

echo "Created '$filename'"
