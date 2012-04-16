#! /bin/bash
set -o xtrace

gerrit_id="$1"
patch_set_id_1="$2"
patch_set_id_2="$3"
if [ -z "$gerrit_id" -o -z "$patch_set_id_1" -o -z "$patch_set_id_2" ]; then
    echo "Usage: `basename $0` GERRIT_ID PATCH_SET_ID_1 PATCH_SET_ID_2"
    exit 1
fi


git_url=`cat .git/config | grep "url =" | awk '{print$3}' | head -n 1`
if [ -z "$git_url" ]; then
    echo "Didn't find git_url"
    exit 1
fi

git fetch ${git_url} refs/changes/${gerrit_id:(-2)}/${gerrit_id}/${patch_set_id_1}
if [ $? != 0 ]; then
    echo "Fetch failed"
    exit 1
fi

sha1_1=$(cat .git/FETCH_HEAD | awk '{print$1}')

git fetch ${git_url} refs/changes/${gerrit_id:(-2)}/${gerrit_id}/${patch_set_id_2}
if [ $? != 0 ]; then
    echo "Fetch failed"
    exit 1
fi

sha1_2=$(cat .git/FETCH_HEAD | awk '{print$1}')

git diff $sha1_1..$sha1_2
echo "Git diff result = $?"
