#!/bin/bash

if [ "x$1" = "x--help" -o "x$1" = "x-h" -o "x$1" = "x-?" ]; then
    cat <<EOF
Usage: `basename $0` [ARGS]

Collects adb logcat -v time logs for infinity. Arguments are passed on to adb.

Example 1: Constantly capture device logcat

  \$ android-constant-logcat.sh -d
  Will capture `adb -d logcat` to '/tmp/logcat/constant-logcat_d_2011-08-24_103714.txt'
  - waiting for device -

Example 1: Constantly capture emulator logcat

  \$ android-constant-logcat.sh -e
  Will capture `adb -e logcat` to '/tmp/logcat/constant-logcat_e_2011-08-24_103509.txt'
  - waiting for device -

EOF
    exit 0
fi

logcat_dir="$HOME/logcat"
mkdir -p "$logcat_dir"

while true; do
    filename="$logcat_dir/constant-logcat_`echo $@ | tr -d ' -'`_`date +%F_%H%M%S`.txt"
    ADB_CMD="adb $@ logcat -v time"
    echo "Will capture \`${ADB_CMD}\` to '$filename'"
    ${ADB_CMD} | tee "$filename"
    echo -e "Done capturing \`${ADB_CMD}\` to '$filename', starting over ...\n\n\n\n\n"
done