#!/bin/bash

# Tests various values for --jobs to make to see which one is fastest. Pass
# max number of attemtps as parameter. Defaults to 8

max=$1
if [ -z "$max" ]; then
    max=8
fi

job_values=`seq 1 $max`

rm -f ~/make-j-benchmark-log.txt

for i in $job_values; do
    make clean
    start=`date +%s`
    make -j$i || exit 42
    end=`date +%s`
    time_in_seconds=$(($end - $start))
    echo make -j$i took $(($time_in_seconds / 60)) min $(($time_in_seconds % 60)) sec >> ~/make-j-benchmark-log.txt
done
