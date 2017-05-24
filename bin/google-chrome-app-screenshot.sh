#!/bin/bash

while [ $# -gt 0 ]; do case "$1" in
	-s)
		window_size="$2"
		shift 2
		;;
	-o)
		output="$2"
		shift 2
		;;
	*)
		url="$1"
		if [ -z "$url" -o \
			 -z "$output" -o \
			 -z "$window_size" ]; then
			echo "Usage: $(basename $0) -o foo.jpg -s WIDTH,HEIGHT URL"
			exit 0
		fi
		break
		;;
esac ; done

# Skip first popup
chrome="google-chrome --user-data-dir=/tmp/clean"
$chrome --make-default-browser

# Launch web app
$chrome --new-window --window-size=$window_size --app=$url &
chrome_pid=$!

# Let the UI settle and take screenshot
sleep 2
import -window $(xdotool getactivewindow) $output

# Done
kill $chrome_pid
