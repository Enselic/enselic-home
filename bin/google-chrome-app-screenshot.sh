#!/bin/bash

set -e

outdirs="$1"
if [ -z "$outdirs" ]; then
	echo "Usage: $(basename $0) OUTDIR"
	exit 0
fi

# The site you want to do visual regression testing on
sites="
https://setofskills.com
"

# Top English-ish SFW sites worlwide for machine learning diversity
sites="
$sites
https://www.google.com
https://www.youtube.com
https://www.facebook.com
https://www.wikipedia.org
https://www.yahoo.com
https://www.reddit.com
https://twitter.com
https://www.live.com
https://www.instagram.com
https://www.linkedin.com
https://www.netflix.com
https://imgur.com
https://www.bing.com
https://www.msn.com
https://www.microsoft.com
https://wordpress.com
https://www.tumblr.com
"

# Too much animation:
# https://www.amazon.com/
# http://www.ebay.com/

# Setup chrome user data dir and make it skip the initialization popup
# TODO: Use --no-first-run instead?
tmpdir=$(mktemp -d)
chrome="google-chrome --user-data-dir=$tmpdir"
$chrome --make-default-browser

# Loop through all sites and take screenshots
for site in $sites; do
	outdir="$outdirs/$(echo $site | tr / _)"
	mkdir -p "$outdir"

	# Launch web app and let it settle
	size=1000
	$chrome --new-window --window-size=$size,$size --app=$site &
	chrome_pid=$!
	sleep 4

	# Take screenshot and scale it for Inception-v3
	chrome_wid=$(xdotool getactivewindow)
	import -window $chrome_wid -resize 299x299 "$outdir/1.jpg"
	for n in $(seq 2 25); do
		cp "$outdir/1.jpg" "$outdir/$n.jpg"
	done

	# Done, next site please...
	sleep 3
	kill $chrome_pid
done


# Get the screenshots
#while [ $size -lt 1200 ]; do
#	# Get window ID and move to upper left corner we can get the size we want
#	chrome_wid=$(xdotool getactivewindow)
#	xdotool windowmove --sync $chrome_wid 0 0
   # xdotool windowsize --sync $chrome_wid $size $size

	# Let the UI settle and take screenshot
#	sleep 0.5

#	size=$((size + 2))
#done

# Done

rm -rf $tmpdir
