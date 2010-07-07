#!/bin/sh

tarball_dir="gimp-2.7.1"
if [ -d "$tarball_dir" ]; then
    chmod +w "$tarball_dir"
fi

git clean -xdf && \
source /home/martin/dev/share/config.site && \
./autogen.sh --enable-gtk-doc && \
make -j3 && \
make distcheck
