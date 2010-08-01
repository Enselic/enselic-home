#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <prefix>"
    exit -1;
fi

prefix="$1"
config_site_file="$prefix/share/config.site"
mkdir -p `dirname $config_site_file`

cat > "$config_site_file" <<EOF
export CFLAGS="-g -O0"
export PATH="$prefix/bin:\$PATH"
export PKG_CONFIG_PATH="$prefix/lib/pkgconfig:\$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH="$prefix/lib:\$LD_LIBRARY_PATH"
export ACLOCAL_FLAGS="-I $prefix/share/aclocal \$ACLOCAL_FLAGS"
EOF

if [ "0" -eq "$?" ]; then
    echo "Created $config_site_file successfully!"
else
    echo "Failure!"
fi
