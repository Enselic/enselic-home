#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <crate-name> <version>"
    exit 1
fi

crate_name="$1"
crate_version="$2"

tmpdir="$(mktemp --directory)"

cleanup() {
    rm --recursive --force "$tmpdir"
}

trap cleanup EXIT

echo "Downloading ${crate_name} ${crate_version}..."
echo "Extracting to ${tmpdir}"
echo

curl --location \
     --header "User-Agent: ${USER:-unknown} at ${HOSTNAME:-unknown}" \
     "https://crates.io/api/v1/crates/${crate_name}/${crate_version}/download" \
| tar --extract \
      --gzip \
      --file - \
      --directory "$tmpdir"

echo "Opening files with bat..."
echo

find "$tmpdir" -type f -print0 \
| xargs --null bat --paging=always
