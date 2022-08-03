#!/usr/bin/env sh

cid="$(cat "$1")"

cat <<EOF
=> ipfs://${cid}
=> https://${cid}.ipfs.dweb.link
=> ipfs.txt
EOF
