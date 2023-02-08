#!/usr/bin/env sh
outfile="$1"
meta="$2"

# TODO: replace that sed command with something more robust

(
cut -f1,4,5 "${meta}" |
sort -hr |
while IFS='	' read update title uri; do
    rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
    echo "=> ${rel_uri} ${update} ${title}"
done
) > "${outfile}"
