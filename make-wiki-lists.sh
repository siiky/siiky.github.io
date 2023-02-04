#!/usr/bin/env sh
outdir="$1"
meta="$2"

# TODO: replace that sed command with something more robust

# Generate list sorted by title
(
cut -f4,5 "${meta}" |
sort -h |
while IFS='	' read title uri; do
    rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
    echo "=> ${rel_uri} ${title}"
done
) > ${outdir}/title.gmi

# Generate list sorted by created date
(
cut -f3,4,5 "${meta}" |
sort -hr |
while IFS='	' read cdate title uri; do
    rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
    echo "=> ${rel_uri} ${cdate} ${title}"
done
) > ${outdir}/created.gmi

# Generate list sorted by updated date
(
cut -f1,4,5 "${meta}" |
sort -hr |
while IFS='	' read update title uri; do
    rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
    echo "=> ${rel_uri} ${update} ${title}"
done
) > ${outdir}/updated.gmi
