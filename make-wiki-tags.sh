#!/usr/bin/env sh
root="$1"
tagsdir="$2"
meta="$3"
GMI2HTML="$4"

rm -f "${tagsdir}"/*

# Build tag pages

while IFS='	' read update mupdate cdate title uri tags; do
    for tag in $(echo "${tags}" | sed 's|,| |g;'); do
        rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
        echo "=> ${rel_uri} ${update} ${title}" >> "${tagsdir}/${tag}.gmi"
    done
done < "${meta}"

cut -f6 "${meta}" | sed 's|,|\n|g;' | sort -u | grep -v '^\s*$' | while read tag; do
	${GMI2HTML} "${root}" "${tagsdir}/${tag}.gmi" < /dev/null > "${tagsdir}/${tag}.html" 
done

# Build index

cut -f6 "${meta}" | sed 's|,|\n|g;' | sort | grep -v '^\s*$' | uniq -c | sort -nr | sed 's|^\s*||;' | awk '{ print "=> " $2 ".gmi " $2 " (" $1 ")" }' > "${tagsdir}/index.gmi"

${GMI2HTML} "${root}" "${tagsdir}/index.gmi" < /dev/null > "${tagsdir}/index.html"
