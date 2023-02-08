#!/usr/bin/env sh
tagsdir="$1"
meta="$2"
GMI2MD="$3"
MD2HTML="$4"

rm -f "${tagsdir}"/*

cut -f6 "${meta}" | sed 's|,|\n|g;' | sort -u | grep -v '^\s*$' | sed 's|^|=> |; s|$|.gmi|;' > "${tagsdir}/index.gmi"

while IFS='	' read update mupdate cdate title uri tags; do
    for tag in $(echo "${tags}" | sed 's|,| |g;'); do
        rel_uri="$(echo "${uri}" | sed 's|^wiki/|../|;')"
        echo "=> ${rel_uri} ${update} ${title}" >> "${tagsdir}/${tag}.gmi"
    done
done < "${meta}"

cut -f6 "${meta}" | sed 's|,|\n|g;' | sort -u | grep -v '^\s*$' | while read tag; do
	${GMI2MD} "${tagsdir}/${tag}.gmi" | ${MD2HTML} standalone > "${tagsdir}/${tag}.html"
done
${GMI2MD} "${tagsdir}/index.gmi" | ${MD2HTML} standalone > "${tagsdir}/index.html"
