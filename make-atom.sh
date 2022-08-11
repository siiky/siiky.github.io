#!/usr/bin/env sh
srht=siiky.srht.site

root="$1"
meta="$2"

last_update="$(head -1 "${meta}" | cut -f 1)T00:00:00Z"

html_escape() {
  sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g'
}

# Write headers

cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
<link href="https://${srht}/atom.xml" rel="self" type="application/atom+xml"/>
<author><name>siiky</name></author>
<id>https://${srht}/</id>
<title type="text">nothing interesting here</title>
<updated>${last_update}</updated>
EOF

# Write entries

while IFS='	' read update cdate title uri; do
  html_uri="$(echo "${uri}" | sed 's/\.\(gmi\|org\|md\)$/.html/;')"
  html_safe_title="$(echo "${title}" | html_escape)"
  updated="${update}T00:00:00Z"
  published="${cdate}T00:00:00Z"
  cat <<EOF
<entry>
  <id>https://${srht}/${uri}</id>
  <title type="text">${html_safe_title}</title>
  <updated>${updated}</updated>
  <published>${published}</published>
  <link href="https://${srht}/${html_uri}" rel="alternate" type="text/html"/>
  <link href="gemini://${srht}/${uri}" rel="alternate" type="text/gemtext"/>
  <content type="text/gemini">
EOF

cat "${root}/${uri}" | html_escape

cat <<EOF
  </content>
  <content type="text/html">
EOF

cat "${root}/${html_uri}" | html_escape

cat <<EOF
  </content>
</entry>
EOF
done < "${meta}"

# End feed

echo '</feed>'
