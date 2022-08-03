#!/usr/bin/env sh
srht=siiky.srht.site

meta=$1
last_update="$(head -1 "$meta" | cut -f 1)"

# Write headers

cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
<author><name>siiky</name></author>
<id>$srht</id>
<title type="text">nothing interesting here</title>
<updated>$last_update</updated>
EOF

# Write entries

while IFS='	' read update cdate title uri; do
  html_uri="$(echo "$uri" | sed 's|\.\(gmi\|org\|md\)$|.html|;')"
  cat <<EOF
<entry>
  <id>$srht/$uri</id>
  <title type="text">$title</title>
  <updated>$update</updated>
  <published>$cdate</published>
  <link href="https://$srht/$html_uri" rel="alternate" type="text/html" />
  <link href="gemini://$srht/$uri" rel="alternate" type="text/gemtext" />
</entry>
EOF
done < "$meta"

# End feed

echo '</feed>'
