#!/usr/bin/env sh
root=$1

while read file; do
  cdate="$(head -3 "$file" | tail +3 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  update="$(head -4 "$file" | tail +4 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  title="$(head -1 "$file" | sed 's|^[^ ]*[ ]*||;')"
  uri="$(echo "$file" | sed "s|^$root/*||; s|^/||;")"

  # The single quotes let `read` split the words correctly always!
  echo "$title" | grep -qvw WIP && echo "$update\t$cdate\t$title\t$uri"
done |
sort -nr
