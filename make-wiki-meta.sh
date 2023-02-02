#!/usr/bin/env sh
root=$1

while read file; do
  uri="$(echo "$file" | sed "s|^$root/*||; s|^/||;")"
  title="$(head -1 "$file" | sed 's|^[^ ]*[ ]*||;')"
  #author="$(head -2 "$file" | tail +2 | sed 's|^@*||;')"
  cdate="$(head -3 "$file" | tail +3 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  signifcant_update="$(head -4 "$file" | tail +4 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  update="$(head -5 "$file" | tail +5 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  tags="$(head -6 "$file" | tail +6)"

  # The single quotes let `read` split the words correctly always!
  echo "$title" | grep -qvw WIP && echo "${signifcant_update}\t${update}\t${cdate}\t${title}\t${uri}\t${tags}"
done |
sort -nr
