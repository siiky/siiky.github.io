#!/usr/bin/env bash
root=$1

find "$root"/*/ -type f \( -iname '*.md' -or -iname '*.org' -or -iname '*.gmi' \) |
while read file; do
  update="$(head -4 "$file" | tail +4 | sed 's|^.*\([0-9]\{4\}/[0-9]\{2\}/[0-9]\{2\}\)\s*$|\1|; s|/|-|g;')"
  title="$(head -1 "$file" | sed 's|^[^ ]*[ ]*||;')"

  # The single quotes let `read` split the words correctly always!
  echo "$title" | grep -qvw WIP && echo -e "'$update'\t'$file'\t$title"
done |
sort -nr |
while read date file title; do
  date="$(echo "$date" | sed "s|^'||; s|'$||;")"
  file="$(echo "$file" | sed "s|^'||; s|'$||;")"

  # Remove formatting and spaces of the beginning of the line, leaving the title
  uri="$(echo "$file" | sed "s|^$root/*||; s|^/||;")"

  echo "=> $uri $date $title"
done
