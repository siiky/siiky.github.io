#!/usr/bin/env bash
find */ -type f \( -iname '*.md' -or -iname '*.org' -or -iname '*.gmi' \) |
while read file; do
  # Get the last commit
  date="$(git log -1 -- "$file" |
    grep ^Date: |
    # Extract the year, month and day of the month
    sed 's|^Date:\s\+[A-Z][a-z]\{2\} \([A-Z][a-z]\{2\}\) \([0-9]\+\) \([0-9]\{2\}:[0-9]\{2\}\):[0-9]\{2\} \([0-9]\{4\}\) \([-+][0-9]\{4\}\)$|\4-\1-0\2|;' |
    # Git's outputted date doesn't 0-pad DoM, but I do on the bit above; e.g. 023 => 23; 07 => 07
    sed 's|0\([0-9]\{2\}\)$|\1|;' |
    sed 's|Jan|01|; s|Feb|02|; s|Mar|03|; s|Apr|04|; s|May|05|; s|Jun|06|; s|Jul|07|; s|Aug|08|; s|Sep|09|; s|Oct|10|; s|Nov|11|; s|Dec|12|;')"
  echo -e "$date\t$file"
done |
sort -nr |
while read date file; do
  # The sed command removes the formatting and spaces of the beginning of the line, leaving the title
  title="$(head -1 "$file" | sed 's|^[^ ]*[ ]*||;')"
  echo "=> $file $date - $title"
done
