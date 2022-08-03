#!/usr/bin/env sh
header=$1
meta=$2

cat "$header"

while IFS='	' read update cdate title uri; do
  echo "=> $uri $update $title"
done < "$meta"
