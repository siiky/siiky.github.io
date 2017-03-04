#!/usr/bin/env bash

function build() {
    cd src
    echo "$(command ls -1 *.md | wc -l) source files found"
    for F in *.md; do
        BNAME=$(basename $F .md)
        echo -e "src/$F\t->\t$BNAME.html"

        # header, title, style
        echo '<html><head><link rel="stylesheet" type="text/css" href="monokai.css"><meta charset="UTF-8"/>' > "../$BNAME.html"
        echo "<title>./$F</title></head><body>" >> "../$BNAME.html"

        # content
        cat "$F" >> "../$BNAME.html"

        # link back to index
        echo '' >> "../$BNAME.html"
        echo '<a href="index.html">./index.md</a>' >> "../$BNAME.html"

        # close html
        echo '</body></html>' >> "../$BNAME.html"
    done
    cd ..
    [ -f index.html ] && ex index.html -c 'g/<a href="index.html"/d' +x
}

function clean() {
    cd src
    echo "$(command ls -1 *.md | wc -l) source files found"
    for F in *.md; do
        BNAME=$(basename $F .md)
        if [ -f "../$BNAME.html" ]; then
            echo "deleting '$BNAME.html'"
            rm -f "../$BNAME.html"
        fi
    done
    cd ..
}

function usage() {
    echo "'$0 build' to build the html files"
    echo "'$0 clean' to clean build html files"
}

if [ ! -d src ]; then
    echo "'src' directory not present"
    exit
fi

if [ "$1" ==  "build" ]; then
    build
elif [ "$1" == "clean" ]; then
    clean
else
    usage
fi
