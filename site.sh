#!/usr/bin/env bash

function _write()
{
    echo -e $2 >> $1
}

function _headers()
{
    _write "$1" '<html><head><link rel="stylesheet" type="text/css" href="assets/monokai.css"><meta charset="UTF-8"/>'
    _write "$1" "<title>./$(basename $1 .html).md</title></head><body>"
}

function _footers()
{
    _write "$1" '\n</body><footer>places:\n<a href="https://github.com/siiky">github.com</a>\ttest\tthis\tshit</footer></html>'
}

function build()
{
    for D in words; do
        cd "$D"
        echo "$(command ls -1 *.md | wc -l) source files found in $D"

        for F in *.md; do
            BNAME=$(basename $F .md)
            echo -e "$D/$F\t->\t$BNAME.html"
            [ -f "../$BNAME.html" ] && rm "../$BNAME.html"

            # header, title, style
            _headers "../$BNAME.html"

            # content
            cat "$F" >> "../$BNAME.html"

            # link back to index
            _write "../$BNAME.html" '\n<a href="index.html">./index.md</a>'

            # footer links
            _footers "../$BNAME.html"
        done
        cd ..
    done

    # create the index.html
    [ -f "index.html" ] && rm "index.html"
    _headers "index.html"
    cat "index.md" >> "index.html"
    _footers "index.html"
}

function clean()
{
    for D in words; do
        cd "$D"
        echo "$(command ls -1 *.md | wc -l) source files found"
        for F in *.md; do
            BNAME=$(basename $F .md)
            if [ -f "../$BNAME.html" ]; then
                echo "deleting '$BNAME.html'"
                rm -f "../$BNAME.html"
            fi
        done
        cd ..
    done
}

function usage()
{
    echo "\`$0 build\` to build the html files"
    echo "\`$0 clean\` to clean build html files"
}

for D in words; do
    if [ ! -d "$D" ]; then
        echo "'$D' directory not present"
        exit
    fi
done

if [ "$1" ==  "build" ]; then
    build
elif [ "$1" == "clean" ]; then
    clean
else
    usage
fi
