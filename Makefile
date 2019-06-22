GV := assets/ctp.gv
PNG := $(GV:.gv=.png)
SVG := $(GV:.gv=.svg)

DIRS := \
    -D algebra/ \
    -D todo/    \
    -D words/   \

all: html $(PNG) $(SVG)

html:
	ssg -v --do-it -s assets/monokai.css $(DIRS) -i index.scm

%.png: %.gv
	dot -Tpng -o $@ $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

.PHONY: all html
