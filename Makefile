GVS := $(wildcard assets/*.gvs)
GV := $(GVS:.gvs=.gv)
PNG := $(GV:.gv=.png)
SVG := $(GV:.gv=.svg)
MD := $(wildcard */*.md)
HTML := $(MD:.md=.html)
SPELL := $(MD:.md=.spell)

DIRS := \
    -D algebra/                \
    -D scheme/                 \
    -D server_stuffs/          \
    -D ssg/                    \
    -D todo/                   \
    -D words/                  \
    -D functional_programming/ \
    -D work/                   \
    -D philosophy/             \

all: $(SVG) html

html: $(MD)
	ssg -v --do-it -s assets/monokai.css $(DIRS) -i index.scm

watch:
	find index.scm $(MD) $(GVS) -type f | entr -c make

%.png: %.gv
	dot -Tpng -o $@ $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

%.gv: %.gvs
	gvs2gv $<

spell: $(SPELL)

%.spell: %.md
	aspell -c $<

.PHONY: all html watch spell
