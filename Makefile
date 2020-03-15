GVS := $(wildcard assets/*.gvs)
GV := $(GVS:.gvs=.gv)
PNG := $(GV:.gv=.png)
SVG := $(GV:.gv=.svg)

DIRS := \
    -D algebra/       \
    -D scheme/        \
    -D server_stuffs/ \
    -D ssg/           \
    -D todo/          \
    -D words/         \

all: $(SVG) html

html:
	ssg -v --do-it -s assets/monokai.css $(DIRS) -i index.scm

watch:
	find $(wildcard */*.md) $(wildcard */*.gvs) -type f | entr -c make

%.png: %.gv
	dot -Tpng -o $@ $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

%.gv: %.gvs
	gvs2gv $<

.PHONY: all html watch
