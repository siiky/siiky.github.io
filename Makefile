GVS := $(wildcard assets/*.gvs)
GV := $(GVS:.gvs=.gv)
PNG := $(GV:.gv=.png)
SVG := $(GV:.gv=.svg)
MD := $(wildcard */*.md)
HTML := $(MD:.md=.html)
SPELL := $(MD:.md=.spell)
FOOTER := footer.html

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

all: cv $(SVG) html

force-redo: cv $(SVG) html-redo

html-redo: $(MD) $(FOOTER)
	./siiky.github.io.scm --force-redo

html: $(MD) $(FOOTER)
	./siiky.github.io.scm

watch:
	find cv-en.template.latex cv-en.md index.scm $(MD) $(GVS) -type f | entr -c make

$(FOOTER): $(FOOTER:.html=.md)
	pandoc -f markdown -t html $< -o $@

cv: cv-en.pdf

cv-en.pdf: cv-en.md cv-en.template.latex
	pandoc -s -f markdown --template cv-en.template.latex -t latex cv-en.md -o cv-en.pdf

%.png: %.gv
	dot -Tpng -o $@ $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

%.gv: %.gvs
	gvs2gv $<

spell: $(SPELL)

%.spell: %.md
	aspell -c $<

.PHONY: all cv html watch spell
