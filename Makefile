GVS := $(wildcard assets/*.gvs)
GV := $(GVS:.gvs=.gv)
PNG := $(GV:.gv=.png)
SVG := $(GV:.gv=.svg)
MD := $(wildcard */*.md)
HTML := $(MD:.md=.html)
SPELL := $(MD:.md=.spell)
FOOTER := footer.html
HEADER := header.html

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

all: curriculum cv $(SVG) html

force-redo: cv $(SVG) html-redo

html-redo: $(MD) $(FOOTER) $(HEADER)
	./siiky.github.io.scm --force-redo

html: $(MD) $(FOOTER) $(HEADER)
	./siiky.github.io.scm

watch:
	find siiky.github.io.scm functional_programming/curriculum.org cv-en.template.latex cv-en.md index.scm $(MD) $(GVS) -type f | entr -c make

$(FOOTER): $(FOOTER:.html=.md)
	pandoc -f markdown -t html $< -o $@

$(HEADER): $(HEADER:.html=.md)
	pandoc -f markdown -t html $< -o $@

curriculum: functional_programming/curriculum.pdf

functional_programming/curriculum.pdf: functional_programming/curriculum.org
	pandoc -s -f org -t latex $< -o $@

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
