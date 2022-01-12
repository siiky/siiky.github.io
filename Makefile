GP := $(wildcard assets/*.gp)
GVS := $(wildcard assets/*.gvs)
GV := $(GVS:.gvs=.gv)
PNG := $(GV:.gv=.png) $(GP:.gp=.png)
SVG := $(GV:.gv=.svg) $(GP:.gp=.svg)
SRCS := $(wildcard */*.md) $(wildcard */*.org)
HTML := $(SRCS:.md=.html)
SPELL := $(SRCS:.md=.spell)
FOOTER := footer.html
HEADER := header.html
PANDOC := pandoc

all: curriculum cv $(SVG) html

force-redo: cv $(SVG) html-redo

html-redo: $(SRCS) $(FOOTER) $(HEADER)
	./siiky.github.io.scm build --force-redo

html: $(SRCS) $(FOOTER) $(HEADER)
	./siiky.github.io.scm build

list-files:
	@./siiky.github.io.scm list-files

watch:
	find siiky.github.io.scm functional_programming/curriculum.org cv-en.template.latex cv-en.md $(SRCS) $(GVS) $(GP) -type f | entr -c make

%.html: %.md
	$(PANDOC) -f markdown -t html $< -o $@

curriculum: functional_programming/curriculum.pdf

functional_programming/curriculum.pdf: functional_programming/curriculum.org
	$(PANDOC) -F pandoc-crossref --number-sections -s -f org -t latex $< -o $@

cv: cv-en.pdf

cv-en.pdf: cv-en.md cv-en.template.latex
	$(PANDOC) -s -f markdown --template cv-en.template.latex -t latex cv-en.md -o cv-en.pdf

%.png: %.gv
	dot -Tpng -o $@ $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

%.gv: %.gvs
	gvs2gv $<

%.svg: %.gp
	gnuplot -c $^

%.png: %.gp
	gnuplot -c $^

spell: $(SPELL)

%.spell: %.md
	aspell -c $<

.PHONY: all curriculum cv force-redo html html-redo list-files spell watch
