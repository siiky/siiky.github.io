# Scripts
MAKE_GEMFEED := ./make-gemfeed.sh
GMI2MD_SCM := gmi2md.scm
GMI2MD := csi -s $(GMI2MD_SCM)
MD2HTML_SCM := md2html.scm
MD2HTML := csi -s $(MD2HTML_SCM)

GVS2GV := gvs2gv
GNUPLOT := gnuplot
DOT := dot

# The root directory of the site/capsule -- may be published to IPFS as-is
ROOT := root

# Source text files and targets
GMI := $(shell find $(ROOT) -type f -iname '*.gmi' -not -name index.gmi)
GMI_HTML := $(GMI:.gmi=.html)

MD := $(shell find $(ROOT) -type f -iname '*.md')
MD_HTML := $(MD:.md=.html)

ORG := $(shell find $(ROOT) -type f -iname '*.org')
ORG_HTML := $(ORG:.org=.html)

SRC := $(GMI) $(MD) $(ORG)
HTML := $(GMI_HTML) $(MD_HTML) $(ORG_HTML)

# Source assets
GVS := $(shell find $(ROOT) -type f -iname '*.gvs')
GP := $(shell find $(ROOT) -type f -iname '*.gp')

SVG := $(GVS:.gvs=.svg) $(GP:.gp=.svg)
PNG := $(GVS:.gvs=.png)


all: index html svg png

index: $(ROOT)/index.html

html: $(HTML)

svg: $(SVG)

png: $(PNG)

$(ROOT)/index.gmi: index.gmi $(SRC)
	cat index.gmi > $@
	$(MAKE_GEMFEED) $(ROOT) >> $@

sourcehut-pages: gemini.tgz http.tgz ipfs-publish

gemini.tgz:
	tar --exclude='*.html' -cvz -C root/ . > gemini.tgz

http.tgz:
	tar --exclude='*.gmi' --exclude='*.org' --exclude='*.md' -cvz -C root/ . > http.tgz

ipfs-publish:

.PHONY: gemini.tgz http.tgz ipfs-publish sourcehut-pages

serve:
	csi -s geminid.scm

watch:
	ls -1 index.gmi $(SRC) $(GVS) $(GP) Makefile | entr -c make

# Text files rules

%.html: %.gmi $(GMI2MD_SCM) $(MD2HTML_SCM)
	$(GMI2MD) $(ROOT) < $< | $(MD2HTML) > $@

%.html: %.md
	pandoc -f markdown -t html $< -o $@

%.html: %.org
	pandoc -f org -t html $< -o $@

# Assets rules

%.svg: %.gv
	$(DOT) -Tsvg -o $@ $<

%.png: %.gv
	$(DOT) -Tpng -o $@ $<

%.gv: %.gvs
	$(GVS2GV) $<

%.svg: %.gp
	$(GNUPLOT) -c $^

.PHONY: all html index png serve svg watch
