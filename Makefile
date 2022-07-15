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


all: index html svg

index: $(ROOT)/index.html

html: $(HTML)

svg: $(SVG)

$(ROOT)/index.gmi: index.gmi $(SRC)
	cat index.gmi > $@
	$(MAKE_GEMFEED) $(ROOT) >> $@

serve:
	csi -s geminid.scm

watch:
	ls -1 index.gmi $(SRC) | entr -c make

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

%.gv: %.gvs
	$(GVS2GV) $<

%.svg: %.gp
	$(GNUPLOT) -c $^

.PHONY: all html index serve svg watch
