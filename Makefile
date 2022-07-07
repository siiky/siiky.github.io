# Scripts
MAKE_GEMFEED := ./make-gemfeed.sh
GMI2MD_SCM := gmi2md.scm
GMI2MD := csi -s $(GMI2MD_SCM)
MD2HTML_SCM := md2html.scm
MD2HTML := csi -s $(MD2HTML_SCM)

# The root directory of the site/capsule -- may be published to IPFS as-is
ROOT := root

GMI := $(shell find $(ROOT) -type f -iname '*.gmi' -not -name index.gmi)
GMI_HTML := $(GMI:.gmi=.html)

MD := $(shell find $(ROOT) -type f -iname '*.md')
MD_HTML := $(MD:.md=.html)

ORG := $(shell find $(ROOT) -type f -iname '*.org')
ORG_HTML := $(ORG:.org=.html)

SRC := $(GMI) $(MD) $(ORG)
HTML := $(GMI_HTML) $(MD_HTML) $(ORG_HTML)

all: $(HTML) $(ROOT)/index.html

$(ROOT)/index.gmi: index.gmi $(SRC)
	cat index.gmi > $@
	$(MAKE_GEMFEED) $(ROOT) >> $@

%.html: %.gmi $(GMI2MD_SCM) $(MD2HTML_SCM)
	$(GMI2MD) $(ROOT) < $< | $(MD2HTML) > $@

%.html: %.md
	pandoc -f markdown -t html $< -o $@

%.html: %.org
	pandoc -f org -t html $< -o $@

watch:
	ls -1 index.gmi $(SRC) | entr -c make

.PHONY: all watch
