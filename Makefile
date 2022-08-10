include env.make

# Scripts
GEMINID := ./geminid.scm
GMI2MD := ./gmi2md.scm
GRAPH2GVS := ./graph2gvs.scm
IPFS_PUBLISH := ./ipfs-publish.scm
MAKE_ATOM := ./make-atom.sh
MAKE_GEMFEED := ./make-gemfeed.sh
MAKE_GRAPH := ./make-graph.scm
MAKE_IPFS_PAGE := ./make-ipfs-page.sh
MAKE_META := ./make-meta.sh
MD2HTML := ./md2html.scm

SCRIPTS := \
 GEMINID \
 GMI2MD \
 IPFS_PUBLISH \
 MAKE_ATOM \
 MAKE_GEMFEED \
 MAKE_IPFS_PAGE \
 MAKE_META \
 MD2HTML \

GVS2GV := gvs2gv
GNUPLOT := gnuplot
DOT := dot

# The root directory of the site/capsule -- may be published to IPFS as-is
REPO_ROOT := $(PWD)
ROOT := docs

# Source text files and targets
NON_POSTS := \
 $(ROOT)/about \
 $(ROOT)/alt \
 $(ROOT)/contact \
 $(ROOT)/tinylog \
 $(ROOT)/follow \

GMI := $(shell find $(ROOT)/*/ -type f -iname '*.gmi')
GMI_HTML := $(GMI:.gmi=.html) $(NON_POSTS:=.html)

MD := $(shell find $(ROOT)/*/ -type f -iname '*.md')
MD_HTML := $(MD:.md=.html)

ORG := $(shell find $(ROOT)/*/ -type f -iname '*.org')
ORG_HTML := $(ORG:.org=.html)

SRC := $(GMI) $(MD) $(ORG) $(NON_POSTS:=.gmi)
HTML := $(GMI_HTML) $(MD_HTML) $(ORG_HTML)

# Source assets
GVS := $(shell find $(ROOT)/* -type f -iname '*.gvs')
GP := $(shell find $(ROOT)/* -type f -iname '*.gp')

SVG := $(GVS:.gvs=.svg) $(GP:.gp=.svg)
PNG := $(GVS:.gvs=.png)


all: index html svg png atom graph.svg

index: $(ROOT)/index.html

html: $(HTML)

svg: $(SVG)

png: $(PNG)

atom: $(ROOT)/atom.xml

meta.tsv: $(MAKE_META) $(SRC)
	$(MAKE_META) $(ROOT) > $@

$(ROOT)/index.gmi: index.gmi meta.tsv $(MAKE_GEMFEED)
	$(MAKE_GEMFEED) index.gmi meta.tsv > $@

$(ROOT)/atom.xml: $(MAKE_ATOM) meta.tsv
	$(MAKE_ATOM) meta.tsv > $@

# TODO: Split IPFS add from publish

publish: $(ROOT)/ipfs.html gemini.tgz http.tgz
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@http.tgz https://pages.sr.ht/publish/siiky.srht.site
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@gemini.tgz -Fprotocol=GEMINI https://pages.sr.ht/publish/siiky.srht.site

gemini.tgz: $(ROOT)/ipfs.html
	cd $(ROOT) && tar --exclude='*.html' -cz * > $(REPO_ROOT)/gemini.tgz

http.tgz: $(ROOT)/ipfs.html
	cd $(ROOT) && tar --exclude='*.gmi' --exclude='*.org' --exclude='*.md' -cz * > $(REPO_ROOT)/http.tgz

$(ROOT)/ipfs.txt: all
	$(IPFS_PUBLISH) $(ROOT) $(IPFS_NODES)

$(ROOT)/ipfs.gmi: $(ROOT)/ipfs.txt
	$(MAKE_IPFS_PAGE) $< > $@

ipfs-publish: $(ROOT)/ipfs.html

antenna-publish: ipfs-publish
	xdg-open gemini://warmedal.se/~antenna/submit?gemini://siiky.srht.site

.PHONY: ipfs-publish publish

serve:
	$(GEMINID) $(ROOT)

graph.scm: $(MAKE_GRAPH) $(SRC)
	@$(MAKE_GRAPH) $(ROOT) $(ROOT)/index.gmi $(SRC) > $@

graph.gvs: graph.scm $(GRAPH2GVS)
	$(GRAPH2GVS) $(ROOT) $< > $@

watch:
	ls -1 $(SCRIPTS) $(SRC) $(GVS) $(GP) Makefile | entr -c $(MAKE)

# Text files rules

%.html: %.gmi $(GMI2MD) $(MD2HTML)
	$(GMI2MD) $(ROOT) < $< | $(MD2HTML) > $@

%.html: %.md
	$(MD2HTML) < $< > $@

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
	cd $(shell dirname "$<") && $(GNUPLOT) -c $(shell basename "$<")

.PHONY: all html index png serve svg watch
