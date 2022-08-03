include env.make

# Scripts
MAKE_GEMFEED := ./make-gemfeed.sh
GMI2MD_SCM := gmi2md.scm
GMI2MD := csi -s $(GMI2MD_SCM)
MD2HTML_SCM := md2html.scm
MD2HTML := csi -s $(MD2HTML_SCM)
IPFS_PUBLISH := csi -s ipfs-publish.scm

GVS2GV := gvs2gv
GNUPLOT := gnuplot
DOT := dot

# The root directory of the site/capsule -- may be published to IPFS as-is
REPO_ROOT := $(PWD)
ROOT := root

# Source text files and targets
NON_POSTS := \
 $(ROOT)/about \
 $(ROOT)/alt \
 $(ROOT)/contact \
 $(ROOT)/publish-to-antenna \
 $(ROOT)/tinylog \

GMI := $(shell find $(ROOT)/*/ -type f -iname '*.gmi')
GMI_HTML := $(GMI:.gmi=.html) $(NON_POSTS:=.html)

MD := $(shell find $(ROOT)/*/ -type f -iname '*.md')
MD_HTML := $(MD:.md=.html)

ORG := $(shell find $(ROOT)/*/ -type f -iname '*.org')
ORG_HTML := $(ORG:.org=.html)

SRC := $(GMI) $(MD) $(ORG) $(NON_POSTS:=.gmi)
HTML := $(GMI_HTML) $(MD_HTML) $(ORG_HTML)

# Source assets
GVS := $(shell find $(ROOT)/*/ -type f -iname '*.gvs')
GP := $(shell find $(ROOT)/*/ -type f -iname '*.gp')

SVG := $(GVS:.gvs=.svg) $(GP:.gp=.svg)
PNG := $(GVS:.gvs=.png)


all: index html svg png

index: $(ROOT)/index.html

html: $(HTML)

svg: $(SVG)

png: $(PNG)

$(ROOT)/index.gmi: $(MAKE_GEMFEED) index.gmi $(SRC)
	cat index.gmi > $@
	$(MAKE_GEMFEED) $(ROOT) >> $@

# TODO: Split IPFS add from publish

publish: ipfs-publish gemini.tgz http.tgz
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@http.tgz https://pages.sr.ht/publish/siiky.srht.site
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@gemini.tgz -Fprotocol=GEMINI https://pages.sr.ht/publish/siiky.srht.site

gemini.tgz:
	cd $(ROOT) && tar --exclude='*.html' -cz * > $(REPO_ROOT)/gemini.tgz

http.tgz:
	cd $(ROOT) && tar --exclude='*.gmi' --exclude='*.org' --exclude='*.md' -cz * > $(REPO_ROOT)/http.tgz

ipfs-publish: all
	$(IPFS_PUBLISH) $(ROOT) $(IPFS_NODES)
	make $(ROOT)/ipfs.html

.PHONY: gemini.tgz http.tgz ipfs-publish publish

serve:
	csi -s geminid.scm

watch:
	ls -1 $(MAKE_GEMFEED) $(SRC) $(GVS) $(GP) Makefile | entr -c make

# Text files rules

%.html: %.gmi $(GMI2MD_SCM) $(MD2HTML_SCM)
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
	$(GNUPLOT) -c $^

.PHONY: all html index png serve svg watch
