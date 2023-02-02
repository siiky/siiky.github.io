include env.make

# Scripts
GEMINID := ./geminid.scm
GMI2MD := ./gmi2md.scm
GRAPH2GVS := ./graph2gvs.scm
IPFS_PUBLISH := ./ipfs-publish.scm
MAKE_ATOM := ./make-atom.scm
MAKE_GEMFEED := ./make-gemfeed.sh
MAKE_GRAPH := ./make-graph.scm
MAKE_IPFS_PAGE := ./make-ipfs-page.sh
MAKE_SITE_META := ./make-site-meta.sh
MAKE_WIKI_META := ./make-wiki-meta.sh
MD2HTML := ./md2html.scm

SCRIPTS := \
 $(GEMINID) \
 $(GMI2MD) \
 $(GRAPH2GVS) \
 $(IPFS_PUBLISH) \
 $(MAKE_ATOM) \
 $(MAKE_GEMFEED) \
 $(MAKE_GRAPH) \
 $(MAKE_IPFS_PAGE) \
 $(MAKE_SITE_META) \
 $(MAKE_WIKI_META) \
 $(MD2HTML) \

GVS2GV := gvs2gv
GNUPLOT := gnuplot
DOT := dot

# The root directory of the site/capsule -- may be published to IPFS as-is
REPO_ROOT := $(PWD)
ROOT := docs
WIKI_ROOT := $(ROOT)/wiki

SITE_META := site-meta.tsv
WIKI_META := wiki-meta.tsv

# Source text files and targets
NON_POSTS := \
 $(ROOT)/about \
 $(ROOT)/alt \
 $(ROOT)/contact \
 $(ROOT)/follow \
 $(ROOT)/reclog \
 $(ROOT)/tinylog \
 $(ROOT)/tinylog-2022 \

# Main site sources -- do NOT include the wiki

SITE_GMI := $(shell find $(ROOT)/*/ -not \( -path $(WIKI_ROOT)/ -prune \) -type f -iname '*.gmi')
SITE_GMI_HTML := $(SITE_GMI:.gmi=.html) $(NON_POSTS:=.html)

SITE_MD := $(shell find $(ROOT)/*/ -not \( -path $(WIKI_ROOT)/ -prune \) -type f -iname '*.md')
SITE_MD_HTML := $(SITE_MD:.md=.html)

SITE_ORG := $(shell find $(ROOT)/*/ -not \( -path $(WIKI_ROOT)/ -prune \) -type f -iname '*.org')
SITE_ORG_HTML := $(SITE_ORG:.org=.html)

SITE_POSTS_SRC := $(SITE_GMI) $(SITE_MD) $(SITE_ORG)
SITE_POSTS_HTML := $(SITE_GMI_HTML) $(SITE_MD_HTML) $(SITE_ORG_HTML)

SITE_SRC := $(SITE_POSTS_SRC) $(NON_POSTS:=.gmi)
SITE_HTML := $(SITE_POSTS_HTML)

# Wiki sources -- do NOT include the main site

## User-edited pages of the wiki will all be at the root /wiki/
## Directories will include all generated pages, i.e., lists
WIKI_SRC := $(shell find $(ROOT)/wiki/* -maxdepth 0 -type f -iname '*.gmi')
WIKI_HTML := $(WIKI_SRC:.gmi=.html)

# Source assets -- no distinction between main site and wiki
GVS := $(shell find $(ROOT)/* -type f -iname '*.gvs')
GP := $(shell find $(ROOT)/* -type f -iname '*.gp')

SVG := $(GVS:.gvs=.svg) $(GP:.gp=.svg)
PNG := $(GVS:.gvs=.png)


all: site wiki assets cv

wiki: wiki-html $(WIKI_META)

site: index site-html

assets: svg png atom graph.svg

index: $(ROOT)/index.html

site-html: $(SITE_HTML)

wiki-html: $(WIKI_HTML)

svg: $(SVG)

png: $(PNG)

atom: $(ROOT)/atom.xml

$(SITE_META): $(MAKE_SITE_META) $(SITE_POSTS_SRC)
	ls -1 $(SITE_POSTS_SRC) | $(MAKE_SITE_META) $(ROOT) > $@

$(WIKI_META): $(MAKE_WIKI_META) $(WIKI_SRC)
	ls -1 $(WIKI_SRC) | $(MAKE_WIKI_META) $(ROOT) > $@

$(ROOT)/index.gmi: index.gmi $(SITE_META) $(MAKE_GEMFEED)
	$(MAKE_GEMFEED) index.gmi $(SITE_META) > $@

$(ROOT)/atom.xml: $(MAKE_ATOM) $(SITE_META)
	$(MAKE_ATOM) $(ROOT) < $(SITE_META) > $@

# TODO: Split IPFS add from publish

publish: $(ROOT)/ipfs.html publish-gemini publish-http

publish-gemini: gemini.tgz
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@gemini.tgz -Fprotocol=GEMINI https://pages.sr.ht/publish/siiky.srht.site

publish-http: http.tgz
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@http.tgz https://pages.sr.ht/publish/siiky.srht.site

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

.PHONY: ipfs-publish publish publish-gemini publish-http

serve:
	$(GEMINID) $(ROOT)

graph.scm: $(MAKE_GRAPH) $(SITE_SRC)
	@$(MAKE_GRAPH) $(ROOT) $(ROOT)/index.gmi $(SITE_SRC) > $@

graph.gvs: graph.scm $(GRAPH2GVS)
	$(GRAPH2GVS) $(ROOT) $< > $@

watch:
	ls -1 cv-en.md index.gmi $(SCRIPTS) $(SITE_SRC) $(GVS) $(GP) Makefile | entr -c $(MAKE)

%.spell: %.gmi
	aspell check $<

spellcheck: $(SITE_SRC:.gmi=.spell)

cv: cv-en.pdf
	
cv-en.pdf: cv-en.md
	pandoc -f markdown -t latex --pdf-engine=xelatex $< -o $@

# Text files rules

%.html: %.gmi $(GMI2MD) $(MD2HTML)
	$(GMI2MD) $< | $(MD2HTML) standalone > $@

%.html: %.md
	$(MD2HTML) standalone < $< > $@

%.html: %.org
	pandoc -f org -t html -s $< -o $@

# Assets rules

%.svg: %.gv
	$(DOT) -Tsvg -o $@ $<

%.png: %.gv
	$(DOT) -Tpng -o $@ $<

%.gv: %.gvs
	$(GVS2GV) $<

%.svg: %.gp
	cd $(shell dirname "$<") && $(GNUPLOT) -c $(shell basename "$<")

.PHONY: all assets index png serve site site-html svg watch wiki wiki-html
