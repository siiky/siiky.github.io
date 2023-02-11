include env.make

# Scripts
GEMINID := ./geminid.scm
GMI2HTML := ./gmi2html
GMI2MD := ./gmi2md
GRAPH2GVS := ./graph2gvs.scm
IPFS_PUBLISH := ./ipfs-publish.scm
MAKE_ATOM := ./make-atom.scm
MAKE_GEMFEED := ./make-gemfeed.sh
MAKE_GRAPH := ./make-graph.scm
MAKE_IPFS_PAGE := ./make-ipfs-page.sh
MAKE_SITE_META := ./make-site-meta.sh
MAKE_WIKI_CREATED_LIST := ./make-wiki-created-list.sh
MAKE_WIKI_META := ./make-wiki-meta.sh
MAKE_WIKI_TAGS := ./make-wiki-tags.sh
MAKE_WIKI_TITLE_LIST := ./make-wiki-title-list.sh
MAKE_WIKI_UPDATED_LIST := ./make-wiki-updated-list.sh
MD2HTML := ./md2html

SCRIPT_SRC := \
 $(GEMINID) \
 $(GMI2HTML).scm \
 $(GMI2MD).scm \
 $(GRAPH2GVS) \
 $(IPFS_PUBLISH) \
 $(MAKE_ATOM) \
 $(MAKE_GEMFEED) \
 $(MAKE_GRAPH) \
 $(MAKE_IPFS_PAGE) \
 $(MAKE_SITE_META) \
 $(MAKE_WIKI_CREATED_LIST) \
 $(MAKE_WIKI_META) \
 $(MAKE_WIKI_TAGS) \
 $(MAKE_WIKI_TITLE_LIST) \
 $(MAKE_WIKI_UPDATED_LIST) \
 $(MD2HTML).scm \

GVS2GV := gvs2gv
GNUPLOT := gnuplot
DOT := dot

# The root directory of the site/capsule -- may be published to IPFS as-is
REPO_ROOT := $(PWD)
ROOT := docs
WIKI_ROOT := $(ROOT)/wiki
WIKI_BY := $(WIKI_ROOT)/by
WIKI_TAG := $(WIKI_ROOT)/tag

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
WIKI_SRC := $(wildcard $(WIKI_ROOT)/*.gmi)
WIKI_HTML := $(WIKI_SRC:.gmi=.html)
WIKI_GENERATED_SRC := $(wildcard $(WIKI_BY)/*.gmi)
WIKI_GENERATED_HTML := $(WIKI_GENERATED_SRC:.gmi=.html)

# Source assets -- no distinction between main site and wiki
GVS := $(shell find $(ROOT)/* -type f -iname '*.gvs')
GP := $(shell find $(ROOT)/* -type f -iname '*.gp')
ASSETS_SRC := $(GVS) $(GP)

SVG := $(GVS:.gvs=.svg) $(GP:.gp=.svg)
PNG := $(GVS:.gvs=.png)


.PHONY: all
all: site wiki assets cv

.PHONY: wiki
wiki: wiki-html wiki-generated-html

.PHONY: site
site: index site-html

.PHONY: assets
assets: svg png atom

.PHONY: assets
index: $(ROOT)/index.html

.PHONY: site-html
site-html: $(SITE_HTML)

.PHONY: wiki-html
wiki-html: $(WIKI_META) $(WIKI_HTML)

.PHONY: svg
svg: $(SVG)

.PHONY: png
png: $(PNG)

.PHONY: atom
atom: $(ROOT)/atom.xml

$(SITE_META): $(MAKE_SITE_META) $(SITE_POSTS_SRC)
	ls -1 $(SITE_POSTS_SRC) | $(MAKE_SITE_META) $(ROOT) > $@

$(WIKI_META): $(MAKE_WIKI_META) $(WIKI_SRC)
	ls -1 $(WIKI_SRC) | $(MAKE_WIKI_META) $(ROOT) > $@

$(ROOT)/index.gmi: index.gmi $(SITE_META) $(MAKE_GEMFEED)
	$(MAKE_GEMFEED) index.gmi $(SITE_META) > $@

$(ROOT)/atom.xml: $(MAKE_ATOM) $(SITE_META)
	$(MAKE_ATOM) $(ROOT) < $(SITE_META) > $@

.PHONY: wiki-lists
wiki-lists: $(WIKI_BY)/title.gmi $(WIKI_BY)/updated.gmi $(WIKI_BY)/created.gmi wiki-tags-lists

$(WIKI_BY)/created.gmi: $(WIKI_META) $(MAKE_WIKI_CREATED_LIST)
	$(MAKE_WIKI_CREATED_LIST) $@ $(WIKI_META)

$(WIKI_BY)/title.gmi: $(WIKI_META) $(MAKE_WIKI_TITLE_LIST)
	$(MAKE_WIKI_TITLE_LIST) $@ $(WIKI_META)

$(WIKI_BY)/updated.gmi: $(WIKI_META) $(MAKE_WIKI_UPDATED_LIST)
	$(MAKE_WIKI_UPDATED_LIST) $@ $(WIKI_META)

.PHONY: wiki-tags-lists
wiki-tags-lists: $(WIKI_TAG)/index.gmi

$(WIKI_TAG)/index.gmi: $(WIKI_META) $(MAKE_WIKI_TAGS)
	$(MAKE_WIKI_TAGS) $(WIKI_TAG) $(WIKI_META) $(GMI2MD) $(MD2HTML)

# TODO: Reliably find and convert all generated files
#$(MAKE) $($(wildcard $(WIKI_BY)/*.gmi):.gmi:.html) $($(wildcard $(WIKI_TAG)/*.gmi):.gmi=.html)
.PHONY: wiki-generated-html
wiki-generated-html: $(WIKI_META) wiki-lists $(WIKI_GENERATED_HTML)

# TODO: Split IPFS add from publish

.PHONY: publish
publish: $(ROOT)/ipfs.html publish-gemini publish-http

.PHONY: publish-gemini
publish-gemini: gemini.tgz
	curl --oauth2-bearer $(SRHT_TOKEN) -Fcontent=@gemini.tgz -Fprotocol=GEMINI https://pages.sr.ht/publish/siiky.srht.site

.PHONY: publish-http
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

.PHONY: ipfs-publish
ipfs-publish: $(ROOT)/ipfs.html

.PHONY: antenna-publish
antenna-publish: ipfs-publish
	xdg-open gemini://warmedal.se/~antenna/submit?gemini://siiky.srht.site

.PHONY: serve
serve:
	$(GEMINID) $(ROOT)

graph.scm: $(MAKE_GRAPH) $(SITE_SRC)
	@$(MAKE_GRAPH) $(ROOT) $(ROOT)/index.gmi $(SITE_SRC) > $@

graph.gvs: graph.scm $(GRAPH2GVS)
	$(GRAPH2GVS) $(ROOT) $< > $@

.PHONY: watch
watch:
	ls -1d Makefile cv-en.md index.gmi $(SCRIPT_SRC) $(SITE_SRC) $(WIKI_SRC) $(ASSETS_SRC) $(ROOT) $(ROOT) | entr -d -n -c $(MAKE)

%.spell: %.gmi
	aspell check $<

.PHONY: spellcheck
spellcheck: $(SITE_SRC:.gmi=.spell)

.PHONY: cv
cv: cv-en.pdf
	
cv-en.pdf: cv-en.md
	pandoc -f markdown -t latex --pdf-engine=xelatex $< -o $@

# Text files rules

%.html: %.gmi $(GMI2HTML)
	$(GMI2HTML) $< > $@

%.html: %.md $(MD2HTML)
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

%: %.scm
	csc $<

.PHONY: push
push:
	for remote in $(shell git remote); do git push "$${remote}"; done
