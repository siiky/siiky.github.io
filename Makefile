GMI2MD := csi -s gmi2md.scm
MD2HTML := csi -s md2html.scm

ROOT := root/

GMI := $(shell find $(ROOT) -type f -iname '*.gmi')
GMI_HTML := $(GMI:.gmi=.html)

MD := $(shell find $(ROOT) -type f -iname '*.md')
MD_HTML := $(MD:.md=.html)

ORG := $(shell find $(ROOT) -type f -iname '*.org')
ORG_HTML := $(ORG:.org=.html)

HTML := $(GMI_HTML) $(MD_HTML) $(ORG_HTML)

all: index.gmi $(HTML)

root/index.gmi: index.gmi
	cat index.gmi > $@
	./make-gemfeed.sh $(ROOT) >> $@

%.html: %.gmi
	@echo '$(GMI2MD) $(ROOT) < $< | $(MD2HTML) > $@'

%.html: %.md
	@echo 'pandoc -f markdown -t html $< -o $@'

%.html: %.org
	@echo 'pandoc -f org -t html $< -o $@'
