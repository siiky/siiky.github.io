GV := assets/ctp.gv
PNG := $(GV:.gv=.png)

DIRS := \
    -D algebra/ \
    -D words/   \

all: html $(PNG)

html:
	ssg -v --do-it -s assets/monokai.css $(DIRS) -i index.scm

%.png: %.gv
	dot -Tpng -o $@ $<

.PHONY: all html
