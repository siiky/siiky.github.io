all:
	ssg -v --do-it -s assets/monokai.css -D words/ -- index.md

.PHONY: all
