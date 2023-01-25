cv: candidatura-tub.pdf
	
candidatura-tub.pdf: candidatura-tub.md
	pandoc -f markdown -t latex --pdf-engine=xelatex candidatura-tub.md -o candidatura-tub.pdf

watch:
	ls -1 candidatura-tub.md Makefile | entr -c $(MAKE)

.PHONY: cv watch
