LATEX=xelatex
LATEXOPT=--shell-escape
NONSTOP=--interaction=nonstopmode

LATEXMK=latexmk
LATEXMKOPT=-xelatex
CONTINUOUS=-pvc

MAIN=main
SOURCES=$(MAIN).tex Makefile theory/theory.tex a-star/a-star.tex not-a-star/not-a-star.tex preamble.tex title.tex
FIGURES := $(shell find figures/* images/* -type f)

all: $(MAIN).pdf

.refresh:
	touch .refresh

$(MAIN).pdf: $(MAIN).tex .refresh $(SOURCES) $(FIGURES)
	$(LATEXMK) -xelatex $(CONTINUOUS) $(MAIN)

force:
	touch .refresh
	rm $(MAIN).pdf
	$(LATEXMK) -xelatex $(CONTINUOUS) $(MAIN)

clean:
	$(LATEXMK) -C $(MAIN)
	rm -f $(MAIN).pdfsync
	rm -rf *~ *.tmp
	rm -f *.bbl *.blg *.aux *.end *.fls *.log *.out *.fdb_latexmk

once:
	$(LATEXMK) $(LATEXMKOPT) $(MAIN)

debug:
	$(LATEX) $(LATEXOPT) $(MAIN)

.PHONY: clean force once all
