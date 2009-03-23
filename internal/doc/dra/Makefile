all:	dra_manual.dvi

dra_manual.dvi: *.tex dra_manual.bbl
	latex dra_manual
	makeindex dra_manual
	latex dra_manual

dra_manual.bbl: bibliography.bib
	latex dra_manual
	bibtex dra_manual

clean:
	rm -rf *.aux *.log *.bbl *.blg *.idx *.ind *.ilg *.toc



