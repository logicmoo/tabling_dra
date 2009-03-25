all:	manual.dvi

manual.dvi: *.tex manual.bbl
	latex manual
	makeindex  -s index_style.ist manual
	latex manual
	latex manual

manual.bbl: bibliography.bib
	latex manual
	bibtex manual

clean:
	rm -rf *.aux *.log *.bbl *.blg *.idx *.ind *.ilg *.toc
