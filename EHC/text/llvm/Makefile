LHS2TEXSOURCES=*.lhs

.PHONY: thesis.pdf

thesis.pdf: thesis.tex thesis.bib
	( \
	pdflatex thesis.tex; \
	bibtex thesis; \
	pdflatex thesis.tex; \
	while grep -q "Rerun to get cross-references right." $(<:.tex=.log); \
	do \
		pdflatex thesis.tex; \
	done \
	) 
     
thesis.tex: $(LHS2TEXSOURCES) thesis.fmt thesis.sty
	lhs2TeX --poly thesis.lhs > thesis.tex 

quick: thesis.tex
	pdflatex thesis.tex

clean:
	rm *.aux *.bbl *.blg *.log *.out *.ptb *.toc thesis.tex
