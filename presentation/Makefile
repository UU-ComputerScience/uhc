all: pres.pdf
	open $<

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	pdflatex $<

# paper.bbl: paper.bib
  # bibtex paper
