file = stc-paper
lhs2tex = lhs2TeX --poly

std:
	$(lhs2tex) $(file).lhs > $(file).tex
	pdflatex $(file).tex
	bibtex $(file)
	pdflatex $(file).tex
	pdflatex $(file).tex
	# latex2html $(file).tex
	# acroread $(file).pdf &
	xpdf $(file).pdf   
  	# reloadable
        # ps2pdf $(file).ps

pdf:
	pdflatex $(file).tex

bib:
	bibtex $(file)

clean:
	rm $(file).log $(file).out $(file).aux $(file).toc $(file).bbl $(file).blg *.tex *.bak

# not working because of macro gen. tex code problem in st texmf
dvi:  
	latex $(file).tex

#ps: dvi
#	dvips -f $(file).dvi > $(file).ps

ps: pdf
	pdf2ps stc-paper-pkeeken.pdf stc-paper.ps
