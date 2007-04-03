file = stc-slides
lhs2tex = lhs2TeX --poly

std:
	$(lhs2tex) $(file).lhs > $(file).tex
	pdflatex  $(file).tex
	xpdf $(file).pdf   

clean:
	rm $(file).log $(file).out $(file).aux $(file).toc $(file).nav $(file).snm $(file).vrb *.tex *.bak
