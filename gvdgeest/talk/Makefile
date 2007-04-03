file = 	thesis-defense
lhs2tex = lhs2TeX --poly

std:
	$(lhs2tex) $(file).lhs > $(file).tex
	cd graphs && make && cd ..
	pdflatex  $(file).tex

clean:
	rm $(file).log $(file).out $(file).aux $(file).toc $(file).nav $(file).snm $(file).vrb *.tex *.bak
