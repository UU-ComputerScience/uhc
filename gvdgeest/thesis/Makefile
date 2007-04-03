file = thesis
lhs2tex = lhs2TeX --poly

default:compile

view:compile
	xpdf $(file).pdf &

compile:
	$(lhs2tex) $(file).lhs > $(file).tex
	cd graphs && make && cd ..
	bibtex $(file)
	pdflatex $(file).tex 

pdf:
	pdflatex $(file).tex

bib:
	bibtex $(file)

clean:
	cd graphs && make clean && cd ..
	rm $(file).log $(file).out $(file).aux $(file).toc $(file).bbl $(file).blg *.tex *.bak

