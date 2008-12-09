ehc-architecture.pdf: text/architecture/ehc-architecture.tex text/architecture/ehc-architecture2.tex
	cd text/architecture
	pdflatex ehc-architecture


$(TOP_PREFIX)doc/AbstrInt.pdf: $(TEXT_SRC_PREFIX)AbstrInt.ltex
	lhs2tex -o $(TEXT_TMP_PREFIX)AbstrInt.tex $(TEXT_SRC_PREFIX)AbstrInt.ltex 
	pdflatex $(TEXT_TMP_PREFIX)AbstrInt.tex -include-directory=$(TOP_PREFIX)latex -output-directory=$(TEXT_TMP_PREFIX)
	cp $(TEXT_TMP_PREFIX)AbstrInt.pdf $(TOP_PREFIX)doc/AbstrInt.pdf


	