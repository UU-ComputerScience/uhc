ehc-architecture.pdf: text/architecture/ehc-architecture.tex text/architecture/ehc-architecture2.tex
	cd text/architecture
	pdflatex ehc-architecture


$(TOP_PREFIX)doc/AbstrInt.pdf: $(TEXT_SRC_PREFIX)AbstrInt.ltex
	lhs2tex -o $(TEXT_TMP_PREFIX)AbstrInt.tex $(TEXT_SRC_PREFIX)AbstrInt.ltex 
	pdflatex -include-directory=$(TOP_PREFIX)latex -output-directory=$(TEXT_TMP_PREFIX) $(TEXT_TMP_PREFIX)AbstrInt.tex
	cp $(TEXT_TMP_PREFIX)AbstrInt.pdf $(TOP_PREFIX)doc/AbstrInt.pdf

$(TOP_PREFIX)doc/gpce10-SpecConst.pdf: $(TEXT_SRC_PREFIX)gpce10-SpecConst.ltex
	lhs2tex -o $(TEXT_TMP_PREFIX)gpce10-SpecConst.tex $(TEXT_SRC_PREFIX)gpce10-SpecConst.ltex 
	pdflatex -include-directory=$(TOP_PREFIX)latex -output-directory=$(TEXT_TMP_PREFIX) $(TEXT_TMP_PREFIX)gpce10-SpecConst.tex
	cp $(TEXT_TMP_PREFIX)gpce10-SpecConst.pdf $(TOP_PREFIX)doc/gpce10-SpecConst.pdf

	