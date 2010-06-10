# generating UHC logos
# Clara L\"oh 05/2010
# uhclogo@wuisch.org

alllogos : pdflogos pnglogos favicon

pdflogos : uhc_u_logo.tex
	sh -c '\
		pdflatex uhc_u_logo.tex; \
		gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=1 -dLastPage=1 -sOutputFile=uhclogo.pdf uhc_u_logo.pdf; \
		gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=2 -dLastPage=2 -sOutputFile=uhcdarklogo.pdf uhc_u_logo.pdf; \
		gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=3 -dLastPage=3 -sOutputFile=uhcfavicon.pdf uhc_u_logo.pdf;'

pnglogos : pdflogos
	sh -c '\
		convert uhclogo.pdf uhclogo.png; \
		convert uhcdarklogo.pdf uhcdarklogo.png; \
		convert uhcfavicon.pdf uhcfavicon.png;'

favicon : pdflogos
	sh -c '\
		convert -resize 16x16 uhcfavicon.pdf favicon.ico;'

.SECONDARY : 

.PHONY : force

force :
        @ :
