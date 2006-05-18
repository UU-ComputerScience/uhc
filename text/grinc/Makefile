default: exceptionalgrin.dvi

IMAGES=big-picture node-layout call-graph trans_overview

exceptionalgrin.dvi: $(addsuffix .eps,$(IMAGES)) exceptionalgrin.bbl FORCE
exceptionalgrin.pdf: $(addsuffix .pdf,$(IMAGES)) exceptionalgrin.bbl FORCE

exceptionalgrin.tex: grin.fmt c--.fmt \
                     intro.ltex grin.ltex hpt.ltex transformations.ltex \
                     exceptions.ltex codegeneration.ltex conclusions-and-future-work.ltex \
                     appendix.ltex

#load library
include rules.mk
