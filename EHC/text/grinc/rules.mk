# Makefile for some standard prefixes

### PROGRAM DEFINITIONS

LHS2TEX    := lhs2TeX
LATEX      := latex
FIG2DEV    := fig2dev
DVIPS      := dvips
PS2PDF     := ps2pdf
PDFLATEX   := pdflatex
BIBTEX     := bibtex
MAKEINDEX  := makeindex
CMM        := qc--

FIGOPTIONSUFFIX := figopts
DOTOPTIONSUFFIX := dotopts

FIGOPTIONS = $(shell cat $(<:.fig=.$(FIGOPTIONSUFFIX)))
DOTOPTIONS = $(shell cat $(<:.dot=.$(DOTOPTIONSUFFIX)))


#### Process literate haskell

%.tex: %.ltex
	$(LHS2TEX) --poly $< > $@ || { rm -f "$@"; exit 1;}

%.tex: %.lhs
	$(LHS2TEX) --poly $< > $@ || { rm -f "$@; exit 1;}"

%.hs: %.lhs
	$(LHS2TEX) --code $< > $@ || { rm -f "$@; exit 1;}"


### Latex and its different output formats

%.dvi: %.tex
	$(LATEX) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.ps.pdf: %.ps
	$(PS2PDF) $<

%.pdf: %.tex
	$(PDFLATEX) $<


### Bibliography and Indexing 

.PRECIOUS: %.aux

%.aux: %.tex
	$(LATEX) $< || { rm '$@'; exit 1;}

%.idx: %.tex
	$(LATEX) $< || { rm '$@'; exit 1;}

%.bbl: %.aux
	$(BIBTEX) $* || { rm '$@'; exit 1;}

%.ind: %.idx
	$(MAKEINDEX) $*


### XFig picture genaration

%.pdf: %.fig %.$(FIGOPTIONSUFFIX)
	$(FIG2DEV) -L pdf $(FIGOPTIONS) $< $@

%.eps: %.fig %.$(FIGOPTIONSUFFIX)
	$(FIG2DEV) -L eps $(FIGOPTIONS) $< $@

%.png: %.fig %.$(FIGOPTIONSUFFIX)
	$(FIG2DEV) -L png $(FIGOPTIONS) $< $@

%.epic: %.fig %.$(FIGOPTIONSUFFIX)
	$(FIG2DEV) -L epic $(FIGOPTIONS) $< $@

# If the xfig options file does not exists - create an empty file.
%.$(FIGOPTIONSUFFIX)::
	touch $@

### Dot picture are converted FIG format

%.fig: %.dot %.$(DOTOPTIONSUFFIX)
	dot -Tfig $(DOTOPTIONS) -o$@ $< 

# If the dot options file does not exists - create an empty file.
%.$(DOTOPTIONSUFFIX)::
	touch $@

# C--
%.s: %.cmm
	$(CMM) -stop .s -o "$@" "$<"

%.o: %.cmm
	$(CMM) -c -o "$@" "$<"

%.om: %.cmm
	$(CMM) -c -globals -o "$@" "$<"

%: %.cmm
	$(CMM) -globals -o "$@" "$<"


%.cmm: %.c--
	cp "$<" "$@"

#####

FORCE:
