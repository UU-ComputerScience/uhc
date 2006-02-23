.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
OPEN_FOR_EDIT		:= bbedit

# lhs2TeX
LHS2TEX_ENV			:= $(LHS2TEX)
LHS2TEX				:= LHS2TEX=".:$(FMT_SRC_PREFIX):$(LHS2TEX_ENV)" lhs2TeX

# latex
LATEX_ENV			:= $(TEXINPUTS)
PDFLATEX			:= TEXINPUTS=".:../../$(LATEX_SRC_PREFIX):$(LATEX_ENV)" pdflatex
BIBTEX				:= BSTINPUTS=".:../../$(LATEX_SRC_PREFIX):$(LATEX_ENV)" BIBINPUTS=".:../../$(LATEX_SRC_PREFIX):$(LATEX_ENV)" bibtex
MAKEINDEX			:= makeindex

# GHC options
GHC_OPTS			:= -fglasgow-exts -package util -package lang -package data -package uulib
GHC_OPTS_OPTIM		:= -O2

# location for binaries
BIN_PREFIX			:= $(TOP_PREFIX)bin/

# location building
BLD_PREFIX			:= $(TOP_PREFIX)build/
BLD_BIN_PREFIX		:= $(BLD_PREFIX)bin/

# location for doc (end products)
DOC_PREFIX			:= $(TOP_PREFIX)doc/

# location of library src
LIB_SRC_PREFIX		:= $(TOP_PREFIX)lib/

# lib src
LIB_SRC_HS			:= $(wildcard $(LIB_SRC_PREFIX)*.hs) 

# distribution
LIB_DIST_FILES		:= $(LIB_SRC_HS)

# lhs2tex options
LHS2TEX_OPTS_DFLT	:= 
LHS2TEX_OPTS_POLY	:= $(LHS2TEX_OPTS_DFLT) --poly
LHS2TEX_OPTS_NEWC	:= $(LHS2TEX_OPTS_DFLT) --newcode

# ruler2 options
#RULER2_OPTS_DFLT	:= --v1
RULER2_OPTS_DFLT	:=
RULER2_OPTS			:= $(RULER2_OPTS_DFLT)

# order to shuffle
# 4_99: interim for stuff from 4, needed for 4_2, because of ruler generated material uptil 4_2
EHC_SHUFFLE_ORDER	:= 1 < 2 < 3 < 4 < 4_99 < 5 < 6 < 7 < 8 < 9 < 10 < 11, 4_99 < 4_2, 6 < 6_4

# target suffix for core
CORE_TARG_SUFFIX	:= grin

# subst's
SUBST_BAR_IN_TT		:= sed -e '/begin{TT[^}]*}/,/end{TT[^}]*}/s/|/||/g'
SUBST_EHC			:= perl $(BIN_PREFIX)substehc.pl
SUBST_LINE_CMT		:= sed -e 's/{-\# LINE[^\#]*\#-}//' -e '/{-\#  \#-}/d'
SUBST_SH			:= perl $(BIN_PREFIX)substsh.pl

# indentation of (test) output
INDENT2				:= sed -e 's/^/  /'
INDENT4				:= sed -e 's/^/    /'

# make programming utils
head				= $(word 1,$(1))
tail				= $(wordlist 2,$(words $(1)),$(1))