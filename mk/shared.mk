.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
OPEN_FOR_EDIT		:= bbedit
STRIP				:= $(STRIP_CMD)

# lhs2TeX
LHS2TEX_ENV			:= $(LHS2TEX)
LHS2TEX_CMD			:= LHS2TEX=".$(PATHS_SEP)../../$(FMT_SRC_PREFIX)$(PATHS_SEP)$(LHS2TEX_ENV)" lhs2TeX

# latex
LATEX_ENV			:= $(TEXINPUTS)
PDFLATEX			:= TEXINPUTS=".$(PATHS_SEP_COL)../../$(LATEX_SRC_PREFIX)$(PATHS_SEP_COL)$(LATEX_EHC_SUBDIRS)$(LATEX_ENV)" pdflatex
BIBTEX				:= BSTINPUTS=".$(PATHS_SEP_COL)../../$(LATEX_SRC_PREFIX)$(PATHS_SEP_COL)$(LATEX_ENV)" BIBINPUTS=".$(PATHS_SEP_COL)../../$(LATEX_SRC_PREFIX)$(LATEX_ENV)" bibtex
MAKEINDEX			:= makeindex

# GHC options
GHC_OPTS			:= -fglasgow-exts -package util -package lang -package data -package uulib
GHC_OPTS_OPTIM		:= -O2

# source location (all src's will gradually move to this place, as from 20061515)
# currently, this definition is duplicated from ./src/files.mk
SRC_PREFIX			:= $(TOP_PREFIX)src/

# location for binaries
BIN_PREFIX			:= $(TOP_PREFIX)bin/

# location for libraries
LIB_PREFIX			:= $(TOP_PREFIX)lib/

# location for cabal installed stuff (mainly libraries)
INS_PREFIX			:= $(TOPABS_PREFIX)install/
INS_FLAG_PREFIX		:= $(INS_PREFIX)ins-flg-

# location building
BLD_PREFIX			:= $(TOP_PREFIX)build/
BLD_BIN_PREFIX		:= $(BLD_PREFIX)bin/
BLD_LIBUTIL_PREFIX	:= $(BLD_PREFIX)libutil/

# location for doc (end products)
DOC_PREFIX			:= $(TOP_PREFIX)doc/

# lhs2tex options
LHS2TEX_OPTS_DFLT	:= 
LHS2TEX_OPTS_POLY	:= $(LHS2TEX_OPTS_DFLT) --poly
LHS2TEX_OPTS_NEWC	:= $(LHS2TEX_OPTS_DFLT) --newcode

# ruler2 options
RULER2_OPTS_DFLT	:= $(RULER2_OPTS_VERSION)
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
comma				:= ,
empty				:=
space				:= $(empty) $(empty)

# generate cabal file
# $1: pkg name
# $2: version
# $3: build-depends (additional)
# $4: extensions (additional)
# $5: synopsis
# $6: exposed modules
GEN_CABAL			= \
					( \
					echo   "Name:				$(strip $(1))" ; \
					echo   "Version:			$(strip $(2))" ; \
					echo   "License:			PublicDomain" ; \
					echo   "Author:				Atze Dijkstra" ; \
					echo   "Homepage:			http://www.cs.uu.nl/wiki/Ehc/WebHome" ; \
					echo   "Category:			Testing" ; \
					echo   "Build-Depends:		$(subst $(space),$(comma),$(strip base haskell98 uulib $(3)))" ; \
					echo   "Extensions:			$(subst $(space),$(comma),$(strip RankNTypes MultiParamTypeClasses FunctionalDependencies $(4)))" ; \
					echo   "Synopsis:			$(strip $(5))" ; \
					echo   "Exposed-Modules:	$(subst $(space),$(comma),$(strip $(6)))" \
					)
