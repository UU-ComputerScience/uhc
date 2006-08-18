.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# all ehc variants
# 1 : explicitly typed lambda calculus
# 2 : + type inference (implicit typing)
# 3 : + polymorphism
# 4 : + quantifiers everywhere, existentials
# 5 : + datatypes
# 6 : + kinds (+inference)
# 7 : + fixed size records
# 8 : + code gen
# 9 : + (class) predicates + class system
# 10: + extensible records (lack predicates)
# 11: + type synonyms
# 12: + module system
# 99: + the rest to make uhc

# 50: + GADT experiment
# 4_2: + 2 pass type inference, quantifier propagation experiment
EHC_UHC_VARIANT							:= 99
EHC_PUB_VARIANTS						:= 1 2 3 4 5 6 7 8 9 10 11 12
EHC_VARIANTS							:= $(EHC_PUB_VARIANTS) 50 $(EHC_UHC_VARIANT) 4_2 6_4

GRIN_PUB_VARIANTS						:= 8 9 10 11 12
GRIN_VARIANTS							:= $(GRIN_PUB_VARIANTS) $(EHC_UHC_VARIANT)

# location for binaries
BIN_PREFIX			:= $(TOP_PREFIX)bin/
BINABS_PREFIX		:= $(TOPABS_PREFIX)bin/

# location for libraries
LIB_PREFIX			:= $(TOP_PREFIX)lib/

# location for cabal installed stuff (mainly libraries)
INS_PREFIX			:= $(TOP_PREFIX)install/
INSABS_PREFIX		:= $(TOPABS_PREFIX)install/
INSABS_FLAG_PREFIX	:= $(INSABS_PREFIX)ins-flg-

# location building
BLD_PREFIX			:= $(TOP_PREFIX)build/
BLD_BIN_PREFIX		:= $(BLD_PREFIX)bin/
BLD_LIBUTIL_PREFIX	:= $(BLD_PREFIX)libutil/

# location for doc (end products)
DOC_PREFIX			:= $(TOP_PREFIX)doc/

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
OPEN_FOR_EDIT		:= bbedit
STRIP				:= $(STRIP_CMD)
FILTER_NONEMP_FILES	:= $(BINABS_PREFIX)filterOutEmptyFiles

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

# lhs2tex options
LHS2TEX_OPTS_DFLT	:= 
LHS2TEX_OPTS_POLY	:= $(LHS2TEX_OPTS_DFLT) --poly
LHS2TEX_OPTS_NEWC	:= $(LHS2TEX_OPTS_DFLT) --newcode

# ruler2 options
RULER2_OPTS_DFLT	:= $(RULER2_OPTS_VERSION)
RULER2_OPTS			:= $(RULER2_OPTS_DFLT)

# order to shuffle (see ehc/src/files1.mk for a complete list)
# 4_99: interim for stuff from 4, needed for 4_2, because of ruler generated material uptil 4_2
EHC_SHUFFLE_ORDER	:= 1 < 2 < 3 < 4 < 4_99 < 5 < 6 < 7 < 8 < 9 < 10 < 11 < 12 < $(EHC_UHC_VARIANT), 4_99 < 4_2, 6 < 6_4, 10 < 50

# target suffix for core
CORE_TARG_SUFFIX	:= grin2

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

# generate simplest cabal Setup.hs
GEN_CABAL_SETUP		= @(echo "import Distribution.Simple" ; echo "main = defaultMain")

# compile cabal setup
# $1: src
# $2: exec
GHC_CABAL			= $(GHC) -package Cabal -o $(2) $(1)  ; $(STRIP_CMD) $(2)

# subst _ by x
# $1: text
SUBS_US_X			= $(subst _,x,$(1))