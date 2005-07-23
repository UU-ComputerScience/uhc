.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
LHS2TEX				:= lhs2TeX

# GHC options
GHC_OPTS			:= -fglasgow-exts -package util -package lang -package data -package uust
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

# order to shuffle
EHC_SHUFFLE_ORDER		:= 1 < 2 < 3 < 4 < 5 < 6 < 7 < 8 < 9 < 10 < 11, 4 < 4_2, 6 < 6_4

# subst's
SUBST_BAR_IN_TT		:= sed -e '/begin{TT}/,/end{TT}/s/|/||/g'
SUBST_EHC			:= perl $(BIN_PREFIX)substehc.pl
SUBST_LINE_CMT		:= sed -e 's/{-\# LINE[^\#]*\#-}//' -e '/{-\#  \#-}/d'

