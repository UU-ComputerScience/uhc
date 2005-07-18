.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
LHS2TEX				:= lhs2TeX

# GHC options
GHC_OPTS			:= -fglasgow-exts -package util -package lang -package data -package uust

# location for binaries
BIN_PREFIX			:= $(TOP_PREFIX)bin/

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
