.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

# build dir's for AG primer related programs
D_BUILD				:= build
D_BUILD_BIN			:= $(D_BUILD)/bin

# main document
AFP					:= afp

# tmp dir, specifically for each document variant,
# assumed to be prefixed by tmp- inside afp.lsty/lhs:
AFP_TMPDIR			:= tmp-$(AFP)/
AFP_FMT				:= afp.fmt

# main text
AFP_LHS				:= afp.lhs
AFP_PDF				:= $(AFP).pdf
AFP_TEX				:= $(AFP).tex
AFP_STY				:= $(AFP).sty

# sub texts
AFP_TEXTS			:= text/
AFP_TEXTS_LHS		:= $(addprefix $(AFP_TEXTS),AGMiniPrimer.lhs AGPatterns.lhs)
AFP_TEXTS_TEX		:= $(patsubst $(AFP_TEXTS)%.lhs,$(AFP_TMPDIR)%.tex,$(AFP_TEXTS_LHS))

# AG primer
AG_PRIMER			:= agprimer/
AG_PRIMER_CAG		:= $(addsuffix .cag,$(addprefix $(AG_PRIMER),RepminAG Expr))
AG_PRIMER_CHS		:= $(addsuffix .chs,$(addprefix $(AG_PRIMER),RepminHS))
AG_PRIMER_CAG_TEX	:= $(patsubst $(AG_PRIMER)%,$(AFP_TMPDIR)%,$(AG_PRIMER_CAG:.cag=.tex))
AG_PRIMER_CHS_TEX	:= $(patsubst $(AG_PRIMER)%,$(AFP_TMPDIR)%,$(AG_PRIMER_CHS:.chs=.tex))
AG_PRIMER_TEX		:= $(AG_PRIMER_CAG_TEX) $(AG_PRIMER_CHS_TEX)

# lhs2tex
LHS2TEX_PATH 		:=
LHS2TEX_OPTS_BASE	:= --set=asArticle --set=wide --unset=optExpandPrevRef --set=yesBeamer --set=useHyperref
LHS2TEX_OPTS		:= $(LHS2TEX_OPTS_BASE) --set=forAfpHandout
LHS2TEX				:= lhs2TeX $(LHS2TEX_OPTS) $(LHS2TEX_PATH)

# indentation of (test) output
INDENT2				:= sed -e 's/^/  /'
INDENT4				:= sed -e 's/^/    /'

# how to latex
AFP_LATEX			:= pdflatex --jobname $(AFP)

# on what AFP.tex depends w.r.t. inclusion of compiler output (from ehc versions)
AFP_TEX_DPDS		:= ehcs

# date
DATE				:= $(shell /bin/date +%Y%m%d)

# pre generated distribution
DIST				:= $(DATE)-ehc
DIST_PREFIX			:= 
DIST_ZIP			:= $(DIST_PREFIX)$(DIST).zip
DIST_TGZ			:= $(DIST_PREFIX)$(DIST).tgz

# distribution for afp04 lecture notes
DIST_AFP04			:= dist-afp04

# distributed/published stuff for WWW
WWW_SRC_ZIP			:= www/current-ehc-src.zip
WWW_SRC_TGZ			:= www/current-ehc-src.tgz
WWW_DOC_PDF			:= www/current-ehc-doc.pdf

# compilers and tools used
AGC					:= uuagc
GHC					:= ghc
SUBSTEHC			:= bin/substehc.pl
SUBSTSH				:= bin/substsh.pl

SUBST_BAR_IN_TT		:= sed -e '/begin{TT}/,/end{TT}/s/|/||/g'
SUBST_LINE_CMT		:= sed -e 's/{-\# LINE[^\#]*\#-}//' -e '/{-\#  \#-}/d'

# Makefile template for making a ehc version
MK_EHFILES			:= mk/ehfiles.mk

# AGC(opts, file)
AGCC				= cd `dirname $2` ; $(AGC) $1 `basename $2`

# lhs2tex format files used
AFP_FMT_OTHER		:= lag2TeX.fmt pretty.fmt parsing.fmt

# type rules, in ruler format
AFP_RULES			:= rules.rul rules2.rul
AFP_RULES_TEX		:= $(AFP_RULES:.rul=.tex)

# pictures in pgf format
AFP_PGF_TEX			:= afp-pgf.tex

# all text sources
ALL_AFP_SRC			:= $(AFP_LHS) $(AFP_RULES)


EHC_SRC_PREFIX				:=
EHC_LAG_FOR_HS_TY			:= $(addsuffix .lag,EHTyQuantify EHTySubst EHTyElimAlts EHTyFreshVar EHTyElimBoth EHTyElimEqual EHTyFtv EHTyPretty EHTyInstantiate )
EHC_LAG_FOR_HS_CORE			:= $(addsuffix .lag,EHCoreJava EHCoreGrin EHCoreTrfRenUniq EHCoreTrfFullLazy EHCoreTrfLamLift \
												EHCoreTrfInlineLetAlias EHCoreTrfLetUnrec EHCorePretty EHCoreSubst EHCoreTrfConstProp)
EHC_LAG_FOR_HS_GRIN_CODE	:= $(addsuffix .lag,GrinCodePretty)
EHC_LAG_FOR_HS				:= $(addsuffix .lag,EHMainAG EHTy EHCore EHError EHErrorPretty GrinCode) \
								$(EHC_LAG_FOR_HS_TY) $(EHC_LAG_FOR_HS_CORE) $(EHC_LAG_FOR_HS_GRIN_CODE)


GRI_SRC_PREFIX				:= gri/

GRI_LAG_FOR_HS_GRIN_CODE	:= $(addsuffix .lag,GRISetup)
GRI_LAG_FOR_HS				:= $(GRI_LAG_FOR_HS_GRIN_CODE)



EHC_DPDS_MAIN					:= EHMainAG.ag EHInfer.ag EHInferExpr.ag \
									EHInferPatExpr.ag EHInferTyExpr.ag EHInferKiExpr.ag EHInferData.ag \
									EHInferCaseExpr.ag EHPretty.ag EHPrettyAST.ag EHAbsSyn.ag \
									EHUniq.ag EHExtraChecks.ag EHGatherError.ag \
									EHGenCore.ag \
									EHResolvePred.ag EHInferClass.ag
EHC_DPDS_CORE					:= EHCore.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_GRIN				:= EHCoreGrin.ag EHCoreCommonLev.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_JAVA				:= EHCoreJava.ag EHCoreCommonLev.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_PRETTY			:= EHCorePretty.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_SUBST				:= EHCoreSubst.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_CONSTPROP		:= EHCoreTrfConstProp.ag EHCoreCommonLev.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_FULLAZY		:= EHCoreTrfFullLazy.ag EHCoreTrfCommonFv.ag EHCoreTrfCommonLev.ag EHCoreCommonLev.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_INLLETALI		:= EHCoreTrfInlineLetAlias.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_LAMLIFT		:= EHCoreTrfLamLift.ag EHCoreTrfCommonFv.ag EHCoreTrfCommonLev.ag EHCoreCommonLev.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_LETUNREC		:= EHCoreTrfLetUnrec.ag EHCoreAbsSyn.ag
EHC_DPDS_CORE_TRF_RENUNQ		:= EHCoreTrfRenUniq.ag EHCoreAbsSyn.ag
EHC_DPDS_ERR					:= EHError.ag EHErrorAbsSyn.ag
EHC_DPDS_ERR_PRETTY				:= EHErrorPretty.ag EHErrorAbsSyn.ag
EHC_DPDS_GRIN_CODE				:= GrinCode.ag GrinCodeAbsSyn.ag
EHC_DPDS_GRIN_CODE_PRETTY		:= GrinCodePretty.ag GrinCodeAbsSyn.ag
EHC_DPDS_TY						:= EHTy.ag EHTyAbsSyn.ag
EHC_DPDS_TY_FTV					:= EHTyFtv.ag EHTyAbsSyn.ag
EHC_DPDS_TY_INST				:= EHTyInstantiate.ag EHTyCommonAG.ag EHTyAbsSyn.ag
EHC_DPDS_TY_PRETTY				:= EHTyPretty.ag EHTyCommonAG.ag EHTyAbsSyn.ag
EHC_DPDS_TY_QUANT				:= EHTyQuantify.ag EHTyCommonAG.ag EHTyAbsSyn.ag
EHC_DPDS_TY_SUBST				:= EHTySubst.ag EHTyAbsSyn.ag
EHC_DPDS_TY_ELIMB				:= EHTyElimAlts.ag EHTyAbsSyn.ag
EHC_DPDS_TY_ELIMBOTH			:= EHTyElimBoth.ag EHTyAbsSyn.ag
EHC_DPDS_TY_ELIMEQUAL			:= EHTyElimEqual.ag EHTyAbsSyn.ag
EHC_DPDS_TY_FRESHVAR			:= EHTyFreshVar.ag EHTyAbsSyn.ag

EHC_DPDS_CORE_TRF				:= $(EHC_DPDS_CORE_TRF_CONSTPROP) $(EHC_DPDS_CORE_TRF_RENUNQ) $(EHC_DPDS_CORE_TRF_INLLETALI) \
									$(EHC_DPDS_CORE_TRF_FULLAZY) $(EHC_DPDS_CORE_TRF_LETUNREC) $(EHC_DPDS_CORE_TRF_LAMLIFT)
EHC_DPDS_ALL					:= $(sort $(EHC_DPDS_MAIN) \
										$(EHC_DPDS_CORE) $(EHC_DPDS_CORE_JAVA) $(EHC_DPDS_CORE_GRIN) $(EHC_DPDS_CORE_PRETTY) $(EHC_DPDS_CORE_SUBST) $(EHC_DPDS_CORE_TRF) \
										$(EHC_DPDS_TY) $(EHC_DPDS_TY_PRETTY) $(EHC_DPDS_TY_QUANT) $(EHC_DPDS_TY_SUBST) $(EHC_DPDS_TY_FTV) $(EHC_DPDS_TY_INST) \
										$(EHC_DPDS_GRIN_CODE) $(EHC_DPDS_GRIN_CODE_PRETTY) \
										$(EHC_DPDS_ERR) $(EHC_DPDS_ERR_PRETTY) \
										$(EHC_DPDS_TY_ELIMBOTH) $(EHC_DPDS_TY_FRESHVAR) $(EHC_DPDS_TY_ELIMB) $(EHC_DPDS_TY_ELIMEQUAL) \
										)
EHC_DPDS_ALL_MIN_TARG			:= $(filter-out $(EHC_LAG_FOR_HS:.lag=.ag),$(EHC_DPDS_ALL))

EHC					:= ehc
EHC_MAIN			:= EHC
EHC_LAG_FOR_AG		:= $(EHC_DPDS_ALL_MIN_TARG:.ag=.lag)
EHC_LAG				:= $(EHC_LAG_FOR_AG) $(EHC_LAG_FOR_HS)
EHC_LHS_FOR_HS		:= $(addsuffix .lhs,$(EHC_MAIN) EHCommon EHOpts EHCnstr EHSubstitutable EHTyFitsIn EHTyFitsInCommon EHGam EHGamUtils EHPred EHParser FPath EHScanner EHScannerMachine EHCoreUtils EHDebug)
EHC_LHS				:= $(EHC_LHS_FOR_HS)
EHC_HS				:= $(EHC_LAG_FOR_HS:.lag=.hs) $(EHC_LHS_FOR_HS:.lhs=.hs)

GRI_DPDS_GRI					:= GRI.hs EHScanner.hs EHScannerMachine.hs EHCommon.hs GRIParser.hs GrinCode.hs GRICommon.hs
GRI_DPDS_GRIN_CODE_SETUP		:= GRISetup.ag
GRI_DPDS_ALL					:= $(sort $(GRI_DPDS_GRIN_CODE_SETUP))
GRI_DPDS_ALL_MIN_TARG			:= $(filter-out $(GRI_LAG_FOR_HS:.lag=.ag),$(GRI_DPDS_ALL))

GRI					:= gri
GRI_MAIN			:= GRI
GRI_LAG_FOR_AG		:= $(GRI_DPDS_ALL_MIN_TARG:.ag=.lag)
GRI_LAG				:= $(GRI_LAG_FOR_AG) $(GRI_LAG_FOR_HS)
GRI_LHS_FOR_HS		:= $(addsuffix .lhs,$(GRI_MAIN) GRICommon GRIParser GRIRun)
GRI_LHS				:= $(GRI_LHS_FOR_HS)
GRI_HS				:= $(GRI_LAG_FOR_HS:.lag=.hs) $(GRI_LHS_FOR_HS:.lhs=.hs) $(GRI_DPDS_GRI)

AFP_DERIV			:= $(addprefix $(AFP),.toc .bbl .blg .aux .tex .log .ind .idx) $(AFP_STY)

SHUFFLE				:= bin/shuffle
SHUFFLE_DIR			:= shuffle
SHUFFLE_MAIN		:= Shuffle
SHUFFLE_AG			:= $(SHUFFLE_MAIN).ag
SHUFFLE_HS			:= $(SHUFFLE_AG:.ag=.hs)
SHUFFLE_DERIV		:= $(SHUFFLE_DIR)/$(SHUFFLE_HS)
SHUFFLE_DOC_PDF		:= $(SHUFFLE_DIR)/ShuffleDoc.pdf

SHUFFLE_SRC			:= $(SHUFFLE_DIR)/$(SHUFFLE_AG)

SHUFFLE_ORDER		:= 1 < 2 < 3 < 4 < 5 < 6 < 7 < 8 < 9 < 10 < 11, 4 < 4_2


RULER				:= bin/ruler
RULER_DIR			:= ruler
RULER_MAIN			:= Ruler
RULER_AG			:= $(RULER_MAIN).ag
RULER_HS			:= $(RULER_AG:.ag=.hs)
RULER_DERIV			:= $(RULER_DIR)/$(RULER_HS)
RULER_DOC_PDF		:= $(RULER_DIR)/RulerDoc.pdf

RULER_SRC			:= $(RULER_DIR)/$(RULER_AG)

BREW				:= bin/brew
BREW_DIR			:= brew
BREW_MAIN			:= Brew
BREW_AG				:= $(BREW_MAIN).ag
BREW_HS				:= $(BREW_AG:.ag=.hs)
BREW_DERIV			:= $(BREW_DIR)/$(BREW_HS)
BREW_DOC_PDF		:= $(BREW_DIR)/BrewDoc.pdf

BREW_SRC			:= $(BREW_DIR)/$(BREW_AG)

CORE_TARG			:= grin


# LHS2TEX_POLY(src file, dst file)
PERL_SUBST_EHC			= \
	perl $(SUBSTEHC) < $(1) > $(2)

LHS2TEX_POLY_MODE	:= --poly
# LHS2TEX_POLY(src file, dst file)
LHS2TEX_POLY			= \
	perl $(SUBSTEHC) < $(1) | $(SUBST_BAR_IN_TT) | $(LHS2TEX) $(LHS2TEX_POLY_MODE) > $(2)

# LHS2TEX_POLY_2(src file, dst file)
LHS2TEX_POLY_2			= \
	perl $(SUBSTSH) < $(1) | $(LHS2TEX) --poly > $(2)

# LHS2TEX_POLY_3(src file, dst file)
LHS2TEX_POLY_3			= \
	$(LHS2TEX) $(LHS2TEX_POLY_MODE) $(1) > $(2)

# LHS2TEX_CODE(src file, dst file)
LHS2TEX_CODE			= \
	$(LHS2TEX) --newcode $(1) > $(2)

default:
	@echo "make <n>/ehc     : make compiler version <n> (where <n> in {$(VERSIONS)})" ; \
	echo  "make <n>/gri     : make grin interpreter version <n> (where <n> in {$(GRI_VERSIONS)})" ; \
	echo  "make ehcs        : make all compiler (ehc) versions" ; \
	echo  "make gris        : make all grin interpreter (gri) versions" ; \
	echo  "make afp         : make afp.pdf, by running latex once" ; \
	echo  "make afp-full    : make afp.pdf, with bib/index" ; \
	echo  "make doc         : make doc of tools" ; \
	echo  "make all         : make all of the above" ; \
	echo ; \
	echo  "make afp-slides  : make slides afp-slides.pdf" ; \
	echo  "make afp04       : make LLNCS variant of afp.pdf, for proceedings" ; \
	echo  "make esop05      : make ESOP2005 article Explicit implicit parameters" ; \
	echo  "make afp-tr      : make UU techreport variant of afp.pdf" ; \
	echo  "make test-regress: run regression test" ; \
	echo  "make test-expect : make expected output (for later comparison with test-regress)" ; \
	echo  "make dist        : make distribution (of ehc src versions) in $(DIST_PREFIX)<today>-ehc.zip" ; \
	echo  "make www         : make 'current' www dist (based on dist)" ; \
	echo  "make www-sync    : sync www dist (proper ssh access required)"

all: afp-full ehcs doc gris
	$(MAKE) initial-test-expect

doc: $(SHUFFLE_DOC_PDF)

%.tex:%.lag
	$(call LHS2TEX_POLY,$<,$@)

%.tex:%.lhs
	$(call LHS2TEX_POLY,$<,$@)

%.tex:%.ltex
	$(call LHS2TEX_POLY_2,$<,$@)

%.sty:%.lsty
	$(call LHS2TEX_POLY_3,$<,$@)

%.ag:%.lag
	$(call LHS2TEX_CODE,$<,$@)

%.hs:%.lhs
	$(call LHS2TEX_CODE,$<,$@)

%.hs:%.ag
	cd `dirname $<` ; $(AGC) -dcfspr `basename $< .ag`

#VPREFIX				:=
#include $(MK_EHFILES)

### Versioned ehc's, gri's
VERSIONS			:= 1 2 3 4 5 6 7 8 9 10
VERSION_LAST		:= $(word $(words $(VERSIONS)), $(VERSIONS))
VERSION_FIRST		:= $(word 1, $(VERSIONS))

EHC_CAG				:= $(EHC_LAG:.lag=.cag)
EHC_CHS				:= $(EHC_LHS:.lhs=.chs)

GRI_VERSIONS		:= 8 9

GRI_CAG				:= $(GRI_LAG:.lag=.cag)
GRI_CHS				:= $(GRI_LHS:.lhs=.chs)

# SHUFFLE_LHS(src file, dst file, how, lhs2tex, version, base)
SHUFFLE_LHS		= \
	dir=`dirname $2` ; \
	mkdir -p $$dir ; \
	$(SHUFFLE) --gen=$5 --base=$6 $3 $1 --order="$(SHUFFLE_ORDER)" | $4 > $2

# SHUFFLE_LHS_AG(src file, dst file, version, base)
SHUFFLE_LHS_AG		= \
	$(call SHUFFLE_LHS,$1,$2,--ag,$(LHS2TEX) --newcode,`dirname $2`,$4)

# SHUFFLE_LHS_HS(src file, dst file, version, base)
SHUFFLE_LHS_HS		= \
	$(call SHUFFLE_LHS,$1,$2,--hs,$(LHS2TEX) --newcode | $(SUBST_LINE_CMT),`dirname $2`,$4)

# SHUFFLE_LHS_TEX(src file, dst file, version,base)
SHUFFLE_LHS_TEX		= \
	$(call SHUFFLE_LHS,$1,$2,--latex --xref-except=shuffleXRefExcept,$(LHS2TEX) $(LHS2TEX_POLY_MODE),`dirname $2`,$4)
#	$(call SHUFFLE_LHS,$1,$2,--latex --index --xref-except=shuffleXRefExcept,$(LHS2TEX) --poly,$3,$4)

# RULER_LHS(base, src file, dst file, lhs2tex)
RULER_LHS		= \
	$(RULER) --latex --base $1 $2 | $4 > $3

# RULER_LHS_TEX(base, src file, dst file)
RULER_LHS_TEX		= \
	$(call RULER_LHS,$1,$2,$3,$(LHS2TEX) --poly)


### Version 1
V					:= 1
VF					:= $(V)
include $(MK_EHFILES)
EHC_V1				:= $(addprefix $(VF)/,$(EHC))
### End of Version 1


### Version 2
V					:= 2
VF					:= $(V)
include $(MK_EHFILES)
EHC_V2				:= $(addprefix $(VF)/,$(EHC))
### End of Version 2


### Version 3
V					:= 3
VF					:= $(V)
include $(MK_EHFILES)
EHC_V3				:= $(addprefix $(VF)/,$(EHC))
### End of Version 3


### Version 4
V					:= 4
VF					:= $(V)
include $(MK_EHFILES)
EHC_V4				:= $(addprefix $(VF)/,$(EHC))
### End of Version 4


### Version 4:2
V					:= 4_2
VF					:= 4_2
include $(MK_EHFILES)
EHC_V4_2			:= $(addprefix $(VF)/,$(EHC))
### End of Version 4_2


### Version 5
V					:= 5
VF					:= $(V)
include $(MK_EHFILES)
EHC_V5				:= $(addprefix $(VF)/,$(EHC))
### End of Version 5


### Version 6
V					:= 6
VF					:= $(V)
include $(MK_EHFILES)
EHC_V6				:= $(addprefix $(VF)/,$(EHC))
### End of Version 6


### Version 6:1
V					:= 6_1
VF					:= 6_1
include $(MK_EHFILES)
EHC_V6_1			:= $(addprefix $(VF)/,$(EHC))
### End of Version 6_1


### Version 7
V					:= 7
VF					:= $(V)
include $(MK_EHFILES)
EHC_V7				:= $(addprefix $(VF)/,$(EHC))
### End of Version 7


### Version 8
V					:= 8
VF					:= $(V)
include $(MK_EHFILES)
EHC_V8				:= $(addprefix $(VF)/,$(EHC))
GRI_V8				:= $(addprefix $(VF)/,$(GRI))
### End of Version 8


### Version 9
V					:= 9
VF					:= $(V)
include $(MK_EHFILES)
EHC_V9				:= $(addprefix $(VF)/,$(EHC))
GRI_V9				:= $(addprefix $(VF)/,$(GRI))
### End of Version 9


### Version 10
V					:= 10
VF					:= $(V)
include $(MK_EHFILES)
EHC_V10				:= $(addprefix $(VF)/,$(EHC))
GRI_V10				:= $(addprefix $(VF)/,$(GRI))
### End of Version 10


### Version 11
V					:= 11
VF					:= $(V)
include $(MK_EHFILES)
EHC_V11				:= $(addprefix $(VF)/,$(EHC))
GRI_V11				:= $(addprefix $(VF)/,$(GRI))
### End of Version 11


### AG Primer's Repmin AG variant
$(AG_PRIMER_CAG:.cag=.ag): %.ag: %.cag $(SHUFFLE)
	$(call SHUFFLE_LHS,$<,$@,--ag,$(LHS2TEX) --newcode,1,Main) ; \
	touch $@

$(D_BUILD_BIN)/repminag: $(AG_PRIMER)RepminAG.hs
	d=`dirname $@` ; mkdir -p $$d ; $(GHC) -package uust -package data -o $@ --make $<
### End of Repmin AG variant


### AG Primer's Expr
$(D_BUILD_BIN)/expr: $(AG_PRIMER)Expr.hs
	d=`dirname $@` ; mkdir -p $$d ; $(GHC) -package uust -package data -o $@ --make $<
### End of Expr


### AG Primer's Repmin Haskell variant
$(AG_PRIMER_CHS:.chs=.hs): %.hs: %.chs $(SHUFFLE)
	$(call SHUFFLE_LHS_HS,$<,$@,1,Main) ; \
	touch $@

$(D_BUILD_BIN)/repminhs: $(AG_PRIMER)RepminHS.hs
	d=`dirname $@` ; mkdir -p $$d ; $(GHC) -package uust -package data -o $@ --make $<
### End of Repmin Haskell variant


### TeX production upto a version
EHC_VLAST_AG_TEX	:= $(addprefix $(AFP_TMPDIR),$(EHC_CAG:.cag=.tex))
EHC_VLAST_HS_TEX	:= $(addprefix $(AFP_TMPDIR),$(EHC_CHS:.chs=.tex))
EHC_VLAST_TEX		:= $(EHC_VLAST_AG_TEX) $(EHC_VLAST_HS_TEX)

$(EHC_VLAST_HS_TEX): $(AFP_TMPDIR)%.tex: %.chs $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all,$(*F))

$(EHC_VLAST_AG_TEX): $(AFP_TMPDIR)%.tex: %.cag $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all,$(*F))

### TeX production of AG primer code
$(AG_PRIMER_CAG_TEX): $(AFP_TMPDIR)%.tex: $(AG_PRIMER)%.cag $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all,$(*F))

$(AG_PRIMER_CHS_TEX): $(AFP_TMPDIR)%.tex: $(AG_PRIMER)%.chs $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all,$(*F))

### TeX production of sub texts
$(AFP_TEXTS_TEX): $(AFP_TMPDIR)%.tex: $(AFP_TEXTS)%.lhs Makefile
	$(call LHS2TEX_POLY,$<,$@)

### Dpds for main text
$(AFP_PDF): $(AFP_TEX) $(AFP_STY) $(EHC_VLAST_TEX) $(AFP_RULES_TEX) $(AFP_PGF_TEX) $(AFP_TEXTS_TEX) $(AG_PRIMER_TEX)
	$(AFP_LATEX) $(AFP_TEX)

$(AFP_TEX): $(AFP_LHS) $(AFP_TEX_DPDS)
	$(call LHS2TEX_POLY,$<,$@)

$(AFP_STY): afp.lsty Makefile
	$(call LHS2TEX_POLY_3,$<,$@)

$(AFP_RULES_TEX): %.tex: %.rul $(RULER) $(AFP_FMT)
	$(call RULER_LHS_TEX,$(*F),$<,$@)

agprimer: $(D_BUILD_BIN)/repminag $(D_BUILD_BIN)/repminhs $(D_BUILD_BIN)/expr

afp: $(AFP_PDF)

afp-full: afp
	bibtex $(AFP)
	$(AFP_LATEX) $(AFP_TEX)
	rm -f $(AFP).ind
	makeindex $(AFP)
	$(AFP_LATEX) $(AFP_TEX)

afp-bib: afp
	bibtex $(AFP)
	$(AFP_LATEX) $(AFP_TEX)
	$(AFP_LATEX) $(AFP_TEX)

afp-tr:
	$(MAKE) AFP=$@ LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=asArticle --set=truu --set=storyAfpTRUU1 --set=omitTBD --set=omitLitDiscuss" afp-full

afp04:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=llncs --set=storyAFP04Notes --set=omitTBD --set=omitLitDiscuss" afp-bib

afp04-dist-tex:
	$(MAKE) AFP=afp04-dist AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=llncs --set=storyAFP04Notes --set=omitTBD --set=omitLitDiscuss --set=dist" afp04-dist.tex

afp-tst:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=llncs --set=storyAFP04Notes --set=omitTBD --set=omitLitDiscuss" afp

icfp05-explimpl-tst:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set icfp05 --set=omitTBD --set=omitLitDiscuss" afp

icfp05-explimpl:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set icfp05 --set=omitTBD --set=omitLitDiscuss" afp-bib

esop05:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=llncs --set=storyExplImpl --set=omitTBD --set=omitLitDiscuss" afp

esop05-tr:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=truu --set=storyExplImpl --set=omitTBD --set=omitLitDiscuss" afp-bib

eh-work:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=onlyCurrentWork --unset=asArticle --set=refToPDF --set=inclOmitted" afp

phd:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=storyPHD --unset=asArticle --set=refToPDF --set=inclOmitted" afp

afp-slides:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=storyPHD --set=asSlides --set=omitTBD --set=omitLitDiscuss" afp

eh-intro:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=storyEHIntro --set=asSlides --set=omitTBD --set=omitLitDiscuss" afp

.PHONY: shuffle ruler brew ehcs dist www www-sync gri gris agprimer dist-afp04

shuffle: $(SHUFFLE)

$(SHUFFLE): $(SHUFFLE_DIR)/$(SHUFFLE_AG) $(wildcard lib/*.hs)
	cd $(SHUFFLE_DIR) ; \
	$(AGC) -csdfr --module=Main `basename $<` ; \
	$(GHC) --make -package uust -package data -i../lib $(SHUFFLE_HS) -o ../$@ ; \
	strip ../$@

$(SHUFFLE_DIR)/ShuffleDoc.tex: $(SHUFFLE)

$(SHUFFLE_DOC_PDF): $(SHUFFLE_DIR)/ShuffleDoc.tex
	cd `dirname $<` ; pdflatex `basename $<`

ruler: $(RULER)

$(RULER): $(RULER_DIR)/$(RULER_AG) $(wildcard lib/*.hs)
	cd $(RULER_DIR) ; \
	$(AGC) -csdfr --module=Main `basename $<` ; \
	$(GHC) --make -package uust -package data -i../lib $(RULER_HS) -o ../$@ ; \
	strip ../$@

$(RULER_DOC_PDF): $(RULER_DIR)/RulerDoc.tex $(RULER)
	cd `dirname $<` ; pdflatex `basename $<`

brew: $(BREW)

$(BREW): $(BREW_DIR)/$(BREW_AG) $(wildcard lib/*.hs)
	cd $(BREW_DIR) ; \
	$(AGC) -csdfr --module=Main `basename $<` ; \
	$(GHC) --make -package uust -package data -i../lib $(BREW_HS) -o ../$@ ; \
	strip ../$@

$(BREW_DOC_PDF): $(BREW_DIR)/RulerDoc.tex $(BREW)
	cd `dirname $<` ; pdflatex `basename $<`

ehcs: $(EHC_V1) $(EHC_V2) $(EHC_V3) $(EHC_V4) $(EHC_V5) $(EHC_V6) $(EHC_V7) $(EHC_V8) $(EHC_V9) $(EHC_V10)

gris: $(GRI_V8) $(GRI_V9) $(GRI_V10)

clean:
	rm -rf $(AFP_DERIV) $(SHUFFLE_DERIV) a.out \
	$(addprefix $(SHUFFLE_DIR)/,*.o *.hi *.pdf) $(SHUFFLE) \
	$(addprefix $(RULER_DIR)/,*.o *.hi *.pdf) $(RULER) \
	$(AFP_PDF) \
	*.o *.hi $(VERSIONS) $(D_BUILD) \
	test/*.reg* test/*.class *.class test/*.java test/*.code \
	$(DIST_PREFIX)*.zip $(DIST_PREFIX)*.tgz \
	20??????-ehc \
	$(WWW_SRC_ZIP) $(WWW_SRC_TGZ) $(WWW_DOC_PDF) \
	tmp-* \
	*.log *.lof *.ilg *.out *.toc *.ind *.idx *.aux

clean-test:
	rm -rf test/*.reg* test/*.exp*

edit:
	bbedit $(EHC_CAG) $(EHC_CHS) $(addprefix $(GRI_SRC_PREFIX),$(GRI_CAG)) $(addprefix $(GRI_SRC_PREFIX),$(GRI_CHS)) $(ALL_AFP_SRC) afp.lsty afp.fmt $(SHUFFLE_SRC) Makefile $(TMPL_TEST) $(MK_EHFILES)

A_EH_TEST			:= $(word 1,$(wildcard test/*.eh))
A_EH_TEST_EXP		:= $(addsuffix .exp$(VERSION_FIRST),$(A_EH_TEST))

tst:
	echo $(VERSION_LAST)
	echo $(A_EH_TEST_EXP)

initial-test-expect: $(A_EH_TEST_EXP)

$(A_EH_TEST_EXP): $(A_EH_TEST)
	$(MAKE) test-expect

test-lists:
	@cd test ; \
	for v in $(VERSIONS) ; \
	do \
	  ehs= ; \
	  vv=`echo $$v | sed -e 's/_[0-9]//'` ; \
	  for ((i = 1 ; i <= $${vv} ; i++)) ; do ehs="$$ehs `ls $${i}-*.eh`" ; done ; \
	  echo "$$ehs" > $$v.lst ; \
	done

TMPL_TEST			:= mk/templ-test-dist.mk
include $(TMPL_TEST)
	
# FILTER_EXISTS(files)
FILTER_EXISTS		= \
	for _f in $(1); do \
	  if test -r $$_f; then echo -n " $$_f"; fi \
	done

# FILTER_EXISTS_HS_OR_AG(files)
FILTER_EXISTS_HS_OR_AG		= \
	for _f in $(1); do \
	  _b=`basename $$_f .hs` ; \
	  if test -r $$_f -o -r $$_b.ag; then echo -n " $$_f"; fi \
	done

# MK_EHC_MKF_FOR(files,AG opts)
MK_EHC_MKF_FOR		= \
	echo ; \
	echo -n "$(patsubst %.ag,%.hs,$(word 1,$(1))):" ; \
	$(call FILTER_EXISTS,$(1)) ; \
	echo ; \
	echo "	uuagc $(2) $$<"

# MK_EHC_MKF()
MK_EHC_MKF			= \
	( echo "\# Generated for distribution $(DATE) (`date`)" ; \
	  echo ; \
	  echo "all: $(EHC) $(GRI)"  ; \
	  echo "	@echo 'Type make test-regress to compare with expected test results'" ; \
	  echo ; \
	  echo -n "$(EHC): $(EHC_MAIN).hs"  ; \
	  $(call FILTER_EXISTS_HS_OR_AG,$(EHC_HS)) ; \
	  echo ; \
	  echo "	$(GHC) -package uust -package data --make -o $(EHC) $$<" ; \
	  echo ; \
	  echo -n "$(GRI): $(GRI_MAIN).hs $(EHC)"  ; \
	  $(call FILTER_EXISTS_HS_OR_AG,$(GRI_HS)) ; \
	  echo ; \
	  echo "	$(GHC) -package uust -package data --make -o $(GRI) $$<" ; \
	  echo ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_GRIN),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_JAVA),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_SUBST),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_CONSTPROP),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_FULLAZY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_INLLETALI),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_LAMLIFT),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_LETUNREC),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE_TRF_RENUNQ),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_CORE),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_ERR_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_ERR),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_GRIN_CODE_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_GRIN_CODE),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_MAIN),-dcfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_FTV),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_INST),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_QUANT),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_SUBST),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_ELIMB),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_ELIMBOTH),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_ELIMEQUAL),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY_FRESHVAR),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(EHC_DPDS_TY),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(GRI_DPDS_GRI),-dcfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(GRI_DPDS_GRIN_CODE_SETUP),-cfspr) ; \
	) > Makefile

dist: $(DIST_ZIP) 

$(DIST_TGZ): $(DIST_ZIP)

$(DIST_ZIP): $(addprefix $(VERSION_LAST)/,$(EHC_DPDS_ALL_MIN_TARG)) Makefile test-lists $(TMPL_TEST)
	@rm -rf $(DIST) ; \
	mkdir -p $(addprefix $(DIST)/,$(VERSIONS)) $(addsuffix /test,$(addprefix $(DIST)/,$(VERSIONS))) ; \
	for v in $(VERSIONS) ; do \
	  echo "== version $$v ==" ; \
	  for f in $$v/*.hs $$v/*.ag ; do \
	    sz=`wc -l $$f | awk '{print $$1}'` ; \
	    bhs=`basename $$f .hs` ; \
	    if test $$sz -gt 0 -a ! -r $$v/$$bhs.ag ; then \
	      $(SUBST_LINE_CMT) < $$f > $(DIST)/$$f ; \
	    fi \
	  done ; \
	  cd test ; \
	  cp `cat $$v.lst` ../$(DIST)/$$v/test ; \
	  cp `cat $$v.lst | sed -e 's/\.eh/&.exp*/g'` ../$(DIST)/$$v/test ; \
	  cd .. ; \
	  pwd=`pwd` ; \
	  cd $(DIST)/$$v ; \
	  $(call MK_EHC_MKF) ; \
	  ( echo ; \
	    sed \
	      -e "s/\$$(VERSIONS)/$$v/g" \
	      -e "s/test-lists//g" \
	      -e "s,\$$\$$v/\$$(EHC),$(EHC),g" \
	      -e "s,\$$\$$v/\$$(GRI),$(GRI),g" \
	      -e "s/\`cat \$$\$$v.lst\`/*.eh/g" \
	      -e "s,\$$(CORE_TARG),$(CORE_TARG),g" \
	      -e "s,\$$(INDENT2),$(INDENT2),g" \
	      -e "s,\$$(INDENT4),$(INDENT4),g" \
	      -e "s/\$$\$$v/$$v/g" \
	      -e "s/\$$\$${v}/$$v/g" \
	      < ../../$(TMPL_TEST) ; \
	  ) >> Makefile ; \
	  cd $$pwd ; \
	done ; \
	echo "== zip ==" ; \
	tar cfz $(DIST_TGZ) $(DIST) ; \
	echo "== tar ==" ; \
	zip -qur $(DIST_ZIP) $(DIST)

dist-afp04: afp04-dist-tex
	@rm -rf $(DIST_AFP04) ; \
	mkdir -p $(DIST_AFP04) ; \
	latex --jobname afp04 afp04 ; \
	cp $(addprefix afp04.,pdf dvi aux toc) rules.tex afp-pgf.tex $(DIST_AFP04) ; \
	cp afp04-dist.tex $(DIST_AFP04)/afp04.tex ; \
	cp -r tmp-afp04 figs $(DIST_AFP04) ; \
	cp $(addsuffix .sty,afp04 doubleequals infrule beamerthemeafp kscode textpos) llncs.cls $(DIST_AFP04) ; \
	cp $(addprefix /Volumes/Apps/Install/LaTeX/,latex-beamer-2.21.tar.gz pgf-0.62.tar.gz xcolor-2.00.tar.gz xkeyval.tar.gz) $(DIST_AFP04) ; \
	cp /Volumes/Apps/Install/lhs2TeX/lhs2tex-1.9.tar.bz2 $(DIST_AFP04) ; \
	cp -r /Volumes/Work/Library/texmf $(DIST_AFP04) ; \
	cp -r /usr/local/share/lhs2tex/lhs2TeX.sty $(DIST_AFP04) ; \
	tar cfz $(DIST_AFP04).tgz $(DIST_AFP04)

wc:
	grep frametitle $(AFP_LHS) | wc
	wc $(EHC_CAG) $(EHC_CHS)

WWW_EXAMPLES_TMPL			:=	www/ehc-examples-templ.html
WWW_EXAMPLES_HTML			:=	www/ehc-examples.html

www-ex: $(WWW_EXAMPLES_HTML)

www: $(WWW_SRC_ZIP) $(WWW_SRC_TGZ) www-ex # $(WWW_DOC_PDF)

www/DoneSyncStamp: www-ex
	(date; echo -n ", " ; svn up) > www/DoneSyncStamp ; \
	rsync --progress -azv -e ssh www/* atze@modena.cs.uu.nl:/users/www/groups/ST/Projects/ehc

www-sync: www/DoneSyncStamp

$(WWW_EXAMPLES_HTML): $(WWW_EXAMPLES_TMPL)
	$(call PERL_SUBST_EHC,$(WWW_EXAMPLES_TMPL),$(WWW_EXAMPLES_HTML))

$(WWW_SRC_ZIP): $(DIST_ZIP)
	cp $< $@

$(WWW_SRC_TGZ): $(DIST_TGZ)
	cp $< $@

$(WWW_DOC_PDF): $(AFP_PDF)
	cp $< $@

