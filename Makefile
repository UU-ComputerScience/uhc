.SUFFIXES:
.SUFFIXES: .pdf .tex .bib .html .lhs .sty .lag .cag .chs

AFP					:= afp
AFP_LHS				:= afp.lhs
AFP_PDF				:= $(AFP).pdf
AFP_TEX				:= $(AFP).tex
AFP_STY				:= $(AFP).sty
# assumed to be prefixed by tmp- inside afp.lsty/lhs:
AFP_TMPDIR			:= tmp-$(AFP)/
AFP_FMT				:= afp.fmt

LHS2TEX_PATH 		:=
LHS2TEX_OPTS_BASE	:= --set=asArticle --set=wide --set=expandPrevRef --set=yesBeamer
LHS2TEX_OPTS		:= $(LHS2TEX_OPTS_BASE) --set=forAfpHandout
LHS2TEX				:= lhs2TeX $(LHS2TEX_OPTS) $(LHS2TEX_PATH)
INDENT2				:= sed -e 's/^/  /'
INDENT4				:= sed -e 's/^/    /'

AFP_LATEX			:= pdflatex --jobname $(AFP)

DATE				:= $(shell /bin/date +%Y%m%d)

DIST				:= $(DATE)-ehc
DIST_PREFIX			:= 
DIST_ZIP			:= $(DIST_PREFIX)$(DIST).zip
DIST_TGZ			:= $(DIST_PREFIX)$(DIST).tgz

WWW_SRC_ZIP			:= www/current-ehc-src.zip
WWW_DOC_PDF			:= www/current-ehc-doc.pdf

AGC					:= uuagc
#AGC					:= /Volumes/Tmp/Tmp/uuag/a.out
GHC					:= ghc
SUBSTEHC			:= bin/substehc.pl
SUBSTSH				:= bin/substsh.pl

SUBST_BAR_IN_TT		:= sed -e '/begin{TT}/,/end{TT}/s/|/||/g'
SUBST_LINE_CMT		:= sed -e 's/{-\# LINE[^\#]*\#-}//' -e '/{-\#  \#-}/d'

# AGC(opts, file)
AGCC				= cd `dirname $2` ; $(AGC) $1 `basename $2`

AFP_FMT_OTHER		:= lag2TeX.fmt pretty.fmt parsing.fmt
AFP_RULES			:= rules.rul
AFP_RULES_TEX		:= $(AFP_RULES:.rul=.tex)
AFP_PGF_TEX			:= afp-pgf.tex

ALL_AFP_SRC			:= $(AFP_LHS) $(AFP_RULES)

EHC_LAG_FOR_HS_TY	:= $(addsuffix .lag,EHTyQuantify EHTyPretty EHTyInstantiate )
EHC_LAG_FOR_HS_CODE	:= $(addsuffix .lag,EHCodeJava EHCodePretty)
EHC_LAG_FOR_HS		:= $(addsuffix .lag,EHMainAG EHTy EHCode EHError EHErrorPretty) $(EHC_LAG_FOR_HS_TY) $(EHC_LAG_FOR_HS_CODE)

DPDS_MAIN			:= EHMainAG.ag EHInfer.ag EHInferExpr.ag \
						EHInferPatExpr.ag EHInferTyExpr.ag EHInferKiExpr.ag EHInferData.ag \
						EHInferCaseExpr.ag EHPretty.ag EHPrettyAST.ag EHAbsSyn.ag \
						EHUniq.ag EHExtraChecks.ag EHGatherError.ag \
						EHGenCode.ag \
						EHResolvePred.ag EHInferClass.ag
DPDS_TY				:= EHTy.ag EHTyAbsSyn.ag
DPDS_TY_PRETTY		:= EHTyPretty.ag EHTyCommonAG.ag EHTyAbsSyn.ag
DPDS_TY_QUANT		:= EHTyQuantify.ag EHTyCommonAG.ag EHTyAbsSyn.ag
DPDS_TY_INST		:= EHTyInstantiate.ag EHTyCommonAG.ag EHTyAbsSyn.ag
DPDS_ERR			:= EHError.ag EHErrorAbsSyn.ag
DPDS_ERR_PRETTY		:= EHErrorPretty.ag EHErrorAbsSyn.ag
DPDS_CODE			:= EHCode.ag EHCodeAbsSyn.ag
DPDS_CODE_JAVA		:= EHCodeJava.ag EHCodeAbsSyn.ag
DPDS_CODE_PRETTY	:= EHCodePretty.ag EHCodeAbsSyn.ag
DPDS_ALL			:= $(sort $(DPDS_MAIN) $(DPDS_CODE) $(DPDS_CODE_JAVA) $(DPDS_CODE_PRETTY) $(DPDS_TY) $(DPDS_TY_PRETTY) $(DPDS_TY_QUANT) $(DPDS_MAIN) $(DPDS_TY_INST) $(DPDS_ERR) $(DPDS_ERR_PRETTY))
DPDS_ALL_MIN_TARG	:= $(filter-out $(EHC_LAG_FOR_HS:.lag=.ag),$(DPDS_ALL))

EHC					:= ehc
EHC_MAIN			:= EHC
EHC_LAG_FOR_AG		:= $(DPDS_ALL_MIN_TARG:.ag=.lag)
EHC_LAG				:= $(EHC_LAG_FOR_AG) $(EHC_LAG_FOR_HS)
EHC_LHS_FOR_HS		:= $(addsuffix .lhs,$(EHC_MAIN) EHCommon EHCnstr EHTyFitsIn EHGam EHPred EHParser FPath EHScanner EHScannerMachine)
EHC_LHS				:= $(EHC_LHS_FOR_HS)
EHC_HS				:= $(EHC_LAG_FOR_HS:.lag=.hs) $(EHC_LHS_FOR_HS:.lhs=.hs)

AFP_DERIV			:= $(addprefix $(AFP),.toc .bbl .blg .aux .tex .log .ind .idx) $(AFP_STY)

SHUFFLE				:= bin/shuffle
SHUFFLE_DIR			:= shuffle
SHUFFLE_MAIN		:= Shuffle
SHUFFLE_AG			:= $(SHUFFLE_MAIN).ag
SHUFFLE_HS			:= $(SHUFFLE_AG:.ag=.hs)
SHUFFLE_DERIV		:= $(SHUFFLE_DIR)/$(SHUFFLE_HS)
SHUFFLE_DOC_PDF		:= $(SHUFFLE_DIR)/ShuffleDoc.pdf

SHUFFLE_SRC			:= $(SHUFFLE_DIR)/$(SHUFFLE_AG)

SHUFFLE_ORDER		:= 1 < 2 < 3 < 4 < 5 < 6 < 7 < 8 < 9, 6 < 6_1


RULER				:= bin/ruler
RULER_DIR			:= ruler
RULER_MAIN			:= Ruler
RULER_AG			:= $(RULER_MAIN).ag
RULER_HS			:= $(RULER_AG:.ag=.hs)
RULER_DERIV			:= $(RULER_DIR)/$(RULER_HS)
RULER_DOC_PDF		:= $(RULER_DIR)/RulerDoc.pdf

RULER_SRC			:= $(RULER_DIR)/$(RULER_AG)


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
	@echo "make <n>/ehc     : make compiler version <n>" ; \
	echo  "make ehcs        : make all compiler versions (where <n> in {$(VERSIONS)})" ; \
	echo  "make afp         : make afp.pdf, by running latex once" ; \
	echo  "make afp-full    : make afp.pdf, with bib/index" ; \
	echo  "make all         : make all of the above" ; \
	echo ; \
	echo  "make afp-slides  : make slides afp-slides.pdf" ; \
	echo  "make afp-llncs   : make LLNCS variant of afp.pdf" ; \
	echo  "make afp-tr      : make UU techreport variant of afp.pdf" ; \
	echo  "make test-regress: run regression test" ; \
	echo  "make test-expect : make expected output (for later comparison with test-regress)" ; \
	echo  "make dist        : make distribution (of ehc src versions) in $(DIST_PREFIX)<today>-ehc.zip" ; \
	echo  "make www         : make 'current' www dist (based on dist)" ; \
	echo  "make www-sync    : sync www dist (proper ssh access required)"

all: afp-full ehcs $(SHUFFLE_DOC_PDF)
	$(MAKE) initial-test-expect

#%.lag:%.cag
#	$(SHUFFLE) --ag $< > $@

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
#include mk/ehfiles.mk

### Versioned ehc's
VERSIONS			:= $(sort 1 2 3 4 5 6 7 8)
VERSION_LAST		:= $(word $(words $(VERSIONS)), $(VERSIONS))
VERSION_FIRST		:= $(word 1, $(VERSIONS))

EHC_CAG				:= $(EHC_LAG:.lag=.cag)
EHC_CHS				:= $(EHC_LHS:.lhs=.chs)

# SHUFFLE_LHS(src file, dst file, how, lhs2tex, version)
SHUFFLE_LHS		= \
	dir=`dirname $2` ; \
	mkdir -p $$dir ; \
	$(SHUFFLE) --gen=$$dir $3 $1 --order="$(SHUFFLE_ORDER)" | $4 > $2

# SHUFFLE_LHS_AG(src file, dst file, version)
SHUFFLE_LHS_AG		= \
	$(call SHUFFLE_LHS,$1,$2,--ag,$(LHS2TEX) --newcode,$3)

# SHUFFLE_LHS_HS(src file, dst file, version)
SHUFFLE_LHS_HS		= \
	$(call SHUFFLE_LHS,$1,$2,--hs,$(LHS2TEX) --newcode | $(SUBST_LINE_CMT),$3)

# SHUFFLE_LHS_TEX(src file, dst file, version)
SHUFFLE_LHS_TEX		= \
	$(call SHUFFLE_LHS,$1,$2,--latex --xref-except=shuffleXRefExcept,$(LHS2TEX) $(LHS2TEX_POLY_MODE),$3)
#	$(call SHUFFLE_LHS,$1,$2,--latex --index --xref-except=shuffleXRefExcept,$(LHS2TEX) --poly,$3)

# RULER_LHS(src file, dst file, lhs2tex)
RULER_LHS		= \
	$(RULER) --latex $1 | $3 > $2

# RULER_LHS_TEX(src file, dst file)
RULER_LHS_TEX		= \
	$(call RULER_LHS,$1,$2,$(LHS2TEX) --poly)


### Version 1
V					:= 1
VF					:= $(V)
include mk/ehfiles.mk
EHC_V1				:= $(addprefix $(VF)/,$(EHC))
### End of Version 1


### Version 2
V					:= 2
VF					:= $(V)
include mk/ehfiles.mk
EHC_V2				:= $(addprefix $(VF)/,$(EHC))
### End of Version 2


### Version 3
V					:= 3
VF					:= $(V)
include mk/ehfiles.mk
EHC_V3				:= $(addprefix $(VF)/,$(EHC))
### End of Version 3


### Version 4
V					:= 4
VF					:= $(V)
include mk/ehfiles.mk
EHC_V4				:= $(addprefix $(VF)/,$(EHC))
### End of Version 4


### Version 5
V					:= 5
VF					:= $(V)
include mk/ehfiles.mk
EHC_V5				:= $(addprefix $(VF)/,$(EHC))
### End of Version 5


### Version 6
V					:= 6
VF					:= $(V)
include mk/ehfiles.mk
EHC_V6				:= $(addprefix $(VF)/,$(EHC))
### End of Version 6


### Version 6:1
V					:= 6_1
VF					:= 6_1
include mk/ehfiles.mk
EHC_V6_1			:= $(addprefix $(VF)/,$(EHC))
### End of Version 6


### Version 7
V					:= 7
VF					:= $(V)
include mk/ehfiles.mk
EHC_V7				:= $(addprefix $(VF)/,$(EHC))
### End of Version 7


### Version 8
V					:= 8
VF					:= $(V)
include mk/ehfiles.mk
EHC_V8				:= $(addprefix $(VF)/,$(EHC))
### End of Version 8


### Version 9
V					:= 9
VF					:= $(V)
include mk/ehfiles.mk
EHC_V9				:= $(addprefix $(VF)/,$(EHC))
### End of Version 9


### TeX production upto a version
EHC_VLAST_AG_TEX	:= $(addprefix $(AFP_TMPDIR),$(EHC_CAG:.cag=.tex))
EHC_VLAST_HS_TEX	:= $(addprefix $(AFP_TMPDIR),$(EHC_CHS:.chs=.tex))
EHC_VLAST_TEX		:= $(EHC_VLAST_AG_TEX) $(EHC_VLAST_HS_TEX)

$(EHC_VLAST_HS_TEX): $(AFP_TMPDIR)%.tex: %.chs $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all)

$(EHC_VLAST_AG_TEX): $(AFP_TMPDIR)%.tex: %.cag $(SHUFFLE) Makefile
	$(call SHUFFLE_LHS_TEX,$<,$@,all)
### End of TeX


$(AFP_PDF): $(AFP_TEX) $(AFP_STY) $(EHC_VLAST_TEX) $(AFP_RULES_TEX) $(AFP_PGF_TEX)
	$(AFP_LATEX) $(AFP_TEX)

$(AFP_TEX): $(AFP_LHS) ehcs
	$(call LHS2TEX_POLY,$<,$@)

$(AFP_STY): afp.lsty Makefile
	$(call LHS2TEX_POLY_3,$<,$@)

$(AFP_RULES_TEX): %.tex: %.rul $(RULER) $(AFP_FMT)
	$(call RULER_LHS_TEX,$<,$@)

afp: $(AFP_PDF)

afp-full: afp
	bibtex $(AFP)
	$(AFP_LATEX) $(AFP_TEX)
	rm -f $(AFP).ind
	makeindex $(AFP)
	$(AFP_LATEX) $(AFP_TEX)

afp-tr:
	$(MAKE) AFP=$@ LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=asArticle --set=forAfpTRUU1 --set=omitEH5Onwards --set=omitAppendix --set=omitTBD --set=omitLitDiscuss" afp-full

afp-llncs:
	$(MAKE) AFP=$@ LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=forAfpLLNCS --set=omitEH5Onwards --set=omitAppendix --set=omitTBD --set=omitLitDiscuss" afp

afp-all:
	$(MAKE) AFP=$@ LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=asArticle --set=refToPDF --set=inclFuture --set=inclOmitted" afp

afp-work:
	$(MAKE) AFP=$@ LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=asArticle --set=refToPDF --set=inclFuture --set=inclOmitted --set=onlyCurrentWork" afp

afp-slides:
	$(MAKE) AFP=$@ LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=asSlides" afp

.PHONY: shuffle ruler ehcs dist www www-sync

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

ehcs: $(EHC_V1) $(EHC_V2) $(EHC_V3) $(EHC_V4) $(EHC_V5) $(EHC_V6) $(EHC_V7) $(EHC_V8)

clean:
	rm -rf $(AFP_DERIV) $(SHUFFLE_DERIV) a.out \
	$(addprefix $(SHUFFLE_DIR)/,*.o *.hi *.pdf) $(SHUFFLE) \
	$(addprefix $(RULER_DIR)/,*.o *.hi *.pdf) $(RULER) \
	$(AFP_PDF) \
	*.o *.hi $(VERSIONS) \
	test/*.reg* test/*.class *.class test/*.java test/*.code \
	$(DIST_PREFIX)*.zip $(DIST_PREFIX)*.tgz \
	20??????-ehc \
	$(WWW_SRC_ZIP) $(WWW_DOC_PDF) \
	tmp-* \
	*.log *.lof *.ilg *.out *.toc *.ind *.idx *.aux

clean-test:
	rm -rf test/*.reg* test/*.exp*

edit:
	bbedit $(EHC_CAG) $(EHC_CHS) $(ALL_AFP_SRC) $(SHUFFLE_SRC) Makefile $(TMPL_TEST)

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
	  for ((i = 1 ; i <= $${v} ; i++)) ; do ehs="$$ehs `ls $${i}-*.eh`" ; done ; \
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
	  echo -n "$(EHC): $(EHC_MAIN).hs"  ; \
	  $(call FILTER_EXISTS_HS_OR_AG,$(EHC_HS)) ; \
	  echo ; \
	  echo "	$(GHC) -package uust -package data --make -o $(EHC) $$<" ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_MAIN),-dcfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_TY),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_TY_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_TY_QUANT),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_TY_INST),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_ERR),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_ERR_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_CODE),-dr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_CODE_PRETTY),-cfspr) ; \
	  $(call MK_EHC_MKF_FOR,$(DPDS_CODE_JAVA),-cfspr) ; \
	) > Makefile

dist: $(DIST_ZIP)

$(DIST_ZIP): $(addprefix $(VERSION_LAST)/,$(DPDS_ALL_MIN_TARG)) Makefile test-lists $(TMPL_TEST)
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
	      -e "s/\`cat \$$\$$v.lst\`/*.eh/g" \
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

wc:
	grep frametitle $(AFP_LHS) | wc

www: $(WWW_SRC_ZIP) $(WWW_DOC_PDF)

www/DoneSyncStamp: $(WWW_SRC_ZIP) $(WWW_DOC_PDF)
	(date; echo -n ", " ; svn up) > www/DoneSyncStamp ; \
	rsync --progress -azv -e ssh www/* atze@modena.cs.uu.nl:/users/www/groups/ST/Projects/ehc

www-sync: www/DoneSyncStamp

$(WWW_SRC_ZIP): $(DIST_ZIP)
	cp $< $@

$(WWW_DOC_PDF): $(AFP_PDF)
	cp $< $@

