TOP_PREFIX			:=

default: explanation

# files, dependencies, rules
# do not change the order of these includes
-include latex/files.mk
-include lhs2TeX/files.mk

include mk/shared.mk

include shuffle/files.mk
include ruler2/files.mk
include grin/files.mk
include ehc/files.mk
include grini/files.mk
include agprimer/files.mk
-include figs/files.mk
-include text/files.mk
include test/files.mk

-include mk/dist.mk

# distributed/published stuff for WWW
WWW_SRC_TGZ					:= www/current-ehc-src.tgz
WWW_DOC_PDF					:= www/current-ehc-doc.pdf

# pictures in pgf format
AFP_PGF_TEX			:= afp-pgf.tex

# pictures in xfig format
AFP_XFIG_TEX		:= $(patsubst %,figs/%.latex,ruler-overview)


#SHUFFLE				:= $(SHUFFLE_BLD_EXEC)
SHUFFLE_DIR			:= shuffle
#SHUFFLE_MAIN		:= Shuffle
#SHUFFLE_AG			:= $(SHUFFLE_MAIN).ag
#SHUFFLE_HS			:= $(SHUFFLE_AG:.ag=.hs)
#SHUFFLE_DERIV		:= $(SHUFFLE_DIR)/$(SHUFFLE_HS)
SHUFFLE_DOC_PDF		:= $(SHUFFLE_DIR)/ShuffleDoc.pdf

#SHUFFLE_SRC			:= $(SHUFFLE_DIR)/$(SHUFFLE_AG)

# Ruler, will be obsolete soon
RULER				:= bin/ruler
RULER_DIR			:= ruler
RULER_MAIN			:= Ruler
RULER_AG			:= $(RULER_MAIN).ag
RULER_HS			:= $(RULER_AG:.ag=.hs)
RULER_DERIV			:= $(RULER_DIR)/$(RULER_HS)
RULER_DOC_PDF		:= $(RULER_DIR)/RulerDoc.pdf

RULER_SRC			:= $(RULER_DIR)/$(RULER_AG)

# Ruler2
RULER2_DIR			:= ruler2
RULER2_DOC_PDF		:= $(RULER2_DIR)/RulerDoc.pdf

# Brew, obsolete
BREW				:= bin/brew
BREW_DIR			:= brew
BREW_MAIN			:= Brew
BREW_AG				:= $(BREW_MAIN).ag
BREW_HS				:= $(BREW_AG:.ag=.hs)
BREW_DERIV			:= $(BREW_DIR)/$(BREW_HS)
BREW_DOC_PDF		:= $(BREW_DIR)/BrewDoc.pdf

BREW_SRC			:= $(BREW_DIR)/$(BREW_AG)

# LHS2TEX_POLY_2(src file, dst file)
LHS2TEX_POLY_2			= \
	$(SUBST_SH) < $(1) | $(LHS2TEX_EXEC_WT_OPTS) --poly > $(2)

# LHS2TEX_POLY_3(src file, dst file)
LHS2TEX_POLY_3			= \
	$(LHS2TEX_EXEC_WT_OPTS) $(LHS2TEX_POLY_MODE) $(1) > $(2)

explanation:
	@echo "make bin/<n>/ehc     : make compiler version <n> (where <n> in {$(EHC_PUB_VARIANTS)})" ; \
	echo  "make bin/<n>/grini   : make grin interpreter version <n> (where <n> in {$(GRIN_PUB_VARIANTS)})" ; \
	echo  "make bin/ruler2      : make ruler tool" ; \
	echo  "make bin/shuffle     : make shuffle tool" ; \
	echo  "make ehcs            : make all compiler ($(EHC_EXEC_NAME)) versions" ; \
	echo  "make grinis          : make all grin interpreter ($(GRINI_EXEC_NAME)) versions" ; \
	echo  "make test-regress    : run regression test" ; \
	echo  "make test-expect     : make expected output (for later comparison with test-regress)" ; \

all: afp-full ehcs doc grinis
	$(MAKE) initial-test-expect

doc: $(SHUFFLE_DOC_PDF)

%.latex:%.fig
	fig2dev -L latex $< > $@

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

hw05-impred-tst:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyImpred --set=hw05 --set=omitTBD --set=omitLitDiscuss" afp

hw05-impred-final:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyImpred --set=hw05 --set=omitTBD --set=omitLitDiscuss" afp-bib

popl06-ruler-tst:
	$(MAKE) AFP=$@ \
	AFP_TEX_DPDS="$(AFP_XFIG_TEX) $(RULER2_DEMO_DRV_RL2) $(RULER2_DEMO_DRV_AG) $(RULER2_DEMO_DRV_CTEX) $(RULER2_DEMO_DRV_RTEX) $(RULER2_DEMO_DRV_ATEX) $(RULER2_DEMO_DRV_AG_MAIN_TEX) $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_HS_UTILS) $(RULER2_DEMO_DRV_HS_UTILS_TEX)" \
	LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyRuler --set=popl06 --set=omitTBD --set=omitLitDiscuss" afp

popl06-ruler:
	$(MAKE) AFP=$@ \
	AFP_TEX_DPDS="$(AFP_XFIG_TEX) $(RULER2_DEMO_DRV_RL2) $(RULER2_DEMO_DRV_AG) $(RULER2_DEMO_DRV_CTEX) $(RULER2_DEMO_DRV_RTEX) $(RULER2_DEMO_DRV_ATEX) $(RULER2_DEMO_DRV_AG_MAIN_TEX) $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_HS_UTILS) $(RULER2_DEMO_DRV_HS_UTILS_TEX)" \
	LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyRuler --set=popl06 --set=omitTBD --set=omitLitDiscuss" afp-bib

popl06-explimpl-tst:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=newDocClassHeader --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=popl06 --set=withChangeBar --set=omitTBD --set=omitLitDiscuss" afp

popl06-explimpl-final:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=newDocClassHeader --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=popl06 --set=omitTBD --set=omitLitDiscuss" afp-bib

popl06-explimpl:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=newDocClassHeader --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=popl06 --set=withChangeBar --set=omitTBD --set=omitLitDiscuss" afp-bib

icfp05-explimpl-tst:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=icfp05 --set=omitTBD --set=omitLitDiscuss" afp

icfp05-explimpl:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=icfp05 --set=asDraft --set=omitTBD --set=omitLitDiscuss" afp-bib

icfp05-explimpl-final:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --unset=yesBeamer --unset=useHyperref --set=acm --set=storyExplImpl --set=icfp05 --set=omitTBD --set=omitLitDiscuss" afp-bib

icfp05-explimpl-slides:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=yesBeamer --set=asSlides --set=storyExplImpl --set=icfp05 --set=omitTBD --set=omitLitDiscuss" afp

icfp05-slides-dist-tex:
	$(MAKE) AFP=icfp05-slides-dist AFP_TEX_DPDS= LHS2TEX_OPTS="--set=yesBeamer --set=asSlides --set=storyExplImpl --set=icfp05 --set=omitTBD --set=omitLitDiscuss --set=dist" icfp05-slides-dist.tex

esop05:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=llncs --set=storyExplImpl --set=omitTBD --set=omitLitDiscuss" afp

esop05-tr:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=truu --set=storyExplImpl --set=omitTBD --set=omitLitDiscuss" afp-bib

eh-work:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= AFP_RULES2_RULER_OPTS="--markchanges='*'" LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=onlyCurrentWork --unset=asArticle --set=refToPDF --set=inclOmitted" afp

phd:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_OPTS="$(LHS2TEX_OPTS_BASE) --set=newDocClassHeader --set=storyPHD --unset=asArticle --set=refToPDF --set=inclOmitted" afp

afp-slides:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=storyPHD --set=asSlides --set=omitTBD --set=omitLitDiscuss" afp

eh-intro:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=storyEHIntro --set=asSlides --set=omitTBD --set=omitLitDiscuss" afp

eh-introETAPSLinks:
	$(MAKE) AFP=$@ AFP_TEX_DPDS= LHS2TEX_POLY_MODE=--poly LHS2TEX_OPTS="$(LHS2TEX_OPTS) --set=storyEHIntro --set=storyVariantETAPSLinks --set=asSlides --set=omitTBD --set=omitLitDiscuss" afp

.PHONY: shuffle ruler ruler2 brew ehcs dist www www-sync gri grinis agprimer $(DIST_AFP04) $(DIST_ICFP05_SLIDES)

$(SHUFFLE_DIR)/ShuffleDoc.tex: $(SHUFFLE)

$(SHUFFLE_DOC_PDF): $(SHUFFLE_DIR)/ShuffleDoc.tex
	cd `dirname $<` ; pdflatex `basename $<`

ruler: $(RULER)

$(RULER): $(RULER_DIR)/$(RULER_AG) $(wildcard lib/*.hs)
	cd $(RULER_DIR) ; \
	$(AGC) -csdfr --module=Main `basename $<` ; \
	$(GHC) --make $(GHC_OPTS) -i../lib $(RULER_HS) -o ../$@ ; \
	strip ../$@

$(RULER_DOC_PDF): $(RULER_DIR)/RulerDoc.tex $(RULER)
	cd `dirname $<` ; pdflatex `basename $<`

$(RULER2_DOC_PDF): $(RULER2_DIR)/RulerDoc.tex $(RULER2)
	cd `dirname $<` ; pdflatex `basename $<`

brew: $(BREW)

$(BREW): $(BREW_DIR)/$(BREW_AG) $(wildcard lib/*.hs)
	cd $(BREW_DIR) ; \
	$(AGC) -csdfr --module=Main `basename $<` ; \
	$(GHC) --make $(GHC_OPTS) -i../lib $(BREW_HS) -o ../$@ ; \
	strip ../$@

$(BREW_DOC_PDF): $(BREW_DIR)/RulerDoc.tex $(BREW)
	cd `dirname $<` ; pdflatex `basename $<`

ehcs: $(EHC_ALL_PUB_EXECS)

grinis: $(GRINI_ALL_PUB_EXECS)

redit:
	bbedit \
	$(SHUFFLE_ALL_SRC) $(RULER2_ALL_SRC) \
	$(EHC_RULES_3_SRC_RL2) \
	Makefile

tedit:
	bbedit \
	$(TEXT_EDIT_SRC) \
	$(EHC_RULES_3_SRC_RL2) \
	Makefile

eedit:
	bbedit \
	$(EHC_ALL_SRC) $(GRIN_ALL_SRC) $(GRINI_ALL_SRC) \
	Makefile

edit: redit eedit tedit

A_EH_TEST			:= $(word 1,$(wildcard test/*.eh))
A_EH_TEST_EXP		:= $(addsuffix .exp$(VERSION_FIRST),$(A_EH_TEST))

tst:
	echo $(VERSION_LAST)
	echo $(A_EH_TEST_EXP)

initial-test-expect: $(A_EH_TEST_EXP)

$(A_EH_TEST_EXP): $(A_EH_TEST)
	$(MAKE) test-expect

WWW_EXAMPLES_TMPL			:=	www/ehc-examples-templ.html
WWW_EXAMPLES_HTML			:=	www/ehc-examples.html

www-ex: $(WWW_EXAMPLES_HTML)

www: $(WWW_SRC_TGZ) www-ex # $(WWW_DOC_PDF)

www/DoneSyncStamp: www-ex
	(date; echo -n ", " ; svn up) > www/DoneSyncStamp ; \
	rsync --progress -azv -e ssh www/* atze@modena.cs.uu.nl:/users/www/groups/ST/Projects/ehc

www-sync: www/DoneSyncStamp

$(WWW_EXAMPLES_HTML): $(WWW_EXAMPLES_TMPL)
	$(call PERL_SUBST_EHC,$(WWW_EXAMPLES_TMPL),$(WWW_EXAMPLES_HTML))

$(WWW_SRC_TGZ): $(DIST_TGZ)
	cp $^ $@

$(WWW_DOC_PDF): doc/phd.pdf
	cp $< $@

