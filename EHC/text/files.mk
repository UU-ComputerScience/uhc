# location of text src
TEXT_SRC_PREFIX				:= $(TOP_PREFIX)text/

# this file
TEXT_MKF					:= $(TEXT_SRC_PREFIX)files.mk

# main
TEXT_MAIN					:= main

# subtext
TEXT_SUBS					:= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred \
								AppxNotation
TEXT_SUBS_ASIS				:= afp-pgf

# variant, to be configured on top level
TEXT_VARIANT				:= $(TEXT_MAIN)
TEXT_SHUFFLE_VARIANT		:= 1
TEXT_TMP_PREFIX				:= $(BLD_PREFIX)
TEXT_TMP_VARIANT_PREFIX		:= $(TEXT_TMP_PREFIX)$(TEXT_VARIANT)/

# all variants
TEXT_PUB_VARIANTS			:= popl06-ruler popl06-explimpl
TEXT_VARIANTS				:= $(TEXT_PUB_VARIANTS) popl06-ruler-tst phd phd-tst scratch

# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism
# 2	: phd
# 3	: popl06-ruler
# 4	: popl06-explimpl
# 5	: impred
# 6	: afp (will be obsolete)
# 7	: scratch
# 8	: slides
# 9	: slides explimpl
# 10: future
TEXT_SHUFFLE_ORDER			:= 1 < 2, 1 < 3, 1 < 4, 1 < 5, 1 < 6, 1 < 7, 1 < 8, 1 < 9, 1 < 10

# configuration of lhs2tex, to be done on top level
LHS2TEX_OPTS_TEXT_CONFIG	:= --unset=optExpandPrevRef
LHS2TEX_OPTS_VARIANT_CONFIG	:= 

# end products, binary, executable, etc
TEXT_BLD_PDF				:= $(DOC_PREFIX)$(TEXT_VARIANT).pdf
TEXT_ALL_PUB_PDFS			:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_PUB_VARIANTS))

# files, source + derived
TEXT_MAIN_SRC_CLTEX			:= $(TEXT_SRC_PREFIX)$(TEXT_MAIN).cltex
TEXT_MAIN_DRV_LTEX			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).ltex
TEXT_MAIN_DRV_TEX			:= $(TEXT_MAIN_DRV_LTEX:.ltex=.tex)
TEXT_MAIN_SRC_LSTY			:= $(TEXT_SRC_PREFIX)$(TEXT_MAIN).lsty
TEXT_MAIN_DRV_STY			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).sty

TEXT_SUBS_SRC_CLTEX			:= $(patsubst %,$(TEXT_SRC_PREFIX)%.cltex,$(TEXT_SUBS))
TEXT_SUBS_ASIS_SRC			:= $(patsubst %,$(TEXT_SRC_PREFIX)%.tex,$(TEXT_SUBS_ASIS))

EHC_CAG_DRV_LTEX			:= $(patsubst $(EHC_SRC_PREFIX)%.cag,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_AG_ALL_MAIN_SRC_CAG) $(EHC_AG_ALL_DPDS_SRC_CAG))
EHC_CAG_DRV_TEX				:= $(EHC_CAG_DRV_LTEX:.ltex=.tex)

EHC_CHS_DRV_LTEX			:= $(patsubst $(EHC_SRC_PREFIX)%.chs,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_HS_ALL_SRC_CHS))
EHC_CHS_DRV_TEX				:= $(EHC_CHS_DRV_LTEX:.ltex=.tex)

AGPRIMER_CAG_DRV_LTEX		:= $(patsubst $(AGPRIMER_SRC_PREFIX)%.cag,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(AGPRIMER_CAG_SRC_CAG))
AGPRIMER_CAG_DRV_TEX		:= $(AGPRIMER_CAG_DRV_LTEX:.ltex=.tex)

AGPRIMER_CHS_DRV_LTEX		:= $(patsubst $(AGPRIMER_SRC_PREFIX)%.chs,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(AGPRIMER_CHS_SRC_CHS))
AGPRIMER_CHS_DRV_TEX		:= $(AGPRIMER_CHS_DRV_LTEX:.ltex=.tex)

RULER_12_DRV_LTEX			:= $(patsubst $(EHC_SRC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_1_SRC_RUL) $(EHC_RULES_2_SRC_RUL))
RULER_12_DRV_TEX			:= $(RULER_12_DRV_LTEX:.ltex=.tex)

RULER_3_DRV_LTEX			:= $(patsubst $(EHC_SRC_PREFIX)%.rl2,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_3_SRC_RL2))
RULER_3_DRV_TEX				:= $(RULER_3_DRV_LTEX:.ltex=.tex)

TEXT_RULES_3_DRV_CAG		:= $(TEXT_TMP_VARIANT_PREFIX)$(EHC_RULER_RULES).cag
TEXT_RULES_3_DRV_LTEX		:= $(TEXT_RULES_3_DRV_CAG:.cag=.ltex)
TEXT_RULES_3_DRV_TEX		:= $(TEXT_RULES_3_DRV_LTEX:.ltex=.tex)

TEXT_HIDE_DRV_TXT			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN)-hide.hltex
TEXT_HIDE_DRV_LTEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.ltex)
TEXT_HIDE_DRV_TEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.tex)

TEXT_SUBS_DRV_TEX			:= $(EHC_CAG_DRV_TEX) $(EHC_CHS_DRV_TEX) $(AGPRIMER_CAG_DRV_TEX) $(AGPRIMER_CHS_DRV_TEX) $(RULER_12_DRV_TEX) $(RULER_3_DRV_TEX) $(TEXT_RULES_3_DRV_TEX)
TEXT_SUBS_ASIS_DRV			:= $(patsubst $(TEXT_SRC_PREFIX)%.tex,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(TEXT_SUBS_ASIS_SRC))

TEXT_RULER2_DEMO_TEX		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_ALL_DRV_TEX))
TEXT_RULER2_DEMO_STUFF		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_DRV_RL2) $(RULER2_DEMO_DRV_AG) $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_HS_UTILS))

TEXT_INCL_LIST_TEX			:= $(TEXT_TMP_VARIANT_PREFIX)InclList.tex

TEXT_BIB_SRC				:= $(TEXT_SRC_PREFIX)LitAdm.bib
TEXT_BIB_DRV				:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).bib

FIGS_XFIG_DRV_TEX			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(FIGS_XFIG_SRC_FIG))
FIGS_XFIG_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_XFIG_SRC_FIG_NOPDF))
FIGS_ASIS_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.pdf,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_ASIS_SRC_PDF))

TEXT_ALL_MK_FILES			:= $(AGPRIMER_MKF) $(EHC_MKF) $(RULER2_MKF) $(TEXT_MKF)

# all src
TEXT_EDIT_SRC				:= $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(TEXT_MAIN_SRC_LSTY)
TEXT_ALL_SRC				:= $(TEXT_EDIT_SRC) $(TEXT_SUBS_ASIS_SRC) $(TEXT_BIB_SRC)

# all deriveds (as counting for make dependencies)
TEXT_ALL_DPD				:= $(TEXT_MAIN_DRV_TEX) $(TEXT_SUBS_DRV_TEX) $(TEXT_MAIN_DRV_STY) $(TEXT_INCL_LIST_TEX) $(TEXT_RULER2_DEMO_TEX) \
								$(TEXT_SUBS_ASIS_DRV) $(FIGS_XFIG_DRV_TEX) $(FIGS_XFIG_DRV_PDF) $(TEXT_RULER2_DEMO_STUFF) $(FIGS_ASIS_DRV_PDF) $(TEXT_HIDE_DRV_TEX) 

# distribution
TEXT_DIST_DOC_FILES			:= $(TEXT_ALL_PUB_PDFS)
TEXT_DIST_FILES				:= $(TEXT_ALL_SRC) $(TEXT_MKF)

# variant dispatch rules
$(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_VARIANTS)): $(DOC_PREFIX)%.pdf: $(TEXT_ALL_SRC) $(EHC_ALL_SRC) $(RULER2_DEMO_ALL_DRV_TEX) $(TEXT_ALL_MK_FILES) $(FIGS_ALL_SRC)
	$(MAKE) TEXT_VARIANT=$(*F) text-variant-$(*F)

# rules
text-variant-phd:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=phd --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=phd --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss --set=asDraft" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-tst

text-variant-popl06-ruler-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl06 --set=acm --set=storyRuler --set=asArticle --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-tst

text-variant-popl06-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl06 --set=acm --set=storyRuler --set=asArticle --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-popl06-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl06 --set=acm --set=storyExplImpl --set=asArticle --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-scratch:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=phd --set=storyPHD --unset=asArticle --set=useHyperref --set=refToPDF --set=newDocClassHeader" \
	  TEXT_SHUFFLE_VARIANT=7 \
	  text-variant-dflt-tst

text-variant-dflt-tst: $(TEXT_ALL_DPD)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; $(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-bib: $(TEXT_ALL_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-bib-inx: $(TEXT_ALL_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	rm -f $(TEXT_MAIN).ind ; \
	$(MAKEINDEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

$(TEXT_MAIN_DRV_LTEX) : $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(SHUFFLE) $(TEXT_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --hidedest=appx=$(TEXT_HIDE_DRV_TXT) --order="$(TEXT_SHUFFLE_ORDER)" $< $(TEXT_SUBS_SRC_CLTEX) > $@

$(TEXT_HIDE_DRV_TXT): $(TEXT_MAIN_DRV_LTEX)
	touch $@

$(TEXT_MAIN_DRV_TEX) : %.tex : %.ltex
	$(SUBST_EHC) $< \
	  | $(SUBST_BAR_IN_TT) \
	  | $(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) \
	  > $@

$(TEXT_SUBS_DRV_TEX) $(TEXT_HIDE_DRV_TEX) : %.tex : %.ltex
	$(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_MAIN_DRV_STY) : $(TEXT_MAIN_SRC_LSTY) $(TEXT_MKF)
	$(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

$(EHC_CHS_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_CAG_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(AGPRIMER_CHS_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(AGPRIMER_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(AGPRIMER_SHUFFLE_ORDER)" $< > $@

$(AGPRIMER_CAG_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(AGPRIMER_SRC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(AGPRIMER_SHUFFLE_ORDER)" $< > $@

$(TEXT_RULES_3_DRV_LTEX) : $(TEXT_RULES_3_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(basename $(@F)) $< > $@

$(RULER_12_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rul
	mkdir -p $(@D)
	$(RULER) --latex --base=$(*F) $< > $@

$(RULER_3_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rl2 $(RULER2)
	mkdir -p $(@D)
	$(RULER2) --lhs2tex --markchanges="E - *" --base=$(*F) $< > $@

$(TEXT_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2)
	mkdir -p $(@D)
	$(RULER2) --ag --wrapshuffle --selrule="((1=K),(2=C),(3=HM),(4=I1),(4_2=I2),(9=P)).(expr.base tyexpr.base patexpr.base decl.base).(*)" --base=$(*F) $< > $@

$(TEXT_RULER2_DEMO_TEX) $(TEXT_RULER2_DEMO_STUFF): $(TEXT_TMP_VARIANT_PREFIX)% : $(RULER2_DEMO_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(TEXT_BIB_DRV): $(TEXT_BIB_SRC)
	mkdir -p $(@D)
	cp $< $@

$(TEXT_SUBS_ASIS_DRV): $(TEXT_TMP_VARIANT_PREFIX)% : $(TEXT_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_ASIS_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)% : $(FIGS_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_XFIG_DRV_TEX): $(TEXT_TMP_VARIANT_PREFIX)%.tex : $(FIGS_SRC_PREFIX)%.fig
	mkdir -p $(@D)
	fig2dev -L latex -E 0 $< > $@

$(FIGS_XFIG_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)%.pdf : $(FIGS_SRC_PREFIX)%.fig
	mkdir -p $(@D)
	fig2dev -L pdf -p dummy $< > $@

$(TEXT_INCL_LIST_TEX): $(TEXT_ALL_MK_FILES)
	@(for f in $(sort $(notdir $(TEXT_SUBS_DRV_TEX) $(TEXT_RULER2_DEMO_TEX))) ; \
	  do \
	    echo "\\input" $$f ; \
	  done \
	) > $@

$(TEXT_HIDE_DRV_LTEX): $(TEXT_HIDE_DRV_TXT)
	(echo '%include lhs2TeX.fmt' ; echo '%include afp.fmt' ; cat $< ) > $@

