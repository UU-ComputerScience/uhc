# location of text src
TEXT_SRC_PREFIX				:= $(TOP_PREFIX)text/

# this file
TEXT_MKF					:= $(TEXT_SRC_PREFIX)files.mk

# main
TEXT_MAIN					:= main

# subtext
TEXT_SUBS					:= AGMiniPrimer TopicRuler TopicExplImpl

# variant, to be configured on top level
TEXT_VARIANT				:= $(TEXT_MAIN)
TEXT_SHUFFLE_VARIANT		:= 1
TEXT_TMP_PREFIX				:= $(BLD_PREFIX)
TEXT_TMP_VARIANT_PREFIX		:= $(TEXT_TMP_PREFIX)$(TEXT_VARIANT)/

# all variants
TEXT_PUB_VARIANTS			:= phd popl06-ruler
TEXT_VARIANTS				:= $(TEXT_PUB_VARIANTS) popl06-ruler-tst

# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism
# 2: phd
# 3: popl06-ruler-tst
# 4: explimpl
# 5: impred
# 6: afp (will be obsolete)
TEXT_SHUFFLE_ORDER			:= 1 < 2, 1 < 3, 1 < 4, 1 < 5, 1 < 6

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
TEXT_MAIN_SRC_FMT			:= $(TOP_PREFIX)afp.fmt
TEXT_MAIN_DRV_STY			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).sty

TEXT_SUBS_SRC_CLTEX			:= $(patsubst %,$(TEXT_SRC_PREFIX)%.cltex,$(TEXT_SUBS))

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

TEXT_SUBS_DRV_TEX			:= $(EHC_CAG_DRV_TEX) $(EHC_CHS_DRV_TEX) $(AGPRIMER_CAG_DRV_TEX) $(AGPRIMER_CHS_DRV_TEX) $(RULER_12_DRV_TEX) $(RULER_3_DRV_TEX)
TEXT_RULER2_DEMO_TEX		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_ALL_DRV_TEX))

TEXT_INCL_LIST_TEX			:= $(TEXT_TMP_VARIANT_PREFIX)InclList.tex

TEXT_ALL_MK_FILES			:= $(AGPRIMER_MKF) $(EHC_MKF) $(RULER2_MKF) $(TEXT_MKF)

# all src
TEXT_ALL_SRC				:= $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(TEXT_MAIN_SRC_LSTY) $(TEXT_MAIN_SRC_FMT)

# distribution
TEXT_DIST_DOC_FILES			:= $(TEXT_ALL_PUB_PDFS)
TEXT_DIST_FILES				:= $(TEXT_ALL_SRC) $(TEXT_MKF)

# variant dispatch rules
$(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_VARIANTS)): $(DOC_PREFIX)%.pdf: $(TEXT_ALL_SRC) $(EHC_ALL_SRC) $(RULER2_DEMO_ALL_DRV_TEX) $(TEXT_ALL_MK_FILES)
	$(MAKE) TEXT_VARIANT=$(*F) text-variant-$(*F)

# rules
$(TEXT_MAIN_DRV_LTEX) : $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(SHUFFLE) $(TEXT_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --order="$(TEXT_SHUFFLE_ORDER)" $< $(TEXT_SUBS_SRC_CLTEX) > $@

$(TEXT_MAIN_DRV_TEX) : %.tex : %.ltex
	$(SUBST_EHC) $< \
	  | $(SUBST_BAR_IN_TT) \
	  | $(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) \
	  > $@

$(TEXT_SUBS_DRV_TEX) : %.tex : %.ltex
	$(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_MAIN_DRV_STY) : $(TEXT_MAIN_SRC_LSTY) $(TEXT_MKF)
	$(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

text-variant-phd:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=wide --set=yesBeamer --set=phd --set=storyPHD --unset=asArticle --set=newDocClassHeader --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-tst

text-variant-popl06-ruler-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl06 --set=acm --set=storyRuler --set=asArticle --set=newDocClassHeader --set=omitTBD --set=omitLitDiscuss" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-tst

text-variant-dflt-tst: $(TEXT_MAIN_DRV_TEX) $(TEXT_SUBS_DRV_TEX) $(TEXT_MAIN_DRV_STY) $(TEXT_INCL_LIST_TEX) $(TEXT_RULER2_DEMO_TEX)
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	TEXINPUTS=".:../..:$$TEXINPUTS" pdflatex $(TEXT_MAIN)
	mkdir -p `dirname $(TEXT_BLD_PDF)`
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

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

$(RULER_12_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rul
	mkdir -p $(@D)
	$(RULER) --latex --base=$(*F) $< > $@

$(RULER_3_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rl2
	mkdir -p $(@D)
	$(RULER2) --lhs2tex --dot2dash --base=$(*F) $< > $@

$(TEXT_RULER2_DEMO_TEX) : $(TEXT_TMP_VARIANT_PREFIX)%tex : $(RULER2_DEMO_PREFIX)%tex
	mkdir -p $(@D)
	cp $< $@

$(TEXT_INCL_LIST_TEX): $(TEXT_ALL_MK_FILES)
	@(for f in $(sort $(notdir $(TEXT_SUBS_DRV_TEX) $(TEXT_RULER2_DEMO_TEX))) ; \
	  do \
	    echo "\\input" $$f ; \
	  done \
	) > $@



