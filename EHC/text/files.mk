# location of text src
TEXT_SRC_PREFIX				:= $(TOP_PREFIX)text/

# this file
TEXT_MKF					:= $(TEXT_SRC_PREFIX)files.mk

# main
TEXT_MAIN					:= main

# subtext
TEXT_SUBS					:= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								SharedTypeLang SharedFIOpts \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred SlidesRuler SlidesShuffle SlidesGRIN \
								CodeFragsExplImpl \
								ToolDocShuffle ToolDocRuler \
								AppxNotation FrontMatter OldText
TEXT_SUBS_ASIS				:= afp-pgf

# variant, to be configured on top level
TEXT_VARIANT				:= $(TEXT_MAIN)
TEXT_SHUFFLE_VARIANT		:= 1
TEXT_TMP_PREFIX				:= $(BLD_PREFIX)
TEXT_TMP_VARIANT_PREFIX		:= $(TEXT_TMP_PREFIX)$(TEXT_VARIANT)/

# all variants
TEXT_PUB_VARIANTS			:= phd shuffle-doc ruler-doc
TEXT_PRIV_VARIANTS			:= flops06-ruler-paper flops06-ruler \
								pldi06-explimpl \
								truu-explimpl truu-ruler \
								phd-paper phd-draft phd-tst \
								scratch poster \
								slides-ruler slides-explimpl slides-explimpl-fpnl slides-overview
TEXT_VARIANTS				:= $(TEXT_PUB_VARIANTS) $(TEXT_PRIV_VARIANTS)

# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism
# 1	: base (share)
# 2	: phd
# 3	: flops06-ruler, truu-ruler
# 4	: pldi06-explimpl, truu-explimpl
# 5	: impred
# 6	: afp (will be obsolete)
# 7	: scratch
# 8	: slides afp
# 9	: slides explimpl: base (share)
# 10: future
# 11: shuffle doc
# 12: garbage
# 13: poster
# 14: slides ruler
# 15: slides explimpl, general
# 16: slides explimpl, for fpnl dag
# 17: slides overview
# 18: slides: base (share)
# 19: ruler doc

TEXT_SHUFFLE_ORDER			:= 1 < 2, 1 < 3, 1 < 4, 1 < 5, 1 < 6, 1 < 7, 1 < 8, 18 < 9, 1 < 10, 1 < 11, 1 < 13, 1 < 14, 9 < 15, 9 < 16, 18 < 17, 1 < 18, 1 < 19

# configuration of lhs2tex, to be done on top level
LHS2TEX_OPTS_TEXT_CONFIG	:= 
LHS2TEX_OPTS_VARIANT_CONFIG	:= 

# configuration of ruler, to be done on top level
TEXT_RULER_MARK_CHANGES_CFG	:= --markchanges="E - *"

# end products, binary, executable, etc
TEXT_BLD_PDF				:= $(DOC_PREFIX)$(TEXT_VARIANT).pdf
TEXT_ALL_PUB_PDFS			:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_PUB_VARIANTS))
TEXT_ALL_PDFS				:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_VARIANTS))

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

RULER_3_DRV_LTEX			:= $(patsubst $(EHC_SRC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_3_SRC_RL2))
RULER_3_DRV_TEX				:= $(RULER_3_DRV_LTEX:.ltex=.tex)

RULER2_RULES_DRV_LTEX		:= $(patsubst $(RULER2_SRC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(RULER2_RULES_SRC_RL2))
RULER2_RULES_DRV_TEX		:= $(RULER2_RULES_DRV_LTEX:.ltex=.tex)

TEXT_RULES_3_DRV_CAG		:= $(TEXT_TMP_VARIANT_PREFIX)$(EHC_RULER_RULES).cag
TEXT_RULES_3_DRV_LTEX		:= $(TEXT_RULES_3_DRV_CAG:.cag=.ltex)
TEXT_RULES_3_DRV_TEX		:= $(TEXT_RULES_3_DRV_LTEX:.ltex=.tex)

TEXT_RULES_EXPLAIN_3_DRV_CAG:= $(TEXT_TMP_VARIANT_PREFIX)rules3Explain.cag

TEXT_RULES_TH_SRC_RUL		:= $(addprefix $(TEXT_SRC_PREFIX),TheoremRules.rul)
TEXT_RULES_TH_DRV_LTEX		:= $(patsubst $(TEXT_SRC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(TEXT_RULES_TH_SRC_RUL))
TEXT_RULES_TH_DRV_TEX		:= $(TEXT_RULES_TH_DRV_LTEX:.ltex=.tex)

TEXT_HIDE_DRV_TXT			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN)-hide.hltex
TEXT_HIDE_DRV_LTEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.ltex)
TEXT_HIDE_DRV_TEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.tex)

#TEXT_SUBS_DRV_TEX			:= $(EHC_CAG_DRV_TEX) $(EHC_CHS_DRV_TEX) $(AGPRIMER_CAG_DRV_TEX) $(AGPRIMER_CHS_DRV_TEX) $(RULER_12_DRV_TEX) \
#								$(RULER_3_DRV_TEX) $(RULER2_RULES_DRV_TEX) $(TEXT_RULES_3_DRV_TEX) $(TEXT_RULES_TH_DRV_TEX)
TEXT_SUBS_DRV_TEX			:= $(RULER_12_DRV_TEX) \
								$(RULER_3_DRV_TEX) $(RULER2_RULES_DRV_TEX) $(TEXT_RULES_3_DRV_TEX) $(TEXT_RULES_TH_DRV_TEX)
TEXT_SUBS_ASIS_DRV			:= $(patsubst $(TEXT_SRC_PREFIX)%.tex,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(TEXT_SUBS_ASIS_SRC))

TEXT_INCL_LIST_TEX			:= $(TEXT_TMP_VARIANT_PREFIX)InclList.tex
TEXT_GEN_BY_RULER_TABLE_TEX	:= $(TEXT_TMP_VARIANT_PREFIX)GenByRuler.tex

TEXT_BIB_SRC				:= $(TEXT_SRC_PREFIX)LitAdm.bib
TEXT_BIB_DRV				:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).bib

FIGS_XFIG_DRV_TEX			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(FIGS_XFIG_SRC_FIG))
FIGS_XFIG_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_XFIG_SRC_FIG_NOPDF))
FIGS_ASIS_DRV				:= $(patsubst $(FIGS_SRC_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(FIGS_ASIS_SRC))

TEXT_ALL_MK_FILES			:= $(AGPRIMER_MKF) $(EHC_MKF) $(RULER2_MKF) $(TEXT_MKF)

# ruler demo
TEXT_RULER2_DEMO_DRV_CAG		:= $(TEXT_TMP_VARIANT_PREFIX)$(RULER2_DEMO_AG_BASE).cag
TEXT_RULER2_DEMO_DRV_LCTEX		:= $(TEXT_TMP_VARIANT_PREFIX)demo.lctex
TEXT_RULER2_DEMO_DRV_CTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.ctex)
TEXT_RULER2_DEMO_DRV_RL2		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.rl2)
TEXT_RULER2_DEMO_DRV_LRTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.lrtex)
TEXT_RULER2_DEMO_DRV_RTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.rtex)
TEXT_RULER2_DEMO_DRV_LATEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.latex)
TEXT_RULER2_DEMO_DRV_ATEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.atex)

TEXT_RULER2_DEMO_ALL_DRV_TEX	:= $(TEXT_RULER2_DEMO_DRV_CTEX) $(TEXT_RULER2_DEMO_DRV_RTEX) $(TEXT_RULER2_DEMO_DRV_ATEX)

TEXT_RULER2_DEMO_TEX		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_ALL_DRV_TEX))
TEXT_RULER2_DEMO_STUFF		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_DRV_AG) $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_HS_UTILS))

# all src
TEXT_EDIT_SRC				:= $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(TEXT_MAIN_SRC_LSTY) $(TEXT_RULES_TH_SRC_RUL)
TEXT_ALL_SRC				:= $(TEXT_EDIT_SRC) $(TEXT_SUBS_ASIS_SRC) $(TEXT_BIB_SRC) $(TEXT_MKF)

# all deriveds (as counting for make dependencies)
TEXT_ALL_DPD				:= $(TEXT_MAIN_DRV_TEX) $(TEXT_SUBS_DRV_TEX) $(TEXT_MAIN_DRV_STY) $(TEXT_RULER2_DEMO_TEX) $(TEXT_RULER2_DEMO_ALL_DRV_TEX) \
								$(TEXT_SUBS_ASIS_DRV) $(FIGS_XFIG_DRV_TEX) $(FIGS_XFIG_DRV_PDF) $(TEXT_RULER2_DEMO_STUFF) $(FIGS_ASIS_DRV) $(TEXT_HIDE_DRV_TEX)  \
								$(TEXT_GEN_BY_RULER_TABLE_TEX) $(TEXT_INCL_LIST_TEX)

# all shuffle included material
TEXT_SUBS_SHUFFLE			:= $(TEXT_SUBS_SRC_CLTEX) $(TEXT_RULES_3_DRV_CAG) $(EHC_ALL_CHUNK_SRC) $(RULER2_ALL_CHUNK_SRC) $(AGPRIMER_ALL_CHUNK_SRC) $(TEXT_RULES_EXPLAIN_3_DRV_CAG)

# distribution
TEXT_DIST_DOC_FILES			:= $(TEXT_ALL_PUB_PDFS)
TEXT_DIST_FILES				:= $(TEXT_ALL_SRC)

# variant dispatch rules
$(TEXT_ALL_PDFS): $(DOC_PREFIX)%.pdf: $(TEXT_ALL_SRC) $(RULER2_DEMO_ALL_SRC) $(EHC_ALL_SRC) $(RULER2_DEMO_ALL_DRV_TEX) $(RULER2_RULES_SRC_RL2) $(TEXT_ALL_MK_FILES) $(FIGS_ALL_SRC)
	$(MAKE) TEXT_VARIANT=$(*F) text-variant-$(*F)

$(TEXT_VARIANTS) : % : $(DOC_PREFIX)%.pdf
	open $<

# rules
text-variant-phd:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-paper:
	$(MAKE) TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-once

text-variant-phd-draft:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-flops06-ruler-paper:
	$(MAKE) RULER2_DEMO_MARK_CHANGES_CFG= \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=limitSize --set=storyRuler --set=asArticle --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-flops06-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=limitSize --set=storyRuler --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-truu-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyRuler --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-pldi06-explimpl-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=pldi06 --set=acm --set=kscode --set=limitSize --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-once

text-variant-pldi06-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=pldi06 --set=acm --set=kscode --set=limitSize --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-truu-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-scratch:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPHD --unset=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=7 \
	  text-variant-dflt-once

text-variant-shuffle-doc:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyShuffle --set=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=11 \
	  text-variant-dflt-once

text-variant-ruler-doc:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyRuler --set=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=19 \
	  text-variant-dflt-once

text-variant-poster:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=13 \
	  text-variant-dflt-once

text-variant-slides-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyRuler --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=14 \
	  text-variant-dflt-once

text-variant-slides-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyExplImpl --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=15 \
	  text-variant-dflt-once

text-variant-slides-explimpl-fpnl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyExplImpl --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=16 \
	  text-variant-dflt-once

text-variant-slides-overview:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyOverview --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=17 \
	  text-variant-dflt-once

text-variant-dflt-once: $(TEXT_ALL_DPD)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; $(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-bib: $(TEXT_ALL_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-bib-inx: $(TEXT_ALL_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	rm -f $(TEXT_MAIN).ind ; \
	$(MAKEINDEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

$(TEXT_MAIN_DRV_LTEX) : $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SHUFFLE) $(SHUFFLE) $(TEXT_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --hidedest=appx=$(TEXT_HIDE_DRV_TXT) --order="$(TEXT_SHUFFLE_ORDER)" $< $(TEXT_SUBS_SHUFFLE) > $@

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

$(RULER_12_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rul $(RULER1)
	mkdir -p $(@D)
	$(RULER1) --latex --base=$(*F) $< > $@

$(RULER_3_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(EHC_SRC_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --lhs2tex $(TEXT_RULER_MARK_CHANGES_CFG) --base=$(*F) $< > $@

$(RULER2_RULES_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(RULER2_SRC_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --lhs2tex --base=$(*F) $< > $@

$(TEXT_RULES_TH_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(TEXT_SRC_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --lhs2tex --base=$(*F) $< > $@

$(TEXT_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --selrule="((1=K),(2=C),(3=HM),(4=EX),(42=I2),(9=P)).(expr.base tyexpr.base patexpr.base decl.base).(*)" --base=$(*F) $< > $@

$(TEXT_RULES_EXPLAIN_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --explain --wrapshuffle $< > $@

$(TEXT_RULER2_DEMO_TEX) $(TEXT_RULER2_DEMO_STUFF): $(TEXT_TMP_VARIANT_PREFIX)% : $(RULER2_DEMO_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(TEXT_BIB_DRV): $(TEXT_BIB_SRC)
	mkdir -p $(@D)
	cp $< $@

$(TEXT_SUBS_ASIS_DRV): $(TEXT_TMP_VARIANT_PREFIX)% : $(TEXT_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_ASIS_DRV): $(TEXT_TMP_VARIANT_PREFIX)% : $(FIGS_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_XFIG_DRV_TEX): $(TEXT_TMP_VARIANT_PREFIX)%.tex : $(FIGS_SRC_PREFIX)%.fig $(TEXT_MKF)
	mkdir -p $(@D)
	(echo '%include lhs2TeX.fmt' ; echo '%include afp.fmt' ; echo '%include oneletter.fmt' ; fig2dev -L epic -E 0 $< | sed -e 's/@/@@/g') > $@.ltex
	$(LHS2TEX) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $@.ltex > $@

$(FIGS_XFIG_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)%.pdf : $(FIGS_SRC_PREFIX)%.fig
	mkdir -p $(@D)
	fig2dev -L pdf -p dummy $< > $@

$(TEXT_INCL_LIST_TEX): $(TEXT_ALL_MK_FILES)
	@(for f in $(sort $(notdir $(TEXT_SUBS_DRV_TEX) $(TEXT_RULER2_DEMO_ALL_DRV_TEX) $(TEXT_RULER2_DEMO_TEX))) ; \
	  do \
	    echo "\\input" $$f ; \
	  done \
	) > $@

$(TEXT_GEN_BY_RULER_TABLE_TEX): $(EHC_MKF) $(TEXT_MKF)
	@(echo "\begin{tabular}{llp{.6\linewidth}}" ; \
	  echo "EH version & Ruler view & rules \\\\ \\hline" ; \
	  for f in $(EHC_VARIANTS) ; \
	  do \
	    $(MAKE) echo-gen-by-ruler-$$f ; \
	  done ; \
	  echo "\end{tabular}" \
	) > $@

$(TEXT_HIDE_DRV_LTEX): $(TEXT_HIDE_DRV_TXT)
	(echo '%include lhs2TeX.fmt' ; echo '%include afp.fmt' ; cat $< ) > $@

# ruler demo make rules
$(TEXT_RULER2_DEMO_DRV_LCTEX): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=all --latex --order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_RUL_BASE) --lhs2tex=yes $< > $@

$(TEXT_RULER2_DEMO_DRV_CTEX): $(TEXT_RULER2_DEMO_DRV_LCTEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_RULER2_DEMO_DRV_RL2): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --plain --order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(TEXT_RULER2_DEMO_DRV_LRTEX): $(TEXT_RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --lhs2tex --selrule="(E - *).(*).(*)" $(RULER2_DEMO_MARK_CHANGES_CFG) --base=rulerDemo $< > $@

$(TEXT_RULER2_DEMO_DRV_RTEX): $(TEXT_RULER2_DEMO_DRV_LRTEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_RULER2_DEMO_DRV_CAG): $(TEXT_RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --ag --ATTR --selrule="(3).(*).(*)" --wrapshuffle  --base=$(RULER2_DEMO_AG_BASE) $< > $@

$(TEXT_RULER2_DEMO_DRV_AG): $(TEXT_RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --plain --order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(TEXT_RULER2_DEMO_DRV_LATEX): $(TEXT_RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --latex --order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_AG_BASE) --lhs2tex=yes $< > $@

$(TEXT_RULER2_DEMO_DRV_ATEX): $(TEXT_RULER2_DEMO_DRV_LATEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

