# rules
text-variant-ehc-book:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyEhcBook --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-ehc-book-tst:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyEhcBook --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-once

text-variant-phd:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-paper:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-tst:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-once

text-variant-phd-draft:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft --set=inclInx --set=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-flops06-ruler-paper:
	$(MAKE)  TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes RULER2_DEMO_MARK_CHANGES_CFG= \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=shortStory --set=storyRuler --set=asArticle --set=targetForPaper --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-flops06-ruler:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=shortStory --set=storyRuler --set=asArticle --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-truu-ruler:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyRuler --set=asArticle --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-popl07-explimpl-tst:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl07 --set=acm --set=kscode --set=shortStory --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-once

text-variant-popl07-explimpl:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl07 --set=acm --set=kscode --set=shortStory --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-truu-explimpl:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-hw06-impred:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=hw06 --set=acm --set=kscode --set=shortStory --set=infer2pass --set=storyImpred --set=asArticle --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-esop07-impred:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=esop07 --set=llncs --set=kscode --set=shortStory --set=infer2pass --set=storyImpred --set=asArticle --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-esop07-impred-tr:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=esop07 --set=truu --set=kscode --set=trStory --set=infer2pass --set=storyImpred --set=asArticle --set=wide --set=inclApp" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-scratch:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPHD --unset=asArticle --set=useHyperref --set=refToPDF --set=blockstyle" \
	  TEXT_SHUFFLE_VARIANT=7 \
	  text-variant-dflt-once

text-variant-scratch2:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=77 \
	  text-variant-dflt-once

text-variant-shuffle-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=40 \
	  text-variant-dflt-doc

text-variant-howtodoc-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=41 \
	  text-variant-dflt-doc

text-variant-ehc-technical-doc:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  TEXT_SHUFFLE_VARIANT=42 \
	  text-variant-dflt-doc

text-variant-ehc-user-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=43 \
	  text-variant-dflt-doc

text-variant-getting-started-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=47 \
	  text-variant-dflt-doc

text-variant-announce-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=48 \
	  text-variant-dflt-doc

text-variant-release-history-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=49 \
	  text-variant-dflt-doc

text-variant-roadmap-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=52 \
	  text-variant-dflt-doc

text-variant-build-system-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=50 \
	  text-variant-dflt-doc

text-variant-text2text-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=44 \
	  text-variant-dflt-doc

text-variant-howtoexperiment-doc:
	$(MAKE) \
	  TEXT_SHUFFLE_VARIANT=45 \
	  text-variant-dflt-doc

text-variant-ehc-structure-doc:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  TEXT_SHUFFLE_VARIANT=46 \
	  text-variant-dflt-doc

text-variant-ruler-doc:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyRulerDoc --set=asArticle --set=useHyperref --set=asDraft --set=refToPDF --set=inclInx" \
	  TEXT_SHUFFLE_VARIANT=19 \
	  text-variant-dflt-once

text-variant-poster:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_RULER_MARK_CHANGES_CFG="--markchanges='I1 - *'" TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --set=fullWide --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=13 \
	  text-variant-dflt-once

text-variant-posterLDL:
	$(MAKE) TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --set=fullWide --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=27 \
	  text-variant-dflt-once

text-variant-posterTrOrPr:
	$(MAKE) TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --set=fullWide --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=31 \
	  text-variant-dflt-once

text-variant-poster-uhcarch:
	$(MAKE) TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --set=fullWide --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=55 \
	  text-variant-dflt-once

text-variant-slides-ruler:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyRuler --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=14 \
	  text-variant-dflt-once

text-variant-slides-ruler-long:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyRuler --set=longStory --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=14 \
	  text-variant-dflt-once

text-variant-slides-explimpl:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyExplImpl --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=15 \
	  text-variant-dflt-once

text-variant-slides-explimpl-fpnl:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyExplImpl --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=16 \
	  text-variant-dflt-once

text-variant-uniqueness:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyUniqueness --set=inclTOC --set=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=20 \
	  text-variant-dflt-bib

text-variant-slides-uniqueness:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyExplImpl --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=21 \
	  text-variant-dflt-once

text-variant-slides-overview:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_CFG_FIGS_INCLUDES_XFIG_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyOverview --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=17 \
	  text-variant-dflt-once

text-variant-slides-status:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyStatus --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=23 \
	  text-variant-dflt-once

text-variant-slides-ehcstruct:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyStructure --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF --set=shortStory" \
	  TEXT_SHUFFLE_VARIANT=28 \
	  text-variant-dflt-once

text-variant-slides-ehcstruct-ufmg:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyStructure --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF --set=dateUFMG" \
	  TEXT_SHUFFLE_VARIANT=28 \
	  text-variant-dflt-once

text-variant-slides-hs09-uhcarch:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyStructure --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF --set=shortStory" \
	  TEXT_SHUFFLE_VARIANT=53 \
	  text-variant-dflt-once

text-variant-gbm:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=asArticle --set=useHyperref --set=refToPDF --set=wide --set=blockstyle" \
	  TEXT_SHUFFLE_VARIANT=22 \
	  text-variant-dflt-bib

text-variant-icfp07-chr-locinst:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyCHRlocinst --set=acm --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=24 \
	  text-variant-dflt-bib

text-variant-icfp07-chr-locinst-blind:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyCHRlocinst --set=acm --set=blockstyle --set=asArticle --set=blinded" \
	  TEXT_SHUFFLE_VARIANT=24 \
	  text-variant-dflt-bib

text-variant-cc08-chr-locinst:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=cc08 --set=kscode --set=storyCHRlocinst --set=llncs --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=24 \
	  text-variant-dflt-bib

text-variant-icfp07-ehcstruct:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyEHCstruct --set=acm --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=25 \
	  text-variant-dflt-bib

text-variant-icfp07-ehcstruct-blind:
	$(MAKE) TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX=yes TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyEHCstruct --set=acm --set=blockstyle --set=asArticle --set=blinded" \
	  TEXT_SHUFFLE_VARIANT=25 \
	  text-variant-dflt-bib

text-variant-ifl07-ehcstruct:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=no TEXT_CFG_FIGS_INCLUDES_XFIG_SRC=no TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=ifl07 --set=kscode --set=storyEHCstruct --set=llncs --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=25 \
	  text-variant-dflt-bib

text-variant-ehc-doc:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyEHCDoc --unset=asArticle --set=useHyperref --set=refToPDF --set=blockstyle --set=inclInx --unset=inclApp --set=inclTOC" \
	  TEXT_SHUFFLE_VARIANT=29 \
	  text-variant-dflt-bib-inx

text-variant-tr-abstrint:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=no TEXT_CFG_FIGS_INCLUDES_XFIG_SRC=no TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=kscode --set=truu --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=33 \
	  text-variant-dflt-bib

text-variant-ldta08-abstrint:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=no TEXT_CFG_FIGS_INCLUDES_XFIG_SRC=no TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=kscode --set=entcs --set=blockstyle --set=shortStory --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=33 \
	  text-variant-dflt-bib

text-variant-icfp08-subst:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC_EXPERIMENTS_SUBST=yes TEXT_CFG_TEXT_INCLUDES_EXPERIMENTS_SUBST_TEX=yes TEXT_SHUFFLE_ORDER="1 < 2 < 3, 3 < 32, 2 < 21, 1 < 26 < 36" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp08 --set=kscode --set=natbib --set=storyUnificSubst --set=acm --set=blockstyle --set=asArticle --set=omitlag2TeX" \
	  TEXT_SHUFFLE_VARIANT=36 \
	  text-variant-dflt-bib

text-variant-padl09-subst:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC_EXPERIMENTS_SUBST=yes TEXT_CFG_TEXT_INCLUDES_EXPERIMENTS_SUBST_TEX=yes TEXT_SHUFFLE_ORDER="1 < 2 < 3, 3 < 32, 2 < 21, 1 < 26 < 36" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=padl09 --set=kscode --set=natbib --set=storyUnificSubst --set=llncs --unset=blockstyle --set=asArticle --set=omitlag2TeX --set=shortStory" \
	  TEXT_SHUFFLE_VARIANT=36 \
	  text-variant-dflt-bib

text-variant-padl09-subst-tr:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC_EXPERIMENTS_SUBST=yes TEXT_CFG_TEXT_INCLUDES_EXPERIMENTS_SUBST_TEX=yes TEXT_SHUFFLE_ORDER="1 < 2 < 3, 3 < 32, 2 < 21, 1 < 26 < 36" \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=kscode --set=natbib --set=storyUnificSubst --set=blockstyle --set=asArticle --set=omitlag2TeX --unset=hasAppendix" \
	  TEXT_SHUFFLE_VARIANT=36 \
	  text-variant-dflt-bib

text-variant-ldta09-agidiom:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_CFG_FIGS_INCLUDES_DOT_SRC=no TEXT_CFG_FIGS_INCLUDES_XFIG_SRC=no \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=kscode --set=entcs --set=blockstyle --set=shortStory --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=38 \
	  text-variant-dflt-once

text-variant-hs09-uhcarch:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=hs09 --set=kscode --set=acm --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=51 \
	  text-variant-dflt-bib

text-variant-theplan:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC= \
	  INCLUDE_DERIVED_MK=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=theplan --set=kscode --set=blockstyle --set=asArticle --set=wide" \
	  TEXT_SHUFFLE_VARIANT=57 \
	  text-variant-dflt-bib-dotdpd

