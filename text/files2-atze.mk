# rules
text-variant-ehc-book:
	$(MAKE) TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyEhcBook --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-ehc-book-tst:
	$(MAKE) TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyEhcBook --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-once

text-variant-phd:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-paper:
	$(MAKE) TEXT_RULER_MARK_CHANGES_CFG= RULER2_DEMO_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-phd-tst:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-once

text-variant-phd-draft:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyPHD --unset=asArticle --set=refToPDF --set=useHyperref --set=asDraft" \
	  TEXT_SHUFFLE_VARIANT=2 \
	  text-variant-dflt-bib-inx

text-variant-flops06-ruler-paper:
	$(MAKE) RULER2_DEMO_MARK_CHANGES_CFG= \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=shortStory --set=storyRuler --set=asArticle --set=targetForPaper" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-flops06-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=flops06 --set=llncs --set=kscode --set=shortStory --set=storyRuler --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-truu-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyRuler --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=3 \
	  text-variant-dflt-bib

text-variant-popl07-explimpl-tst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl07 --set=acm --set=kscode --set=shortStory --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-once

text-variant-popl07-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=popl07 --set=acm --set=kscode --set=shortStory --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-truu-explimpl:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=truu --set=storyExplImpl --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=4 \
	  text-variant-dflt-bib

text-variant-hw06-impred:
	$(MAKE) TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=hw06 --set=acm --set=kscode --set=shortStory --set=infer2pass --set=storyImpred --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-esop07-impred:
	$(MAKE) TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=esop07 --set=llncs --set=kscode --set=shortStory --set=infer2pass --set=storyImpred --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-esop07-impred-tr:
	$(MAKE) TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=esop07 --set=truu --set=kscode --set=trStory --set=infer2pass --set=storyImpred --set=asArticle --set=wide" \
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
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyShuffle --set=asArticle --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=11 \
	  text-variant-dflt-once

text-variant-ruler-doc:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyRulerDoc --set=asArticle --set=useHyperref --set=asDraft --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=19 \
	  text-variant-dflt-once

text-variant-poster:
	$(MAKE) TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=storyPoster --set=asArticle --set=fullWide --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=13 \
	  text-variant-dflt-once

text-variant-slides-ruler:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyRuler --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=14 \
	  text-variant-dflt-once

text-variant-slides-ruler-long:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyRuler --set=longStory --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
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
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyOverview --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=17 \
	  text-variant-dflt-once

text-variant-slides-status:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--set=yesBeamer --set=storyStatus --unset=asArticle --set=asSlides --unset=useHyperref --unset=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=23 \
	  text-variant-dflt-once

text-variant-gbm:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=asArticle --set=useHyperref --set=refToPDF --set=blockstyle" \
	  TEXT_SHUFFLE_VARIANT=22 \
	  text-variant-dflt-bib

text-variant-icfp07-chr-locinst:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyCHRlocinst --set=acm --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=24 \
	  text-variant-dflt-bib

text-variant-icfp07-chr-locinst-blind:
	$(MAKE) \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyCHRlocinst --set=acm --set=blockstyle --set=asArticle --set=blinded" \
	  TEXT_SHUFFLE_VARIANT=24 \
	  text-variant-dflt-bib

text-variant-icfp07-ehcstruct:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyEHCstruct --set=acm --set=blockstyle --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=25 \
	  text-variant-dflt-bib

text-variant-icfp07-ehcstruct-blind:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes TEXT_RULER_DEFS_TEX="-DrulerRuleCmd=rulerRuleVert" \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=icfp07 --set=kscode --set=natbib --set=storyEHCstruct --set=acm --set=blockstyle --set=asArticle --set=blinded" \
	  TEXT_SHUFFLE_VARIANT=25 \
	  text-variant-dflt-bib

