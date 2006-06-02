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

text-variant-hw06-impred:
	$(MAKE) TEXT_INF2PS_MARK_CHANGES_CFG= TEXT_RULER_MARK_CHANGES_CFG= TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=hw06 --set=acm --set=kscode --set=limitSize --set=infer2pass --set=storyImpred --set=asArticle" \
	  TEXT_SHUFFLE_VARIANT=5 \
	  text-variant-dflt-bib

text-variant-scratch:
	$(MAKE) TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC=yes \
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
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=storyRulerDoc --set=asArticle --set=useHyperref --set=asDraft --set=refToPDF" \
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
