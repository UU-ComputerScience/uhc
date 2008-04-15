text-variant-llvm:
	$(MAKE) TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=inclTOC --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=35 \
	  text-variant-dflt-bib


