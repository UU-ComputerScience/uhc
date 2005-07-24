# version info
DIST_VERSION_NAME	:= 0.1alpha

# date
DIST_DATE			:= $(shell /bin/date +%Y%m%d)

# dist files
TOP_DIST_FILES		:= Makefile mk/shared.mk $(wildcard lib/*.hs)
DIST_ALL_FILES		:= $(TOP_DIST_FILES) $(RULER2_DIST_FILES) $(TEXT_DIST_DOC_FILES) $(SHUFFLE_DIST_FILES) $(EHC_DIST_FILES) \
						$(AGPRIMER_DIST_FILES) $(GRIN_DIST_FILES) $(GRINI_DIST_FILES) $(TEST_DIST_FILES)

# depending on desired config (currently not exploited)
DIST_NAME			:= ehc
DIST_FILES			:= $(DIST_ALL_FILES)

# all variants
DIST_VARIANTS		:= ehc

# name of dist
DIST_BASE			:= $(DIST_DATE)-$(DIST_NAME)-$(DIST_VERSION_NAME)
DIST_PREFIX			:= $(DIST_BASE)/
DIST_TGZ			:= $(DIST_BASE).tgz

# dispatch
dist-ehc: dist-variant-dflt

# rules
dist-variant-dflt:
	@rm -rf $(DIST_PREFIX) ; \
	mkdir -p $(DIST_PREFIX) ; \
	((tar --ignore-failed-read -cf - $(DIST_FILES)) | (cd $(DIST_PREFIX) && tar xf -)) ; \
	echo "== $(DIST_TGZ) ==" ; \
	tar cfz $(DIST_TGZ) $(DIST_PREFIX)
