###########################################################################################
# ehc/uhc variant dispatch rules
###########################################################################################

# for (e.g.) 99/ehc, shorthand notation for ehc binaries
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(EHC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

# for (e.g.) bin/99/ehc, ehc binaries
$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC) $(GRINC_ALL_SRC) $(EHC_MKF) $(RTS_ALL_SRC)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-variant

# for haddock
$(EHC_ALL_HADDOCKS): %: $(EHC_ALL_SRC) $(GRINC_ALL_SRC) $(EHC_MKF)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-haddock-variant

# for (e.g.) lib-eh-99, ehc libraries
$(patsubst %,lib-eh-%,$(EHC_VARIANTS)):
	@v=`echo $@ | sed -e 's/.*eh-\([0-9_]*\)/\1/'` ; \
	$(MAKE) EHC_VARIANT=$$v lib-eh-variant

# for (e.g.) 99/bare, barebones versions
$(patsubst %,%/bare,$(EHC_VARIANTS)):
	@v="$(@D)" ; \
	$(MAKE) EHC_VARIANT=$$v ehc-barebones-variant

###########################################################################################
# rules for library
###########################################################################################

lib-eh-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  $(LIB_EHC_INS_FLAG)

###########################################################################################
# rules for ehc compiler
###########################################################################################

ehc-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-variant-dflt

ehc-variant-dflt: $(EHC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG) \
			$(if $(EHC_CFG_USE_GRIN) \
				,$(LIB_GRINC_INS_FLAG) \
				 $(if $(EHC_CFG_USE_CODEGEN),$(GEN_ALL_DPDS) $(INSABS_LIB_RTS),),)
	mkdir -p $(dir $(EHC_BLD_EXEC)) && \
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) \
	       $(if $(EHC_CFG_USE_GRIN),-package $(LIB_GRINC_PKG_NAME),) \
	       -i$(EHC_BLD_VARIANT_PREFIX) $(EHC_BLD_VARIANT_PREFIX)$(EHC_MAIN).hs -o $(EHC_BLD_EXEC)

###########################################################################################
# rules for ehc haddock
###########################################################################################

ehc-haddock-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-haddock-variant-dflt

ehc-haddock-variant-dflt: $(EHC_ALL_DPDS) $(GRINC_ALL_DPDS) $(LIB_EH_UTIL_HS_DRV)
	mkdir -p hdoc/$(EHC_VARIANT)
	haddock --html --ignore-all-exports --odir=hdoc/$(EHC_VARIANT) $(EHC_ALL_DPDS_NOPREPROC) $(GRINC_ALL_DPDS_NOPREPROC) $(LIB_EH_UTIL_HS_DRV)



###########################################################################################
# rules for barebones distribution
###########################################################################################

ehc-barebones-variant: $(EHC_AG_ALL_MAIN_DRV_AG) $(EHC_AG_ALL_DPDS_DRV_AG) $(EHC_ALL_LIB_FROMHS_HS) \
						$(EHC_HS_MAIN_DRV_HS) \
						$(if $(EHC_CFG_USE_GRIN),$(GRINC_AG_ALL_MAIN_DRV_AG) $(GRINC_AG_ALL_DPDS_DRV_AG) $(GRINC_ALL_LIB_FROMHS_HS),) \
						$(if $(EHC_CFG_USE_GRIN),$(if $(EHC_CFG_USE_CODEGEN),$(RTS_H_RTS_ALL_DRV_H) $(RTS_C_RTS_ALL_DRV_C),),)						
	@rm -rf $(EHC_BARE_VARIANT_PREFIX) ; \
	mkdir -p $(EHC_BARE_VARIANT_PREFIX) ; \
	ehc_mainag_d_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_D_MAIN_DRV_AG)))" ; \
	ehc_mainag_s_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_S_MAIN_DRV_AG)))" ; \
	ehc_mainhs_files="$(subst $(EHC_BLD_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_HS_MAIN_DRV_HS)))" ; \
	ehc_other_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_ALL_DPDS_DRV_AG) $(EHC_ALL_LIB_FROMHS_HS)))" ; \
	grinc_mainag_d_files="$(subst $(EHC_BLD_LIBGRINC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(if $(EHC_CFG_USE_GRIN),$(GRINC_AG_D_MAIN_DRV_HS) $(GRINC_AG_D_MAIN_DRV_HS_PRE),)))" ; \
	grinc_mainag_s_files="$(subst $(EHC_BLD_LIBGRINC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(if $(EHC_CFG_USE_GRIN),$(GRINC_AG_S_MAIN_DRV_HS) $(GRINC_AG_S_MAIN_DRV_HS_PRE),)))" ; \
	grinc_other_files="$(subst $(EHC_BLD_LIBGRINC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(if $(EHC_CFG_USE_GRIN),$(GRINC_AG_ALL_DPDS_DRV_AG) $(GRINC_ALL_LIB_FROMHS_HS),)))" ; \
	$(call COPY_FILES_BY_TAR,$(EHC_BLD_VARIANT_PREFIX),$(EHC_BARE_VARIANT_PREFIX),$$ehc_mainhs_files) ; \
	$(call COPY_FILES_BY_TAR,$(EHC_BLD_LIBEHC_VARIANT_PREFIX),$(EHC_BARE_VARIANT_PREFIX),$$ehc_mainag_d_files $$ehc_mainag_s_files $$ehc_other_files) ; \
	$(if $(EHC_CFG_USE_GRIN),$(call COPY_FILES_BY_TAR,$(EHC_BLD_LIBGRINC_VARIANT_PREFIX),$(EHC_BARE_VARIANT_PREFIX),$$grinc_mainag_d_files $$grinc_mainag_s_files $$grinc_other_files);,) \
	(cd $(EHC_BARE_VARIANT_PREFIX) && \
	  (echo ehc: $$ehc_mainag_d_files $$ehc_mainag_s_files $$grinc_mainag_d_files $$grinc_mainag_s_files | sed -e 's+\.ag+.hs+g' ; \
	   echo "	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -fallow-undecidable-instances -package $(LIB_EH_UTIL_PKG_NAME) -o $(EHC_EXEC_NAME)$(EXEC_SUFFIX) $(EHC_MAIN).hs" ; \
	   echo ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(LIB_EHC_HS_PREFIX)" \
	     $$ehc_mainag_d_files ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) -P$(LIB_EHC_HS_PREFIX)" \
	     $$ehc_mainag_s_files ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_GRINC_HS_PREFIX) --searchdir=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(LIB_GRINC_HS_PREFIX) -P$(LIB_EHC_HS_PREFIX)" \
	     $$grinc_mainag_d_files ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_GRINC_HS_PREFIX) --searchdir=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) -P$(LIB_GRINC_HS_PREFIX) -P$(LIB_EHC_HS_PREFIX)" \
	     $$grinc_mainag_s_files \
	  ) \
	  > Makefile \
	)

###########################################################################################
# rules for uhc
###########################################################################################

$(UHC_BLD_EXEC): $(EHC_FOR_UHC_BLD_EXEC)
	mkdir -p $(@D)
	cp $< $@
