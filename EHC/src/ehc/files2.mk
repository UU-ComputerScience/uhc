###########################################################################################
# ehc/uhc variant build dispatch rules
###########################################################################################

# for (e.g.) 99/ehc, shorthand notation for ehc binaries
$(patsubst %,%/ehc,$(EHC_VARIANTS)): %/ehc: $(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,%)
#$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(EHC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

# for (e.g.) install/99/bin/ehc, ehc binaries
$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC_FIND) $(EHC_MKF) $(RTS_ALL_SRC)
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	$(MAKE) INCLUDE_DERIVED_MK=yes EHC_VARIANT=`echo $@ | sed -n -e 's+$(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,\([0-9_]*\)).*+\1+p'` ehc-variant

# for haddock
$(EHC_ALL_HADDOCKS): %: $(EHC_ALL_SRC) $(EHC_MKF)
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
# ehc/uhc variant clean dispatch rules
###########################################################################################

# for (e.g.) 99/clean
$(patsubst %,%/clean,$(EHC_VARIANTS)):
	@v=`echo $@ | sed -e 's+\([0-9_]*\)/.*+\1+'` ; \
	$(MAKE) EHC_VARIANT=$$v ehc-clean-variant

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

ehc-variant-dflt: $(EHC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG)
	mkdir -p $(dir $(EHC_INSTALL_VARIANT_ASPECTS_EXEC)) && \
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) \
	       -i$(EHC_BLD_VARIANT_ASPECTS_PREFIX) $(EHC_BLD_VARIANT_ASPECTS_PREFIX)$(EHC_MAIN).hs -o $(EHC_INSTALL_VARIANT_ASPECTS_EXEC)
	$(if $(EHC_CFG_USE_CODEGEN), \
	  if test -x $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) ; then \
	    for targ in `$(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --meta-targets` ; do \
	      $(MAKE) ehc-codegentargetspecific-$${targ} ; \
	    done \
	  fi \
	,)

#ehc-variant-dflt: $(EHC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG) \
#			$(if $(EHC_CFG_USE_GRIN) \
#				,$(if $(EHC_CFG_USE_CODEGEN),$(INSTALLFORBLDABS_LIB_RTS),),)
#	mkdir -p $(dir $(EHC_INSTALL_VARIANT_ASPECTS_EXEC)) && \
#	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) \
#	       -i$(EHC_BLD_VARIANT_ASPECTS_PREFIX) $(EHC_BLD_VARIANT_ASPECTS_PREFIX)$(EHC_MAIN).hs -o $(EHC_INSTALL_VARIANT_ASPECTS_EXEC)

###########################################################################################
# code generation target specific make targets, for each $(EHC_TARGETS)
###########################################################################################

ehc-codegentargetspecific-bc:

ehc-codegentargetspecific-C:

ehc-codegentargetspecific-jazy:

ehc-codegentargetspecific-core:

ehc-codegentargetspecific-clr:

ehc-codegentargetspecific-llvm: $(call FUN_INSTALL_VARIANT_BIN_PREFIX,$(EHC_VARIANT))llvmc

###########################################################################################
# LLVM specific
###########################################################################################

$(call FUN_INSTALL_VARIANT_BIN_PREFIX,$(EHC_VARIANT))llvmc: $(BIN_PREFIX)llvmc
	install $(BIN_PREFIX)llvmc $@

###########################################################################################
# rules for ehc haddock
###########################################################################################

ehc-haddock-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-haddock-variant-dflt

ehc-haddock-variant-dflt: $(EHC_ALL_DPDS) $(LIB_EH_UTIL_HS_DRV)
	mkdir -p hdoc/$(EHC_VARIANT)
	haddock --html --ignore-all-exports --odir=hdoc/$(EHC_VARIANT) $(EHC_ALL_DPDS_NOPREPROC) $(LIB_EH_UTIL_HS_DRV)

###########################################################################################
# rules for ehc clean: executable, build library, runtime library, ...
###########################################################################################

ehc-clean-variant:
	@echo "Cleaning variant $(EHC_VARIANT)"
	@if test -d $(EHC_BLD_LIBEHC_VARIANT_PREFIX) ; \
	then \
	  cd $(EHC_BLD_LIBEHC_VARIANT_PREFIX) ; \
	  if test -x $(LIB_EHC_SETUP) ; \
	  then \
	    $(LIB_EHC_SETUP) unregister $(CABAL_OPT_INSTALL_LOC) ; \
	    $(LIB_EHC_SETUP) clean ; \
	  fi ; \
	  rm -rf dist Setup.* *.cabal $(LIB_EHC_SETUP) ; \
	fi
	@if test -x $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) ; \
	then \
	  variant=`$(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --meta-variant` ; \
	  if test $${variant} -ge $(EHC_PREL_VARIANT) ; \
	  then \
	    rm -rf `$(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --meta-dir-env` ; \
	  fi \
	fi
	@rm -rf $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) \
	        $(EHC_BLD_VARIANT_ASPECTS_PREFIX) \
	        $(INSTALLFORBLD_EHC_LIB_PREFIX) $(LIB_EHC_INS_FLAG) \
	        $(INSTALLFORBLD_VARIANT_ASPECTS_PREFIX) \
	        $(INSTALL_VARIANT_PREFIX)

###########################################################################################
# rules for barebones distribution
###########################################################################################

ehc-barebones-variant: $(EHC_AG_ALL_MAIN_DRV_AG) $(EHC_AG_ALL_DPDS_DRV_AG) $(EHC_ALL_LIB_FROMHS_HS) \
						$(EHC_HS_MAIN_DRV_HS) \
						$(if $(EHC_CFG_USE_GRIN),$(if $(EHC_CFG_USE_CODEGEN),$(RTS_H_RTS_ALL_DRV_H) $(RTS_C_RTS_ALL_DRV_C),),)						
	@rm -rf $(EHC_BARE_VARIANT_ASPECTS_PREFIX) ; \
	mkdir -p $(EHC_BARE_VARIANT_ASPECTS_PREFIX) ; \
	ehc_mainag_d_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_D_MAIN_DRV_AG)))" ; \
	ehc_mainag_s_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_S_MAIN_DRV_AG)))" ; \
	ehc_mainhs_files="$(subst $(EHC_BLD_VARIANT_ASPECTS_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_HS_MAIN_DRV_HS)))" ; \
	ehc_other_files="$(subst $(EHC_BLD_LIBEHC_VARIANT_PREFIX),,$(call FILTER_OUT_EMPTY_FILES,$(EHC_AG_ALL_DPDS_DRV_AG) $(EHC_ALL_LIB_FROMHS_HS)))" ; \
	$(call FUN_COPY_FILES_BY_TAR,$(EHC_BLD_VARIANT_ASPECTS_PREFIX),$(EHC_BARE_VARIANT_ASPECTS_PREFIX),$$ehc_mainhs_files) ; \
	$(call FUN_COPY_FILES_BY_TAR,$(EHC_BLD_LIBEHC_VARIANT_PREFIX),$(EHC_BARE_VARIANT_ASPECTS_PREFIX),$$ehc_mainag_d_files $$ehc_mainag_s_files $$ehc_other_files) ; \
	(cd $(EHC_BARE_VARIANT_ASPECTS_PREFIX) && \
	  (echo $(EHC_EXEC_NAME)$(EXEC_SUFFIX): $$ehc_mainag_d_files $$ehc_mainag_s_files | sed -e 's+\.ag+.hs+g' ; \
	   echo "	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -fallow-undecidable-instances -package $(LIB_EH_UTIL_PKG_NAME) -o $(EHC_EXEC_NAME)$(EXEC_SUFFIX) $(EHC_MAIN).hs" ; \
	   echo ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(LIB_EHC_HS_PREFIX)" \
	     $$ehc_mainag_d_files ; \
	   $(SHELL_AGDEPEND) --baseprefix=$(LIB_EHC_HS_PREFIX) \
	     --agc="$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) -P$(LIB_EHC_HS_PREFIX)" \
	     $$ehc_mainag_s_files ; \
	  ) \
	  > Makefile \
	)

