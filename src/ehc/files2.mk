# ehc/uhc variant dispatch rules
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(EHC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

#$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC) $(GRINC_ALL_SRC) $(EHC_MKF) \
#			$(if $(filter $(EHC_VARIANT),$(EHC_CODE_VARIANTS)),$(RTS_ALL_SRC),)
#	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-variant
$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC) $(GRINC_ALL_SRC) $(EHC_MKF) $(RTS_ALL_SRC)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-variant

$(EHC_ALL_HADDOCKS): %: $(EHC_ALL_SRC) $(GRINC_ALL_SRC) $(EHC_MKF)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-haddock-variant


$(patsubst %,lib-eh-%,$(EHC_VARIANTS)):
	@v=`echo $@ | sed -e 's/.*eh-\([0-9_]*\)/\1/'` ; \
	$(MAKE) EHC_VARIANT=$$v lib-eh-variant

# rules for outside construction of library
lib-eh-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  $(LIB_EHC_INS_FLAG)

# rules for ehc compiler
ehc-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-variant-dflt

ehc-variant-dflt: $(EHC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG) $(LIB_GRINC_INS_FLAG) \
			$(if $(filter $(EHC_VARIANT),$(EHC_CODE_VARIANTS)),$(INSABS_LIB_RTS),)
	mkdir -p $(dir $(EHC_BLD_EXEC)) && \
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -package $(LIB_GRINC_PKG_NAME) -i$(EHC_BLD_VARIANT_PREFIX) $(EHC_BLD_VARIANT_PREFIX)$(EHC_MAIN).hs -o $(EHC_BLD_EXEC)

# rules for ehc haddock
ehc-haddock-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-haddock-variant-dflt

ehc-haddock-variant-dflt: $(EHC_ALL_DPDS) $(GRINC_ALL_DPDS) $(LIB_EH_UTIL_HS_DRV)
	mkdir -p hdoc/$(EHC_VARIANT)
	haddock --html --ignore-all-exports --odir=hdoc/$(EHC_VARIANT) $(EHC_ALL_DPDS_NOPREPROC) $(GRINC_ALL_DPDS_NOPREPROC) $(LIB_EH_UTIL_HS_DRV)



#ehc-variant-selrule: 
#	$(MAKE) EHC_VARIANT_RULER_SEL="($(EHC_VARIANT)).(expr.base).(e.int e.char)" ehc-variant-dflt

# rules for uhc
$(BIN_PREFIX)uhc$(EXEC_SUFFIX): $(BIN_PREFIX)$(EHC_UHC_VARIANT)/ehc$(EXEC_SUFFIX)
	cp $< $@
