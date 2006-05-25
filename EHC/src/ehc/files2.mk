# variant dispatch rules
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(EHC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC) $(EHC_MKF)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) ehc-variant

$(patsubst %,lib-eh-%,$(EHC_VARIANTS)): $(EHC_ALL_SRC)
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

ehc-variant-dflt: $(EHC_ALL_DPDS) $(EHC_RULES_3_DRV_CAG) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG)
	mkdir -p $(dir $(EHC_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -i$(EHC_BLD_VARIANT_PREFIX) $(EHC_BLD_VARIANT_PREFIX)$(EHC_MAIN).hs -o $(EHC_BLD_EXEC)

#ehc-variant-selrule: 
#	$(MAKE) EHC_VARIANT_RULER_SEL="($(EHC_VARIANT)).(expr.base).(e.int e.char)" ehc-variant-dflt

