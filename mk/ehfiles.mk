VPREFIX				:= $(VF)/

$(addprefix $(VPREFIX),$(EHC_CAG:.cag=.ag)): $(VPREFIX)%.ag: %.cag $(SHUFFLE)
	$(call SHUFFLE_LHS_AG,$<,$@,$(V)) ; \
	touch $@

$(addprefix $(VPREFIX),$(EHC_CHS:.chs=.hs)): $(VPREFIX)%.hs: %.chs $(SHUFFLE)
	$(call SHUFFLE_LHS_HS,$<,$@,$(V)) ; \
	touch $@

$(VPREFIX)EHTy.hs: $(addprefix $(VPREFIX),$(DPDS_TY))
	$(call AGCC,-dr,$<)

$(VPREFIX)EHCode.hs: $(addprefix $(VPREFIX),$(DPDS_CODE))
	$(call AGCC,-dr,$<)

$(VPREFIX)EHError.hs: $(addprefix $(VPREFIX),$(DPDS_ERR))
	$(call AGCC,-dr,$<)

$(addprefix $(VPREFIX),$(EHC_LAG_FOR_HS_TY:.lag=.hs)): %.hs: %.ag $(addprefix $(VPREFIX),EHTyCommonAG.ag EHTyAbsSyn.ag)
	$(call AGCC,-cfspr,$<)

$(addprefix $(VPREFIX),$(EHC_LAG_FOR_HS_CODE:.lag=.hs)): %.hs: %.ag $(addprefix $(VPREFIX),EHCodeAbsSyn.ag)
	$(call AGCC,-cfspr,$<)

$(VPREFIX)EHErrorPretty.hs: $(addprefix $(VPREFIX),$(DPDS_ERR_PRETTY))
	$(call AGCC,-cfspr,$<)

$(VPREFIX)EHMainAG.hs: $(addprefix $(VPREFIX),$(DPDS_MAIN))
	$(call AGCC,-dcfspr,$<)

$(VPREFIX)$(EHC): $(addprefix $(VPREFIX),$(EHC_MAIN).hs $(EHC_HS))
	cd `dirname $@` ; $(GHC) -package uust -package data -o `basename $@` --make `basename $<`
