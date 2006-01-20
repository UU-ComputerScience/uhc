# variant, based on ehc config
UHC_BLD_VARIANT_PREFIX					:= $(EHC_BLD_VARIANT_PREFIX)
UHC_BLD_BIN_VARIANT_PREFIX				:= $(EHC_BLD_BIN_VARIANT_PREFIX)
UHC_SHUFFLE_ORDER						:= $(EHC_SHUFFLE_ORDER)

# location of ehc src
UHC_SRC_PREFIX							:= $(TOP_PREFIX)uhc/

# this file
UHC_MKF									:= $(UHC_SRC_PREFIX)files.mk

# main + sources + dpds, for .chs
UHC_HS_UTIL_SRC_CHS						:= $(patsubst %,$(UHC_SRC_PREFIX)%.chs,\
													HSParser \
											)
UHC_HS_UTIL_DRV_HS						:= $(patsubst $(UHC_SRC_PREFIX)%.chs,$(UHC_BLD_VARIANT_PREFIX)%.hs,$(UHC_HS_UTIL_SRC_CHS))

UHC_HS_ALL_SRC_CHS						:= $(UHC_HS_MAIN_SRC_CHS) $(UHC_HS_UTIL_SRC_CHS)
UHC_HS_ALL_DRV_HS						:= $(UHC_HS_MAIN_DRV_HS) $(UHC_HS_UTIL_DRV_HS)

# main + sources + dpds, for .cag
UHC_AGMAIN_MAIN_SRC_CAG					:= $(patsubst %,$(UHC_SRC_PREFIX)%.cag,HSMainAG)
UHC_AGMAIN_DPDS_SRC_CAG					:= $(patsubst %,$(UHC_SRC_PREFIX)%.cag,UHA_Syntax)
$(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.hs,$(UHC_AGMAIN_MAIN_SRC_CAG)) \
										: $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AGMAIN_DPDS_SRC_CAG))

UHC_AGHSAST_MAIN_SRC_CAG				:= $(patsubst %,$(UHC_SRC_PREFIX)%.cag,HSAbsSyn)
UHC_AGHSAST_DPDS_SRC_CAG				:= $(patsubst %,$(UHC_SRC_PREFIX)%.cag,UHA_Syntax)
$(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.hs,$(UHC_AGHSAST_MAIN_SRC_CAG)) \
										: $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AGHSAST_DPDS_SRC_CAG))

UHC_AG_D_MAIN_SRC_CAG					:= $(UHC_AGHSAST_MAIN_SRC_CAG)
UHC_AG_S_MAIN_SRC_CAG					:= $(UHC_AGMAIN_MAIN_SRC_CAG)
UHC_AG_DS_MAIN_SRC_CAG					:= 
UHC_AG_ALL_MAIN_SRC_CAG					:= $(UHC_AG_D_MAIN_SRC_CAG) $(UHC_AG_S_MAIN_SRC_CAG) $(UHC_AG_DS_MAIN_SRC_CAG)

UHC_AG_ALL_DPDS_SRC_CAG					:= $(sort \
											$(UHC_AGMAIN_DPDS_SRC_CAG) \
											$(UHC_AGHSAST_DPDS_SRC_CAG) \
											)

# all src
UHC_ALL_CHUNK_SRC						:= $(UHC_AG_ALL_MAIN_SRC_CAG) $(UHC_AG_ALL_DPDS_SRC_CAG) $(UHC_HS_ALL_SRC_CHS)
UHC_ALL_SRC								:= $(UHC_ALL_CHUNK_SRC) $(UHC_RULES_ALL_SRC)

# distribution
UHC_DIST_FILES							:= $(UHC_ALL_SRC) $(UHC_MKF)

# derived
UHC_AG_D_MAIN_DRV_AG					:= $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AG_D_MAIN_SRC_CAG))
UHC_AG_S_MAIN_DRV_AG					:= $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AG_S_MAIN_SRC_CAG))
UHC_AG_DS_MAIN_DRV_AG					:= $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AG_DS_MAIN_SRC_CAG))
UHC_AG_ALL_MAIN_DRV_AG					:= $(UHC_AG_D_MAIN_DRV_AG) $(UHC_AG_S_MAIN_DRV_AG) $(UHC_AG_DS_MAIN_DRV_AG)

UHC_AG_D_MAIN_DRV_HS					:= $(UHC_AG_D_MAIN_DRV_AG:.ag=.hs)
UHC_AG_S_MAIN_DRV_HS					:= $(UHC_AG_S_MAIN_DRV_AG:.ag=.hs)
UHC_AG_DS_MAIN_DRV_HS					:= $(UHC_AG_DS_MAIN_DRV_AG:.ag=.hs)
UHC_AG_ALL_MAIN_DRV_HS					:= $(UHC_AG_D_MAIN_DRV_HS) $(UHC_AG_S_MAIN_DRV_HS) $(UHC_AG_DS_MAIN_DRV_HS)

UHC_AG_ALL_DPDS_DRV_AG					:= $(patsubst $(UHC_SRC_PREFIX)%.cag,$(UHC_BLD_VARIANT_PREFIX)%.ag,$(UHC_AG_ALL_DPDS_SRC_CAG))

# all dependents for a variant to kick of building
$(UHC_AG_ALL_MAIN_DRV_AG) $(UHC_AG_ALL_DPDS_DRV_AG): $(UHC_BLD_VARIANT_PREFIX)%.ag: $(UHC_SRC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(UHC_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(UHC_SHUFFLE_ORDER)" $< > $@

$(UHC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr -P$(UHC_BLD_VARIANT_PREFIX) $<

$(UHC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(UHC_BLD_VARIANT_PREFIX) $<

$(UHC_AG_DS_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dcfspr -P$(UHC_BLD_VARIANT_PREFIX) $<

$(UHC_HS_UTIL_DRV_HS): $(UHC_BLD_VARIANT_PREFIX)%.hs: $(UHC_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(UHC_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(UHC_SHUFFLE_ORDER)" $< > $@


