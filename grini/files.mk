# location of agprimer src
GRINI_SRC_PREFIX						:= $(TOP_PREFIX)grini/

# this file
GRINI_MKF								:= $(GRINI_SRC_PREFIX)files.mk

# end products, binary, executable, etc
GRINI_EXEC_NAME							:= grini
GRINI_BLD_EXEC							:= $(GRIN_BLD_BIN_VARIANT_PREFIX)$(GRINI_EXEC_NAME)$(EXEC_SUFFIX)
GRINI_ALL_PUB_EXECS						:= $(patsubst %,$(GRIN_BIN_PREFIX)%/$(GRINI_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_PUB_VARIANTS))
GRINI_ALL_EXECS							:= $(patsubst %,$(GRIN_BIN_PREFIX)%/$(GRINI_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_VARIANTS))

# main + sources + dpds, for .chs
GRINI_MAIN								:= GRI
GRINI_HS_MAIN_SRC_CHS					:= $(patsubst %,$(GRINI_SRC_PREFIX)%.chs,$(GRINI_MAIN))
GRINI_HS_MAIN_DRV_HS					:= $(patsubst $(GRINI_SRC_PREFIX)%.chs,$(GRIN_BLD_VARIANT_PREFIX)%.hs,$(GRINI_HS_MAIN_SRC_CHS))

GRINI_HS_UTIL_SRC_CHS					:= $(patsubst %,$(GRINI_SRC_PREFIX)%.chs,GRICommon GRIParser GRIRun)
GRINI_HS_UTIL_DRV_HS					:= $(patsubst $(GRINI_SRC_PREFIX)%.chs,$(GRIN_BLD_VARIANT_PREFIX)%.hs,$(GRINI_HS_UTIL_SRC_CHS))

GRINI_HS_ALL_SRC_CHS					:= $(GRINI_HS_MAIN_SRC_CHS) $(GRINI_HS_UTIL_SRC_CHS)
GRINI_HS_ALL_DRV_HS						:= $(GRINI_HS_MAIN_DRV_HS) $(GRINI_HS_UTIL_DRV_HS)

# main + sources + dpds, for .cag
GRINI_AGSETUP_MAIN_SRC_CAG				:= $(patsubst %,$(GRINI_SRC_PREFIX)%.cag,GRISetup)
GRIN_AGSETUP_DPDS_SRC_CAG				:= $(patsubst %,$(GRIN_SRC_PREFIX)%.cag,GrinCodeAbsSyn)
$(patsubst $(GRINI_SRC_PREFIX)%.cag,$(GRIN_BLD_VARIANT_PREFIX)%.hs,$(GRINI_AGSETUP_MAIN_SRC_CAG)) \
										: $(patsubst $(GRIN_SRC_PREFIX)%.cag,$(GRIN_BLD_VARIANT_PREFIX)%.ag,$(GRIN_AGSETUP_DPDS_SRC_CAG))

GRINI_AG_S_MAIN_SRC_CAG					:= $(GRINI_AGSETUP_MAIN_SRC_CAG)
GRINI_AG_ALL_MAIN_SRC_CAG				:= $(GRINI_AG_S_MAIN_SRC_CAG)

# all src
GRINI_ALL_SRC							:= $(GRINI_AG_ALL_MAIN_SRC_CAG) $(GRINI_AG_ALL_DPDS_SRC_CAG) $(GRINI_HS_ALL_SRC_CHS)

# derived
GRINI_AG_S_MAIN_DRV_AG					:= $(patsubst $(GRINI_SRC_PREFIX)%.cag,$(GRIN_BLD_VARIANT_PREFIX)%.ag,$(GRINI_AG_S_MAIN_SRC_CAG))
GRINI_AG_ALL_MAIN_DRV_AG				:= $(GRINI_AG_S_MAIN_DRV_AG)

GRINI_AG_S_MAIN_DRV_HS					:= $(GRINI_AG_S_MAIN_DRV_AG:.ag=.hs)
GRINI_AG_ALL_MAIN_DRV_HS				:= $(GRINI_AG_S_MAIN_DRV_HS)

# distribution
GRINI_DIST_FILES						:= $(GRINI_ALL_SRC) $(GRINI_MKF)

# variant dispatch rules
$(GRINI_ALL_EXECS): %: $(GRINI_ALL_SRC) $(GRIN_ALL_SRC)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) GRIN_VARIANT=$(notdir $(*D)) grini-variant-$(notdir $(*D))

# rules
$(patsubst %,grini-variant-%,$(GRIN_VARIANTS)): grini-variant-dflt

grini-variant-dflt: $(GRINI_HS_ALL_DRV_HS) $(GRINI_AG_ALL_MAIN_DRV_HS) $(GRIN_AG_ALL_MAIN_DRV_HS) $(EHC_HS_ALL_DRV_HS)
	mkdir -p $(dir $(GRINI_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_OPTIM) -i$(GRIN_BLD_VARIANT_PREFIX) -i$(LIB_SRC_PREFIX) $(GRIN_BLD_VARIANT_PREFIX)$(GRINI_MAIN).hs -o $(GRINI_BLD_EXEC)

$(GRINI_AG_ALL_MAIN_DRV_AG) $(GRINI_AG_ALL_DPDS_DRV_AG): $(GRIN_BLD_VARIANT_PREFIX)%.ag: $(GRINI_SRC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(GRIN_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINI_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(GRIN_BLD_VARIANT_PREFIX) $<

$(GRINI_HS_MAIN_DRV_HS): $(GRIN_BLD_VARIANT_PREFIX)%.hs: $(GRINI_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(GRIN_VARIANT) --base=Main --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINI_HS_UTIL_DRV_HS): $(GRIN_BLD_VARIANT_PREFIX)%.hs: $(GRINI_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(GRIN_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

