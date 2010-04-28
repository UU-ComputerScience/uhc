###########################################################################################
# location of text2text src
###########################################################################################

SRC_TEXT2TEXT_PREFIX						:= $(SRC_PREFIX)text2text/

###########################################################################################
# location of text2text build
###########################################################################################

TEXT2TEXT_BLD_PREFIX						:= $(BLD_PREFIX)text2text/

###########################################################################################
# this file
###########################################################################################

TEXT2TEXT_MKF								:= $(SRC_TEXT2TEXT_PREFIX)files.mk

###########################################################################################
# main + sources + dpds
###########################################################################################

TEXT2TEXT_MAIN								:= Text2Text

TEXT2TEXT_HS_MAIN_SRC_HS					:= $(addprefix $(SRC_TEXT2TEXT_PREFIX),$(TEXT2TEXT_MAIN).hs)
TEXT2TEXT_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_TEXT2TEXT_PREFIX)%.hs,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_HS_MAIN_SRC_HS))
TEXT2TEXT_HS_DPDS_SRC_HS					:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.hs,Common Plugin Text/Parser Text/Parser/Common Text/Parser/DocLaTeX)
TEXT2TEXT_HS_DPDS_DRV_HS					:= $(patsubst $(SRC_TEXT2TEXT_PREFIX)%.hs,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_HS_DPDS_SRC_HS))

TEXT2TEXT_AGTEXT_MAIN_SRC_AG				:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text)
TEXT2TEXT_AGTEXT_DPDS_SRC_AG				:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/AbsSyn)
$(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_AGTEXT_MAIN_SRC_AG)) \
											: $(TEXT2TEXT_AGTEXT_DPDS_SRC_AG)

TEXT2TEXT_TEXT2DOCLTX_MAIN_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/To/DocLaTeX)
TEXT2TEXT_TEXT2DOCLTX_DPDS_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/AbsSyn Text/To/Common)
$(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_TEXT2DOCLTX_MAIN_SRC_AG)) \
											: $(TEXT2TEXT_TEXT2DOCLTX_DPDS_SRC_AG)

TEXT2TEXT_TEXT2TWIKI_MAIN_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/To/TWiki)
TEXT2TEXT_TEXT2TWIKI_DPDS_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/AbsSyn Text/To/Common)
$(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_TEXT2TWIKI_MAIN_SRC_AG)) \
											: $(TEXT2TEXT_TEXT2TWIKI_DPDS_SRC_AG)

TEXT2TEXT_TEXT2UNIFCONT_MAIN_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/Trf/UniformContent)
TEXT2TEXT_TEXT2UNIFCONT_DPDS_SRC_AG			:= $(patsubst %,$(SRC_TEXT2TEXT_PREFIX)%.ag,Text/AbsSyn)
$(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_TEXT2UNIFCONT_MAIN_SRC_AG)) \
											: $(TEXT2TEXT_TEXT2UNIFCONT_DPDS_SRC_AG)

TEXT2TEXT_AG_D_MAIN_SRC_AG					:= $(TEXT2TEXT_AGTEXT_MAIN_SRC_AG)
TEXT2TEXT_AG_S_MAIN_SRC_AG					:= $(TEXT2TEXT_TEXT2DOCLTX_MAIN_SRC_AG) \
												$(TEXT2TEXT_TEXT2TWIKI_MAIN_SRC_AG) \
												$(TEXT2TEXT_TEXT2UNIFCONT_MAIN_SRC_AG)

TEXT2TEXT_AG_ALL_DPDS_SRC_AG				:= $(sort \
												$(TEXT2TEXT_TEXT2DOCLTX_DPDS_SRC_AG) \
												$(TEXT2TEXT_TEXT2TWIKI_DPDS_SRC_AG) \
												$(TEXT2TEXT_TEXT2UNIFCONT_DPDS_SRC_AG) \
												$(TEXT2TEXT_AGTEXT_DPDS_SRC_AG) \
												)

TEXT2TEXT_AG_ALL_MAIN_SRC_AG				:= $(TEXT2TEXT_AG_D_MAIN_SRC_AG) $(TEXT2TEXT_AG_S_MAIN_SRC_AG)

###########################################################################################
# all src
###########################################################################################

TEXT2TEXT_ALL_SRC							:= $(TEXT2TEXT_AG_ALL_MAIN_SRC_AG) $(TEXT2TEXT_AG_ALL_DPDS_SRC_AG) $(TEXT2TEXT_HS_MAIN_SRC_HS) $(TEXT2TEXT_HS_DPDS_SRC_HS)

###########################################################################################
# derived
###########################################################################################

TEXT2TEXT_AG_D_MAIN_DRV_HS					:= $(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_AG_D_MAIN_SRC_AG))
TEXT2TEXT_AG_S_MAIN_DRV_HS					:= $(patsubst $(SRC_TEXT2TEXT_PREFIX)%.ag,$(TEXT2TEXT_BLD_PREFIX)%.hs,$(TEXT2TEXT_AG_S_MAIN_SRC_AG))
TEXT2TEXT_AG_ALL_MAIN_DRV_HS				:= $(TEXT2TEXT_AG_D_MAIN_DRV_HS) $(TEXT2TEXT_AG_S_MAIN_DRV_HS)

TEXT2TEXT_HS_ALL_DRV_HS						:= $(TEXT2TEXT_HS_MAIN_DRV_HS) $(TEXT2TEXT_HS_DPDS_DRV_HS)

###########################################################################################
# flags
###########################################################################################

TEXT2TEXT_UUAGC_OPTS						:= --strictdata --strictwrap -O

###########################################################################################
# binary/executable
###########################################################################################

TEXT2TEXT_NAME								:= text2text
TEXT2TEXT_BLD_EXEC							:= $(BIN_PREFIX)$(TEXT2TEXT_NAME)$(EXEC_SUFFIX)
TEXT2TEXT									:= $(TEXT2TEXT_BLD_EXEC)

###########################################################################################
# distribution
###########################################################################################

TEXT2TEXT_DIST_FILES			:= $(TEXT2TEXT_ALL_SRC) $(TEXT2TEXT_MKF)

###########################################################################################
# make rules
###########################################################################################

$(TEXT2TEXT_NAME): $(TEXT2TEXT_BLD_EXEC)

$(TEXT2TEXT_BLD_EXEC): $(TEXT2TEXT_AG_ALL_MAIN_DRV_HS) $(TEXT2TEXT_HS_ALL_DRV_HS) $(LIB_EH_UTIL_INS_FLAG)
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_OPTIM) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -i$(TEXT2TEXT_BLD_PREFIX) $(TEXT2TEXT_BLD_PREFIX)$(TEXT2TEXT_MAIN).hs -o $@
	$(STRIP) $@

$(TEXT2TEXT_AG_D_MAIN_DRV_HS): $(TEXT2TEXT_BLD_PREFIX)%.hs: $(SRC_TEXT2TEXT_PREFIX)%.ag
	mkdir -p $(@D) ; \
	$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(TEXT2TEXT_UUAGC_OPTS) -P$(SRC_TEXT2TEXT_PREFIX) -o $@ $<

$(TEXT2TEXT_AG_S_MAIN_DRV_HS): $(TEXT2TEXT_BLD_PREFIX)%.hs: $(SRC_TEXT2TEXT_PREFIX)%.ag
	mkdir -p $(@D) ; \
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(TEXT2TEXT_UUAGC_OPTS) -P$(SRC_TEXT2TEXT_PREFIX) -o $@ $<

$(TEXT2TEXT_HS_ALL_DRV_HS): $(TEXT2TEXT_BLD_PREFIX)%.hs: $(SRC_TEXT2TEXT_PREFIX)%.hs
	mkdir -p $(@D) ; \
	cp $< $@

