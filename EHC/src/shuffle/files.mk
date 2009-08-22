###########################################################################################
# defines
###########################################################################################

# location of shuffle src
SRC_SHUFFLE_PREFIX	:= $(SRC_PREFIX)shuffle/

# location of shuffle build
SHUFFLE_BLD_PREFIX	:= $(BLD_PREFIX)shuffle/

# this file
SHUFFLE_MKF			:= $(SRC_SHUFFLE_PREFIX)files.mk

# main + sources + dpds
SHUFFLE_MAIN		:= Shuffle

#SHUFFLE_AG_MAIN_SRC	:= $(addprefix $(SRC_SHUFFLE_PREFIX),$(SHUFFLE_MAIN).ag)

SHUFFLE_HS_MAIN_SRC_HS					:= $(addprefix $(SRC_SHUFFLE_PREFIX),$(SHUFFLE_MAIN).hs)
SHUFFLE_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_SHUFFLE_PREFIX)%.hs,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_HS_MAIN_SRC_HS))
SHUFFLE_HS_DPDS_SRC_HS					:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.hs,Common ChunkParser CDocCommon)
SHUFFLE_HS_DPDS_DRV_HS					:= $(patsubst $(SRC_SHUFFLE_PREFIX)%.hs,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_HS_DPDS_SRC_HS))

SHUFFLE_AGMAIN_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,MainAG)
SHUFFLE_AGMAIN_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocAbsSyn ChunkAbsSyn \
											)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AGMAIN_MAIN_SRC_AG)) \
										: $(SHUFFLE_AGMAIN_DPDS_SRC_AG)

SHUFFLE_AGCDOC_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDoc)
SHUFFLE_AGCDOC_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocAbsSyn)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AGCDOC_MAIN_SRC_AG)) \
										: $(SHUFFLE_AGCDOC_DPDS_SRC_AG)

SHUFFLE_CDSUBS_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocSubst)
SHUFFLE_CDSUBS_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocAbsSyn CDocCommonAG)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_CDSUBS_MAIN_SRC_AG)) \
										: $(SHUFFLE_CDSUBS_DPDS_SRC_AG)

SHUFFLE_CDINLN_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocInline)
SHUFFLE_CDINLN_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,CDocAbsSyn)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_CDINLN_MAIN_SRC_AG)) \
										: $(SHUFFLE_CDINLN_DPDS_SRC_AG)

SHUFFLE_ASPEXP_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,AspectExpr)
SHUFFLE_ASPEXP_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,AspectExprAbsSyn)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_ASPEXP_MAIN_SRC_AG)) \
										: $(SHUFFLE_ASPEXP_DPDS_SRC_AG)

SHUFFLE_AEEVAL_MAIN_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,AspectExprEval)
SHUFFLE_AEEVAL_DPDS_SRC_AG				:= $(patsubst %,$(SRC_SHUFFLE_PREFIX)%.ag,AspectExprAbsSyn)
$(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AEEVAL_MAIN_SRC_AG)) \
										: $(SHUFFLE_AEEVAL_DPDS_SRC_AG)

SHUFFLE_AG_D_MAIN_SRC_AG				:= $(SHUFFLE_AGCDOC_MAIN_SRC_AG) $(SHUFFLE_ASPEXP_MAIN_SRC_AG)
SHUFFLE_AG_S_MAIN_SRC_AG				:= $(SHUFFLE_CDSUBS_MAIN_SRC_AG) $(SHUFFLE_CDINLN_MAIN_SRC_AG) $(SHUFFLE_AEEVAL_MAIN_SRC_AG)
SHUFFLE_AG_DS_MAIN_SRC_AG				:= $(SHUFFLE_AGMAIN_MAIN_SRC_AG)

SHUFFLE_AG_ALL_DPDS_SRC_AG				:= $(sort \
											$(SHUFFLE_AGMAIN_DPDS_SRC_AG) \
											$(SHUFFLE_CDSUBS_DPDS_SRC_AG) \
											$(SHUFFLE_AEEVAL_DPDS_SRC_AG) \
											$(SHUFFLE_CDINLN_DPDS_SRC_AG) \
											$(SHUFFLE_AGCDOC_DPDS_SRC_AG) \
											$(SHUFFLE_ASPEXP_DPDS_SRC_AG) \
											)

SHUFFLE_AG_ALL_MAIN_SRC_AG				:= $(SHUFFLE_AG_D_MAIN_SRC_AG) $(SHUFFLE_AG_S_MAIN_SRC_AG) $(SHUFFLE_AG_DS_MAIN_SRC_AG)



# all src
SHUFFLE_ALL_SRC							:= $(SHUFFLE_AG_ALL_MAIN_SRC_AG) $(SHUFFLE_AG_ALL_DPDS_SRC_AG) $(SHUFFLE_HS_MAIN_SRC_HS) $(SHUFFLE_HS_DPDS_SRC_HS)

# derived
SHUFFLE_AG_D_MAIN_DRV_HS				:= $(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AG_D_MAIN_SRC_AG))
SHUFFLE_AG_S_MAIN_DRV_HS				:= $(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AG_S_MAIN_SRC_AG))
SHUFFLE_AG_DS_MAIN_DRV_HS				:= $(patsubst $(SRC_SHUFFLE_PREFIX)%.ag,$(SHUFFLE_BLD_PREFIX)%.hs,$(SHUFFLE_AG_DS_MAIN_SRC_AG))
SHUFFLE_AG_ALL_MAIN_DRV_HS				:= $(SHUFFLE_AG_D_MAIN_DRV_HS) $(SHUFFLE_AG_S_MAIN_DRV_HS) $(SHUFFLE_AG_DS_MAIN_DRV_HS)

SHUFFLE_HS_ALL_DRV_HS					:= $(SHUFFLE_HS_MAIN_DRV_HS) $(SHUFFLE_HS_DPDS_DRV_HS)

# binary/executable
SHUFFLE_NAME		:= shuffle
SHUFFLE_BLD_EXEC	:= $(BIN_PREFIX)$(SHUFFLE_NAME)$(EXEC_SUFFIX)
SHUFFLE				:= $(SHUFFLE_BLD_EXEC)
SHUFFLE_HS			:= $(SHUFFLE) --hs --preamble=no --lhs2tex=no --line=yes --compiler=$(GHC_VERSION)
SHUFFLE_HS_PRE		:= $(SHUFFLE) --hs --preamble=yes --lhs2tex=no --line=yes --compiler=$(GHC_VERSION)
SHUFFLE_AG			:= $(SHUFFLE) --ag --preamble=no --lhs2tex=no --line=no --compiler=$(GHC_VERSION)
SHUFFLE_AG_PRE			:= $(SHUFFLE) --ag --preamble=yes --lhs2tex=no --line=no --compiler=$(GHC_VERSION)
SHUFFLE_PLAIN		:= $(SHUFFLE) --plain --preamble=no --lhs2tex=no --line=no
SHUFFLE_C			:= $(SHUFFLE_PLAIN)
SHUFFLE_JAVA		:= $(SHUFFLE_PLAIN)
# setting --line=yes for AG is not possible because of uuagc's weird interpretation of the layout rule


# distribution
SHUFFLE_DIST_FILES			:= $(SHUFFLE_ALL_SRC) $(SHUFFLE_MKF)

###########################################################################################
# targets
###########################################################################################

$(SHUFFLE_NAME): $(SHUFFLE_BLD_EXEC)

shuffle-clean:
	rm -rf $(SHUFFLE_BLD_EXEC) $(SHUFFLE_BLD_PREFIX)

###########################################################################################
# rules
###########################################################################################

$(SHUFFLE_BLD_EXEC): $(SHUFFLE_AG_ALL_MAIN_DRV_HS) $(SHUFFLE_HS_ALL_DRV_HS) $(LIB_EH_UTIL_INS_FLAG)
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_OPTIM) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -i$(SHUFFLE_BLD_PREFIX) $(SHUFFLE_BLD_PREFIX)$(SHUFFLE_MAIN).hs -o $@
	$(STRIP) $@

$(SHUFFLE_AG_D_MAIN_DRV_HS): $(SHUFFLE_BLD_PREFIX)%.hs: $(SRC_SHUFFLE_PREFIX)%.ag
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	mkdir -p $(@D) ; \
	$(AGC) --module=$(*F) -dr $(UUAGC_OPTS_WHEN_EHC) -P$(SRC_SHUFFLE_PREFIX) -o $@ $<

$(SHUFFLE_AG_S_MAIN_DRV_HS): $(SHUFFLE_BLD_PREFIX)%.hs: $(SRC_SHUFFLE_PREFIX)%.ag
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	mkdir -p $(@D) ; \
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) -P$(SRC_SHUFFLE_PREFIX) -o $@ $<

$(SHUFFLE_AG_DS_MAIN_DRV_HS): $(SHUFFLE_BLD_PREFIX)%.hs: $(SRC_SHUFFLE_PREFIX)%.ag
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	mkdir -p $(@D) ; \
	$(AGC) --module=$(*F) -dcfspr $(UUAGC_OPTS_WHEN_EHC) -P$(SRC_SHUFFLE_PREFIX) -o $@ $<

$(SHUFFLE_HS_ALL_DRV_HS): $(SHUFFLE_BLD_PREFIX)%.hs: $(SRC_SHUFFLE_PREFIX)%.hs
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	mkdir -p $(@D) ; \
	cp $< $@

