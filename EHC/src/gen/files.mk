###########################################################################################
# location of utilities for generating various stuff
###########################################################################################

# this location
SRC_GEN_PREFIX							:= $(SRC_PREFIX)gen/
SRC_GEN_RTSGBCCALL_PREFIX				:= $(SRC_GEN_PREFIX)

# this file
GEN_MKF									:= $(SRC_GEN_PREFIX)files.mk

###########################################################################################
# end products, binary, executable, etc
###########################################################################################

# generate FFI wrappers for interpreter
# 2010: obsolete, now generated based on which wrappers are really required
GEN_RTSGBCCALL_EXEC_NAME				:= gen-rts-gb-ccall
GEN_RTSGBCCALL_BLD_EXEC					:= $(EHC_BLD_BIN_VARIANT_PREFIX)$(GEN_RTSGBCCALL_EXEC_NAME)$(EXEC_SUFFIX)

# generate library module with support instances for generic deriving for tuples
GEN_GENERTUPINST_EXEC_NAME				:= gen-gener-tuple-instances
GEN_GENERTUPINST_BLD_EXEC				:= $(EHC_BLD_BIN_VARIANT_PREFIX)$(GEN_GENERTUPINST_EXEC_NAME)$(EXEC_SUFFIX)

###########################################################################################
# main + sources + dpds, for .chs
###########################################################################################

# for GEN_RTSGBCCALL
GEN_RTSGBCCALL_MAIN						:= RtsGBCallC
GEN_RTSGBCCALL_MAIN_HS                 	:= $(EHC_BLD_GEN_VARIANT_PREFIX)$(GEN_RTSGBCCALL_MAIN).hs
GEN_RTSGBCCALL_HS_MAIN_SRC_CHS			:= $(patsubst %,$(SRC_GEN_PREFIX)%.chs,$(GEN_RTSGBCCALL_MAIN))
GEN_RTSGBCCALL_HS_MAIN_DRV_HS			:= $(patsubst $(SRC_GEN_PREFIX)%.chs,$(EHC_BLD_GEN_VARIANT_PREFIX)%.hs,$(GEN_RTSGBCCALL_HS_MAIN_SRC_CHS))

GEN_RTSGBCCALL_HS_ALL_SRC_CHS			:= $(GEN_RTSGBCCALL_HS_MAIN_SRC_CHS)
GEN_RTSGBCCALL_HS_ALL_DRV_HS			:= $(GEN_RTSGBCCALL_HS_MAIN_DRV_HS)

# for GEN_GENERTUPINST
GEN_GENERTUPINST_MAIN					:= GenericTupleInstances
GEN_GENERTUPINST_MAIN_HS                := $(EHC_BLD_GEN_VARIANT_PREFIX)$(GEN_GENERTUPINST_MAIN).hs
GEN_GENERTUPINST_HS_MAIN_SRC_CHS		:= $(patsubst %,$(SRC_GEN_PREFIX)%.chs,$(GEN_GENERTUPINST_MAIN))
GEN_GENERTUPINST_HS_MAIN_DRV_HS			:= $(patsubst $(SRC_GEN_PREFIX)%.chs,$(EHC_BLD_GEN_VARIANT_PREFIX)%.hs,$(GEN_GENERTUPINST_HS_MAIN_SRC_CHS))

GEN_GENERTUPINST_HS_ALL_SRC_CHS			:= $(GEN_GENERTUPINST_HS_MAIN_SRC_CHS)
GEN_GENERTUPINST_HS_ALL_DRV_HS			:= $(GEN_GENERTUPINST_HS_MAIN_DRV_HS)

###########################################################################################
# all dependents for a variant to kick of building
###########################################################################################

GEN_RTSGBCCALL_ALL_DPDS					:= $(GEN_RTSGBCCALL_HS_ALL_DRV_HS)
GEN_GENERTUPINST_ALL_DPDS				:= $(GEN_GENERTUPINST_HS_ALL_DRV_HS)

GEN_ALL_DPDS							:= $(GEN_RTSGBCCALL_ALL_DPDS) $(GEN_GENERTUPINST_ALL_DPDS)

###########################################################################################
# all src
###########################################################################################

GEN_ALL_CHUNK_SRC						:= $(GEN_RTSGBCCALL_HS_MAIN_SRC_CHS) $(GEN_GENERTUPINST_HS_MAIN_SRC_CHS)
GEN_ALL_SRC								:= $(GEN_ALL_CHUNK_SRC) $(GEN_MKF)

###########################################################################################
### Build rules for intermediates ####
###########################################################################################

# rules for library sources+derived
$(GEN_RTSGBCCALL_HS_MAIN_DRV_HS): $(EHC_BLD_GEN_VARIANT_PREFIX)%.hs: $(SRC_GEN_PREFIX)%.chs $(SHUFFLE) $(GEN_MKF) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GEN_RTSGBCCALL_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=Main --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

# rules for library sources+derived
$(GEN_GENERTUPINST_HS_MAIN_DRV_HS): $(EHC_BLD_GEN_VARIANT_PREFIX)%.hs: $(SRC_GEN_PREFIX)%.chs $(SHUFFLE) $(GEN_MKF) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GEN_GENERTUPINST_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=Main --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

###########################################################################################
### Build rules for endproducts ####
###########################################################################################

$(GEN_RTSGBCCALL_BLD_EXEC): $(GEN_RTSGBCCALL_HS_MAIN_DRV_HS)
	mkdir -p $(dir $(GEN_RTSGBCCALL_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -i$(EHC_BLD_GEN_VARIANT_PREFIX) $(GEN_RTSGBCCALL_MAIN_HS) -o $(GEN_RTSGBCCALL_BLD_EXEC)

$(GEN_GENERTUPINST_BLD_EXEC): $(GEN_GENERTUPINST_HS_MAIN_DRV_HS)
	mkdir -p $(dir $(GEN_GENERTUPINST_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -i$(EHC_BLD_GEN_VARIANT_PREFIX) $(GEN_GENERTUPINST_MAIN_HS) -o $(GEN_GENERTUPINST_BLD_EXEC)
