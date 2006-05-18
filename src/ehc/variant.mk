# variant, EHC_VARIANT to be configured at top level
EHC_VARIANT								:= X
EHC_VARIANT_PREFIX						:= $(EHC_VARIANT)/
EHC_BLD_VARIANT_PREFIX					:= $(BLD_PREFIX)$(EHC_VARIANT_PREFIX)
EHC_BLD_LIB_VARIANT_PREFIX				:= $(EHC_BLD_VARIANT_PREFIX)lib/
EHC_BIN_PREFIX							:= $(BIN_PREFIX)
EHC_LIB_PREFIX							:= $(LIB_PREFIX)
EHC_BIN_VARIANT_PREFIX					:= $(EHC_BIN_PREFIX)$(EHC_VARIANT_PREFIX)
EHC_VARIANT_RULER_SEL					:= ().().()

# lib/cabal/module config
LIB_EHC_BASE							:= EH
LIB_EHC_QUAL							:= $(LIB_EHC_BASE)$(EHC_VARIANT)
LIB_EHC_QUAL_PREFIX						:= $(LIB_EHC_QUAL).
LIB_EHC_HS_PREFIX						:= $(subst .,$(PATH_SEP),$(LIB_EHC_QUAL_PREFIX))
LIB_EHC_PKG_NAME						:= $(subst .,-,$(LIB_EHC_QUAL))
LIB_EHC_INS_FLAG						:= $(INS_FLAG_PREFIX)$(LIB_EHC_PKG_NAME)

# tool use
LIB_EHC_SHUFFLE_DEFS					:= --def=EHC:$(LIB_EHC_QUAL_PREFIX) \
											--def=AST:$(LIB_EHC_AST_QUAL_PREFIX)
