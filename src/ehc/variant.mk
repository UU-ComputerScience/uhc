###########################################################################################
# Although named 'variant.mk' this file also holds lots of configuration, should be renamed someday
###########################################################################################

###########################################################################################
# aspects, EHC_ASPECTS to be configured at top level, for now here
###########################################################################################

EHC_ASPECTS								:= $(if $(ASPECTS),$(ASPECTS),base hmtyinfer codegen grin noHmTyRuler $(if $(ENABLE_JAVA),java jazy,) $(if $(ENABLE_LLVM),llvm,) $(if $(ENABLE_CLR),clr,))
EHC_ASPECTS_SUFFIX						:= $(if $(ASPECTS),-$(subst $(space),-,$(ASPECTS)),)
EHC_ASPECTS_SUFFIX2						:= $(subst -,,$(EHC_ASPECTS_SUFFIX))

###########################################################################################
# config depending on EHC_ASPECTS, EHC_VARIANT: Booleans telling whether some aspect is used
###########################################################################################

EHC_CFG_USE_GRIN						:= $(filter grin,$(EHC_ASPECTS))
EHC_CFG_USE_CODEGEN						:= $(filter $(EHC_VARIANT),$(EHC_CODE_VARIANTS))
EHC_CFG_USE_PRELUDE						:= $(filter $(EHC_VARIANT),$(EHC_PREL_VARIANTS) $(EHC_OTHER_PREL_VARIANTS))
EHC_CFG_IS_A_VARIANT					:= $(filter $(EHC_VARIANT),$(EHC_VARIANTS))

###########################################################################################
# variant, EHC_VARIANT to be configured at top level, by a recursive make invocation
###########################################################################################

EHC_VARIANT								:= X
EHC_VARIANT_ASPECTS						:= $(EHC_VARIANT)$(EHC_ASPECTS_SUFFIX)
EHC_VARIANT_ASPECTS_PREFIX				:= $(EHC_VARIANT_ASPECTS)/
EHC_BLD_VARIANT_ASPECTS_PREFIX			:= $(BLD_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
EHC_BARE_VARIANT_ASPECTS_PREFIX			:= $(BARE_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
EHC_BLD_LIBEHC_VARIANT_PREFIX			:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)lib-ehc/
EHC_BLD_BIN_VARIANT_PREFIX				:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)bin/
EHC_BLD_GEN_VARIANT_PREFIX				:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)gen/
EHC_BIN_PREFIX							:= $(BIN_PREFIX)
EHC_LIB_PREFIX							:= $(LIB_PREFIX)
EHC_BIN_VARIANT_ASPECTS_PREFIX			:= $(EHC_BIN_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
EHC_LIB_VARIANT_ASPECTS_PREFIX			:= $(EHC_LIB_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
EHC_VARIANT_RULER_SEL					:= ().().()

###########################################################################################
# name of executable
###########################################################################################

FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC		= $(call FUN_INSTALL_VARIANT_BIN_PREFIX,$(1))$(EHC_EXEC_NAME)$(EXEC_SUFFIX)
FUN_EHC_INSTALLABS_VARIANT_ASPECTS_EXEC		= $(call FUN_INSTALLABS_VARIANT_BIN_PREFIX,$(1))$(EHC_EXEC_NAME)$(EXEC_SUFFIX)

#EHC_INSTALL_VARIANT_ASPECTS_EXEC			:= $(EHC_BIN_VARIANT_ASPECTS_PREFIX)$(EHC_EXEC_NAME)$(EXEC_SUFFIX)
EHC_ALL_PUB_EXECS						:= $(patsubst %,$(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,%),$(EHC_PUB_VARIANTS))
EHC_ALL_EXECS							:= $(patsubst %,$(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,%),$(EHC_VARIANTS))
EHC_INSTALL_VARIANT_ASPECTS_EXEC			:= $(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,$(EHC_VARIANT_ASPECTS))
EHC_INSTALLABS_VARIANT_ASPECTS_EXEC			:= $(call FUN_EHC_INSTALLABS_VARIANT_ASPECTS_EXEC,$(EHC_VARIANT_ASPECTS))

###########################################################################################
# code generation targets, leading to target dependend locations
###########################################################################################

EHC_VARIANT_TARGETS						:= $(shell if test -x $(EHC_INSTALL_VARIANT_ASPECTS_EXEC); then $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --meta-targets; else echo bc; fi)
EHC_VARIANT_TARGET						:= $(shell if test -x $(EHC_INSTALL_VARIANT_ASPECTS_EXEC); then $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --meta-target-default; else echo bc; fi)
EHC_VARIANT_TARGET_PREFIX				:= $(EHC_VARIANT_TARGET)/

###########################################################################################
# lib/cabal/module config
###########################################################################################

LIB_EHC_BASE							:= EH
LIB_EHC_QUAL							:= $(subst _,x,$(LIB_EHC_BASE)$(EHC_VARIANT))$(EHC_ASPECTS_SUFFIX2)
LIB_EHC_QUAL_PREFIX						:= $(LIB_EHC_QUAL).
LIB_EHC_HS_PREFIX						:= $(subst .,$(PATH_SEP),$(LIB_EHC_QUAL_PREFIX))
LIB_EHC_PKG_NAMEBASE					:= $(GHC_PKG_NAME_PREFIX)$(subst .,-,$(LIB_EHC_QUAL))
LIB_EHC_PKG_NAME						:= $(LIB_EHC_PKG_NAMEBASE)
LIB_EHC_INS_FLAG						:= $(INSTALLFORBLDABS_FLAG_PREFIX)$(LIB_EHC_PKG_NAME)

EHC_BASE								:= $(LIB_EHC_BASE)C

###########################################################################################
# ehc runtime config
###########################################################################################

# assumed packages, useful only for prelude variants
EHC_PACKAGES_ASSUMED					:= base array
#EHC_PACKAGES_ASSUMED					:= base containers

###########################################################################################
# installation locations for ehc building time
###########################################################################################

INSTALLFORBLD_VARIANT_ASPECTS_PREFIX	:= $(INSTALLFORBLD_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
INSTALLFORBLDABS_VARIANT_ASPECTS_PREFIX	:= $(INSTALLFORBLDABS_PREFIX)$(EHC_VARIANT_ASPECTS_PREFIX)
INSTALLFORBLD_EHC_LIB_PREFIX			:= $(INSTALLFORBLD_PREFIX)lib/$(LIB_EHC_PKG_NAME)-$(EH_VERSION_SHORT)/
INSTALLFORBLDABS_EHC_LIB_PREFIX			:= $(INSTALLFORBLDABS2_PREFIX)lib/$(LIB_EHC_PKG_NAME)-$(EH_VERSION_SHORT)/
INSTALLFORBLD_EHC_LIB_AG_PREFIX			:= $(INSTALLFORBLD_EHC_LIB_PREFIX)ag/
INSTALLFORBLDABS_EHC_LIB_AG_PREFIX		:= $(INSTALLFORBLDABS2_EHC_LIB_PREFIX)ag/
###########################################################################################
# installation locations for ehc running time
###########################################################################################

# expanded to current variant
INSTALL_VARIANT_PREFIX					:= $(call FUN_INSTALL_VARIANT_PREFIX,$(EHC_VARIANT))
INSTALL_VARIANT_LIB_PREFIX				:= $(call FUN_INSTALL_VARIANT_LIB_PREFIX,$(EHC_VARIANT))
#INSTALL_VARIANT_LIB_TARGET_PREFIX		:= $(call FUN_INSTALL_VARIANT_LIB_TARGET_PREFIX,$(EHC_VARIANT),$(EHC_VARIANT_TARGET))
#INSTALL_VARIANT_PKGLIB_TARGET_PREFIX	:= $(call FUN_INSTALL_VARIANT_PKGLIB_TARGET_PREFIX,$(EHC_VARIANT),$(EHC_VARIANT_TARGET))

###########################################################################################
# further derived info
###########################################################################################

EHC_BLD_LIB_HS_VARIANT_PREFIX			:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)$(LIB_EHC_HS_PREFIX)
SRC_EHC_LIB_PREFIX						:= $(SRC_EHC_PREFIX)$(LIB_EHC_BASE)

###########################################################################################
# shuffle commandline config for building the building time ehc library
###########################################################################################

LIB_EHC_SHUFFLE_DEFS					:= --def=EH:$(LIB_EHC_QUAL_PREFIX) --def=VARIANT:$(EHC_VARIANT) --def="ASPECTS:$(EHC_ASPECTS)"

