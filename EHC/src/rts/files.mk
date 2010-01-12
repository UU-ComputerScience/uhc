###########################################################################################
# Prefixes
###########################################################################################

# Source location
RTS_SRC_PREFIX := $(SRC_PREFIX)rts/

# Build location
RTS_BLD_PREFIX := $(EHC_BLD_VARIANT_ASPECTS_PREFIX)rts/$(EHC_VARIANT_TARGET)/

# lib/cabal config
#RTS_PKG_NAME						:= EH-RTS # via mk/config.mk.in
RTS_INSTALLFORBLD_FLAG				:= $(INSTALLFORBLDABS_FLAG_PREFIX)$(RTS_PKG_NAME)

# Install location
RTS_LIB_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_LIB_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))
RTS_INC_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_INC_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))

# install
INSTALL_LIB_RTS						:= $(call FUN_MK_CLIB_FILENAME,$(RTS_LIB_PREFIX),$(RTS_PKG_NAME))

# this file
RTS_MKF						:= $(patsubst %,$(RTS_SRC_PREFIX)%.mk,files)



###########################################################################################
# names of sources files
###########################################################################################

RTS_SRC_CC_SHARED	:= \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc, \
        rts \
        llvm-gc \
        timing \
        base/panic \
        base/utils \
        base/sysalloc \
        mm/mm \
        mm/common \
        mm/basic/iterator \
        mm/basic/flexarray \
        mm/basic/freelistarray \
        mm/basic/dll \
        mm/basic/deque \
        mm/basic/rangemap \
        mm/pages \
        mm/allocator \
        mm/trace \
        mm/tracesupply \
        mm/collector \
        mm/space \
        mm/mutator \
        mm/roots \
        mm/weakptr \
        mm/plan \
        mm/pages/buddy \
        mm/tracesupply/group \
        mm/tracesupply/buffer \
        mm/tracesupply/bumpsupply \
        mm/tracesupply/supplyroots \
        mm/tracesupply/weakptrfinalizeque \
        mm/space/fragment \
        mm/space/copyspace \
        mm/semispace/ss \
        mm/semispace/sscollector \
        mm/allocator/listoffree \
        mm/allocator/bump \
        mm/weakptr/weakptrlist \
    )

RTS_SRC_CC_BYTECODE	:= \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc, \
        bc/interpreter \
        mm/semispace/gbssmutator \
        mm/semispace/gbssmodule \
        mm/gbm/gbtrace \
        mm/gbm/gbtracesupregs \
        mm/gbm/gbtracesupstack \
        mm/gbm/gbtracesupmodule \
    )

RTS_SRC_CC_WHOLEPROG := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc,\
    )

RTS_SRC_CH_SHARED := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.ch,\
        rts \
        rtsbase \
        config \
        timing \
        priminline \
        primdecl \
        base/sizes \
        base/bits \
        base/panic \
        base/utils \
        base/sysalloc \
        base/basictypes \
        base/types \
        event/event \
        mm/mmitf \
        mm/mm \
        mm/config \
        mm/common \
        mm/basic/iterator \
        mm/basic/flexarray \
        mm/basic/freelistarray \
        mm/basic/dll \
        mm/basic/deque \
        mm/basic/rangemap \
        mm/pages \
        mm/allocator \
        mm/trace \
        mm/tracesupply \
        mm/collector \
        mm/space \
        mm/mutatormutrec \
        mm/mutator \
        mm/roots \
        mm/weakptr \
        mm/plan \
        mm/module \
        mm/pages/buddy \
        mm/tracesupply/group \
        mm/tracesupply/buffer \
        mm/tracesupply/bumpsupply \
        mm/tracesupply/supplyroots \
        mm/tracesupply/weakptrfinalizeque \
        mm/space/fragment \
        mm/space/copyspace \
        mm/semispace/ss \
        mm/semispace/sscollector \
        mm/allocator/listoffree \
        mm/allocator/bump \
        mm/weakptr/weakptrlist \
    )

RTS_SRC_CH_BYTECODE := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.ch,\
        bc/types \
        bc/interpreter \
        mm/semispace/gbssmutator \
        mm/semispace/gbssmodule \
        mm/gbm/gbtrace \
        mm/gbm/gbtracesupregs \
        mm/gbm/gbtracesupstack \
        mm/gbm/gbtracesupmodule \
        bc/prim-const \
        bc/primdecl \
    )

RTS_SRC_CH_WHOLEPROG := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.ch,\
    	C/prim-const \
    )

PRM_SRC_CC_SHARED := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc,\
        prim-shared \
    )

PRM_SRC_CC_BYTECODE := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc,\
        bc/prim \
        bc/prim-handle \
        bc/prim-array \
        bc/prim-thread \
        bc/prim-integer \
    )

PRM_SRC_CC_WHOLEPROG := \
    $(patsubst %,$(RTS_SRC_PREFIX)%.cc,\
        C/prim \
    )


RTS_GEN_C_BYTECODE := \
    $(patsubst %,$(RTS_BLD_PREFIX)%.c,\
        bc/ccall \
    )

RTS_GEN_H_BYTECODE := \
    $(patsubst %,$(RTS_BLD_PREFIX)%.h,\
        bc/ccall \
    )



###########################################################################################
# special: src files coming from outside src/rts, copied into rts
###########################################################################################

# machine dependencies from ehclib
RTS_EHCLIB_SRC_H := $(EHCLIB_ASIS_RTS_SRC_ASIS)

# LibTomMath, multiple precision int library
RTS_LTM_SRC_C := $(if $(EHC_CFG_USE_LTM),$(LTM_SRC_C),)
RTS_LTM_SRC_H := $(if $(EHC_CFG_USE_LTM),$(LTM_SRC_H),)

###########################################################################################
# bundles of files
###########################################################################################

# unconditional as trigger for build

RTS_ALL_SRC := \
    $(RTS_SRC_CC_SHARED) \
    $(RTS_SRC_CC_BYTECODE) \
    $(RTS_SRC_CC_WHOLEPROG) \
    $(RTS_SRC_CH_SHARED) \
    $(RTS_SRC_CH_BYTECODE) \
    $(RTS_SRC_CH_WHOLEPROG) \
    $(PRM_SRC_CC_SHARED) \
    $(PRM_SRC_CC_BYTECODE) \
    $(PRM_SRC_CC_WHOLEPROG)


# conditional on specified target

RTS_SRC_CC := \
    $(RTS_SRC_CC_SHARED) \
    $(if $(EHC_CFG_TARGET_IS_bc),$(RTS_SRC_CC_BYTECODE),) \

PRM_SRC_CC := \
    $(PRM_SRC_CC_SHARED) \
    $(if $(EHC_CFG_TARGET_IS_bc),    $(PRM_SRC_CC_BYTECODE)  ,) \
    $(if $(EHC_CFG_TARGET_IS_C),     $(PRM_SRC_CC_WHOLEPROG) ,) \
    $(if $(EHC_CFG_TARGET_IS_llvm),  $(PRM_SRC_CC_WHOLEPROG) ,)

RTS_SRC_CH := \
    $(RTS_SRC_CH_SHARED)  \
    $(if $(EHC_CFG_TARGET_IS_bc),    $(RTS_SRC_CH_BYTECODE)  ,) \
    $(if $(EHC_CFG_TARGET_IS_C),     $(RTS_SRC_CH_WHOLEPROG) ,) \
    $(if $(EHC_CFG_TARGET_IS_llvm),  $(RTS_SRC_CH_WHOLEPROG) ,)

RTS_GEN_C := \
    $(RTS_GEN_C_SHARED)  \
    $(if $(EHC_CFG_TARGET_IS_bc),    $(RTS_GEN_C_BYTECODE)  ,) \
    $(if $(EHC_CFG_TARGET_IS_C),     $(RTS_GEN_C_WHOLEPROG) ,) \
    $(if $(EHC_CFG_TARGET_IS_llvm),  $(RTS_GEN_C_WHOLEPROG) ,)

RTS_GEN_H := \
    $(RTS_GEN_H_SHARED)  \
    $(if $(EHC_CFG_TARGET_IS_bc),    $(RTS_GEN_H_BYTECODE)  ,) \
    $(if $(EHC_CFG_TARGET_IS_C),     $(RTS_GEN_H_WHOLEPROG) ,) \
    $(if $(EHC_CFG_TARGET_IS_llvm),  $(RTS_GEN_H_WHOLEPROG) ,)


###########################################################################################
# Derived files and installable files
###########################################################################################

# external, outside rts
RTS_EHCLIB_DRV_H	:= $(patsubst $(EHCLIB_BASE_INC_SRC_PREFIX)%,$(RTS_BLD_PREFIX)%,$(RTS_EHCLIB_SRC_H))
RTS_LTM_DRV_H		:= $(patsubst $(EXTLIBS_PREFIX)%,$(RTS_BLD_PREFIX)%,$(RTS_LTM_SRC_H))
RTS_LTM_DRV_C		:= $(patsubst $(EXTLIBS_PREFIX)%,$(RTS_BLD_PREFIX)%,$(RTS_LTM_SRC_C))

RTS_LTM_DRV_O    	:= $(patsubst %.c,%.o,$(RTS_LTM_DRV_C))

RTS_ALLEXT_DRV_H	:= $(RTS_LTM_DRV_H) $(RTS_EHCLIB_DRV_H)
RTS_ALLEXT_DRV_C	:= $(RTS_LTM_DRV_C)
RTS_ALLEXT_DRV_O	:= $(RTS_LTM_DRV_O)

# internal, inside rts
RTS_DRV_C    	:= $(patsubst $(RTS_SRC_PREFIX)%.cc,$(RTS_BLD_PREFIX)%.c,$(RTS_SRC_CC))
RTS_DRV_H    	:= $(patsubst $(RTS_SRC_PREFIX)%.ch,$(RTS_BLD_PREFIX)%.h,$(RTS_SRC_CH))
PRM_DRV_C    	:= $(patsubst $(RTS_SRC_PREFIX)%.cc,$(RTS_BLD_PREFIX)%.c,$(PRM_SRC_CC))
PRM_DRV_H    	:= $(patsubst $(RTS_BLD_PREFIX)%.c,$(RTS_BLD_PREFIX)%.h,$(PRM_DRV_C))
RTS_DRV_O    	:= $(patsubst $(RTS_BLD_PREFIX)%.c,$(RTS_BLD_PREFIX)%.o,$(RTS_DRV_C) $(RTS_GEN_C))
PRM_DRV_O    	:= $(patsubst $(RTS_BLD_PREFIX)%.c,$(RTS_BLD_PREFIX)%.o,$(PRM_DRV_C))
RTS_INS_O    	:= $(patsubst $(RTS_BLD_PREFIX)%.o,$(RTS_LIB_PREFIX)%.o,$(RTS_DRV_O))
PRM_INS_O    	:= $(patsubst $(RTS_BLD_PREFIX)%.o,$(RTS_LIB_PREFIX)%.o,$(PRM_DRV_O))
RTS_INS_H    	:= $(patsubst $(RTS_BLD_PREFIX)%.h,$(RTS_INC_PREFIX)%.h,$(RTS_DRV_H) $(RTS_GEN_H) $(RTS_ALLEXT_DRV_H))
PRM_INS_H    	:= $(patsubst $(RTS_BLD_PREFIX)%.h,$(RTS_INC_PREFIX)%.h,$(PRM_DRV_H))

###########################################################################################
# Top level
###########################################################################################

# dispatch: to build (e.g.) 99/rts, build "rts-variant-dflt" with EHC_VARIANT set to "99"

$(patsubst %,%/rts,$(EHC_CODE_VARIANTS)): %/rts: $(RTS_ALL_SRC) $(RTS_MKF)
	$(MAKE) EHC_VARIANT=$(@D) rts-variant-dflt

# top level goal: install libraries, but also separate .o and .h files

rts-variant-dflt: $(INSTALL_LIB_RTS)

$(INSTALL_LIB_RTS): $(EHC_RTS_INSTALL_DPDS_EXTLIBS) $(RTS_DRV_O) $(PRM_DRV_O) $(RTS_MKF)  $(RTS_INS_H) $(PRM_INS_H) $(RTS_INS_O) $(PRM_INS_O) $(RTS_ALLEXT_DRV_O)
	mkdir -p $(@D)
	$(call FUN_LIB_MK_STATIC,$@,$(RTS_DRV_O) $(PRM_DRV_O) $(RTS_ALLEXT_DRV_O))
	touch $@


###########################################################################################
# Build rules
###########################################################################################

# Generate gbcall files

$(RTS_GEN_C): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) c 3 > $@

$(RTS_GEN_H): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) h 3 > $@


# Use Shuffle to convert .cc to .c (RTS and Primitives)

$(RTS_DRV_C) $(PRM_DRV_C): $(RTS_BLD_PREFIX)%.c: $(RTS_SRC_PREFIX)%.cc $(MK_CONFIG_MKF)
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@


# Use Shuffle to convert .ch to .h (RTS only)

$(RTS_DRV_H): $(RTS_BLD_PREFIX)%.h: $(RTS_SRC_PREFIX)%.ch $(MK_CONFIG_MKF)
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@


# Generate .h files for primitives flagged as "PRIM" in corresponding .c files (Primitives only)

$(PRM_DRV_H): %.h: %.c $(RTS_MKF)
	( echo "/* Generated from $< */" ; \
	  sed -n -e 's/^PRIM \(.*\)$$/extern \1 ;/p' < $< ; \
	) > $@


# Use the C compiler to compile .c to .o    (for primitives, use -O2 optimization)

$(RTS_DRV_O): $(RTS_BLD_PREFIX)%.o: $(RTS_BLD_PREFIX)%.c $(RTS_DRV_H) $(RTS_ALLEXT_DRV_H) $(PRM_DRV_H) $(RTS_GEN_H)
	$(GCC) $(call FUN_EHC_GCC_CC_OPTS,$(EHC_VARIANT_ASPECTS)) $(RTS_GCC_CC_OPTS_OPTIM) -o $@ -c $<

$(PRM_DRV_O): $(RTS_BLD_PREFIX)%.o: $(RTS_BLD_PREFIX)%.c $(RTS_DRV_H) $(RTS_ALLEXT_DRV_H) $(PRM_DRV_H) $(RTS_GEN_H)
	$(GCC) $(call FUN_EHC_GCC_CC_OPTS,$(EHC_VARIANT_ASPECTS)) $(RTS_GCC_CC_OPTS) -O2   -o $@ -c $<

$(RTS_LTM_DRV_O): $(RTS_BLD_PREFIX)%.o: $(RTS_BLD_PREFIX)%.c $(RTS_DRV_H) $(RTS_ALLEXT_DRV_H) $(RTS_GEN_H)
	$(GCC) $(call FUN_EHC_GCC_CC_OPTS,$(EHC_VARIANT_ASPECTS)) $(RTS_GCC_CC_OPTS_OPTIM) $(LTM_GCC_OPTS) -o $@ -c $<


# install .h files in the ehc/install/99/include directory

$(RTS_INS_H) $(PRM_INS_H): $(RTS_INC_PREFIX)%: $(RTS_BLD_PREFIX)%
	mkdir -p $(@D)
	install $< $@


# install special 'outside rts' .h files in the ehc/install/99/include directory

$(RTS_EHCLIB_DRV_H): $(RTS_BLD_PREFIX)%: $(EHCLIB_BASE_INC_SRC_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(RTS_LTM_DRV_H) $(RTS_LTM_DRV_C): $(RTS_BLD_PREFIX)%: $(EXTLIBS_PREFIX)%
	mkdir -p $(@D)
	install $< $@


# install .o files in the ehc/install/99/lib directory

$(RTS_INS_O) $(PRM_INS_O): $(RTS_LIB_PREFIX)%: $(RTS_BLD_PREFIX)%
	mkdir -p $(@D)
	install $< $@
