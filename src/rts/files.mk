###########################################################################################
# location of these srcs
###########################################################################################

SRC_RTS_PREFIX				:= $(SRC_PREFIX)rts/

###########################################################################################
# this file
###########################################################################################

RTS_MKF						:= $(patsubst %,$(SRC_RTS_PREFIX)%.mk,files)

###########################################################################################
# build location
###########################################################################################

RTS_BLD_RTS_PREFIX			:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)rts/

###########################################################################################
# install location, config
###########################################################################################

# lib/cabal config
#RTS_PKG_NAME						:= EH-RTS # via mk/config.mk.in
RTS_INSTALLFORBLD_FLAG				:= $(INSTALLFORBLDABS_FLAG_PREFIX)$(RTS_PKG_NAME)

# install location, global only, to be obsolete
#INSTALLFORBLDABS_RTS_LIB_PREFIX		:= $(INSTALLFORBLDABS_VARIANT_ASPECTS_PREFIX)lib/
#INSTALLFORBLDABS_RTS_INC_PREFIX		:= $(INSTALLFORBLDABS_VARIANT_ASPECTS_PREFIX)include/

# install location
INSTALLABS_RTS_LIB_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_LIB_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))
INSTALLABS_RTS_INC_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_INC_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))

# inplace install, global only, to be obsolete
#INSTALLFORBLDABS_LIB_RTS			:= $(INSTALLFORBLDABS_RTS_LIB_PREFIX)lib$(RTS_PKG_NAME)$(LIB_SUFFIX)

# install
INSTALL_LIB_RTS						:= $(call FUN_MK_CLIB_FILENAME,$(INSTALLABS_RTS_LIB_PREFIX),$(RTS_PKG_NAME))

###########################################################################################
# names of sources + deriveds
###########################################################################################

# main + sources + dpds, for .c/.h


RTS_C_RTS_SRC_CC_SHARED		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
									rts utils llvm-gc timing prim-shared\
									mm/mm mm/common \
									mm/basic/flexarray mm/basic/dll mm/basic/deque mm/basic/rangemap \
									mm/pages mm/allocator mm/trace mm/tracesupply mm/collector mm/space mm/mutator mm/roots mm/plan \
									mm/pages/buddy \
									mm/tracesupply/group mm/tracesupply/buffer mm/tracesupply/bumpsupply mm/tracesupply/supplyroots \
									mm/space/fragment mm/space/copyspace \
									mm/semispace/ss mm/semispace/sscollector mm/semispace/gbssmutator mm/semispace/gbssmodule \
									mm/allocator/listoffree mm/allocator/bump \
								)
RTS_C_RTS_SRC_CC_BC		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
									bc/interpreter mm/gbm/gbtrace mm/gbm/gbtracesupregs mm/gbm/gbtracesupstack mm/gbm/gbtracesupmodule \
								)
RTS_C_RTS_SRC_CC_C		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
									C/prim \
								)
RTS_C_RTS_SRC_CC_ALL		:=  $(RTS_C_RTS_SRC_CC_SHARED) $(RTS_C_RTS_SRC_CC_BC) $(RTS_C_RTS_SRC_CC_C)

RTS_C_RTS_SRC_CC			:= $(RTS_C_RTS_SRC_CC_SHARED) \
								$(if $(EHC_CFG_TARGET_IS_bc),$(RTS_C_RTS_SRC_CC_BC),) \
								$(if $(EHC_CFG_TARGET_IS_C),$(RTS_C_RTS_SRC_CC_C),)


RTS_C_RTS_SRC_CC_OPTIM_O2_SHARED		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
								)
RTS_C_RTS_SRC_CC_OPTIM_O2_BC		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
								bc/prim bc/prim-handle bc/prim-array bc/prim-thread bc/prim-integer bc/prim-C \
								)
RTS_C_RTS_SRC_CC_OPTIM_O2_C		:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
								)
RTS_C_RTS_SRC_CC_OPTIM_O2_ALL		:=  $(RTS_C_RTS_SRC_CC_OPTIM_O2_SHARED) $(RTS_C_RTS_SRC_CC_OPTIM_O2_BC) $(RTS_C_RTS_SRC_CC_OPTIM_O2_C)

RTS_C_RTS_SRC_CC_OPTIM_O2			:= $(RTS_C_RTS_SRC_CC_OPTIM_O2_SHARED) \
								$(if $(EHC_CFG_TARGET_IS_bc), $(RTS_C_RTS_SRC_CC_OPTIM_O2_BC) ,) \
								$(if $(EHC_CFG_TARGET_IS_C),  $(RTS_C_RTS_SRC_CC_OPTIM_O2_C) ,)

RTS_H_RTS_SRC_CH_SHARED		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
									rts config sizes bits utils timing priminline \
									bc/interpreter \
									mm/mmitf mm/mm mm/config mm/common \
									mm/basic/flexarray mm/basic/dll mm/basic/deque mm/basic/rangemap \
									mm/pages mm/allocator mm/trace mm/tracesupply mm/collector mm/space mm/mutator mm/roots mm/plan mm/module \
									mm/pages/buddy \
									mm/tracesupply/group mm/tracesupply/buffer mm/tracesupply/bumpsupply mm/tracesupply/supplyroots \
									mm/space/fragment mm/space/copyspace \
									mm/semispace/ss mm/semispace/sscollector mm/semispace/gbssmutator mm/semispace/gbssmodule \
									mm/allocator/listoffree mm/allocator/bump \
								)
RTS_H_RTS_SRC_CH_BC		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
								mm/gbm/gbtrace mm/gbm/gbtracesupregs mm/gbm/gbtracesupstack mm/gbm/gbtracesupmodule bc/primdecl \
								)
RTS_H_RTS_SRC_CH_C		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
								)
RTS_H_RTS_SRC_CH_ALL		:=  $(RTS_H_RTS_SRC_CH_SHARED) $(RTS_H_RTS_SRC_CH_BC) $(RTS_H_RTS_SRC_CH_C)

RTS_H_RTS_SRC_CH			:= $(RTS_H_RTS_SRC_CH_SHARED)  \
								$(if $(EHC_CFG_TARGET_IS_bc), $(RTS_H_RTS_SRC_CH_BC) ,) \
								$(if $(EHC_CFG_TARGET_IS_C),  $(RTS_H_RTS_SRC_CH_C)  ,)

MAIN_C_MAIN_SRC_CC_SHARED		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
								)
MAIN_C_MAIN_SRC_CC_BC		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
								)
MAIN_C_MAIN_SRC_CC_C		:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
								mainSil \
								)
MAIN_C_MAIN_SRC_CC_ALL		:=  $(RTS_C_RTS_SRC_CH_SHARED) $(RTS_C_RTS_SRC_CH_BC) $(RTS_C_RTS_SRC_CH_C)

MAIN_C_MAIN_SRC_CC			:= $(RTS_C_RTS_SRC_CH_SHARED) \
								$(if $(EHC_CFG_TARGET_IS_bc), $(RTS_C_RTS_SRC_CH_BC) ,) \
								$(if $(EHC_CFG_TARGET_IS_C),  $(RTS_C_RTS_SRC_CH_C) ,)


RTS_C_RTS_GBCCALL_DRV_C		:= $(addprefix $(RTS_BLD_RTS_PREFIX),\
									$(if $(EHC_CFG_TARGET_IS_bc),bc/ccall.c,) \
									$(if $(EHC_CFG_TARGET_IS_C),,) \
								)
RTS_H_RTS_GBCCALL_DRV_H		:= $(patsubst %.c,%.h,$(RTS_C_RTS_GBCCALL_DRV_C))

RTS_C_RTS_DRV_C				:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC))
RTS_C_RTS_DRV_C_OPTIM_O2	:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC_OPTIM_O2))
RTS_C_RTS_DRV_C_OTHER		:= $(RTS_C_RTS_GBCCALL_DRV_C)
RTS_H_RTS_DRV_H				:= $(patsubst $(SRC_RTS_PREFIX)%.ch,$(RTS_BLD_RTS_PREFIX)%.h,$(RTS_H_RTS_SRC_CH))
RTS_H_RTS_DRV_H_OTHER		:= $(RTS_H_RTS_GBCCALL_DRV_H)
MAIN_C_MAIN_DRV_C			:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(MAIN_C_MAIN_SRC_CC))
RTS_H_RTS_PRIM_DRV_H		:= $(addprefix $(RTS_BLD_RTS_PREFIX),\
									prim-shared.h \
									$(if $(EHC_CFG_TARGET_IS_bc),$(addprefix bc/,prim.h prim-array.h prim-handle.h prim-thread.h prim-integer.h prim-C.h),) \
									$(if $(EHC_CFG_TARGET_IS_C),C/prim.h,) \
								)

RTS_H_RTS_ALL_DRV_H			:= $(RTS_H_RTS_DRV_H) $(RTS_H_RTS_PRIM_DRV_H) $(RTS_H_RTS_DRV_H_OTHER)
RTS_C_RTS_ALL_DRV_C			:= $(RTS_C_RTS_DRV_C) $(RTS_C_RTS_DRV_C_OPTIM_O2) $(RTS_C_RTS_DRV_C_OTHER)

MAIN_C_MAIN_ALL_DRV_C		:= $(MAIN_C_MAIN_DRV_C)

RTS_C_RTS_DRV_O				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C))
RTS_C_RTS_DRV_O_OPTIM_O2	:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C_OPTIM_O2))
RTS_C_RTS_DRV_O_OTHER		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C_OTHER))
RTS_O_RTS_ALL_DRV_O			:= $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER)

RTS_ALL_SRC					:= $(RTS_H_RTS_SRC_CH_ALL) $(RTS_C_RTS_SRC_CC_ALL) $(RTS_C_RTS_SRC_CC_OPTIM_O2_ALL) $(MAIN_C_MAIN_SRC_CC_ALL)

# for installation
#RTS_O_RTS_INSFORBLD_O		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.o,$(INSTALLFORBLDABS_RTS_LIB_PREFIX)%.o,$(RTS_O_RTS_ALL_DRV_O))
#RTS_H_RTS_INSFORBLD_H		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.h,$(INSTALLFORBLDABS_RTS_INC_PREFIX)%.h,$(RTS_H_RTS_ALL_DRV_H))
#MAIN_C_MAIN_INSFORBLD_C		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(INSTALLFORBLDABS_RTS_INC_PREFIX)%.c,$(MAIN_C_MAIN_ALL_DRV_C))

RTS_O_RTS_INSTALL_O			:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.o,$(INSTALLABS_RTS_LIB_PREFIX)%.o,$(RTS_O_RTS_ALL_DRV_O))
RTS_H_RTS_INSTALL_H			:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.h,$(INSTALLABS_RTS_INC_PREFIX)%.h,$(RTS_H_RTS_ALL_DRV_H))
MAIN_C_MAIN_INSTALL_C		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(INSTALLABS_RTS_INC_PREFIX)%.c,$(MAIN_C_MAIN_ALL_DRV_C))

###########################################################################################
# rts dispatch
###########################################################################################

# for (e.g.) 99/rts
$(patsubst %,%/rts,$(EHC_CODE_VARIANTS)): %/rts: $(RTS_ALL_SRC) $(RTS_MKF)
	$(MAKE) EHC_VARIANT=$(@D) rts-variant-dflt

# for (e.g.) 99/ehclibs
#$(patsubst %,%/ehclibs,$(EHC_VARIANTS)): %/ehclibs:
#	$(MAKE) EHC_VARIANT=$(@D) ehclibs-variant-dflt

#$(EHCLIB_ALL_LIBS2): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF) $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) $(RTS_ALL_SRC)
#	mkdir -p $(@D)
#	$(MAKE) EHC_VARIANT=`       echo $(*D) | sed -n -e 's+$(call FUN_INSTALL_VARIANT_LIB_TARGET_PREFIX,\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\1+p'` \
#	        EHC_VARIANT_TARGET=`echo $(*D) | sed -n -e 's+$(call FUN_INSTALL_VARIANT_LIB_TARGET_PREFIX,\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\2+p'` \
#	        ehclib-variant-dflt
#	touch $@

#$(EHCLIB_ALL_LIBS): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
#	$(MAKE) EHC_VARIANT=`echo $(*D) | sed -n -e 's+$(BLD_PREFIX)\([0-9]*\)/$(EHCLIB_EHCLIB_EHCBASE)+\1+p'` ehclib-variant-dflt

###########################################################################################
# top level build
###########################################################################################

rts-variant-dflt: $(INSTALL_LIB_RTS)

# library install
$(INSTALL_LIB_RTS): $(EHC_RTS_INSTALL_DPDS_EXTLIBS) $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER) $(RTS_H_RTS_INSTALL_H) $(RTS_O_RTS_INSTALL_O) $(MAIN_C_MAIN_INSTALL_C) $(RTS_MKF)
	mkdir -p $(@D)
	$(call FUN_LIB_MK_STATIC,$@,$(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER))
	touch $@

###########################################################################################
# build rules for subparts
###########################################################################################

$(RTS_C_RTS_GBCCALL_DRV_C): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) c 3 > $@

$(RTS_H_RTS_GBCCALL_DRV_H): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) h 3 > $@

$(RTS_H_RTS_DRV_H): $(RTS_BLD_RTS_PREFIX)%.h: $(SRC_RTS_PREFIX)%.ch
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(MAIN_C_MAIN_DRV_C): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_C) $(RTS_C_RTS_DRV_C_OPTIM_O2): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OTHER): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(RTS_GCC_CC_OPTS_OPTIM) $(call FUN_EHC_GCC_CC_OPTS,$(EHC_VARIANT_ASPECTS)) -o $@ -c $<

$(RTS_C_RTS_DRV_O_OPTIM_O2): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(call FUN_EHC_GCC_CC_OPTS,$(EHC_VARIANT_ASPECTS)) $(RTS_GCC_CC_OPTS) -O2 -o $@ -c $<

$(RTS_H_RTS_PRIM_DRV_H): %.h: %.c $(RTS_MKF)
	( echo "/* Generated from $< */" ; \
	  sed -n -e 's/^PRIM \(.*\)$$/extern \1 ;/p' < $< ; \
	) > $@

# inplace install rules, global for build, to be obsolete
#$(RTS_H_RTS_INSFORBLD_H): $(INSTALLFORBLDABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
#	mkdir -p $(@D)
#	install $< $@

#$(RTS_O_RTS_INSFORBLD_O): $(INSTALLFORBLDABS_RTS_LIB_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
#	mkdir -p $(@D)
#	install $< $@

#$(MAIN_C_MAIN_INSFORBLD_C): $(INSTALLFORBLDABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
#	mkdir -p $(@D)
#	install $< $@

# install rules
$(RTS_H_RTS_INSTALL_H): $(INSTALLABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(RTS_O_RTS_INSTALL_O): $(INSTALLABS_RTS_LIB_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(MAIN_C_MAIN_INSTALL_C): $(INSTALLABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

