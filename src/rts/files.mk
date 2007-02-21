# location of library src
SRC_RTS_PREFIX				:= $(SRC_PREFIX)rts/

# build location
RTS_BLD_RTS_PREFIX			:= $(EHC_BLD_VARIANT_PREFIX)rts/

# this file + other mk files
RTS_MKF						:= $(patsubst %,$(SRC_RTS_PREFIX)%.mk,files)

# lib/cabal config
#RTS_PKG_NAME				:= EH-RTS # via mk/config.mk.in
RTS_INS_FLAG				:= $(INSABS_FLAG_PREFIX)$(RTS_PKG_NAME)

# install location
INSABS_RTS_PREFIX			:= $(INSABS_PREFIX)$(EHC_VARIANT_PREFIX)
INSABS_RTS_LIB_PREFIX		:= $(INSABS_PREFIX)$(EHC_VARIANT_PREFIX)lib/
INSABS_RTS_INC_PREFIX		:= $(INSABS_PREFIX)$(EHC_VARIANT_PREFIX)include/

# inplace install
INSABS_LIB_RTS				:= $(INSABS_RTS_LIB_PREFIX)lib$(RTS_PKG_NAME)$(LIB_SUFFIX)

# main + sources + dpds, for .c/.h
RTS_C_RTS_SRC_CC			:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,rts prim utils grinbc/grinbc)
RTS_C_RTS_SRC_CC_NO_OPTIM	:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,grinbc/gbprim)
RTS_H_RTS_SRC_CH			:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,rts config bits utils grinbc/grinbc)
MAIN_C_MAIN_SRC_CC			:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,mainSil)


RTS_C_RTS_DRV_C				:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC))
RTS_C_RTS_DRV_C_NO_OPTIM	:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC_NO_OPTIM))
RTS_H_RTS_DRV_H				:= $(patsubst $(SRC_RTS_PREFIX)%.ch,$(RTS_BLD_RTS_PREFIX)%.h,$(RTS_H_RTS_SRC_CH))
MAIN_C_MAIN_DRV_C			:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(MAIN_C_MAIN_SRC_CC))
RTS_H_RTS_PRIM_DRV_H		:= $(addprefix $(RTS_BLD_RTS_PREFIX),prim.h grinbc/gbprim.h)

RTS_H_RTS_ALL_DRV_H			:= $(RTS_H_RTS_DRV_H) $(RTS_H_RTS_PRIM_DRV_H)
MAIN_C_MAIN_ALL_DRV_C		:= $(MAIN_C_MAIN_DRV_C)

RTS_C_RTS_DRV_O				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C))
RTS_C_RTS_DRV_O_NO_OPTIM	:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C_NO_OPTIM))
RTS_H_RTS_INS_H				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.h,$(INSABS_RTS_INC_PREFIX)%.h,$(RTS_H_RTS_ALL_DRV_H))
MAIN_C_MAIN_INS_C			:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(INSABS_RTS_INC_PREFIX)%.c,$(MAIN_C_MAIN_ALL_DRV_C))

RTS_ALL_SRC					:= $(RTS_H_RTS_SRC_CH) $(RTS_C_RTS_SRC_CC) $(RTS_C_RTS_SRC_CC_NO_OPTIM) $(MAIN_C_MAIN_SRC_CC)

# target
#rts: $(RTS_INS_FLAG)

# build rules
$(RTS_H_RTS_DRV_H): $(RTS_BLD_RTS_PREFIX)%.h: $(SRC_RTS_PREFIX)%.ch
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(MAIN_C_MAIN_DRV_C): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_C) $(RTS_C_RTS_DRV_C_NO_OPTIM): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_O): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(RTS_GCC_CC_OPTS_OPTIM) $(EHC_GCC_CC_OPTS) -o $@ -c $<

$(RTS_C_RTS_DRV_O_NO_OPTIM): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(EHC_GCC_CC_OPTS) -o $@ -c $<

$(INSABS_LIB_RTS): $(EHC_RTS_DPDS_EXTLIBS) $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_NO_OPTIM) $(RTS_H_RTS_INS_H) $(MAIN_C_MAIN_INS_C) $(RTS_MKF)
	mkdir -p $(@D)
	$(call LIB_MK_STATIC,$@,$(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_NO_OPTIM))
	touch $@

$(RTS_H_RTS_PRIM_DRV_H): %.h: %.c $(RTS_MKF)
	( echo "/* Generated from $< */" ; \
	  sed -n -e 's/^PRIM \(.*\)$$/extern \1 ;/p' < $< ; \
	) > $@

# inplace install rules
$(RTS_H_RTS_INS_H): $(INSABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(MAIN_C_MAIN_INS_C): $(INSABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

