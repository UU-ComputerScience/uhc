# location of library src
SRC_RTS_PREFIX				:= $(SRC_PREFIX)rts/

# build location
RTS_BLD_RTS_PREFIX			:= $(BLD_PREFIX)rts/

# this file + other mk files
RTS_MKF						:= $(patsubst %,$(SRC_RTS_PREFIX)%.mk,files)

# lib/cabal config
#RTS_PKG_NAME				:= EH-RTS # via mk/config.mk.in
RTS_INS_FLAG				:= $(INSABS_FLAG_PREFIX)$(RTS_PKG_NAME)

# install location
INSABS_RTS_PREFIX			:= $(INSABS_PREFIX)

# inplace install
INSABS_LIB_RTS				:= $(INSABS_LIB_PREFIX)lib$(RTS_PKG_NAME)$(LIB_SUFFIX)

# main + sources + dpds, for .c/.h
RTS_C_RTS_SRC_C				:= $(patsubst %,$(SRC_RTS_PREFIX)%.c,rts prim)
RTS_C_RTS_SRC_H				:= $(patsubst %,$(SRC_RTS_PREFIX)%.h,rts config prim)

RTS_C_RTS_DRV_C				:= $(patsubst $(SRC_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_C))
RTS_C_RTS_DRV_O				:= $(patsubst $(SRC_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_SRC_C))
RTS_C_RTS_DRV_H				:= $(patsubst $(SRC_RTS_PREFIX)%.h,$(RTS_BLD_RTS_PREFIX)%.h,$(RTS_C_RTS_SRC_H))
RTS_C_RTS_INS_H				:= $(patsubst $(SRC_RTS_PREFIX)%.h,$(INSABS_INC_PREFIX)%.h,$(RTS_C_RTS_SRC_H))

RTS_ALL_SRC					:= $(RTS_C_RTS_SRC_H) $(RTS_C_RTS_SRC_C)

# target
rts: $(RTS_INS_FLAG)

# build rules
$(RTS_C_RTS_DRV_C) $(RTS_C_RTS_DRV_H): $(RTS_BLD_RTS_PREFIX)%: $(SRC_RTS_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(RTS_C_RTS_DRV_O): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_C_RTS_DRV_H)
	$(GCC) $(EHC_GCC_CC_OPTS) -o $@ -c $<

$(RTS_INS_FLAG): $(EXTLIBS_BGC_INS_FLAG) $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_INS_H) $(RTS_MKF)
	$(LIBTOOL_STATIC) $(INSABS_LIB_RTS) $(RTS_C_RTS_DRV_O)
	touch $@

# inplace install rules
$(RTS_C_RTS_INS_H): $(INSABS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

