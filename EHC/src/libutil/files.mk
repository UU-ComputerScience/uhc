# location of library src
SRC_LIBUTIL_PREFIX				:= $(SRC_PREFIX)libutil/

# this file + other mk files
LIBUTIL_MKF						:= $(patsubst %,$(SRC_LIBUTIL_PREFIX)%.mk,files)

# lib/cabal config
LIB_EH_UTIL_QUAL				:= EH.Util
LIB_EH_UTIL_QUAL_PREFIX			:= $(LIB_EH_UTIL_QUAL).
LIB_EH_UTIL_PKG_NAME			:= $(subst .,-,$(LIB_EH_UTIL_QUAL))
LIB_EH_UTIL_HS_PREFIX			:= $(SRC_LIBUTIL_PREFIX)$(subst .,$(PATH_SEP),$(LIB_EH_UTIL_QUAL_PREFIX))
LIB_EH_UTIL_CABAL_SRC			:= $(SRC_LIBUTIL_PREFIX)lib-eh-util.cabal
LIB_EH_UTIL_HS_SRC				:= $(wildcard $(LIB_EH_UTIL_HS_PREFIX)*.hs)
LIB_EH_UTIL_INS_FLAG			:= $(INS_FLAG_PREFIX)$(LIB_EH_UTIL_PKG_NAME)

# derived stuff
LIB_EH_UTIL_CABAL_DRV			:= $(patsubst $(SRC_LIBUTIL_PREFIX)%,$(BLD_LIBUTIL_PREFIX)%,$(LIB_EH_UTIL_CABAL_SRC))
LIB_EH_UTIL_HS_DRV				:= $(patsubst $(SRC_LIBUTIL_PREFIX)%,$(BLD_LIBUTIL_PREFIX)%,$(LIB_EH_UTIL_HS_SRC))
LIB_EH_UTIL_SETUP_HS_DRV		:= $(BLD_LIBUTIL_PREFIX)Setup.hs
#LIB_EH_UTIL_SETUP				:= $(BLD_LIBUTIL_PREFIX)setup
LIB_EH_UTIL_SETUP				:= ./setup

# rules
$(LIB_EH_UTIL_CABAL_DRV): $(LIB_EH_UTIL_CABAL_SRC)
	mkdir -p $(@D)
	cp $< $@

$(LIB_EH_UTIL_HS_DRV): $(BLD_LIBUTIL_PREFIX)%.hs: $(SRC_LIBUTIL_PREFIX)%.hs
	mkdir -p $(@D)
	cp $< $@

$(LIB_EH_UTIL_SETUP_HS_DRV): $(LIBUTIL_MKF)
	mkdir -p $(@D)
	@(echo "import Distribution.Simple" ; echo "main = defaultMain") > $@

$(LIB_EH_UTIL_SETUP): $(LIB_EH_UTIL_SETUP_HS_DRV)
	$(GHC) -package Cabal -o $@ $<

$(LIB_EH_UTIL_INS_FLAG): $(LIB_EH_UTIL_HS_DRV) $(LIB_EH_UTIL_CABAL_DRV) $(LIB_EH_UTIL_SETUP) $(LIBUTIL_MKF)
	cd $(BLD_LIBUTIL_PREFIX) && \
	$(LIB_EH_UTIL_SETUP) configure --prefix=$(INS_PREFIX) --user --scratchdir=$(BLD_LIBUTIL_PREFIX) && \
	$(LIB_EH_UTIL_SETUP) build && \
	$(LIB_EH_UTIL_SETUP) install --user && \
	echo $@ > $@

