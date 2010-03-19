# location of library src
SRC_LIBUTIL_PREFIX				:= $(SRC_PREFIX)libutil/

# this file + other mk files
LIBUTIL_MKF						:= $(patsubst %,$(SRC_LIBUTIL_PREFIX)%.mk,files)

# lib/cabal config
LIB_EH_UTIL_QUAL				:= EH.Util
LIB_EH_UTIL_QUAL_PREFIX			:= $(LIB_EH_UTIL_QUAL).
LIB_EH_UTIL_PKG_NAME			:= $(GHC_PKG_NAME_PREFIX)$(subst .,-,$(LIB_EH_UTIL_QUAL))
LIB_EH_UTIL_HS_PREFIX			:= $(SRC_LIBUTIL_PREFIX)$(subst .,$(PATH_SEP),$(LIB_EH_UTIL_QUAL_PREFIX))
LIB_EH_UTIL_HS_SRC				:= $(wildcard $(LIB_EH_UTIL_HS_PREFIX)*.hs) $(wildcard $(LIB_EH_UTIL_HS_PREFIX)*/*.hs)
LIB_EH_UTIL_HS_MOD				:= $(subst $(PATH_SEP),.,$(patsubst $(LIB_EH_UTIL_HS_PREFIX)%.hs,%,$(LIB_EH_UTIL_HS_SRC)))
LIB_EH_UTIL_INS_FLAG			:= $(INSTALLFORBLDABS_FLAG_PREFIX)$(LIB_EH_UTIL_PKG_NAME)

# derived stuff
LIB_EH_UTIL_CABAL_DRV			:= $(BLD_LIBUTIL_PREFIX)lib-$(GHC_PKG_NAME_PREFIX)eh-util.cabal
LIB_EH_UTIL_HS_DRV				:= $(patsubst $(SRC_LIBUTIL_PREFIX)%,$(BLD_LIBUTIL_PREFIX)%,$(LIB_EH_UTIL_HS_SRC))
LIB_EH_UTIL_SETUP_HS_DRV		:= $(BLD_LIBUTIL_PREFIX)Setup.hs
LIB_EH_UTIL_SETUP2				:= $(BLD_LIBUTIL_PREFIX)setup$(EXEC_SUFFIX)
LIB_EH_UTIL_SETUP				:= ./setup$(EXEC_SUFFIX)

# distribution
LIBUTIL_DIST_FILES				:= $(LIBUTIL_MKF) $(LIB_EH_UTIL_HS_SRC)

# target
libutil-eh: $(LIB_EH_UTIL_INS_FLAG)

libutil-clean:
	rm -rf $(LIB_EH_UTIL_INS_FLAG) ; \
	if test -x $(LIB_EH_UTIL_SETUP2) ; \
	then \
	  cd $(BLD_LIBUTIL_PREFIX) && \
	  $(LIB_EH_UTIL_SETUP) unregister ; \
	fi
	rm -rf $(BLD_LIBUTIL_PREFIX)

# rules
$(LIB_EH_UTIL_CABAL_DRV): $(LIBUTIL_MKF) $(LIB_EH_UTIL_HS_SRC)
	mkdir -p $(@D)
	$(call FUN_GEN_CABAL_LIB \
		, $(LIB_EH_UTIL_PKG_NAME) \
		, $(EH_VERSION_SHORT) \
		, mtl binary bytestring uulib \
		,  \
		, General purpose utilities for EH \
		, $(patsubst %,$(LIB_EH_UTIL_QUAL_PREFIX)%,$(LIB_EH_UTIL_HS_MOD)) \
		, \
	) > $@

$(LIB_EH_UTIL_HS_DRV): $(BLD_LIBUTIL_PREFIX)%.hs: $(SRC_LIBUTIL_PREFIX)%.hs
	mkdir -p $(@D)
	cp $< $@

$(LIB_EH_UTIL_SETUP_HS_DRV): $(LIBUTIL_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_EH_UTIL_SETUP2): $(LIB_EH_UTIL_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_EH_UTIL_INS_FLAG): $(LIB_EH_UTIL_HS_DRV) $(LIB_EH_UTIL_CABAL_DRV) $(LIB_EH_UTIL_SETUP2) $(LIBUTIL_MKF)
	mkdir -p $(@D)
	cd $(BLD_LIBUTIL_PREFIX) && \
	$(LIB_EH_UTIL_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSTALLFORBLDABS_PREFIX) $(CABAL_OPT_INSTALL_LOC) && \
	$(LIB_EH_UTIL_SETUP) build && \
	$(LIB_EH_UTIL_SETUP) install && \
	echo $@ > $@

