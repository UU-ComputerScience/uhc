# 20110913 NOTE: dependency on this lib has been removed, latest version used was 7.0

# location of Boehm's garbage collector src
SRC_EXTLIBS_BGC_PREFIX					:= $(UHC_TOP_PREFIX)extlibs/bgc/
SRCABS_EXTLIBS_BGC_PREFIX				:= $(TOPABS2_PREFIX)extlibs/bgc/

# this file
EXTLIBS_BGC_MKF							:= $(SRC_EXTLIBS_BGC_PREFIX)files.mk

# srcs
EXTLIBS_BGC_NAME						:= gc-7.0
EXTLIBS_BGC_ARCHIVE						:= $(SRCABS_EXTLIBS_BGC_PREFIX)$(EXTLIBS_BGC_NAME).tar.gz

# lib/cabal config
EXTLIBS_BGC_INSTALL_FLAG				:= $(call FUN_INSTALLABS_FLAG_PREFIX,$(EHC_VARIANT_ASPECTS))$(EXTLIBS_BGC_PKG_NAME)

# options for C compiler
#EHC_GCC_LD_OPTS							+= -l$(EXTLIBS_BGC_PKG_NAME)

# dependencies for rts
#EHC_RTS_INSTALL_DPDS_EXTLIBS			+= $(if $(filter $(EHC_VARIANT),$(EHC_CODE_VARIANTS)),$(EXTLIBS_BGC_INSTALL_FLAG),)

# building location
BLDABS_EXTLIBS_BGC_PREFIX				:= $(BLDABS_PREFIX)bgc-non-threaded/

# install location
INSTALLFORBLDABS_EXTLIBS_BGC_PREFIX		:= $(INSTALLFORBLDABS_PREFIX)
INSTALLABS_EXTLIBS_BGC_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_SHARED_PREFIX,$(EHC_VARIANT_ASPECTS))
INSTALLABS_EXTLIBS_BGC_LIB_PREFIX		:= $(call FUN_INSTALLABS_VARIANT_LIB_SHARED_PREFIX,$(EHC_VARIANT_ASPECTS))
INSTALLABS_EXTLIBS_BGC_INC_PREFIX		:= $(call FUN_INSTALLABS_VARIANT_INC_SHARED_PREFIX,$(EHC_VARIANT_ASPECTS))

# files to remove before building, relative to build directory
EXTLIBS_BGC_REMOVE_BEFORE_BUILD			:= $(patsubst %,lib%.la,gc cord)

# distribution
LIBUTIL_DIST_FILES						:= $(EXTLIBS_BGC_MKF) $(EXTLIBS_BGC_ARCHIVE)

# target: build
lib-bgc: $(EXTLIBS_BGC_INS_NONTHREAD_FLAG)

# target: clean
bgc-clean:
	rm -rf $(BLDABS_EXTLIBS_BGC_PREFIX)

$(EXTLIBS_BGC_INSTALL_FLAG): $(EXTLIBS_BGC_ARCHIVE) $(EXTLIBS_BGC_MKF)
	export CFLAGS=$(EXTLIBS_GCC_CC_OPTS) ; \
	mkdir -p $(BLDABS_EXTLIBS_BGC_PREFIX) $(@D) $(INSTALLABS_EXTLIBS_BGC_LIB_PREFIX) $(INSTALLABS_EXTLIBS_BGC_INC_PREFIX) && \
	cd $(BLDABS_EXTLIBS_BGC_PREFIX) && \
	tar xfz $(EXTLIBS_BGC_ARCHIVE) && \
	cd $(EXTLIBS_BGC_NAME) && \
	rm -f $(EXTLIBS_BGC_REMOVE_BEFORE_BUILD) && \
	./configure \
	  --prefix=$(INSTALLABS_EXTLIBS_BGC_PREFIX) \
	  --libdir=$(call FUN_PREFIX2DIR,$(INSTALLABS_EXTLIBS_BGC_LIB_PREFIX)) \
	  --includedir=$(call FUN_PREFIX2DIR,$(INSTALLABS_EXTLIBS_BGC_INC_PREFIX)) \
	  --disable-threads && \
	make CFLAGS="$(CFLAGS) $(EXTLIBS_BGC_OPTS)" && \
	make install && \
	touch $@
