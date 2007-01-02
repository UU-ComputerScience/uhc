# location of GMP's src
SRC_EXTLIBS_GMP_PREFIX			:= $(TOP_PREFIX)extlibs/gmp/
SRCABS_EXTLIBS_GMP_PREFIX		:= $(TOPABS2_PREFIX)extlibs/gmp/

# this file
EXTLIBS_GMP_MKF					:= $(SRC_EXTLIBS_GMP_PREFIX)files.mk

# srcs
EXTLIBS_GMP_NAME				:= gmp-4.2.1
EXTLIBS_GMP_ARCHIVE				:= $(SRCABS_EXTLIBS_GMP_PREFIX)$(EXTLIBS_GMP_NAME).tar.gz

# lib/cabal config
EXTLIBS_GMP_INS_FLAG			:= $(INSABS_FLAG_PREFIX)$(EXTLIBS_GMP_PKG_NAME)

# options for C compiler
EHC_GCC_LD_OPTS					+= -l$(EXTLIBS_GMP_PKG_NAME)

# dependencies for rts
EHC_RTS_DPDS_EXTLIBS			+= $(if $(filter $(EHC_VARIANT),$(EHC_UHC_VARIANT)),$(EXTLIBS_GMP_INS_FLAG),)

# building location
BLDABS_EXTLIBS_GMP_PREFIX		:= $(BLDABS_PREFIX)gmp/

# install location
INSABS_EXTLIBS_GMP_PREFIX		:= $(INSABS_PREFIX)

# distribution
LIBUTIL_DIST_FILES				:= $(EXTLIBS_GMP_MKF) $(EXTLIBS_GMP_ARCHIVE)

# target
lib-gmp: $(EXTLIBS_GMP_INS_FLAG)

# rules
$(EXTLIBS_GMP_INS_FLAG): $(EXTLIBS_GMP_ARCHIVE) $(EXTLIBS_GMP_MKF)
	mkdir -p $(BLDABS_EXTLIBS_GMP_PREFIX) && \
	cd $(BLDABS_EXTLIBS_GMP_PREFIX) && \
	tar xfz $(EXTLIBS_GMP_ARCHIVE) && \
	cd $(EXTLIBS_GMP_NAME) && \
	./configure --prefix=$(INSABS_EXTLIBS_GMP_PREFIX) && \
	make && \
	make install && \
	touch $@
