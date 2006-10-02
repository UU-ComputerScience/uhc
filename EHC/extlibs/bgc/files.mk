# location of Boehm's garbage collector src
SRC_EXTLIBS_BGC_PREFIX			:= $(TOP_PREFIX)extlibs/bgc/
SRCABS_EXTLIBS_BGC_PREFIX		:= $(TOPABS_PREFIX)extlibs/bgc/

# this file
EXTLIBS_BGC_MKF					:= $(SRC_EXTLIBS_BGC_PREFIX)files.mk

# srcs
EXTLIBS_BGC_NAME				:= gc6.8
EXTLIBS_BGC_ARCHIVE				:= $(SRCABS_EXTLIBS_BGC_PREFIX)$(EXTLIBS_BGC_NAME).tar.gz

# options for C compiler
EHC_GCC_LD_OPTS					+= -l$(EXTLIBS_BGC_PKG_NAME)

# lib/cabal config
EXTLIBS_BGC_INS_FLAG			:= $(INSABS_FLAG_PREFIX)$(EXTLIBS_BGC_PKG_NAME)

# building location
BLDABS_EXTLIBS_BGC_PREFIX		:= $(BLDABS_PREFIX)bgc/

# install location
INSABS_EXTLIBS_BGC_PREFIX		:= $(INSABS_PREFIX)

# distribution
LIBUTIL_DIST_FILES				:= $(EXTLIBS_BGC_MKF) $(EXTLIBS_BGC_ARCHIVE)

# target
lib-bgc: $(EXTLIBS_BGC_INS_FLAG)

# rules
$(EXTLIBS_BGC_INS_FLAG): $(EXTLIBS_BGC_ARCHIVE) $(EXTLIBS_BGC_MKF)
	mkdir -p $(BLDABS_EXTLIBS_BGC_PREFIX) && \
	cd $(BLDABS_EXTLIBS_BGC_PREFIX) && \
	tar xfz $(EXTLIBS_BGC_ARCHIVE) && \
	cd $(EXTLIBS_BGC_NAME) && \
	./configure --prefix=$(INSABS_EXTLIBS_BGC_PREFIX) && \
	make && \
	make install && \
	touch $@
