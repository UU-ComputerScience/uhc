###########################################################################################
# locations
###########################################################################################

# location of lib src
EHCLIB_EHCLIB							:= ehclib
EHCLIB_EHCLIB_PREFIX					:= $(EHCLIB_EHCLIB)/
EHCLIB_EHCBASE							:= ehcbase
EHCLIB_EHCBASE_PREFIX					:= $(EHCLIB_EHCBASE)/
EHCLIB_EHCLIB_EHCBASE					:= $(EHCLIB_EHCLIB_PREFIX)$(EHCLIB_EHCBASE)
EHCLIB_EHCLIB_EHCBASE_PREFIX			:= $(EHCLIB_EHCLIB_PREFIX)$(EHCLIB_EHCBASE_PREFIX)
EHCLIB_SRC_PREFIX						:= $(TOP_PREFIX)$(EHCLIB_EHCLIB_PREFIX)
EHCLIB_BASE_SRC_PREFIX					:= $(TOP_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)
EHCLIBABS_BASE_SRC_PREFIX				:= $(TOPABS_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)

# location of GHC sync'ed lib src
EHCLIB_GHCSYNC							:= ehclib-ghc-sync
EHCLIB_GHCSYNC_PREFIX					:= $(EHCLIB_GHCSYNC)/

# sync download location + name
EHCLIB_GHCSYNC_DOWNLOAD_PREFIX			:= http://www.haskell.org/ghc/dist/stable/dist/
EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE		:= ghc-6.10.1.20090106
EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH		:= $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)-src.tar.bz2
EHCLIB_GHCSYNC_DOWNLOAD					:= $(EHCLIB_GHCSYNC_DOWNLOAD_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH)

# extracted sync, frozen as .tgz into svn repo
EHCLIB_GHCSYNC_FROZEN_NAME_BASE			:= ehclib-ghc-sync-frozen
EHCLIB_GHCSYNC_FROZEN_NAME_ARCH			:= $(EHCLIB_GHCSYNC_FROZEN_NAME_BASE).tgz
EHCLIB_GHCSYNC_FROZEN					:= $(EHCLIBABS_BASE_SRC_PREFIX)$(EHCLIB_GHCSYNC_FROZEN_NAME_ARCH)

# build locations
EHCLIB_BLD_VARIANT_PREFIX				:= $(EHC_BLD_VARIANT_PREFIX)$(EHCLIB_EHCLIB_PREFIX)
EHCLIB_BASE_BLD_VARIANT_PREFIX			:= $(EHC_BLD_VARIANT_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)
EHCLIB_BLD_SYNC_PREFIX					:= $(BLD_PREFIX)$(EHCLIB_GHCSYNC_PREFIX)
EHCLIB_BLD_SYNC_SRC_PREFIX				:= $(EHCLIB_BLD_SYNC_PREFIX)src/

###########################################################################################
# which library files to get from which GHC packages
###########################################################################################

# all packages which are used
EHCLIB_SYNC_ALL_PKG						:= base
#EHCLIB_SYNC_ALL_PKG						:= base containers
# for each package a list of modules
EHCLIB_SYNC_ALL_PKG_base				:= $(patsubst %,Data/%.hs,Maybe)
EHCLIB_SYNC_ALL_PKG_containers			:= $(patsubst %,Data/%.hs,Set Map)
EHCLIB_SYNC_ALL_PKG_SRC_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg))))
EHCLIB_SYNC_ALL_PKG_DRV_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX),$(EHCLIB_SYNC_ALL_PKG_$(pkg))))

###########################################################################################
# files, intermediate files, for ehclib
###########################################################################################

# this file
EHCLIB_MKF								:= $(EHCLIB_SRC_PREFIX)files.mk

# end products
# NOTE: library is just a bunch of compiled .hs files, triggered by compile of a Main
EHCLIB_MAIN								:= CompileAll
EHCLIB_ALL_LIBS							:= $(patsubst %,$(BLD_PREFIX)%/$(EHCLIB_EHCLIB_EHCBASE_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX),$(EHC_PREL_VARIANTS))

# top level
EHCLIB_HS_MAIN_DRV_HS					:= $(EHCLIB_BASE_BLD_VARIANT_PREFIX)$(EHCLIB_MAIN).hs
EHCLIB_TRIGGER_EXEC						:= $(EHCLIB_BASE_BLD_VARIANT_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX)

# shuffled
EHCLIB_CHS_ALL_SRC_CHS					:= $(wildcard $(EHCLIB_BASE_SRC_PREFIX)*.chs $(EHCLIB_BASE_SRC_PREFIX)[A-Z]*/*.chs)
EHCLIB_CHS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%.chs,$(EHCLIB_BLD_VARIANT_PREFIX)%.hs,$(EHCLIB_CHS_ALL_SRC_CHS))

# as haskell, as is in svn repo
EHCLIB_HS_ALL_SRC_HS					:= $(wildcard $(EHCLIB_BASE_SRC_PREFIX)*.hs $(EHCLIB_BASE_SRC_PREFIX)[A-Z]*/*.hs)
EHCLIB_HS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%.hs,$(EHCLIB_BLD_VARIANT_PREFIX)%.hs,$(EHCLIB_HS_ALL_SRC_HS))

# as haskell, from frozen sync
EHCLIB_FROZEN_ALL_DRV_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BASE_BLD_VARIANT_PREFIX),$(EHCLIB_SYNC_ALL_PKG_$(pkg))))

# all
EHCLIB_ALL_SRC							:= $(EHCLIB_HS_ALL_SRC_HS) $(EHCLIB_CHS_ALL_SRC_CHS)
EHCLIB_ALL_DRV_HS						:= $(EHCLIB_HS_ALL_DRV_HS) $(EHCLIB_CHS_ALL_DRV_HS) $(EHCLIB_FROZEN_ALL_DRV_HS)

# distribution
EHCLIB_DIST_FILES						:= $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)

###########################################################################################
# files, intermediate files, for ghc sync
###########################################################################################

# ghc sync files
EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH		:= $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH)

# ghc frozen sync files
EHCLIB_GHCSYNC_FROZEN_DRV_ARCH			:= $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_FROZEN_NAME_ARCH)

###########################################################################################
# ehclib targets
###########################################################################################

ehclib-variant-dflt: $(EHCLIB_ALL_DRV_HS) $(EHCLIB_HS_MAIN_DRV_HS)
	$(EHC_BLD_EXEC) --cpp $(EHCLIB_HS_MAIN_DRV_HS)

###########################################################################################
# ehclib dispatch
###########################################################################################

$(patsubst $(BLD_PREFIX)%/$(EHCLIB_EHCBASE_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX),%,$(EHCLIB_ALL_LIBS)): %: $(BLD_PREFIX)%/$(EHCLIB_EHCBASE_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX)

$(EHCLIB_ALL_LIBS): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
	$(MAKE) EHC_VARIANT=`echo $(*D) | sed -n -e 's+$(BLD_PREFIX)\([0-9]*\)/$(EHCLIB_EHCLIB_EHCBASE)+\1+p'` ehclib-variant-dflt

###########################################################################################
# rules for intermediate files
###########################################################################################

# top level 'compile all' module imports all other modules
$(EHCLIB_HS_MAIN_DRV_HS): $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
	@(echo "module $(EHCLIB_MAIN) where" ; \
	  for imp in $(patsubst $(EHCLIB_BASE_BLD_VARIANT_PREFIX)%.hs,%,$(EHCLIB_ALL_DRV_HS)) ; \
	  do \
	    echo "import $${imp}" | sed -e "s+/+.+g" ; \
	  done ; \
	  echo "main = return ()" ; \
	) > $@

# plainly copy .hs files
$(EHCLIB_HS_ALL_DRV_HS): $(EHCLIB_BLD_VARIANT_PREFIX)%.hs: $(EHCLIB_SRC_PREFIX)%.hs
	mkdir -p $(@D)
	cp $< $@
	touch $@

# extract .hs files from frozen archive
$(EHCLIB_FROZEN_ALL_DRV_HS): $(EHCLIB_GHCSYNC_FROZEN)
	mkdir -p $(EHCLIB_BASE_BLD_VARIANT_PREFIX)
	cd $(EHCLIB_BASE_BLD_VARIANT_PREFIX) && tar xfoz $< `echo $@ | sed -e 's+$(EHCLIB_BASE_BLD_VARIANT_PREFIX)++'`
	touch $@

# generate .hs from .chs via shuffle
$(EHCLIB_CHS_ALL_DRV_HS): $(EHCLIB_BLD_VARIANT_PREFIX)%.hs: $(EHCLIB_SRC_PREFIX)%.chs
	mkdir -p $(@D)
	$(SHUFFLE_PLAIN) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

###########################################################################################
# ehclib sync with GHC libraries
###########################################################################################

# download ghc dist
$(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH): $(EHCLIB_MKF)
	mkdir -p $(@D)
	cd $(EHCLIB_BLD_SYNC_PREFIX) && curl -O $(EHCLIB_GHCSYNC_DOWNLOAD)

# template for extraction for a package
define EHCLIB_PKG_TEMPLATE
$$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX),$$(EHCLIB_SYNC_ALL_PKG_$(1))): $(EHCLIB_BLD_SYNC_SRC_PREFIX)%: $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/$(1)/%
	mkdir -p $$(@D)
	cp $$< $$@
	touch $$@
endef

# expansion for each defined package
$(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(eval $(call EHCLIB_PKG_TEMPLATE,$(pkg))))

# construction of frozen archive
$(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH): $(EHCLIB_SYNC_ALL_PKG_DRV_HS)
	cd $(EHCLIB_BLD_SYNC_SRC_PREFIX) && tar cfz $(EHCLIB_GHCSYNC_FROZEN) *

# use this target to download the ghc dist from which the frozen extract will be made
ehclib-ghc-sync-download: $(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH)

# use this target to extract what will be frozen
ehclib-ghc-sync-extract:
	cd $(EHCLIB_BLD_SYNC_PREFIX) && tar xfoj $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH) $(addprefix $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/,$(EHCLIB_SYNC_ALL_PKG_SRC_HS))

# use this target to make the frozen extract from the ghc libraries, to be used as part of ehclib
ehclib-ghc-sync-frozen: $(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH)
	cp $< $(EHCLIB_BASE_SRC_PREFIX)

# use this target to do all the three above
ehclib-ghc-sync:
	$(MAKE) ehclib-ghc-sync-download
	$(MAKE) ehclib-ghc-sync-extract
	$(MAKE) ehclib-ghc-sync-frozen
