###########################################################################################
# which library files to get from which GHC packages
###########################################################################################

# all packages which are used
EHCLIB_SYNC_ALL_PKG						:= $(EHC_PACKAGES_ASSUMED)

# for each package a list of modules
EHCLIB_SYNC_ALL_PKG_base_ASIS			:= $(patsubst %,include/%.h,Typeable dirUtils consUtils)
EHCLIB_SYNC_ALL_PKG_base_C				:= $(patsubst %,cbits/%.c,)
EHCLIB_SYNC_ALL_PKG_base				:= $(patsubst %,%.hs,Foreign) \
											$(patsubst %,Data/%.hs,Bool Eq Ord Function Ratio List String Complex Ix Dynamic) \
											$(patsubst %,Unsafe/%.hs,Coerce) \
											$(patsubst %,Foreign/%.hs,C Marshal Marshal/Utils Marshal/Array C/String) \
											$(patsubst %,System/%.hs,IO/Unsafe Console/GetOpt Posix/Types) \
											$(patsubst %,Text/%.hs,ParserCombinators/ReadPrec Read Show Show/Functions) \
											$(patsubst %,Control/%.hs,Monad Category Monad/Instances)
EHCLIB_SYNC_ALL_PKG_array_ASIS			:= 
EHCLIB_SYNC_ALL_PKG_array_C				:= 
EHCLIB_SYNC_ALL_PKG_array				:=
EHCLIB_SYNC_ALL_PKG_containers_ASIS		:= 
EHCLIB_SYNC_ALL_PKG_containers_C		:= 
EHCLIB_SYNC_ALL_PKG_containers			:= $(patsubst %,Data/%.hs,Set Map)

# to compile as HS
EHCLIB_SYNC_ALL_PKG_SRC_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg))))
EHCLIB_SYNC_ALL_PKG_DRV_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg))))

# to compile as C
EHCLIB_SYNC_ALL_PKG_SRC_C				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_C)))
EHCLIB_SYNC_ALL_PKG_DRV_C				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_C)))

# copy as is
EHCLIB_SYNC_ALL_PKG_SRC_ASIS			:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_ASIS)))
EHCLIB_SYNC_ALL_PKG_DRV_ASIS			:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_ASIS)))

# all src & drv
EHCLIB_SYNC_ALL_PKG_SRC					:= $(EHCLIB_SYNC_ALL_PKG_SRC_HS) $(EHCLIB_SYNC_ALL_PKG_SRC_ASIS) $(EHCLIB_SYNC_ALL_PKG_SRC_C)
EHCLIB_SYNC_ALL_PKG_DRV					:= $(EHCLIB_SYNC_ALL_PKG_DRV_HS) $(EHCLIB_SYNC_ALL_PKG_DRV_ASIS) $(EHCLIB_SYNC_ALL_PKG_DRV_C)

###########################################################################################
# files, intermediate files, for ehclib
###########################################################################################

# this (and other make)file
EHCLIB_MKF								:= $(addprefix $(EHCLIB_SRC_PREFIX),files1.mk files2.mk)

# end products
# NOTE: library is just a bunch of compiled .hs files, triggered by compile of a Main
EHCLIB_MAIN								:= CompileAll
FUN_EHCLIB_ALL_LIB						= $(call FUN_INSTALL_VARIANT_PKGLIB_TARGET_PREFIX,$(1),$(2))$(EHCLIB_EHCBASE_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX)
FUN_EHCLIB_ALL_LIB2						= $(patsubst %,$(call FUN_EHCLIB_ALL_LIB,$(1),%),$(EHC_VARIANT_TARGETS))
EHCLIB_ALL_LIBS							= $(patsubst %,$(call FUN_EHCLIB_ALL_LIB,%,$(EHC_VARIANT_TARGET)),$(EHC_VARIANTS))
EHCLIB_ALL_LIBS2						= $(foreach variant,$(EHC_VARIANTS),$(call FUN_EHCLIB_ALL_LIB2,$(variant)))

# top level
FUN_EHCLIB_HS_MAIN_DRV_HS				= $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$(1)/$(EHCLIB_MAIN).hs
EHCLIB_HS_MAIN_DRV_HS					= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(call FUN_EHCLIB_HS_MAIN_DRV_HS,$(pkg)))

# shuffled .hs
EHCLIB_CHS_ALL_SRC_CHS					:= $(wildcard $(EHCLIB_BASE_SRC_PREFIX)*.chs $(EHCLIB_BASE_SRC_PREFIX)[A-Z]*/*.chs)
EHCLIB_CHS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%.chs,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCLIB_CHS_ALL_SRC_CHS))

# shuffled .c
EHCLIB_CC_ALL_SRC_CC					:= # not done yet
EHCLIB_CC_ALL_DRV_C						:= $(patsubst $(EHCLIB_SRC_PREFIX)%.chs,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCLIB_CC_ALL_SRC_CC))

# as haskell .hs, as is in svn repo
EHCLIB_HS_ALL_SRC_HS					:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),\
											$(foreach suff,hs lhs,\
											 $(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*/*.$(suff) \
											  )))

EHCLIB_HS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%,$(EHCLIB_HS_ALL_SRC_HS))

# as C .c, as is in svn repo
EHCLIB_C_ALL_SRC_C						:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/cbits/*.c))
EHCLIB_C_ALL_DRV_C						:= $(patsubst $(EHCLIB_SRC_PREFIX)%,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%,$(EHCLIB_C_ALL_SRC_C))

# as C .h include file, as is in svn repo
EHCLIB_ASIS_ALL_SRC_ASIS				:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/include/*.h))
EHCLIB_ASIS_ALL_DRV_ASIS				:= $(patsubst $(EHCLIB_SRC_PREFIX)%.h,$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)%.h,$(EHCLIB_ASIS_ALL_SRC_ASIS))

# as haskell, from frozen sync
EHCLIB_FROZEN_ALL_DRV_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg))))
EHCLIB_FROZEN_ALL_DRV_C					:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_C)))
EHCLIB_FROZEN_ALL_DRV_ASIS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_ASIS)))

# all
EHCLIB_ALL_SRC							:= $(EHCLIB_HS_ALL_SRC_HS) $(EHCLIB_CHS_ALL_SRC_CHS) $(EHCLIB_ASIS_ALL_SRC_ASIS)
EHCLIB_ALL_DRV_HS						:= $(EHCLIB_HS_ALL_DRV_HS) $(EHCLIB_CHS_ALL_DRV_HS) $(EHCLIB_FROZEN_ALL_DRV_HS)
EHCLIB_ALL_DRV_C						:= $(EHCLIB_C_ALL_DRV_C) $(EHCLIB_CC_ALL_DRV_C) $(EHCLIB_FROZEN_ALL_DRV_C)
EHCLIB_ALL_DRV_ASIS						:= $(EHCLIB_FROZEN_ALL_DRV_ASIS) $(EHCLIB_ASIS_ALL_DRV_ASIS)
EHCLIB_ALL_DRV							:= $(EHCLIB_ALL_DRV_HS) $(EHCLIB_ALL_DRV_ASIS) $(EHCLIB_ALL_DRV_C)

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

EHCLIB_DEBUG_OPTS						=
#EHCLIB_DEBUG_OPTS						= --no-hi-check
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= --target-variant=debug --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= -O0
#EHCLIB_DEBUG_OPTS						= -v3
#EHCLIB_DEBUG_OPTS						= -v4
#EHCLIB_DEBUG_OPTS						= -v4 --debug-stopat-hi-error=1 
#EHCLIB_DEBUG_OPTS						= -O0 --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1 -O0
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1 --priv=1

ehclib-variant-dflt: \
			$(if $(EHC_CFG_USE_CODEGEN),ehclib-codegentargetspecific-$(EHC_VARIANT_TARGET),) \
			$(if $(EHC_CFG_USE_PRELUDE),$(EHCLIB_ALL_DRV),) \
			$(EHC_INSTALL_VARIANT_ASPECTS_EXEC)
	$(if $(EHC_CFG_USE_PRELUDE) \
	     ,pkgs="" ; \
	     $(EHC_INSTALLABS_VARIANT_ASPECTS_EXEC) --meta-export-env ; \
	      for pkg in $(EHC_PACKAGES_ASSUMED) ; \
	      do \
	        mkdir -p $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$${pkg} ;\
	        hsFiles="`find $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg} -name '*.*hs'`" ; \
	        ( echo $${hsFiles} | \
	            sed -e 's+^+exposed-modules: +' \
	                -e "s+$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg}/++g" \
	                -e 's+\.[^.]*hs++g' \
	                -e 's+/+.+g' ; \
	        ) > $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$${pkg}/$(UHC_PKG_CONFIGFILE_NAME) ; \
	        $(EHC_INSTALLABS_VARIANT_ASPECTS_EXEC) \
	          --cpp \
	          $(EHCLIB_DEBUG_OPTS) \
	          --compile-only \
	          --pkg-hide-all \
	          --target=$(EHC_VARIANT_TARGET) \
	          --odir=$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$${pkg} \
	          --pkg-build=$${pkg} \
	          --import-path=$(call FUN_MK_PKG_INC_DIR,$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$${pkg}/) \
	          $${pkgs} \
	          $${hsFiles} \
	          `find $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg} -name '*.c'` \
	          +RTS -K30m ; \
	        err=$$? ; \
	        if test $${err} -ne 0 ; then exit $${err} ; fi ; \
	        pkgs="$${pkgs} --pkg-expose $${pkg}" ; \
	      done \
	     ,)

#	     ,$(EHC_INSTALL_VARIANT_ASPECTS_EXEC) --cpp --target=$(EHC_VARIANT_TARGET) $(EHCLIB_HS_MAIN_DRV_HS) \
#	      set -xv;\
#	          --pkg-build-libdir=$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX) \

###########################################################################################
# make all ehclibs target
###########################################################################################

ehclibs-variant-dflt: \
			$(patsubst %,$(call FUN_EHCLIB_ALL_LIB,$(EHC_VARIANT),%),$(EHC_VARIANT_TARGETS)) \
			$(EHC_INSTALL_VARIANT_ASPECTS_EXEC)

###########################################################################################
# code generation target specific make targets, for each $(EHC_TARGETS)
###########################################################################################

ehclib-codegentargetspecific-bc: $(if $(EHC_CFG_USE_GRIN),$(INSTALL_LIB_RTS),)

ehclib-codegentargetspecific-C: $(if $(EHC_CFG_USE_GRIN),$(INSTALL_LIB_RTS),)

ehclib-codegentargetspecific-jazy: $(if $(ENABLE_JAVA),$(INSTALL_LIB_JAZY),)

ehclib-codegentargetspecific-core:

ehclib-codegentargetspecific-clr:

ehclib-codegentargetspecific-llvm: $(if $(EHC_CFG_USE_GRIN),$(if $(ENABLE_LLVM),$(INSTALL_LIB_RTS),),)

###########################################################################################
# ehclib dispatch
###########################################################################################

# for (e.g.) 99/ehclib
$(patsubst %,%/ehclib,$(EHC_VARIANTS)): %/ehclib:
	$(MAKE) EHC_VARIANT=$(@D) $(call FUN_EHCLIB_ALL_LIB,$(@D),$(EHC_VARIANT_TARGET))

# for (e.g.) 99/ehclibs
$(patsubst %,%/ehclibs,$(EHC_VARIANTS)): %/ehclibs:
	$(MAKE) EHC_VARIANT=$(@D) ehclibs-variant-dflt

$(EHCLIB_ALL_LIBS2): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF) $(EHC_INSTALL_VARIANT_ASPECTS_EXEC) $(RTS_ALL_SRC)
	mkdir -p $(@D)
	$(MAKE) EHC_VARIANT=`       echo $(*D) | sed -n -e 's+$(call FUN_INSTALL_VARIANT_PKGLIB_TARGET_PREFIX,\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\1+p'` \
	        EHC_VARIANT_TARGET=`echo $(*D) | sed -n -e 's+$(call FUN_INSTALL_VARIANT_PKGLIB_TARGET_PREFIX,\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\2+p'` \
	        ehclib-variant-dflt
	touch $@

#$(EHCLIB_ALL_LIBS): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
#	$(MAKE) EHC_VARIANT=`echo $(*D) | sed -n -e 's+$(BLD_PREFIX)\([0-9]*\)/$(EHCLIB_EHCLIB_EHCBASE)+\1+p'` ehclib-variant-dflt

###########################################################################################
# rules for intermediate files
###########################################################################################

# top level 'compile all' module imports all other modules
#$(EHCLIB_HS_MAIN_DRV_HS): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
#	@pkg=`echo $@ | sed -n -e 's+$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)\([a-zA-Z0-9_]*\).*+\1+p'` ; \
#	pkgFileNames="" ; \
#	for f in $(EHCLIB_ALL_DRV_HS) ; \
#	do \
#	  fn=`echo $${f} | sed -n -e "s+$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$${pkg}/\([a-zA-Z0-9_/]*\).hs+\1+p"` ; \
#	  pkgFileNames="$${fn} $${pkgFileNames}" ; \
#	done ; \
#	(echo "module $(EHCLIB_MAIN) where" ; \
#	  for imp in $${pkgFileNames} ; \
#	  do \
#	    echo "import $${imp}" | sed -e "s+/+.+g" ; \
#	  done ; \
#	  echo "main = return ()" ; \
#	) > $@

# plainly copy .hs & .c files
$(EHCLIB_HS_ALL_DRV_HS) $(EHCLIB_C_ALL_DRV_C): $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%: $(EHCLIB_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@
	touch $@

# plainly copy .h files to install directly
$(EHCLIB_ASIS_ALL_DRV_ASIS): $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)%.h: $(EHCLIB_SRC_PREFIX)%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

# extract .hs & .c files from frozen archive
$(EHCLIB_FROZEN_ALL_DRV_HS) $(EHCLIB_FROZEN_ALL_DRV_C): $(EHCLIB_GHCSYNC_FROZEN)
	mkdir -p $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)
	cd $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX) && tar xfoz $< `echo $@ | sed -e 's+$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)++'`
	touch $@

# extract 'as is' files from frozen archive
$(EHCLIB_FROZEN_ALL_DRV_ASIS): $(EHCLIB_GHCSYNC_FROZEN)
	mkdir -p $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)
	cd $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX) && tar xfoz $< `echo $@ | sed -e 's+$(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)++'`
	touch $@

# generate .hs from .chs via shuffle
$(EHCLIB_CHS_ALL_DRV_HS): $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs: $(EHCLIB_SRC_PREFIX)%.chs
	mkdir -p $(@D)
	$(SHUFFLE_PLAIN) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

###########################################################################################
# ehclib sync with GHC libraries; needs to be done only when set of library module changes
###########################################################################################

# download ghc dist
$(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH): $(EHCLIB_MKF)
	mkdir -p $(@D)
	cd $(EHCLIB_BLD_SYNC_PREFIX) && curl -O $(EHCLIB_GHCSYNC_DOWNLOAD)

# extraction for frozen from ghc libraries
$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX),$(EHCLIB_SYNC_ALL_PKG_SRC)) \
			: $(EHCLIB_BLD_SYNC_SRC_PREFIX)% \
			: $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/%
	mkdir -p $(@D)
	cp $< $@
	touch $@

# template for extraction for a package
#define EHCLIB_PKG_TEMPLATE
#$$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX)$(1)/,$$(EHCLIB_SYNC_ALL_PKG_$(1)) $$(EHCLIB_SYNC_ALL_PKG_$(1)_ASIS) $$(EHCLIB_SYNC_ALL_PKG_$(1)_C)): $(EHCLIB_BLD_SYNC_SRC_PREFIX)$(1)/%: $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/$(1)/%
#	mkdir -p $$(@D)
#	cp $$< $$@
#	touch $$@
#endef

# expansion for each defined package
#ifneq ($(DEVELOPMENT_PLATFORM),CYGWIN)
#$(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(eval $(call EHCLIB_PKG_TEMPLATE,$(pkg))))
#endif


# construction of frozen archive
$(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH): $(EHCLIB_SYNC_ALL_PKG_DRV)
	cd $(EHCLIB_BLD_SYNC_SRC_PREFIX) && tar cfz $(EHCLIB_GHCSYNC_FROZEN) *

# use this target to download the ghc dist from which the frozen extract will be made
ehclib-ghc-sync-download: $(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH)

# use this target to extract what will be frozen
ehclib-ghc-sync-extract:
	cd $(EHCLIB_BLD_SYNC_PREFIX) && tar xfoj $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH) $(addprefix $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/,$(EHCLIB_SYNC_ALL_PKG_SRC))

# use this target to make the frozen extract from the ghc libraries, to be used as part of ehclib
ehclib-ghc-sync-frozen: $(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH)

# use this target to do last 2 of the three above
ehclib-ghc-sync2:
	$(MAKE) ehclib-ghc-sync-extract
	$(MAKE) ehclib-ghc-sync-frozen

# use this target to do all the three above
ehclib-ghc-sync:
	$(MAKE) ehclib-ghc-sync-download
	$(MAKE) ehclib-ghc-sync2




