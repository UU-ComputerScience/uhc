###########################################################################################
# versions for builtin libraries, currently here and not done via cabal
###########################################################################################

# construct unversioned pkg name
# $1: unversioned pkg name
FUN_PKG_VERSIONED						= $(1)-$(EHCLIB_PKG_$(call FUN_STRIP_DASH,$(1))_VERSION)

# extract unversioned pkg name
# $1: versioned pkg name
#FUN_PKG_UNVERSIONED						= $(patsubst %-%,,$(1))

EHCLIB_PKG_uhcbase_VERSION				:= $(EH_VERSION_FULL)
EHCLIB_PKG_base_VERSION					:= 3.0.0.0
EHCLIB_PKG_array_VERSION				:= 1.0.0.0
EHCLIB_PKG_filepath_VERSION				:= 1.1.0.4
EHCLIB_PKG_oldlocale_VERSION			:= 1.0.0.2
EHCLIB_PKG_oldtime_VERSION				:= 1.0.0.4
EHCLIB_PKG_unix_VERSION					:= 1.0.0.0
EHCLIB_PKG_directory_VERSION			:= 1.0.0.0
EHCLIB_PKG_random_VERSION				:= 1.0.0.2
EHCLIB_PKG_haskell98_VERSION			:= 1.0.1.1

#EHC_PACKAGES_ASSUMED_VERSIONED			:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(call FUN_PKG_VERSIONED,$(pkg)))

###########################################################################################
# which library files to get from which GHC packages
###########################################################################################

# all packages which are used
EHCLIB_SYNC_ALL_PKG						:= $(EHC_PACKAGES_ASSUMED)

# for each package a list of modules
EHCLIB_SYNC_ALL_PKG_base_ASIS			:= 
EHCLIB_SYNC_ALL_PKG_base_C				:= 
EHCLIB_SYNC_ALL_PKG_base				:= 
EHCLIB_SYNC_ALL_PKG_array_ASIS			:= 
EHCLIB_SYNC_ALL_PKG_array_C				:= 
EHCLIB_SYNC_ALL_PKG_array				:=
EHCLIB_SYNC_ALL_PKG_containers_ASIS		:= 
EHCLIB_SYNC_ALL_PKG_containers_C		:= 
EHCLIB_SYNC_ALL_PKG_containers			:= 

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
# FUN_EHCLIB_ALL_LIB						= $(call FUN_INSTALL_VARIANT_PKGLIB_TARGET_PREFIX,$(1),$(2))$(EHCLIB_EHCBASE_PREFIX)$(EHCLIB_MAIN)$(EXEC_SUFFIX)
FUN_EHCLIB_ALL_LIB						= $(call FUN_INSTALL_PKG_VARIANT_TARGET_PREFIX,$(call FUN_PKG_VERSIONED,$(EHCLIB_EHCBASE)),$(1),$(2))$(EHCLIB_MAIN)$(EXEC_SUFFIX)
#FUN_EHCLIB_ALL_LIB						= $(call FUN_INSTALL_PKG_VARIANT_TARGET_PREFIX,$(EHCLIB_EHCBASE),$(1),$(2))$(EHCLIB_MAIN)$(EXEC_SUFFIX)
FUN_EHCLIB_ALL_LIB2						= $(patsubst %,$(call FUN_EHCLIB_ALL_LIB,$(1),%),$(EHC_VARIANT_TARGETS))
EHCLIB_ALL_LIBS							:= $(patsubst %,$(call FUN_EHCLIB_ALL_LIB,%,$(EHC_VARIANT_TARGET)),$(EHC_VARIANTS))
EHCLIB_ALL_LIBS2						:= $(foreach variant,$(EHC_VARIANTS),$(call FUN_EHCLIB_ALL_LIB2,$(variant)))

# top level
#FUN_EHCLIB_HS_MAIN_DRV_HS				= $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$(1)/$(EHCLIB_MAIN).hs
#EHCLIB_HS_MAIN_DRV_HS					= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(call FUN_EHCLIB_HS_MAIN_DRV_HS,$(pkg)))

# shuffled .hs
EHCLIB_CHS_ALL_SRC_CHS					:= $(wildcard $(EHCLIB_UHCBASE_SRC_PREFIX)*.chs $(EHCLIB_UHCBASE_SRC_PREFIX)[A-Z]*/*.chs) \
											$(wildcard $(EHCLIB_BASE_SRC_PREFIX)*.chs $(EHCLIB_BASE_SRC_PREFIX)[A-Z]*/*.chs)
EHCLIB_CHS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%.chs,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCLIB_CHS_ALL_SRC_CHS))

# .hsc files
#EHCLIB_HSC_ALL_SRC_HSC					:= $(wildcard $(EHCLIB_BASE_SRC_PREFIX)*.hsc $(EHCLIB_BASE_SRC_PREFIX)[A-Z]*/*.hsc)
EHCLIB_HSC_ALL_SRC_HSC          		:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),\
											$(foreach suff,hsc,\
											 $(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*/*/*.$(suff) \
											  )))

EHCLIB_HSC_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%.hsc,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCLIB_HSC_ALL_SRC_HSC))

# shuffled .c
EHCLIB_CC_ALL_SRC_CC					:= # not done yet
EHCLIB_CC_ALL_DRV_C						:= $(patsubst $(EHCLIB_SRC_PREFIX)%.chs,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCLIB_CC_ALL_SRC_CC))

# as haskell .hs, as is in svn repo
EHCLIB_HS_ALL_SRC_HS					:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),\
											$(foreach suff,hs lhs,\
											 $(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*/*.$(suff) \
											            $(EHCLIB_SRC_PREFIX)$(pkg)/[A-Z]*/*/*/*.$(suff) \
											  )))

EHCLIB_HS_ALL_DRV_HS					:= $(patsubst $(EHCLIB_SRC_PREFIX)%,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%,$(EHCLIB_HS_ALL_SRC_HS))

# as C .c, as is in svn repo
EHCLIB_C_ALL_SRC_C						:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/cbits/*.c))
EHCLIB_C_ALL_DRV_C						:= $(patsubst $(EHCLIB_SRC_PREFIX)%,$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%,$(EHCLIB_C_ALL_SRC_C))

# as C .h include file, as is in svn repo
EHCLIB_ASIS_ALL_SRC_ASIS				:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(wildcard $(EHCLIB_SRC_PREFIX)$(pkg)/include/*.h))
EHCLIB_ASIS_ALL_SRC_uhcbase_ASIS		:= $(wildcard $(EHCLIB_SRC_PREFIX)uhcbase/include/*.h)
EHCLIB_ASIS_ALL_SRC_base_ASIS			:= $(wildcard $(EHCLIB_SRC_PREFIX)base/include/*.h)
EHCLIB_ASIS_ALL_SRC_array_ASIS			:= $(wildcard $(EHCLIB_SRC_PREFIX)array/include/*.h)
EHCLIB_ASIS_ALL_SRC_oldtime_ASIS		:= $(wildcard $(EHCLIB_SRC_PREFIX)old-time/include/*.h)
EHCLIB_ASIS_ALL_SRC_unix_ASIS			:= $(wildcard $(EHCLIB_SRC_PREFIX)unix/include/*.h)
EHCLIB_ASIS_ALL_SRC_directory_ASIS		:= $(wildcard $(EHCLIB_SRC_PREFIX)directory/include/*.h)

EHCLIB_ASIS_ALL_DRV_uhcbase_ASIS		:= $(patsubst $(EHCLIB_SRC_PREFIX)uhcbase/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,uhcbase))%.h,$(EHCLIB_ASIS_ALL_SRC_uhcbase_ASIS))
EHCLIB_ASIS_ALL_DRV_base_ASIS			:= $(patsubst $(EHCLIB_SRC_PREFIX)base/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,base))%.h,$(EHCLIB_ASIS_ALL_SRC_base_ASIS))
EHCLIB_ASIS_ALL_DRV_array_ASIS			:= $(patsubst $(EHCLIB_SRC_PREFIX)array/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,array))%.h,$(EHCLIB_ASIS_ALL_SRC_array_ASIS))
EHCLIB_ASIS_ALL_DRV_oldtime_ASIS		:= $(patsubst $(EHCLIB_SRC_PREFIX)old-time/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,old-time))%.h,$(EHCLIB_ASIS_ALL_SRC_oldtime_ASIS))
EHCLIB_ASIS_ALL_DRV_unix_ASIS			:= $(patsubst $(EHCLIB_SRC_PREFIX)unix/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,unix))%.h,$(EHCLIB_ASIS_ALL_SRC_unix_ASIS))
EHCLIB_ASIS_ALL_DRV_directory_ASIS		:= $(patsubst $(EHCLIB_SRC_PREFIX)directory/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,directory))%.h,$(EHCLIB_ASIS_ALL_SRC_directory_ASIS))
EHCLIB_ASIS_ALL_DRV_ASIS				:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(EHCLIB_ASIS_ALL_DRV_$(call FUN_STRIP_DASH,$(pkg))_ASIS))
#EHCLIB_ASIS_ALL_DRV_ASIS				:= $(foreach pkg,$(EHC_PACKAGES_ASSUMED),$(patsubst $(EHCLIB_SRC_PREFIX)$(pkg)/%.h,$(call FUN_INSTALL_PKG_PREFIX,$(pkg))%.h,$(EHCLIB_ASIS_ALL_SRC_ASIS)))

# as haskell, from frozen sync
#EHCLIB_FROZEN_ALL_DRV_HS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg))))
#EHCLIB_FROZEN_ALL_DRV_C					:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$(pkg)/,$(EHCLIB_SYNC_ALL_PKG_$(pkg)_C)))
#EHCLIB_FROZEN_ALL_DRV_ASIS				:= $(foreach pkg,$(EHCLIB_SYNC_ALL_PKG),$(addprefix $(call FUN_INSTALL_PKG_PREFIX,$(pkg)),$(EHCLIB_SYNC_ALL_PKG_$(pkg)_ASIS)))
#EHCLIB_FROZEN_ALL_DRV_base_ASIS			:= $(addprefix $(call FUN_INSTALL_PKG_PREFIX,base),$(EHCLIB_SYNC_ALL_PKG_base_ASIS))
#EHCLIB_FROZEN_ALL_DRV_array_ASIS		:= $(addprefix $(call FUN_INSTALL_PKG_PREFIX,array),$(EHCLIB_SYNC_ALL_PKG_array_ASIS))

# specials: tuple instances for generic deriving
EHCLIB_GEN_GENERTUPINST_HS				:= $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)uhcbase/UHC/Generics/Tuple.hs
EHCLIB_GEN_HS							:= $(EHCLIB_GEN_GENERTUPINST_HS)

EHCLIB_ALL_SPECIALS						:= $(EHCLIB_GEN_HS) $(GEN_GENERTUPINST_BLD_EXEC)

# all
EHCLIB_ALL_SRC							:= $(EHCLIB_HS_ALL_SRC_HS) $(EHCLIB_CHS_ALL_SRC_CHS) $(EHCLIB_ASIS_ALL_SRC_ASIS)
EHCLIB_ALL_DRV_HS						:= $(EHCLIB_HS_ALL_DRV_HS) $(EHCLIB_CHS_ALL_DRV_HS) $(EHCLIB_FROZEN_ALL_DRV_HS) \
											$(if $(EHC_CFG_USE_UNIX_AND_C),$(EHCLIB_HSC_ALL_DRV_HS),) \
											$(EHCLIB_GEN_HS)
EHCLIB_ALL_DRV_C						:= $(EHCLIB_C_ALL_DRV_C) $(EHCLIB_CC_ALL_DRV_C) $(EHCLIB_FROZEN_ALL_DRV_C)
EHCLIB_ALL_DRV_ASIS						:= $(EHCLIB_FROZEN_ALL_DRV_ASIS) $(EHCLIB_ASIS_ALL_DRV_ASIS)
EHCLIB_ALL_DRV							:= $(EHCLIB_ALL_DRV_HS) $(EHCLIB_ALL_DRV_ASIS) $(EHCLIB_ALL_DRV_C)

# shell script: map package name to its versioned name
EHCLIB_MAP_PKG2VERSIONED_SH				:= $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)mapPkg2Versioned.sh

# distribution
EHCLIB_DIST_FILES						:= $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)

###########################################################################################
# files, intermediate files, for ghc sync
###########################################################################################

# ghc sync files
#EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH		:= $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH)

# ghc frozen sync files
#EHCLIB_GHCSYNC_FROZEN_DRV_ARCH			:= $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_FROZEN_NAME_ARCH)

###########################################################################################
# ehclib targets
###########################################################################################

#EHCLIB_BASE_OPTS						= 
EHCLIB_BASE_OPTS						= -O2

EHCLIB_DEBUG_OPTS						=
#EHCLIB_DEBUG_OPTS						= --gen-trace=1
#EHCLIB_DEBUG_OPTS						= --gen-trace=1 --dump-core-stages=1
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 -OStrictnessAnalysis
#EHCLIB_DEBUG_OPTS						= --priv=1
#EHCLIB_DEBUG_OPTS						= -peh -v3
#EHCLIB_DEBUG_OPTS						= --no-hi-check
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1  --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 -v4
#EHCLIB_DEBUG_OPTS						= --target-flavor=debug --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= -O0
#EHCLIB_DEBUG_OPTS						= -v3
#EHCLIB_DEBUG_OPTS						= -v4 --priv=1
#EHCLIB_DEBUG_OPTS						= -v4 --debug-stopat-hi-error=1 
#EHCLIB_DEBUG_OPTS						= -O0 --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1 -O0
#EHCLIB_DEBUG_OPTS						= --dump-core-stages=1 --dump-grin-stages=1 --gen-trace=1 --gen-cmt=1 --priv=1

ehclib-variant-dflt: \
			$(if $(EHC_CFG_USE_CODEGEN),ehclib-codegentargetspecific-$(EHC_VARIANT_TARGET),) \
			$(if $(EHC_CFG_USE_PRELUDE),$(EHCLIB_ALL_DRV) $(EHCLIB_ALL_SPECIALS) $(EHCLIB_MAP_PKG2VERSIONED_SH),) \
			$(EHC_INSTALL_VARIANT_ASPECTS_EXEC)
	$(if $(EHC_CFG_USE_PRELUDE) \
	     ,pkgsexpopt="" ; \
	     pkgs="" ; \
	     $(EHC_INSTALLABS_VARIANT_ASPECTS_EXEC) ; \
	      for pkg in $(EHC_PACKAGES_ASSUMED) ; \
	      do \
	        pkgv=`/bin/sh $(EHCLIB_MAP_PKG2VERSIONED_SH) $${pkg}` ; \
	        mkdir -p $(call FUN_INSTALL_PKG_PREFIX,$${pkgv}) ;\
	        hsFiles="`find $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg} -name '*.*hs'`" ; \
	        expModules="`echo $${hsFiles} | sed -e s+$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg}/++g -e 's+\.[^.]*hs++g' -e 's+/+.+g'`" ; \
	        bldDepends="$${pkgs}" ; \
	        $(EHC_INSTALLABS_VARIANT_ASPECTS_EXEC) \
	          $(EHCLIB_BASE_OPTS) \
	          $(EHCLIB_DEBUG_OPTS) \
	          --compile-only \
	          --pkg-hide-all \
	          --target=$(EHC_VARIANT_TARGET) \
	          --odir=$(call FUN_DIR_VARIANT_LIB_PKG_PREFIX,$(INSTALL_DIR),$(EHC_VARIANT)) \
	          --pkg-build=$${pkgv} \
	          --pkg-build-exposed="$${expModules}" \
	          --pkg-build-depends="$${bldDepends}" \
	          --import-path=$(call FUN_MK_PKG_INC_DIR,$(call FUN_INSTALL_PKG_PREFIX,$${pkgv})) \
	          $${pkgsexpopt} \
	          $${hsFiles} \
	          $(if $(EHC_CFG_USE_UNIX_AND_C),`find $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)$${pkg} -name '*.c'`,) \
	          +RTS -K30m ; \
	        err=$$? ; \
	        if test $${err} -ne 0 ; then exit $${err} ; fi ; \
	        pkgsexpopt="$${pkgsexpopt} --pkg-expose $${pkgv}" ; \
	        pkgs="$${pkgs} $${pkgv}" ; \
	      done \
	     ,)

#	          --cpp \
#	        pkgv=`sh $(EHCLIB_MAP_PKG2VERSIONED_SH) $${pkg}` ; \

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

ehclib-codegentargetspecific-js: $(if $(ENABLE_JS),$(INSTALL_LIB_JS) $(JSCRIPT_ALL_SRC),)

ehclib-codegentargetspecific-cmmjs: $(if $(ENABLE_JS),$(INSTALL_LIB_JS) $(JSCRIPT_ALL_SRC),)

ehclib-codegentargetspecific-core:

ehclib-codegentargetspecific-cr:

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
	$(MAKE) EHC_VARIANT=`       echo $(*D)/ | sed -n -e 's+$(call FUN_INSTALL_PKG_VARIANT_TARGET_PREFIX,\([a-zA-Z0-9_.-]*\),\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\1+p'` \
	        EHC_VARIANT_TARGET=`echo $(*D)/ | sed -n -e 's+$(call FUN_INSTALL_PKG_VARIANT_TARGET_PREFIX,\([a-zA-Z0-9_.-]*\),\([0-9]*\),\([a-zA-Z0-9_]*\)).*+\4+p'` \
	        ehclib-variant-dflt
	touch $@

#$(EHCLIB_ALL_LIBS): %: $(EHCLIB_ALL_SRC) $(EHCLIB_MKF)
#	$(MAKE) EHC_VARIANT=`echo $(*D) | sed -n -e 's+$(BLD_PREFIX)\([0-9]*\)/$(EHCLIB_EHCLIB_EHCBASE)+\1+p'` ehclib-variant-dflt

###########################################################################################
# rules for intermediate files
###########################################################################################

# plainly copy .hs & .c files
$(EHCLIB_HS_ALL_DRV_HS) $(EHCLIB_C_ALL_DRV_C): $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%: $(EHCLIB_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@
	touch $@

# plainly copy .h files to install directly
$(EHCLIB_ASIS_ALL_DRV_uhcbase_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,uhcbase))%.h : $(EHCLIB_SRC_PREFIX)uhcbase/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

$(EHCLIB_ASIS_ALL_DRV_base_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,base))%.h : $(EHCLIB_SRC_PREFIX)base/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

$(EHCLIB_ASIS_ALL_DRV_array_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,array))%.h : $(EHCLIB_SRC_PREFIX)array/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

$(EHCLIB_ASIS_ALL_DRV_oldtime_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,old-time))%.h : $(EHCLIB_SRC_PREFIX)old-time/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

$(EHCLIB_ASIS_ALL_DRV_unix_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,unix))%.h : $(EHCLIB_SRC_PREFIX)unix/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

$(EHCLIB_ASIS_ALL_DRV_directory_ASIS): $(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,directory))%.h : $(EHCLIB_SRC_PREFIX)directory/%.h
	mkdir -p $(@D)
	cp $< $@
	touch $@

# extract .hs & .c files from frozen archive
#$(EHCLIB_FROZEN_ALL_DRV_HS) $(EHCLIB_FROZEN_ALL_DRV_C): $(EHCLIB_GHCSYNC_FROZEN)
#	mkdir -p $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)
#	cd $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX) && $(TAR) xfoz $< `echo $@ | sed -e 's+$(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)++'`
#	touch $@

# extract 'as is' files from frozen archive
#$(EHCLIB_FROZEN_ALL_DRV_base_ASIS): $(EHCLIB_GHCSYNC_FROZEN)
#	prefix=$(call FUN_INSTALL_PKG_PREFIX,base); \
#	mkdir -p $${prefix}; \
#	cd $${prefix} && $(TAR) --strip-components 1 -xoz -f $< `echo $@ | sed -e "s+$${prefix}+base/+"`
#	touch $@

#$(EHCLIB_FROZEN_ALL_DRV_array_ASIS): $(EHCLIB_GHCSYNC_FROZEN)
#	prefix=$(call FUN_INSTALL_PKG_PREFIX,array); \
#	mkdir -p $${prefix}; \
#	cd $${prefix} && $(TAR) --strip-components 1 -xoz -f $< `echo $@ | sed -e "s+$${prefix}+array/+"`
#	touch $@

# generate .hs from .chs via shuffle
$(EHCLIB_CHS_ALL_DRV_HS): $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs: $(EHCLIB_SRC_PREFIX)%.chs
	mkdir -p $(@D)
	$(SHUFFLE_PLAIN) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

# generate .hs from .hsc via hsc2hs
# hsc2hs makes assumptions about flags to be passed to the C compiler, the patch to HSC2HS_EXTRA removes these flags
# we only apply the patch on platforms where HSC2HS_EXTRA is specified, otherwise we use the original method
$(EHCLIB_HSC_ALL_DRV_HS): $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)%.hs: $(EHCLIB_SRC_PREFIX)%.hsc
	mkdir -p $(@D) && \
	if grep -q 'HSC2HS_EXTRA' $(HSC2HS) ;\
	then \
	   sed -e 's/^HSC2HS_EXTRA=.*$$/HSC2HS_EXTRA=/' < $(HSC2HS) > $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)hsc2hs && chmod +x $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)hsc2hs && \
         $(EHCLIB_BLD_VARIANT_ASPECTS_PREFIX)hsc2hs -v --output=$@ \
              --include=HsFFI.h \
              --cc=$(GCC) \
	         -D__UHC__=$(EH_VERSION_ASNUMBER) \
	         -D$(EHC_VARIANT_TARGET_UHC_DEFINE1) \
	         -D$(EHC_VARIANT_TARGET_UHC_DEFINE2) \
	         -I$(call FUN_INSTALLABS_VARIANT_INC_TARGET_PREFIX,$(EHC_VARIANT),$(EHC_VARIANT_TARGET)) \
	         -I$(call FUN_INSTALLABS_VARIANT_INC_SHARED_PREFIX,$(EHC_VARIANT)) \
		      $(foreach pkg,$(EHC_PACKAGES_ASSUMED),-I$(EHCLIB_SRC_PREFIX)$(pkg)/include/) \
	         $(foreach pkg,$(EHC_PACKAGES_ASSUMED),-I$(call FUN_MK_PKG_INC_DIR,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,$(pkg))))) \
		   $< ; \
	else \
      $(HSC2HS) -v --output=$@ --no-compile --include=HsFFI.h $< && \
	   $(GCC) -D__UHC__=$(EH_VERSION_ASNUMBER) \
	      $(RTS_GCC_CC_OPTS_VARIANT_TARGET) \
	      -I$(call FUN_INSTALLABS_VARIANT_INC_TARGET_PREFIX,$(EHC_VARIANT),$(EHC_VARIANT_TARGET)) \
	      -I$(call FUN_INSTALLABS_VARIANT_INC_SHARED_PREFIX,$(EHC_VARIANT)) \
		    $(foreach pkg,$(EHC_PACKAGES_ASSUMED),-I$(EHCLIB_SRC_PREFIX)$(pkg)/include/) \
	        $(foreach pkg,$(EHC_PACKAGES_ASSUMED),-I$(call FUN_MK_PKG_INC_DIR,$(call FUN_INSTALL_PKG_PREFIX,$(call FUN_PKG_VERSIONED,$(pkg))))) \
		  -o $(@:.hs=_hsc_make) \
		  $(@:.hs=_hsc_make.c) \
	   && $(@:.hs=_hsc_make) > $@ \
	   && rm -f $(@:.hs=_hsc_make.c) ; \
	fi \
	&& touch $@

# generate shell script for mapping package names
$(EHCLIB_MAP_PKG2VERSIONED_SH): $(EHCLIB_MKF)
	@(echo "#!/bin/sh" ; \
	  echo  "case \$$1 in" ; \
	  $(foreach pkg,$(EHC_PACKAGES_ASSUMED),echo "  $(pkg) $(cparen) echo $(call FUN_PKG_VERSIONED,$(pkg)) ;;" ;) \
	  echo  "esac" ; \
	) > $@

# specials: generate tuple instances for generic deriving
$(EHCLIB_GEN_GENERTUPINST_HS): $(GEN_GENERTUPINST_BLD_EXEC)
	mkdir -p $(@D)
	$(GEN_GENERTUPINST_BLD_EXEC) 15 > $@

###########################################################################################
# ehclib sync with GHC libraries; needs to be done only when set of library module changes
###########################################################################################

# download ghc dist
#$(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH): $(EHCLIB_MKF)
#	mkdir -p $(@D)
#	cd $(EHCLIB_BLD_SYNC_PREFIX) && curl -O $(EHCLIB_GHCSYNC_DOWNLOAD)

# extraction for frozen from ghc libraries
#$(addprefix $(EHCLIB_BLD_SYNC_SRC_PREFIX),$(EHCLIB_SYNC_ALL_PKG_SRC)) \
#			: $(EHCLIB_BLD_SYNC_SRC_PREFIX)% \
#			: $(EHCLIB_BLD_SYNC_PREFIX)$(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/%
#	mkdir -p $(@D)
#	cp $< $@
#	touch $@

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
#$(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH): $(EHCLIB_SYNC_ALL_PKG_DRV)
#	cd $(EHCLIB_BLD_SYNC_SRC_PREFIX) && $(TAR) cfz $(EHCLIB_GHCSYNC_FROZEN) *

# use this target to download the ghc dist from which the frozen extract will be made
#ehclib-ghc-sync-download: $(EHCLIB_GHCSYNC_DOWNLOAD_DRV_ARCH)

# use this target to extract what will be frozen
#ehclib-ghc-sync-extract:
#	cd $(EHCLIB_BLD_SYNC_PREFIX) && $(TAR) xfoj $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_ARCH) $(addprefix $(EHCLIB_GHCSYNC_DOWNLOAD_NAME_BASE)/libraries/,$(EHCLIB_SYNC_ALL_PKG_SRC))

# use this target to make the frozen extract from the ghc libraries, to be used as part of ehclib
#ehclib-ghc-sync-frozen: $(EHCLIB_GHCSYNC_FROZEN_DRV_ARCH)

# use this target to do last 2 of the three above
#ehclib-ghc-sync2:
#	$(MAKE) ehclib-ghc-sync-extract
#	$(MAKE) ehclib-ghc-sync-frozen

# use this target to do all the three above
#ehclib-ghc-sync:
#	$(MAKE) ehclib-ghc-sync-download
#	$(MAKE) ehclib-ghc-sync2




