###########################################################################################
# locations
###########################################################################################

# location of lib src
EHCLIB_EHCLIB							:= ehclib
EHCLIB_EHCLIB_PREFIX					:= $(EHCLIB_EHCLIB)/
EHCLIB_EHCBASE							:= base
EHCLIB_EHCBASE_PREFIX					:= $(EHCLIB_EHCBASE)/
EHCLIB_EHCLIB_EHCBASE					:= $(EHCLIB_EHCLIB_PREFIX)$(EHCLIB_EHCBASE)
EHCLIB_EHCLIB_EHCBASE_PREFIX			:= $(EHCLIB_EHCLIB_EHCBASE)/
EHCLIB_EHCLIB_EHCBASE_INC				:= $(EHCLIB_EHCLIB_EHCBASE_PREFIX)include
EHCLIB_EHCLIB_EHCBASE_INC_PREFIX		:= $(EHCLIB_EHCLIB_EHCBASE_INC)/
EHCLIB_SRC_PREFIX						:= $(TOP_PREFIX)$(EHCLIB_EHCLIB_PREFIX)
EHCLIBABS_SRC_PREFIX					:= $(TOPABS2_PREFIX)$(EHCLIB_EHCLIB_PREFIX)
EHCLIB_BASE_SRC_PREFIX					:= $(TOP_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)
EHCLIBABS_BASE_SRC_PREFIX				:= $(TOPABS2_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)
EHCLIB_BASE_INC_SRC_PREFIX				:= $(TOP_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_INC_PREFIX)
EHCLIBABS_BASE_INC_SRC_PREFIX			:= $(TOPABS2_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_INC_PREFIX)

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
EHCLIB_GHCSYNC_FROZEN					:= $(EHCLIBABS_SRC_PREFIX)$(EHCLIB_GHCSYNC_FROZEN_NAME_ARCH)

# build locations
EHCLIB_BLD_VARIANT_ASPECTS_PREFIX		:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)$(EHCLIB_EHCLIB_PREFIX)
EHCLIB_BASE_BLD_VARIANT_ASPECTS_PREFIX	:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)$(EHCLIB_EHCLIB_EHCBASE_PREFIX)
EHCLIB_BLD_SYNC_PREFIX					:= $(BLD_PREFIX)$(EHCLIB_GHCSYNC_PREFIX)
EHCLIB_BLD_SYNC_SRC_PREFIX				:= $(EHCLIB_BLD_SYNC_PREFIX)frozen/

# install locations
#EHCLIB_INSTALL_VARIANT_TARGET_PREFIX		:= $(INSTALL_VARIANT_PKGLIB_TARGET_PREFIX)
#EHCLIB_INSTALL_VARIANT_TARGET_BASE_PREFIX	:= $(EHCLIB_INSTALL_VARIANT_TARGET_PREFIX)$(EHCLIB_EHCBASE_PREFIX)

###########################################################################################
# files which are used outside ehclib as well, in particular for configuration of libraries
###########################################################################################

# As C .h include file, as is in svn repo, after configuration.
# These files are copied into rts as well
EHCLIB_ASIS_RTS_SRC_ASIS				:= $(addprefix $(EHCLIB_BASE_INC_SRC_PREFIX),MachDeps.h HsBase.h HsBaseConfig.h)

