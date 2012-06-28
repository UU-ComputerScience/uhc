###########################################################################################
# this file
###########################################################################################

MK_PREFIX									:= $(TOP_PREFIX)mk/
MK_CONFIG_MKF								:= $(MK_PREFIX)config.mk

###########################################################################################
# current date
###########################################################################################

DATE			:= $(shell /bin/date +%Y%m%d)

###########################################################################################
# the overall version of EH
###########################################################################################

EH_VERSION_MAJOR		:= 1
EH_VERSION_MINOR		:= 1
EH_VERSION_MINORMINOR	:= 4
EH_VERSION_STABILITY	:= alpha
EH_VERSION_SHORT		:= 1.1
EH_VERSION_FULL			:= 1.1.4
EH_VERSION_ASNUMBER		:= 114

###########################################################################################
# platform config, currently changed to config by autoconf
###########################################################################################

# EXEC_SUFFIX	: suffix for executables
# PATH_SEP		: separator in file paths
# PATHS_SEP		: separator in sequences of file paths, as in environments
# PATHS_SEP_COL	: separator in sequences of file paths, as in environments which do not use absolute paths
# STRIP_CMD		: strip cmd
# TOPABS_PREFIX	: absolute prefix/pathname to top dir

EXEC_SUFFIX			:= 
LIBC_SUFFIX			:= .a
LIBJS_SUFFIX		:= .mjs
PATH_SEP			:= /
PATHS_SEP			:= :
PATHS_SEP_COL		:= :
STRIP_CMD			:= strip
TOPABS_PREFIX		:= /Users/alessandro/Documents/Uni/uhc/EHC/
TOPABS2_PREFIX		:= /Users/alessandro/Documents/Uni/uhc/EHC/

### remaining config
# which version (usually v1 = current, v2 == under development)
#RULER2_OPTS_VERSION	:= --v1
#RULER2_OPTS_VERSION	:= --v2
RULER2_OPTS_VERSION	:=

### names of packages assumed by the compiler driver
RTS_PKG_NAME				:= EH-RTS
EXTLIBS_BGC_PKG_NAME		:= gc
EXTLIBS_GMP_PKG_NAME		:= gmp

###########################################################################################
# naming of files
###########################################################################################

# name of pkg config file, assumed to be present for each package in its package dir
UHC_PKG_CONFIGFILE_NAME		:= installed-pkg-config

###########################################################################################
# Platform we develop on; stuff depending on it, in particular to cater for cygwin
###########################################################################################

# platform: UNIX, CYGWIN
DEVELOPMENT_PLATFORM				:= UNIX
# platform: as known by configure, with wordsize added
HOST_PLATFORM_NAME				:= i386-apple-darwin11.2.0
HOST_PLATFORM_NRWORDBITS		:= 64

# A prefix to be added for direct access to otherwise hidden (by cygwin) locations
TOPLEVEL_SYSTEM_ABSPATH_PREFIX		:= 

###########################################################################################
# inclusion of features
###########################################################################################

# include java code generation
ifeq (no,yes)
ENABLE_JAVA					:= yes
endif

# include clr code generation
ifeq (no,yes)
ENABLE_CLR					:= yes
endif

# include javascript code generation
ifeq (yes,yes)
ENABLE_JS					:= yes
endif

# include llvm code generation
ifeq (no,yes)
ENABLE_LLVM					:= yes
endif

# include cmm route for C generation
ifeq (no,yes)
ENABLE_CMM					:= yes
endif

# include C backend combined using whole program analysis
ifeq (no,yes)
ENABLE_WHOLEPROGC			:= yes
ENABLE_WHOLEPROGANAL		:= yes
endif

# include whole program analysis
ifeq (no,yes)
ENABLE_WHOLEPROGANAL		:= yes
endif

# include System F Ty generation for Core
ifeq (no,yes)
ENABLE_CORESYSF				:= yes
endif

# include TyCore
ifeq (no,yes)
ENABLE_CORESYSF				:= yes
ENABLE_TYCORE				:= yes
endif

# include TyCore based transformations
ifeq (no,yes)
ENABLE_CORESYSF				:= yes
ENABLE_TYCORE				:= yes
ENABLE_TAUPHI				:= yes
endif

