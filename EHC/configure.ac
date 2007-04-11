### Process this file with autoconf to produce a configure script.
AC_INIT(EH, [0.1], [ehc-developers@cs.uu.nl, http://www.cs.uu.nl/wiki/Ehc/WebHome])

### version of autoconf used (and required)
#AC_PREREQ(2.52)

### revision
AC_REVISION($Revision: 1.26 $)

### set MAKE flag in relevant makefiles
AC_PROG_MAKE_SET
# Checks for host/target
AC_CANONICAL_HOST
dnl ** canonicalize platform names
HostPlatform=`/bin/sh $srcdir/config.sub $host` || exit 1
echo $host

### Version info
# big change
EH_VERSION_MAJOR=0
# small change, not usable with previous
EH_VERSION_MINOR=1
# small change, still usable with previous
EH_VERSION_MINORMINOR=1
EH_VERSION_STABILITY=alpha

AC_SUBST(EH_VERSION_MAJOR)
AC_SUBST(EH_VERSION_MINOR)
AC_SUBST(EH_VERSION_MINORMINOR)
AC_SUBST(EH_VERSION_STABILITY)
AC_SUBST(EH_VERSION,$EH_VERSION_MAJOR.$EH_VERSION_MINOR)

dnl macro for missing program
AC_DEFUN(MISSING_FOR_EH,
[
  AC_MSG_ERROR([
     You must install $1 before you can continue
     Perhaps it is already installed, but not in your PATH?])
])

dnl macro for missing program which does not stop an alternate route of installing
AC_DEFUN(MISSING_FOR_EH_BUT_CAN_GO_ON,
[
  AC_MSG_WARN([
     You should have installed $1, but I will proceed without it.
     However, this may lead to a partial install only.
     Perhaps $1 is already installed, but not in your PATH?])
])

dnl macro for required programs
AC_DEFUN(REQUIRED_PROG_FOR_EH,
[
  AC_PATH_PROG($1,$2)
  if test -z "[$]$1"; then
    ifelse([$3], , [ MISSING_FOR_EH([$2]) ], [ MISSING_FOR_EH([$3]) ])
  fi
])

dnl macro for optional required programs, that is, an alternate install route exists
AC_DEFUN(REQUIRED_OPTIONAL_PROG_FOR_EH,
[
  AC_PATH_PROG($1,$2)
  if test -z "[$]$1"; then
    ifelse([$3], , [ MISSING_FOR_EH_BUT_CAN_GO_ON([$2]) ], [ MISSING_FOR_EH_BUT_CAN_GO_ON([$3]) ])
  fi
])

### ar, ranlib
REQUIRED_PROG_FOR_EH(ranlibCmd,ranlib)
AC_SUBST(RANLIB_CMD,$ranlibCmd)

REQUIRED_PROG_FOR_EH(arCmd,ar)
AC_SUBST(AR_CMD,$arCmd)

### 'with' option for gcc options
AC_ARG_WITH(gcc-ehc-options,
[AC_HELP_STRING([--with-gcc-ehc-options=<gcc compiler options for compiling runtime and ehc's gcc invocation>],
                [Additional options to pass to GCC when compiling with EHC.])],
[ gccEhcOptions="$withval" ],)
AC_SUBST(GCC_EHC_OPTIONS, "$gccEhcOptions")

### 'with' option for ghc options
AC_ARG_WITH(ghc-ehc-options,
[AC_HELP_STRING([--with-ghc-ehc-options=<haskell compiler options for compiling ehc>],
                [Additional options to pass to GHC when compiling EHC.])],
[ ghcEhcOptions="$withval" ],)
AC_SUBST(GHC_EHC_OPTIONS, "$ghcEhcOptions")

### 'with' option for uuagc options
AC_ARG_WITH(uuagc-options,
[AC_HELP_STRING([--with-uuagc-options=<AG compiler options for compiling ehc>],
                [Additional options to pass to UUAGC when compiling EHC.])],
[ uuagcOptions="$withval" ],)
AC_SUBST(UUAGC_OPTIONS, "$uuagcOptions")

### 'with' option for uuagc options, specifically for EHC's AST
AC_ARG_WITH(uuagc-ehc-ast-options,
[AC_HELP_STRING([--with-uuagc-ehc-ast-options=<AG compiler options for compiling ehc's AST data types>],
                [Additional options to pass to UUAGC when compiling EHC's AST data types.])],
[ uuagcEhcAstOptions="$withval" ],)
AC_SUBST(UUAGC_EHC_AST_OPTIONS, "$uuagcEhcAstOptions")

### 'with' option for uuagc options, specifically for EHC's semantics
AC_ARG_WITH(uuagc-ehc-sem-options,
[AC_HELP_STRING([--with-uuagc-ehc-sem-options=<AG compiler options for compiling ehc's semantics>],
                [Additional options to pass to UUAGC when compiling EHC's AST semantics.])],
[ uuagcEhcSemOptions="$withval" ],)
AC_SUBST(UUAGC_EHC_SEM_OPTIONS, "$uuagcEhcSemOptions")

### 'with' option for cabal config options
AC_ARG_WITH(cabal-config-options,
[AC_HELP_STRING([--with-cabal-config-options=<cabal configure options for compiling ehc>],
                [Additional options to pass to cabal's ./setup configure when compiling EHC.])],
[ cabalConfigureOptions="$withval" ],)
AC_SUBST(CABAL_CONFIGURE_OPTIONS, "$cabalConfigureOptions")

### 'with' option for ghc
AC_ARG_WITH(ghc,
[AC_HELP_STRING([--with-ghc=<haskell compiler>],
                [Use a command different from 'ghc' to compile with GHC.])],
[ ghcCmd="$withval" ],
[ 
  if test "$GHC" = ""; then
    AC_PATH_PROG(ghcCmd,ghc)
  else
    ghcCmd=$GHC
  fi
]
)
if test "$ghcCmd" = "no"; then
  ghcCmd=""
  ghcExists="no"
else
  ghcExists="yes"
fi
AC_SUBST(GHC_CMD,$ghcCmd)
ghcVersion=`$ghcCmd --numeric-version`
AC_SUBST(GHC_VERSION, $ghcVersion)

# GHC 6.6 and higher: no package util, lang, and data anymore
case $ghcVersion in
  6.4.1)
    optStandardGHCPackages="-package util -package lang -package data"
    ;;
  6.4.2)
    optStandardGHCPackages="-package util -package lang -package data"
    ;;
  *)
    optStandardGHCPackages=""
    ;;
esac

AC_SUBST(OPT_GHC_STANDARD_PACKAGES,"$optStandardGHCPackages")

# after ghc 6.4.1 the names of cabal option(s) changed
case $ghcVersion in
  6.4.1)
    optCabalAllowUndecidableInstances=AllowUndecidableInstances
    ;;
  *)
    optCabalAllowUndecidableInstances=UndecidableInstances
    ;;
esac

AC_SUBST(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES,"$optCabalAllowUndecidableInstances")

# uuagc
AC_ARG_WITH(uuagc,
[AC_HELP_STRING([--with-uuagc=<uu ag compiler>],
                [Use a command different from 'uuagc' to compile with UUAGC.])],
[ uuagcCmd="$withval" ],
[ 
  if test "$UUAGC" = ""; then
    AC_PATH_PROG(uuagcCmd,uuagc)
  else
    uuagcCmd=$UUAGC
  fi
]
)
if test "$uuagcCmd" = "no"; then
  uuagcCmd=""
  uuagcExists="no"
else
  uuagcExists="yes"
  uuagcVersion=`$uuagcCmd --version`
fi
AC_SUBST(UUAGC_CMD,$uuagcCmd)

# gcc
AC_ARG_WITH(gcc,
[AC_HELP_STRING([--with-gcc=<gcc compiler>],
                [Use a command different from 'gcc' to compile with GCC.])],
[ gccCmd="$withval" ],
[ 
  if test "$GCC" = ""; then
    AC_PATH_PROG(gccCmd,gcc)
  else
    gccCmd=$GCC
  fi
]
)
if test "$gccCmd" = "no"; then
  gccCmd=""
  gccExists="no"
else
  gccExists="yes"
  gccVersion=`$gccCmd --version`
fi
AC_SUBST(GCC_CMD,$gccCmd)

# cpp
AC_ARG_WITH(cpp,
[AC_HELP_STRING([--with-cpp=<cpp preprocessor>],
                [Use a command different from 'cpp' to compile with CPP.])],
[ cppCmd="$withval" ],
[ 
  if test "$CPP" = ""; then
    AC_PATH_PROG(cppCmd,cpp)
  else
    cppCmd=$CPP
  fi
]
)
if test "$cppCmd" = "no"; then
  cppCmd=""
  cppExists="no"
else
  cppExists="yes"
  cppVersion=`$cppCmd --version`
fi
AC_SUBST(CPP_CMD,$cppCmd)


### Checks for libraries.

### Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

### what to make
#AC_SUBST(UUST_SUBDIRS,"shc lib")

### type configuration, machine characteristics

# sizeof GrWord
# AC_CHECK_SIZEOF([unsigned int])

# sizeof GrWord*, Pointer
# AC_CHECK_SIZEOF([unsigned int *])
AC_CHECK_SIZEOF([uintptr_t])

AC_C_BIGENDIAN([AC_DEFINE([BIGENDIAN],[1]) AC_DEFINE([LITTLEENDIAN],[0])],
               [AC_DEFINE([LITTLEENDIAN],[1]) AC_DEFINE([BIGENDIAN],[0])],
               [])

### general configuration

# Use Boehm's GC
AC_DEFINE([USE_BOEHM_GC],[1])

# Use GMP
AC_DEFINE([USE_GMP],[1])

# Prefix used for global values, assumed to be present by RTS
AC_DEFINE([RTS_GLOBAL_VAR_PREFIX],["global_"])

### machine dependent code
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        mach_dep_c_for_tailcall_leave1="asm(\\\"leave\\\") ;"
        mach_dep_c_for_tailcall_leave2=""
        mach_dep_c_for_tailcall_enter=""
        ;;
  powerpc-apple-* )
        mach_dep_c_for_tailcall_leave1="asm(\\\"lwz r1,0(r1)\\\") ; asm(\\\"lwz r0,8(r1)\\\") ; asm(\\\"mtlr r0\\\") ;"
        mach_dep_c_for_tailcall_leave2="asm(\\\"lmw r30,-8(r1)\\\") ;"
        mach_dep_c_for_tailcall_enter="asm( \\\"lwz r2,0(r1)\\\" ) ; asm( \\\"stw r0,8(r2)\\\" ) ;"
        ;;
  i*86-pc-linux-gnu )
        mach_dep_c_for_tailcall_leave1="asm(\\\"leave\\\") ;"
        ;;
  x86_64-* )
        mach_dep_c_for_tailcall_leave1="asm(\\\"leave\\\") ;"
        ;;
  i386-apple-* )
        mach_dep_c_for_tailcall_leave1="asm(\\\"leave\\\") ;"
        mach_dep_c_for_tailcall_leave2="asm(\\\"movl -8(%esp), %ebx\\\") ;"
        ;;
  *)
        mach_dep_c_for_tailcall_leave1=""
        mach_dep_c_for_tailcall_leave2=""
        mach_dep_c_for_tailcall_enter=""
        ;;
esac
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_ENTER,"$mach_dep_c_for_tailcall_enter")
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_LEAVE1,"$mach_dep_c_for_tailcall_leave1")
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_LEAVE2,"$mach_dep_c_for_tailcall_leave2")

### platform specific stuff
# defaults:
# file suffixes
suffix_shell=""
suffix_exec=""
suffix_lib=".a"
prefix_lib=""
# path separator
paths_sep=":"
slash="/"
# libtool usage
libtoolStatic="libtool -static -o"
libtoolDynamic="echo Dynamic linking not available: "
ehcGccOptsStatic=""
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        # file suffixes
        suffix_shell=".bat"
        suffix_exec=".exe"
        #suffix_lib=".lib"
        #prefix_lib="lib"
        # path separator
        paths_sep=";"
        slash="\\\\"
        # libtool usage
        libtoolStatic="no"
        ;;
  i*86-pc-linux-gnu )
        # file suffixes
        # path separator
        # libtool usage
        # libtoolStatic="no"
        # ehcGccOptsStatic="-static"
	    libtoolStatic="libtool --mode=link gcc -static -o"
        ;;
  x86*-*-linux-gnu )
        # file suffixes
        # path separator
        # libtool usage
        # libtoolStatic="libtool --mode=link gcc -static -o"
        libtoolStatic="no"
        ;;
  *)
        # file suffixes
        # path separator
        # libtool usage
        ;;
esac
# suffixes, path separator, ...
AC_SUBST(SUFFIX_SHELL,"$suffix_shell")
AC_SUBST(SUFFIX_EXEC,"$suffix_exec")
AC_SUBST(SUFFIX_LIB,"$suffix_lib")
AC_SUBST(PREFIX_LIB,"$prefix_lib")
AC_SUBST(PATHS_SEP,"$paths_sep")
AC_SUBST(SLASH,"$slash")

# libtool used, if any
AC_SUBST(LIBTOOL_STATIC_CMD, "$libtoolStatic")
AC_SUBST(LIBTOOL_DYNAMIC_CMD, "$libtoolDynamic")

# flags passed to gcc by ehc related to static/dynamic libraries
AC_SUBST(EHC_GCC_OPTS_STATIC, "$ehcGccOptsStatic")

### absolute path to this dir
hardtop=`pwd`
hardtop=`echo $hardtop | sed 's|^/tmp_mnt.*\(/local/.*\)$|\1|' | sed 's|^/tmp_mnt/|/|' | sed 's|^/grasp_tmp|/local/grasp_tmp|' | sed 's|^//\(.\)/|\1:/|' `
#
# The native format for 'hardtop' (i.e., right kind of slashes on a Win32 box).
# (but with b-slashes being escaped).
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        # get rid off /cygdrive/ prefix
        hardtop=`echo ${hardtop} | sed -e 's%^/cygdrive/\(.\)/%\1:/%' `
        hardtop_plat=`cygpath -w ${hardtop} | sed -e 's@\\\\@/@g' `
        hardtop_plat2=`echo ${hardtop_plat} | sed -e 's@^\(.\):@/\1@' `
        ;;
  *)
        hardtop_plat=${hardtop}
        hardtop_plat2=${hardtop}
        ;;
esac
AC_SUBST(TOP_ABS, "$hardtop_plat")
AC_SUBST(TOP_ABS2, "$hardtop_plat2")

### install locations
inplaceInstallDir="install"
installPrefix=${prefix}
AC_SUBST(INSTALL_PREFIX, ${installPrefix})
AC_SUBST(INPLACE_PREFIX, "$hardtop_plat/$inplaceInstallDir")
AC_SUBST(INPLACE_INSTALL_DIR, "$inplaceInstallDir")

### package names assumed by compiler driver
AC_SUBST(RTS_PKG_NAME, "EH-RTS")
AC_SUBST(EXTLIBS_BGC_PKG_NAME, "gc") # should coincide with installed lib in extlibs/bgc
AC_SUBST(EXTLIBS_GMP_PKG_NAME, "gmp") # should coincide with installed lib in extlibs/gmp

### file generation
AC_CONFIG_FILES([mk/config.mk mk/shared.mk])
AC_CONFIG_FILES([src/grinc/Config.chs src/ehc/Config.chs src/helium/Utils/OSSpecific.hs])
AC_CONFIG_HEADERS([src/grinc/ConfigDefines.chs src/ehc/ConfigDefines.chs src/rts/config.ch])
AC_OUTPUT

echo
echo "Installation/build summary:"
echo "  host                          : $host"
echo "  prefix                        : $prefix"

echo "  uuagc is available?           : $uuagcExists"
if test "x$uuagcExists" = "xyes" ;then 
echo "    version                     : $uuagcVersion"
echo "    command for uuagc           : $uuagcCmd"
fi

echo "  ghc is available?             : $ghcExists"
if test "x$ghcExists" = "xyes" ;then 
echo "    version                     : $ghcVersion"
echo "    command for ghc             : $ghcCmd"
#echo "    library install dir for ghc : $ghcLibDir"
fi

#echo
#echo "Misc debug/tmp info:"
#echo "  hardtop_plat                        : $hardtop_plat"

