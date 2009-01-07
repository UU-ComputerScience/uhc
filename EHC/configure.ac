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

### 'with' option for shuffle options
AC_ARG_WITH(shuffle-ehc-options,
[AC_HELP_STRING([--with-shuffle-ehc-options=<shuffle options for compiling ehc>],
                [Additional options to pass to SHUFFLE when compiling EHC.])],
[ shuffleEhcOptions="$withval" ],)
AC_SUBST(SHUFFLE_EHC_OPTIONS, "$shuffleEhcOptions")

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

### 'with' option for additional ehc unixtool (gcc, cpp) prefix.
### Usually empty, but set for (e.g.) cygwin
AC_ARG_WITH(ehc-unixtool-prefix,
[AC_HELP_STRING([--with-ehc-unixtool-prefix=<additional prefix for unixtools used by ehc>],
                [Additional prefix for unixtools used by ehc.])],
[ additionalEhcUnixtoolPrefix="$withval" ],)

# when no unixtool prefix is specified && running cygwin, take cygwin's equivalent of /
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test "$additionalEhcUnixtoolPrefix" = ""
        then
          additionalEhcUnixtoolPrefix="`cygpath --mixed /`"
        fi
        ;;
  *)
        ;;
esac             

AC_SUBST(EHC_UNIXTOOL_PREFIX, "$additionalEhcUnixtoolPrefix")

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

### 'with' option for haddock
AC_ARG_WITH(haddock,
[AC_HELP_STRING([--with-haddock=<haddock>],
                [Use a command different from 'haddock' to generate doc.])],
[ haddockCmd="$withval" ],
[ 
  if test "$HADDOCK" = ""; then
    AC_PATH_PROG(haddockCmd,haddock)
  else
    haddockCmd=$HADDOCK
  fi
]
)
if test "$haddockCmd" = "no"; then
  haddockCmd=""
  haddockExists="no"
else
  haddockExists="yes"
fi
AC_SUBST(HADDOCK_CMD,$haddockCmd)
haddockVersion=`$haddockCmd --version | head -n 1`
AC_SUBST(HADDOCK_VERSION, $haddockVersion)

# GHC version dependencies: packages passed as option, cabal packages
cabal_base_lib_depends="base haskell98 mtl fgl directory old-time"
cabal_extra_lib_depends=""
optCabalAllowUndecidableInstances=UndecidableInstances
optStandardGHCPackages=""
# (1) GHC 6.6 and higher: no package util, lang, and data anymore
# (2) after ghc 6.4.1 the names of cabal option(s) changed
case $ghcVersion in
  6.4.1)
    optStandardGHCPackages="-package util -package lang -package data"
    optCabalAllowUndecidableInstances=AllowUndecidableInstances
    ;;
  6.4.2)
    optStandardGHCPackages="-package util -package lang -package data"
    ;;
  6.8*)
    cabal_base_lib_depends="$cabal_base_lib_depends containers array"
    ;;
  6.10*)
    cabal_base_lib_depends="$cabal_base_lib_depends containers array"
    ;;
  *)
    cabal_base_lib_depends="$cabal_base_lib_depends containers array"
    ;;
esac

AC_SUBST(OPT_GHC_STANDARD_PACKAGES,"$optStandardGHCPackages")
AC_SUBST(CABAL_BASE_LIB_DEPENDS,"$cabal_base_lib_depends")
AC_SUBST(CABAL_EXTRA_LIB_DEPENDS,"$cabal_extra_lib_depends")
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

# See remarks about cpp
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$additionalEhcUnixtoolPrefix/$gccCmd"
        then
          gccCmd="`echo $gccCmd | sed -e 's+/usr++'`"
        fi
        ;;
  *)
        ;;
esac  

AC_SUBST(GCC_CMD,$gccCmd)

AC_RUN_IFELSE([AC_LANG_PROGRAM(
[[#include <stdio.h>]], [[
FILE *fp;
fp = fopen("conftest.cc", "w");
if (fp == 0) return(1);
fprintf(fp, "cc_major_version=%d;cc_minor_version=%d;cc_patch_version=%d",
__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
fclose(fp);
]])],
[eval `cat conftest.cc`],
[cc_major_version=no],
)
  
AC_MSG_CHECKING([Version of $CC])
if test "$cc_major_version" = "no"; then
  AC_MSG_RESULT([no])
  AC_MSG_FAILURE([Requires that $CC is usable], [1])
else
  cc_version="$cc_major_version.$cc_minor_version.$cc_patch_version"
  AC_MSG_RESULT([$cc_version])
fi

case $cc_version in
  4.0.1)
    cc_std_flag=""
    ;;
  *)
    cc_std_flag="-std=gnu99"
    ;;
esac
AC_SUBST(CC_STD_FLAG,$cc_std_flag)

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

# Now we have to deal with weird cygwin behaviour which allows /usr/bin to be used
# whereas in reality it is mapped onto /bin. Direct acccess to /usr/bin with a c:/cygwin prefix therefore does not work
# and has to be stripped of the /usr part.
# For other tools like gcc we have to this too.
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$additionalEhcUnixtoolPrefix/$cppCmd"
        then
          cppCmd="`echo $cppCmd | sed -e 's+/usr++'`"
        fi
        ;;
  *)
        ;;
esac  

AC_SUBST(CPP_CMD,$cppCmd)

# LLVM Assembler 
AC_ARG_WITH(llvm-as,
[AC_HELP_STRING([--with-llvm-as=<LLVM Assembler>],
                [Path to the LLVM Assembler])],
[ llvmAs="$withval" ],
[ 
  if test "$LLVM_AS" = ""; then
    AC_PATH_PROG(llvmAs,llvm-as)
  else
    llvmAs=$LLVM_AS
  fi
]
)
AC_SUBST(LLVM_AS_CMD,$llvmAs)

# LLVM Optimizer 
AC_ARG_WITH(opt,
[AC_HELP_STRING([--with-llvm-opt=<LLVM bitcode optimizer>],
                [Path to the LLVM bitcode optimizer])],
[ llvmOpt="$withval" ],
[
  if test "$LLVM_OPT" = ""; then
    AC_PATH_PROG(llvmOpt,opt)
  else
    llvmOpt=$LLVM_OPT
  fi
]
)
AC_SUBST(LLVM_OPT_CMD,$llvmOpt)

# LLVM Compiler
AC_ARG_WITH(llc,
[AC_HELP_STRING([--with-llc=<LLVM Compiler>],
                [Path to the LLVM Compiler])],
[ llvmLlc="$withval" ],
[
  if test "$LLC" = ""; then
    AC_PATH_PROG(llvmLlc, llc)
  else
    llvmLlc=$LLC
  fi
]
)
AC_SUBST(LLVM_LLC_CMD,$llvmLlc)

### Checks for libraries.

### Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

### what to make
#AC_SUBST(UUST_SUBDIRS,"shc lib")

### type configuration, machine characteristics

# sizeof GrWord
# AC_CHECK_SIZEOF([unsigned int])

# sizeof GrWord*, Pointer, ...
# AC_CHECK_SIZEOF([nsigned int *])
AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([intptr_t])
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
        gcc_ehc_extra_extern_libs="pthread"
        mach_dep_c_for_tailcall_leave1="asm(\\\"leave\\\") ;"
        ;;
  *-linux-gnu )
        gcc_ehc_extra_extern_libs="pthread"
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
AC_SUBST(GCC_EHC_EXTRA_EXTERN_LIBS,"$gcc_ehc_extra_extern_libs")

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
        #libtoolStatic="no"
        # ehcGccOptsStatic="-static"
	# libtoolStatic="libtool --mode=link gcc -static -o"
        libtoolStatic="no"
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
        shellrun="/bin/bash "
        ;;
  *)
        hardtop_plat=${hardtop}
        hardtop_plat2=${hardtop}
        shellrun=""
        ;;
esac
AC_SUBST(TOP_ABS, "$hardtop_plat")
AC_SUBST(TOP_ABS2, "$hardtop_plat2")
AC_SUBST(SHELLRUN, "$shellrun")

### Platform information, needed for LLVM code generation
AC_SUBST(TARGET_TRIPLE, "$host")

### names of compiler executables
AC_SUBST(EHC_EXEC_NAME, "ehc")
AC_SUBST(UHC_EXEC_NAME, "uhc")

### install locations
inplaceInstallDir="install"
AC_SUBST(INPLACE_TOP, "$hardtop")
AC_SUBST(INPLACE_PREFIX, "$hardtop_plat/$inplaceInstallDir")
AC_SUBST(INPLACE_INSTALL_DIR, "$inplaceInstallDir")
AC_SUBST(INSTALL_LIB_SUFFIX, "lib/uhc")

### package names assumed by compiler driver
AC_SUBST(RTS_PKG_NAME, "EH-RTS")
AC_SUBST(EXTLIBS_BGC_PKG_NAME, "gc") # should coincide with installed lib in extlibs/bgc
AC_SUBST(EXTLIBS_GMP_PKG_NAME, "gmp") # should coincide with installed lib in extlibs/gmp

### file generation
AC_CONFIG_FILES([mk/config.mk mk/shared.mk])
AC_CONFIG_FILES([src/ehc/Config.chs src/helium/Utils/OSSpecific.hs])
AC_CONFIG_FILES([bin/llvm-compilerdriver], [chmod +x bin/llvm-compilerdriver])
AC_CONFIG_HEADERS([src/ehc/ConfigDefines.chs src/rts/config.ch src/rts/mm/config.ch])
AC_OUTPUT

echo
echo "Installation/build summary."
echo "  host                          : $host"
echo "  prefix                        : $prefix"

echo "Required:"
echo "  uuagc is available?           : $uuagcExists"
if test "x$uuagcExists" = "xyes" ;then 
echo "    version                     : $uuagcVersion"
echo "    command for uuagc           : $uuagcCmd"
fi

echo "  ghc is available?             : $ghcExists"
if test "x$ghcExists" = "xyes" ;then 
echo "    version                     : $ghcVersion"
echo "    command for ghc             : $ghcCmd"
fi

echo "Optional:"
echo "  haddock is available?         : $haddockExists"
if test "x$haddockExists" = "xyes" ;then 
echo "    version                     : $haddockVersion"
echo "    command for haddock         : $haddockCmd"
fi

#echo
#echo "Misc debug/tmp info:"
#echo "  hardtop_plat                        : $hardtop_plat"

