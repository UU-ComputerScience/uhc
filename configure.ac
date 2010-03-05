### Process this file with autoconf to produce a configure script.
AC_INIT(EH, [0.1], [uhc-developers@cs.uu.nl, http://www.cs.uu.nl/wiki/UHC/WebHome])

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

### Version info, extract from file VERSION
# big change
EH_VERSION_MAJOR=`awk -F . '{print $1}' VERSION`
# small change, not usable with previous
EH_VERSION_MINOR=`awk -F . '{print $2}' VERSION`
# small change, still usable with previous
EH_VERSION_MINORMINOR=`awk -F . '{print $3}' VERSION`

# 20090415: this amount of detail will not be used, will be obsolete
EH_VERSION_STABILITY=alpha

AC_SUBST(EH_VERSION_MAJOR)
AC_SUBST(EH_VERSION_MINOR)
AC_SUBST(EH_VERSION_MINORMINOR)
AC_SUBST(EH_VERSION_STABILITY)
AC_SUBST(EH_VERSION_SHORT,$EH_VERSION_MAJOR.$EH_VERSION_MINOR)
AC_SUBST(EH_VERSION_FULL,$EH_VERSION_MAJOR.$EH_VERSION_MINOR.$EH_VERSION_MINORMINOR)

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

### obvious utils which are amiss on some platforms: m4
REQUIRED_PROG_FOR_EH(m4Cmd,m4)

### svn log/history
AC_PATH_PROG(svnCmd,svn)
if test -x "$svnCmd"
then
  svnExists="yes"
  ### $svnCmd log -rHEAD:1 --limit 100 > SVNLOG
else
  svnExists="no"
fi
AC_SUBST(SVN_EXISTS,$svnExists)
AC_SUBST(SVN_CMD,$svnCmd)

### svn revision
AC_PATH_PROG(svnVersionCmd,svnversion)
if test -x "$svnVersionCmd"
then
  svnVersionExists="yes"
  $svnVersionCmd > SVNREVISION
else
  svnVersionExists="no"
fi
svnRevision=`cat SVNREVISION`
AC_SUBST(SVN_VERSION_EXISTS,$svnVersionExists)
AC_SUBST(SVN_VERSION_CMD,$svnVersionCmd)
AC_SUBST(SVN_REVISION,$svnRevision)

### enable options: java
AC_ARG_ENABLE(java, AS_HELP_STRING([--enable-java],[include java code generation]), [enableJava=yes], [enableJava=no])
AC_SUBST(ENABLE_JAVA,$enableJava)
if test x$enableJava = xyes
then
  REQUIRED_PROG_FOR_EH(javacCmd,javac)
  REQUIRED_PROG_FOR_EH(jarCmd,jar)
  if test "$javacCmd" = "no"; then
    javacCmd=""
    jarCmd=""
    javacExists="no"
  else
    javacExists="yes"
  fi
  case $HostPlatform in
    i*86-*-mingw* | i*86-*-cygwin* )
          if test ! -x "$toplevelSystemAbsolutePathPrefix/$jarCmd"
          then
            jarCmd="`echo $jarCmd | sed -e 's+/usr++'`"
          fi
          if test ! -x "$toplevelSystemAbsolutePathPrefix/$javacCmd"
          then
            javacCmd="`echo $javacCmd | sed -e 's+/usr++'`"
          fi
          ;;
    *)
          ;;
  esac  
  AC_SUBST(JAVAC_CMD,$javacCmd)
  AC_SUBST(JAR_CMD,$jarCmd)
fi

### enable options: llvm
AC_ARG_ENABLE(llvm, AS_HELP_STRING([--enable-llvm],[include llvm code generation]), [enableLlvm=yes], [enableLlvm=no])
AC_SUBST(ENABLE_LLVM,$enableLlvm)

### enable options: clr
AC_ARG_ENABLE(clr, AS_HELP_STRING([--enable-clr],[include clr code generation]), [enableClr=yes], [enableClr=no])
AC_SUBST(ENABLE_CLR,$enableClr)

### ar, ranlib, libtool
REQUIRED_PROG_FOR_EH(ranlibCmd,ranlib)
# See remarks about cpp
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$toplevelSystemAbsolutePathPrefix/$ranlibCmd"
        then
          ranlibCmd="`echo $ranlibCmd | sed -e 's+/usr++'`"
        fi
        ;;
  *)
        ;;
esac  
AC_SUBST(RANLIB_CMD,$ranlibCmd)

REQUIRED_PROG_FOR_EH(arCmd,ar)
# See remarks about cpp
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$toplevelSystemAbsolutePathPrefix/$arCmd"
        then
          arCmd="`echo $arCmd | sed -e 's+/usr++'`"
        fi
        ;;
  *)
        ;;
esac  
AC_SUBST(AR_CMD,$arCmd)

REQUIRED_OPTIONAL_PROG_FOR_EH(libtoolStaticCmd,libtool)

### 'with' option for gcc options
AC_ARG_WITH(gcc-ehc-options,
[AC_HELP_STRING([--with-gcc-ehc-options=<gcc compiler options for compiling runtime and ehc's gcc invocation>],
                [Additional options to pass to GCC when compiling with EHC.])],
[ gccEhcOptions="$withval" ],)

# additional platform dependend options for gcc when compiling C from within ehc
case $HostPlatform in
  i*86-pc-linux-gnu )
        #gccEhcOptions="$gccEhcOptions -D__USE_BSD -D__USE_POSIX"
        ;;
  *)
        ;;
esac  

AC_SUBST(GCC_EHC_OPTIONS, "$gccEhcOptions")

### 'with' option for cpp options
AC_ARG_WITH(cpp-ehc-options,
[AC_HELP_STRING([--with-cpp-ehc-options=<cpp preprocessor options for ehc's cpp invocation>],
                [Additional options to pass to GCC when compiling with EHC.])],
[ cppEhcOptions="$withval" ],)

# additional platform dependend options for cpp when compiling C from within ehc
case $HostPlatform in
  i*86-pc-linux-gnu )
        cppEhcOptions="$cppEhcOptions -D__STDC__"
        #cppEhcOptions="$cppEhcOptions -D__USE_BSD -D__USE_POSIX -D__STDC__"
        ;;
  x86_64-*-linux-gnu )
        cppEhcOptions="$cppEhcOptions -D__STDC__"
        #cppEhcOptions="$cppEhcOptions -D__USE_BSD -D__USE_POSIX -D__STDC__"
        ;;
  *)
        ;;
esac  

AC_SUBST(CPP_EHC_OPTIONS, "$cppEhcOptions")

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
[AC_HELP_STRING([--with-ehc-unixtool-prefix=<additional prefix for unixtools (already with absolute pathnames) used by ehc>],
                [Additional prefix for unixtools used by ehc.])],
[ toplevelSystemAbsolutePathPrefix="$withval" ],)

# when no unixtool prefix is specified && running cygwin, take cygwin's equivalent of /
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test "$toplevelSystemAbsolutePathPrefix" = ""
        then
          toplevelSystemAbsolutePathPrefix="`cygpath --mixed /`"
        fi
        ;;
  *)
        ;;
esac             

AC_SUBST(TOPLEVEL_SYSTEM_ABSPATH_PREFIX, "$toplevelSystemAbsolutePathPrefix")

### 'with' option for Haskell compiler, by default ghc
AC_ARG_WITH(ghc,
[AC_HELP_STRING([--with-hc=<haskell compiler>],
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
if test "$ghcCmd" != "no" -a -x "$ghcCmd" ; then
  ghcExists="yes"
  ghcVersion=`$ghcCmd --numeric-version`
else
  ghcCmd=""
  ghcExists="no"
fi
AC_SUBST(GHC_CMD,$ghcCmd)
AC_SUBST(GHC_VERSION, $ghcVersion)

case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
  ghcCmd1=`echo $ghcCmd | sed 's+/\(.\)/+\1:/+'`
  echo "ghcCmd is $ghcCmd, raw is $ghcCmd1"
  echo $ghcCmd | sed 's+/\(.\)/+\1:/+'
  ;;
  *)
  ghcCmd1=$ghcCmd
  ;;
esac

AC_SUBST(GHC_CMD1,$ghcCmd1)


### now we know we have ghc, check for some libraries using ghc-pkg
AC_PATH_PROG(ghcPkgCmd,ghc-pkg)
ghcLibUulibExists=no
ghcLibFglExists=no
ghcLibNetwork=no
ghcLibBinary=no
if test x$ghcExists = xyes
then
  if test -x "$ghcCmd" -a -x "$ghcPkgCmd"
  then
    # fgl
    ghcLibFgl=`$ghcPkgCmd list --simple-output fgl`
    if test "x$ghcLibFgl" != "x" ; then
      ghcLibFglExists=yes
    fi

    # uulib
    ghcLibUulib=`$ghcPkgCmd list --simple-output uulib`
    if test "x$ghcLibUulib" != "x" ; then
      ghcLibUulibExists=yes
    fi

    # network
    ghcLibNetwork=`$ghcPkgCmd list --simple-output network`
    if test "x$ghcLibNetwork" != "x" ; then
      ghcLibNetworkExists=yes
    fi

    # binary
    ghcLibBinary=`$ghcPkgCmd list --simple-output binary`
    if test "x$ghcLibBinary" != "x" ; then
      ghcLibBinaryExists=yes
    fi
  fi
fi

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
if test "$haddockCmd" != "no" -a -x "$haddockCmd"; then
  haddockExists="yes"
  haddockVersion=`$haddockCmd --version | head -n 1`
else
  haddockCmd=""
  haddockExists="no"
fi
AC_SUBST(HADDOCK_CMD,$haddockCmd)
AC_SUBST(HADDOCK_VERSION, $haddockVersion)

# GHC version dependencies: packages passed as option, cabal packages
cabal_base_lib_depends="base haskell98 mtl fgl directory old-time"
if test x$enableClr = "xyes"
then
  cabal_base_lib_depends="$cabal_base_lib_depends language-cil"
fi
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
    cabal_base_lib_depends="$cabal_base_lib_depends containers array process"
    ;;
  6.10*)
    cabal_base_lib_depends="$cabal_base_lib_depends containers array process"
    ;;
  *)
    cabal_base_lib_depends="$cabal_base_lib_depends containers array process"
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
if test "$uuagcCmd" != "no" -a -x "$uuagcCmd" ; then
  uuagcExists="yes"
  uuagcVersion=`$uuagcCmd --version`
else
  uuagcCmd=""
  uuagcExists="no"
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
if test "$gccCmd" != "no" -a -x "$gccCmd" ; then
  gccExists="yes"
  gccVersion=`$gccCmd -dumpversion`
else
  gccCmd=""
  gccExists="no"
fi

# See remarks about cpp
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$toplevelSystemAbsolutePathPrefix/$gccCmd"
        then
          gccCmd="`echo $gccCmd | sed -e 's+/usr++'`"
          if test -L "$toplevelSystemAbsolutePathPrefix/$gccCmd"
          then
            gccCmd="$gccCmd-3"
          fi
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
    cc_std_flag="-std=gnu99"
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
if test "$cppCmd" != "no" -a -x "$cppCmd"; then
  cppExists="yes"
  cppVersion=`$cppCmd --version`
else
  cppCmd=""
  cppExists="no"
fi

# Now we have to deal with weird cygwin behaviour which allows /usr/bin to be used
# whereas in reality it is mapped onto /bin. Direct acccess to /usr/bin with a c:/cygwin prefix therefore does not work
# and has to be stripped of the /usr part.
# Then, the resulting filename may refer to a symbolic link. If so, Try to dereference it by postfixing -3, that is cpp becomes cpp-3
# For other tools like gcc we have to this too.
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        if test ! -x "$toplevelSystemAbsolutePathPrefix/$cppCmd"
        then
          cppCmd="`echo $cppCmd | sed -e 's+/usr++'`"
          if test -L "$toplevelSystemAbsolutePathPrefix/$cppCmd"
          then
            cppCmd="$cppCmd-3"
          fi
        fi
        ;;
  *)
        ;;
esac  

AC_SUBST(CPP_CMD,$cppCmd)


if test x$enableLlvm = xyes
then
  # LLVM Assembler 
  AC_ARG_WITH(llvm-as,
  [AC_HELP_STRING([--with-llvm-as=<LLVM Assembler>],
                  [Path to the LLVM Assembler])],
  [ llvmAs="$withval" ],
  [ 
    if test "x$LLVM_AS" = "x"; then
      if test x$enableLlvm = xyes
      then
        REQUIRED_PROG_FOR_EH(llvmAs,llvm-as)
      fi
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
    if test "x$LLVM_OPT" = "x"; then
      if test x$enableLlvm = xyes
      then
        REQUIRED_PROG_FOR_EH(llvmOpt,opt)
      fi
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
    if test "x$LLC" = "x"; then
      if test x$enableLlvm = xyes
      then
        REQUIRED_PROG_FOR_EH(llvmLlc,llc)
      fi
    else
      llvmLlc=$LLC
    fi
  ]
  )
  AC_SUBST(LLVM_LLC_CMD,$llvmLlc)
  
  if test -x $llvmLlc -a -x $llvmAs -a -x $llvmOpt
  then
    llvmExists=yes
  else
    llvmExists=no
  fi
fi

# md5
AC_PATH_PROG(md5Cmd,md5 md5sum)
AC_SUBST(MD5_CMD,$md5Cmd)

# xxx
#AC_PATH_PROG(xxxCmd,xxx)
#echo xxxCmd: $xxxCmd

### Checks for libraries.

### Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

### what to make
#AC_SUBST(UUST_SUBDIRS,"shc lib")

### header & function presence

AC_HEADER_TIOCGWINSZ
AC_CHECK_HEADERS(dirent.h errno.h fcntl.h inttypes.h limits.h signal.h stdint.h string.h sys/resource.h sys/select.h sys/stat.h sys/syscall.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/types.h sys/utsname.h sys/wait.h termios.h time.h unistd.h utime.h vfork.h wctype.h windows.h)
# problematic headers, currently not dealt with:
#AC_CHECK_HEADERS(winsock.h)
AC_CHECK_FUNCS(ftruncate getclock getrusage gettimeofday lstat readdir_r _chsize)

### type configuration, machine characteristics

# sizeof GrWord
# AC_CHECK_SIZEOF([unsigned int])

# sizeof GrWord*, Pointer, ...
AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([unsigned char])
AC_CHECK_SIZEOF([unsigned short])
AC_CHECK_SIZEOF([unsigned int])
AC_CHECK_SIZEOF([unsigned long])
AC_CHECK_SIZEOF([unsigned long long])
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([intptr_t])
AC_CHECK_SIZEOF([uintptr_t])

AC_CHECK_SIZEOF([uint8_t])
AC_CHECK_SIZEOF([uint16_t])
AC_CHECK_SIZEOF([uint32_t])
AC_CHECK_SIZEOF([uint64_t])

AC_CHECK_SIZEOF([dev_t])
AC_CHECK_SIZEOF([ino_t])
AC_CHECK_SIZEOF([mode_t])
AC_CHECK_SIZEOF([off_t])
AC_CHECK_SIZEOF([pid_t])
AC_CHECK_SIZEOF([ssize_t])
AC_CHECK_SIZEOF([gid_t])
AC_CHECK_SIZEOF([nlink_t])
AC_CHECK_SIZEOF([uid_t])
AC_CHECK_SIZEOF([cc_t])
AC_CHECK_SIZEOF([speed_t])
AC_CHECK_SIZEOF([tcflag_t])
AC_CHECK_SIZEOF([rlim_t])
AC_CHECK_SIZEOF([clock_t])
AC_CHECK_SIZEOF([time_t])
AC_CHECK_SIZEOF([size_t])
AC_CHECK_SIZEOF([wchar_t])

AC_CHECK_ALIGNOF([char])
AC_CHECK_ALIGNOF([short])
AC_CHECK_ALIGNOF([int])
AC_CHECK_ALIGNOF([long])
AC_CHECK_ALIGNOF([long long])
AC_CHECK_ALIGNOF([unsigned char])
AC_CHECK_ALIGNOF([unsigned short])
AC_CHECK_ALIGNOF([unsigned int])
AC_CHECK_ALIGNOF([unsigned long])
AC_CHECK_ALIGNOF([unsigned long long])
AC_CHECK_ALIGNOF([void *])
AC_CHECK_ALIGNOF([float])
AC_CHECK_ALIGNOF([double])
AC_CHECK_ALIGNOF([intptr_t])
AC_CHECK_ALIGNOF([uintptr_t])

AC_CHECK_ALIGNOF([uint8_t])
AC_CHECK_ALIGNOF([uint16_t])
AC_CHECK_ALIGNOF([uint32_t])
AC_CHECK_ALIGNOF([uint64_t])

AC_C_BIGENDIAN([AC_DEFINE([BIGENDIAN],[1],[Big Endian architecture]) AC_DEFINE([LITTLEENDIAN],[0],[Little Endian architecture])],
               [AC_DEFINE([LITTLEENDIAN],[1],[Little Endian architecture]) AC_DEFINE([BIGENDIAN],[0],[Big Endian architecture])],
               [])

# Haskell types corresponding to C types, defined here for those independent of 32/64 bitness of platform
AC_DEFINE([HTYPE_CHAR],[Int8],[Haskell type for char])
AC_DEFINE([HTYPE_DOUBLE],[Double],[Haskell type for double])
AC_DEFINE([HTYPE_FLOAT],[Float],[Haskell type for float])
AC_DEFINE([HTYPE_INTMAX_T],[Int64],[Haskell type for intmax_t])
AC_DEFINE([HTYPE_LONG_LONG],[Int64],[Haskell type for long long])
AC_DEFINE([HTYPE_SHORT],[Int16],[Haskell type for short])
AC_DEFINE([HTYPE_SIGNED_CHAR],[Int8],[Haskell type for signed char])
AC_DEFINE([HTYPE_UINTMAX_T],[Word64],[Haskell type for uintmax_t])
AC_DEFINE([HTYPE_UNSIGNED_CHAR],[Word8],[Haskell type for unsigned char])
AC_DEFINE([HTYPE_UNSIGNED_LONG_LONG],[Word64],[Haskell type for unsigned long long])
AC_DEFINE([HTYPE_UNSIGNED_SHORT],[Word16],[Haskell type for short])

# C error codes
AC_DEFINE([CONST_E2BIG],[E2BIG],[Value of E2BIG])
AC_DEFINE([CONST_EACCES],[EACCES],[Value of EACCES])
AC_DEFINE([CONST_EADDRINUSE],[EADDRINUSE],[Value of EADDRINUSE])
AC_DEFINE([CONST_EADDRNOTAVAIL],[EADDRNOTAVAIL],[Value of EADDRNOTAVAIL])
AC_DEFINE([CONST_EADV],[EADV],[Value of EADV])
AC_DEFINE([CONST_EAFNOSUPPORT],[EAFNOSUPPORT],[Value of EAFNOSUPPORT])
AC_DEFINE([CONST_EAGAIN],[EAGAIN],[Value of EAGAIN])
AC_DEFINE([CONST_EALREADY],[EALREADY],[Value of EALREADY])
AC_DEFINE([CONST_EBADF],[EBADF],[Value of EBADF])
AC_DEFINE([CONST_EBADMSG],[EBADMSG],[Value of EBADMSG])
AC_DEFINE([CONST_EBADRPC],[EBADRPC],[Value of EBADRPC])
AC_DEFINE([CONST_EBUSY],[EBUSY],[Value of EBUSY])
AC_DEFINE([CONST_ECHILD],[ECHILD],[Value of ECHILD])
AC_DEFINE([CONST_ECOMM],[ECOMM],[Value of ECOMM])
AC_DEFINE([CONST_ECONNABORTED],[ECONNABORTED],[Value of ECONNABORTED])
AC_DEFINE([CONST_ECONNREFUSED],[ECONNREFUSED],[Value of ECONNREFUSED])
AC_DEFINE([CONST_ECONNRESET],[ECONNRESET],[Value of ECONNRESET])
AC_DEFINE([CONST_EDEADLK],[EDEADLK],[Value of EDEADLK])
AC_DEFINE([CONST_EDESTADDRREQ],[EDESTADDRREQ],[Value of EDESTADDRREQ])
AC_DEFINE([CONST_EDIRTY],[EDIRTY],[Value of EDIRTY])
AC_DEFINE([CONST_EDOM],[EDOM],[Value of EDOM])
AC_DEFINE([CONST_EDQUOT],[EDQUOT],[Value of EDQUOT])
AC_DEFINE([CONST_EEXIST],[EEXIST],[Value of EEXIST])
AC_DEFINE([CONST_EFAULT],[EFAULT],[Value of EFAULT])
AC_DEFINE([CONST_EFBIG],[EFBIG],[Value of EFBIG])
AC_DEFINE([CONST_EFTYPE],[EFTYPE],[Value of EFTYPE])
AC_DEFINE([CONST_EHOSTDOWN],[EHOSTDOWN],[Value of EHOSTDOWN])
AC_DEFINE([CONST_EHOSTUNREACH],[EHOSTUNREACH],[Value of EHOSTUNREACH])
AC_DEFINE([CONST_EIDRM],[EIDRM],[Value of EIDRM])
AC_DEFINE([CONST_EILSEQ],[EILSEQ],[Value of EILSEQ])
AC_DEFINE([CONST_EINPROGRESS],[EINPROGRESS],[Value of EINPROGRESS])
AC_DEFINE([CONST_EINTR],[EINTR],[Value of EINTR])
AC_DEFINE([CONST_EINVAL],[EINVAL],[Value of EINVAL])
AC_DEFINE([CONST_EIO],[EIO],[Value of EIO])
AC_DEFINE([CONST_EISCONN],[EISCONN],[Value of EISCONN])
AC_DEFINE([CONST_EISDIR],[EISDIR],[Value of EISDIR])
AC_DEFINE([CONST_ELOOP],[ELOOP],[Value of ELOOP])
AC_DEFINE([CONST_EMFILE],[EMFILE],[Value of EMFILE])
AC_DEFINE([CONST_EMLINK],[EMLINK],[Value of EMLINK])
AC_DEFINE([CONST_EMSGSIZE],[EMSGSIZE],[Value of EMSGSIZE])
AC_DEFINE([CONST_EMULTIHOP],[EMULTIHOP],[Value of EMULTIHOP])
AC_DEFINE([CONST_ENAMETOOLONG],[ENAMETOOLONG],[Value of ENAMETOOLONG])
AC_DEFINE([CONST_ENETDOWN],[ENETDOWN],[Value of ENETDOWN])
AC_DEFINE([CONST_ENETRESET],[ENETRESET],[Value of ENETRESET])
AC_DEFINE([CONST_ENETUNREACH],[ENETUNREACH],[Value of ENETUNREACH])
AC_DEFINE([CONST_ENFILE],[ENFILE],[Value of ENFILE])
AC_DEFINE([CONST_ENOBUFS],[ENOBUFS],[Value of ENOBUFS])
AC_DEFINE([CONST_ENOCIGAR],[ENOCIGAR],[Value of ENOCIGAR])
AC_DEFINE([CONST_ENODATA],[ENODATA],[Value of ENODATA])
AC_DEFINE([CONST_ENODEV],[ENODEV],[Value of ENODEV])
AC_DEFINE([CONST_ENOENT],[ENOENT],[Value of ENOENT])
AC_DEFINE([CONST_ENOEXEC],[ENOEXEC],[Value of ENOEXEC])
AC_DEFINE([CONST_ENOLCK],[ENOLCK],[Value of ENOLCK])
AC_DEFINE([CONST_ENOLINK],[ENOLINK],[Value of ENOLINK])
AC_DEFINE([CONST_ENOMEM],[ENOMEM],[Value of ENOMEM])
AC_DEFINE([CONST_ENOMSG],[ENOMSG],[Value of ENOMSG])
AC_DEFINE([CONST_ENONET],[ENONET],[Value of ENONET])
AC_DEFINE([CONST_ENOPROTOOPT],[ENOPROTOOPT],[Value of ENOPROTOOPT])
AC_DEFINE([CONST_ENOSPC],[ENOSPC],[Value of ENOSPC])
AC_DEFINE([CONST_ENOSR],[ENOSR],[Value of ENOSR])
AC_DEFINE([CONST_ENOSTR],[ENOSTR],[Value of ENOSTR])
AC_DEFINE([CONST_ENOSYS],[ENOSYS],[Value of ENOSYS])
AC_DEFINE([CONST_ENOTBLK],[ENOTBLK],[Value of ENOTBLK])
AC_DEFINE([CONST_ENOTCONN],[ENOTCONN],[Value of ENOTCONN])
AC_DEFINE([CONST_ENOTDIR],[ENOTDIR],[Value of ENOTDIR])
AC_DEFINE([CONST_ENOTEMPTY],[ENOTEMPTY],[Value of ENOTEMPTY])
AC_DEFINE([CONST_ENOTSOCK],[ENOTSOCK],[Value of ENOTSOCK])
AC_DEFINE([CONST_ENOTTY],[ENOTTY],[Value of ENOTTY])
AC_DEFINE([CONST_ENXIO],[ENXIO],[Value of ENXIO])
AC_DEFINE([CONST_EOPNOTSUPP],[EOPNOTSUPP],[Value of EOPNOTSUPP])
AC_DEFINE([CONST_EPERM],[EPERM],[Value of EPERM])
AC_DEFINE([CONST_EPFNOSUPPORT],[EPFNOSUPPORT],[Value of EPFNOSUPPORT])
AC_DEFINE([CONST_EPIPE],[EPIPE],[Value of EPIPE])
AC_DEFINE([CONST_EPROCLIM],[EPROCLIM],[Value of EPROCLIM])
AC_DEFINE([CONST_EPROCUNAVAIL],[EPROCUNAVAIL],[Value of EPROCUNAVAIL])
AC_DEFINE([CONST_EPROGMISMATCH],[EPROGMISMATCH],[Value of EPROGMISMATCH])
AC_DEFINE([CONST_EPROGUNAVAIL],[EPROGUNAVAIL],[Value of EPROGUNAVAIL])
AC_DEFINE([CONST_EPROTO],[EPROTO],[Value of EPROTO])
AC_DEFINE([CONST_EPROTONOSUPPORT],[EPROTONOSUPPORT],[Value of EPROTONOSUPPORT])
AC_DEFINE([CONST_EPROTOTYPE],[EPROTOTYPE],[Value of EPROTOTYPE])
AC_DEFINE([CONST_ERANGE],[ERANGE],[Value of ERANGE])
AC_DEFINE([CONST_EREMCHG],[EREMCHG],[Value of EREMCHG])
AC_DEFINE([CONST_EREMOTE],[EREMOTE],[Value of EREMOTE])
AC_DEFINE([CONST_EROFS],[EROFS],[Value of EROFS])
AC_DEFINE([CONST_ERPCMISMATCH],[ERPCMISMATCH],[Value of ERPCMISMATCH])
AC_DEFINE([CONST_ERREMOTE],[ERREMOTE],[Value of ERREMOTE])
AC_DEFINE([CONST_ESHUTDOWN],[ESHUTDOWN],[Value of ESHUTDOWN])
AC_DEFINE([CONST_ESOCKTNOSUPPORT],[ESOCKTNOSUPPORT],[Value of ESOCKTNOSUPPORT])
AC_DEFINE([CONST_ESPIPE],[ESPIPE],[Value of ESPIPE])
AC_DEFINE([CONST_ESRCH],[ESRCH],[Value of ESRCH])
AC_DEFINE([CONST_ESRMNT],[ESRMNT],[Value of ESRMNT])
AC_DEFINE([CONST_ESTALE],[ESTALE],[Value of ESTALE])
AC_DEFINE([CONST_ETIME],[ETIME],[Value of ETIME])
AC_DEFINE([CONST_ETIMEDOUT],[ETIMEDOUT],[Value of ETIMEDOUT])
AC_DEFINE([CONST_ETOOMANYREFS],[ETOOMANYREFS],[Value of ETOOMANYREFS])
AC_DEFINE([CONST_ETXTBSY],[ETXTBSY],[Value of ETXTBSY])
AC_DEFINE([CONST_EUSERS],[EUSERS],[Value of EUSERS])
AC_DEFINE([CONST_EWOULDBLOCK],[EWOULDBLOCK],[Value of EWOULDBLOCK])
AC_DEFINE([CONST_EXDEV],[EXDEV],[Value of EXDEV])
AC_DEFINE([CONST_O_BINARY],[O_BINARY],[Value of O_BINARY])
AC_DEFINE([CONST_SIGINT],[SIGINT],[Value of SIGINT])

### Find out whether we use 32 or 64 bits platform
# Here we assume a bit of the implementation to check on the result of AC_CHECK_SIZEOF([intptr_t]).
# We need it to add 32/64 bit related flags for external libraries.
# Usually this is ok, but for some combinations not so.
if test "x$ac_cv_sizeof_intptr_t" = "x4"
then
  use32bits="yes"
  use64bits="no"
elif test "x$ac_cv_sizeof_intptr_t" = "x8"
then
  use32bits="no"
  use64bits="yes"
else
  use32bits=""
  use64bits=""
fi

if test "x$use32bits" = "xyes"
then
  gccExtlibsOptions="$gccExtlibsOptions -m32"
  gmpABI="32"
fi

if test "x$use64bits" = "xyes"
then
  gccExtlibsOptions="$gccExtlibsOptions -m64"
  gmpABI="64"
fi

AC_SUBST(GCC_EXTLIBS_OPTIONS,$gccExtlibsOptions)
AC_SUBST(GMP_ABI,$gmpABI)

### general configuration

# Use Boehm's GC
AC_DEFINE([USE_BOEHM_GC],[1],[Use Boehm's garbage collector])

# which MP library
AC_ARG_WITH(mplib,
[AC_HELP_STRING([--with-mplib=ltm|gmp],
                [Specify which MP library to use. Default is ltm (builtin derivative of LibTomMath).])],
[ mplib="$withval" ],
[ mplib="ltm" ]
)
AC_SUBST(MPLIB,$mplib)

# Use GMP or LTM
if test "$mplib" = "gmp" ; then
  AC_DEFINE([USE_GMP],[1],[Use GMP (GNU Multiple Precision) library])
  AC_DEFINE([USE_LTM],[0],[Use LTM (LibTomMath) library])
else
  AC_DEFINE([USE_GMP],[0],[Use GMP (GNU Multiple Precision) library])
  AC_DEFINE([USE_LTM],[1],[Use LTM (LibTomMath) library])
fi

# which GMP archive
AC_ARG_WITH(gmplibarchive,
[AC_HELP_STRING([--with-gmplibarchive=<gmp lib archive>],
                [If GMP is used, specify absolute location of GMP archive (named like .../gmp-X.Y.Z.tar.gz).])],
[ gmpLibArchive="$withval" ],
[ gmpLibArchive=`pwd`/"extlibs/gmp/gmp-4.2.1.tar.gz" ]
)
if test -r "$gmpLibArchive" ; then
  gmpLibArchiveExists="yes"
else
  gmpLibArchiveExists="no"
fi
AC_SUBST(GMP_LIB_ARCHIVE, "$gmpLibArchive")

# Prefix used for global values, assumed to be present by RTS
AC_DEFINE([RTS_GLOBAL_VAR_PREFIX],["global_"],[Prefix for RTS globals (obsolete?)])

### machine dependent code
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        ;;
  powerpc-apple-* )
        ;;
  i*86-pc-linux-gnu )
        gcc_ehc_extra_extern_libs="pthread"
        ;;
  *-linux-gnu )
        gcc_ehc_extra_extern_libs="pthread"
        ;;
  x86_64-* )
        ;;
  i386-apple-* )
        ;;
  *)
        ;;
esac
AC_SUBST(GCC_EHC_EXTRA_EXTERN_LIBS,"$gcc_ehc_extra_extern_libs")

### other platform specific stuff
# defaults:
# file suffixes
suffix_shell=""
suffix_exec=""
suffix_lib=".a"
prefix_lib=""
# development platform
development_platform="UNIX"
# path separator
paths_sep=":"
slash="/"
# libtool usage
#libtoolStaticCmd="libtool"
libtoolStaticOpts="-static -o"
libtoolDynamicCmd="echo Dynamic linking not available: "
libtoolDynamicOpts=""
ehcGccOptsStatic=""
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        # file suffixes
        suffix_shell=".bat"
        suffix_exec=".exe"
        #suffix_lib=".lib"
        #prefix_lib="lib"
        development_platform="CYGWIN"
        # path separator
        paths_sep=";"
        slash="\\\\"
        # libtool usage
        libtoolStaticCmd="no"
        ;;
  i*86-pc-linux-gnu )
        # file suffixes
        # path separator
        # libtool usage
        #libtoolStaticCmd="no"
        # ehcGccOptsStatic="-static"
	    # libtoolStaticCmd="libtool --mode=link gcc -static -o"
        libtoolStaticCmd="no"
        ;;
  x86*-*-linux-gnu )
        # file suffixes
        # path separator
        # libtool usage
        # libtoolStaticCmd="libtool --mode=link gcc -static -o"
        libtoolStaticCmd="no"
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

# misc
AC_SUBST(DEVELOPMENT_PLATFORM,"$development_platform")

# libtool used, if any
AC_SUBST(LIBTOOL_STATIC_CMD, "$libtoolStaticCmd")
AC_SUBST(LIBTOOL_STATIC_OPTS, "$libtoolStaticOpts")
AC_SUBST(LIBTOOL_DYNAMIC_CMD, "$libtoolDynamic")
AC_SUBST(LIBTOOL_DYNAMIC_OPTS, "$libtoolDynamicOpts")

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
        hardtop_plat2=${hardtop}
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

### name of subdirectory for shared ehc library artifacts
AC_SUBST(EHCLIB_SHARED, "shared")

### install locations
inplaceInstallDir="install"
inplaceInstallForBldDir="install-for-build"
AC_SUBST(INPLACE_TOP, "$hardtop")
AC_SUBST(INPLACE_PREFIX, "$hardtop_plat/$inplaceInstallDir")
AC_SUBST(INPLACE_INSTALL_DIR, "$inplaceInstallDir")
AC_SUBST(INPLACE_INSTALLFORBLD_DIR, "$inplaceInstallForBldDir")
AC_SUBST(INSTALL_LIB_SUFFIX, "lib/uhc")

### package names assumed by compiler driver
AC_SUBST(RTS_PKG_NAME, "EH-RTS")
AC_SUBST(EXTLIBS_BGC_PKG_NAME, "gc") # should coincide with installed lib in extlibs/bgc
AC_SUBST(EXTLIBS_GMP_PKG_NAME, "gmp") # should coincide with installed lib in extlibs/gmp

### if some of the required libraries/tools are absent, this def will check for it and exit when make is invoked
if test "x$uuagcExists" = "xyes" \
   -a "x$ghcExists" = "xyes" \
   -a "x$gccExists" = "xyes" \
   -a "x$ghcLibFglExists" = "xyes" \
   -a "x$ghcLibUulibExists" = "xyes" \
   -a "x$ghcLibNetworkExists" = "xyes" \
   -a "x$ghcLibBinaryExists" = "xyes"
then
  exitIfAbsentLibOrTool=""
else
  exitIfAbsentLibOrTool='echo "**** ERROR **** : A tool or library is missing. Check ./configure summary." ; exit 1'
fi
AC_SUBST(EXIT_IF_ABSENT_LIB_OR_TOOL, $exitIfAbsentLibOrTool)

### file generation
AC_CONFIG_FILES([mk/config.mk mk/shared.mk])
AC_CONFIG_FILES([test/benchmark/runbenchmark.pl])
AC_CONFIG_FILES([src/ehc/Config.chs src/helium/Utils/OSSpecific.hs])
# AC_CONFIG_FILES([bin/llvm-compilerdriver], [chmod +x bin/llvm-compilerdriver])
AC_CONFIG_HEADERS([src/ehc/ConfigDefines.chs src/rts/config.ch src/rts/mm/config.ch ehclib/base/include/MachDeps.h ehclib/base/include/HsBaseConfig.h])
AC_OUTPUT

echo
echo "Version: ${EH_VERSION_FULL}"
echo "Installation/build summary."
echo "  host                          : $host"
echo "  prefix                        : $prefix"

echo "Required:"
echo "  uuagc is available?           : $uuagcExists"
if test "x$uuagcExists" = "xyes" ;then 
echo "    version                     : $uuagcVersion"
echo "    command for uuagc           : $uuagcCmd"
else
echo "    **** ERROR **** : uuagc must be installed, use (1) cabal+hackage, or (2) http://www.cs.uu.nl/wiki/HUT"
fi

echo "  ghc is available?             : $ghcExists"
if test "x$ghcExists" = "xyes" ;then 
echo "    version                     : $ghcVersion"
echo "    command for ghc             : $ghcCmd"
echo "    command for ghc raw         : $ghcCmd1"
echo "    libraries:"
echo "      uulib is available?       : $ghcLibUulibExists"
  if test "x$ghcLibUulibExists" = "xyes"
  then
    echo "        version                 : $ghcLibUulib"
  else
    echo "        **** ERROR **** : 'uulib' must be installed, use (1) cabal+hackage, or (2) http://www.cs.uu.nl/wiki/HUT"
  fi
echo "      fgl is available?         : $ghcLibFglExists"
  if test "x$ghcLibFglExists" = "xyes"
  then
    echo "        version                 : $ghcLibFgl"
  else
    echo "        **** ERROR **** : 'fgl' must be installed, use cabal+hackage"
  fi
echo "      network is available?     : $ghcLibNetworkExists"
  if test "x$ghcLibNetworkExists" = "xyes"
  then
    echo "        version                 : $ghcLibNetwork"
  else
    echo "        **** ERROR **** : 'network' must be installed, use cabal+hackage"
  fi
echo "      binary is available?      : $ghcLibBinaryExists"
  if test "x$ghcLibBinaryExists" = "xyes"
  then
    echo "        version                 : $ghcLibBinary"
  else
    echo "        **** ERROR **** : 'binary' must be installed, use cabal+hackage"
  fi
else
echo "    **** ERROR **** : ghc must be installed, see http://www.haskell.org/ghc"
fi

echo "  gcc is available?             : $gccExists"
if test "x$gccExists" = "xyes" ;then 
echo "    version                     : $gccVersion"
echo "    command for gcc             : $gccCmd"
echo "    command for cpp             : $cppCmd"
else
echo "    **** ERROR **** : gcc must be installed, ask your OS guru"
fi

if test x$enableJava = xyes
then
  echo "  javac is available?           : $javacExists"
  if test "x$javacExists" = "xyes" ;then 
  echo "    command for javac           : $javacCmd"
  fi
fi

if test x$enableLlvm = xyes
then
  echo "  llvm is available?            : $llvmExists"
  echo "    command for llc             : $llvmLlc"
  if test -x $llvmLlc
  then
    echo "    version                     :" `$llvmLlc -version`
  else
    echo "    **** ERROR **** : llc is not available and/or executable"
  fi
  echo "    command for llvm-as         : $llvmAs"
  if test ! -x $llvmAs
  then
    echo "    **** ERROR **** : llvm-as is not available and/or executable"
  fi
  echo "    command for opt             : $llvmOpt"
  if test ! -x $llvmOpt
  then
    echo "    **** ERROR **** : opt is not available and/or executable"
  fi
fi

echo "  MP library                    : $mplib"
if test "$mplib" == "gmp" ; then
  if test "x$gmpLibArchiveExists" = "xyes" ;then
    echo "    gmp lib archive             : $gmpLibArchive"
  else
    echo "    **** ERROR **** : GMP lib archive not readable: $gmpLibArchive"
  fi
fi

echo "Optional:"
echo "  haddock is available?         : $haddockExists"
if test "x$haddockExists" = "xyes" ;then 
echo "    version                     : $haddockVersion"
echo "    command for haddock         : $haddockCmd"
fi

echo "Absolute location of directories:"
echo "  ehc repository                : $hardtop_plat2"
echo "  ehc repository raw            : $hardtop_plat"
echo "  unix root prefix raw          : $toplevelSystemAbsolutePathPrefix"
