# Process this file with autoconf to produce a configure script.
AC_INIT(EH, [0.1], [atze@cs.uu.nl])

# version of autoconf used (and required)
#AC_PREREQ(2.52)

# revision
AC_REVISION($Revision: 1.26 $)

# set MAKE flag in relevant makefiles
AC_PROG_MAKE_SET
# Checks for host/target
AC_CANONICAL_HOST
dnl ** canonicalize platform names
HostPlatform=`/bin/sh $srcdir/config.sub $host` || exit 1
echo $host

# Version info
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

# 'with' options
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

AC_ARG_WITH(uuagc,
[AC_HELP_STRING([--with-uuagc=<uu ag compiler>],
                [Use a command different from 'uuagc' to compile with UUAGC.])],
[ UUAGC_CMD="$withval" ],
[ 
  if test "$UUAGC" = ""; then
    AC_PATH_PROG(UUAGC_CMD,uuagc)
  else
    UUAGC_CMD=$UUAGC
  fi
]
)
if test "$UUAGC_CMD" = "no"; then
  UUAGC_CMD=""
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# what to make
#AC_SUBST(UUST_SUBDIRS,"shc lib")

# machine dependent code
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
  *)
        mach_dep_c_for_tailcall_leave1=""
        mach_dep_c_for_tailcall_leave2=""
        mach_dep_c_for_tailcall_enter=""
        ;;
esac
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_ENTER,"$mach_dep_c_for_tailcall_enter")
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_LEAVE1,"$mach_dep_c_for_tailcall_leave1")
AC_SUBST(MACH_DEP_C_FOR_TAILCALL_LEAVE2,"$mach_dep_c_for_tailcall_leave2")

# platform specific stuff
case $HostPlatform in
  i*86-*-mingw* | i*86-*-cygwin* )
        suffix_shell=".bat"
        suffix_exec=".exe"
        paths_sep=";"
        ;;
  *)
        suffix_shell=""
        suffix_exec=""
        paths_sep=":"
        ;;
esac
AC_SUBST(SUFFIX_SHELL,"$suffix_shell")
AC_SUBST(SUFFIX_EXEC,"$suffix_exec")
AC_SUBST(PATHS_SEP,"$paths_sep")

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
        ;;
  *)
        hardtop_plat=${hardtop}
        ;;
esac
AC_SUBST(TOP_ABS, "$hardtop_plat")

# file generation
AC_CONFIG_FILES([mk/config.mk])
AC_CONFIG_FILES([mk/shared.mk])
AC_CONFIG_FILES([src/grinc/Config.chs])
AC_CONFIG_FILES([src/ehc/Config.chs])
AC_OUTPUT

echo
echo "Installation/build summary:"
echo "  host                          : $host"
echo "  prefix                        : $prefix"

echo "  ghc is available?             : $ghcExists"
if test "x$ghcExists" = "xyes" ;then 
echo "    version                     : $ghcVersion"
echo "    command for ghc             : $ghcCmd"
#echo "    library install dir for ghc : $ghcLibDir"
fi

echo
echo "Misc debug/tmp info:"
echo "  hardtop_plat                        : $hardtop_plat"

