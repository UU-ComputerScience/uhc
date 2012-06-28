/* ehclib/uhcbase/include/HsBaseConfig.h.  Generated from HsBaseConfig.h.in by configure.  */
#ifndef HSBASECONFIG_H
#define HSBASECONFIG_H

#include "MachDeps.h"

/********************************************************************/
/**** Sizes                                                      ****/
/********************************************************************/

#if SIZEOF_INTPTR_T == 8
#define HTYPE_INTPTR_T 			Int64
#define HTYPE_UINTPTR_T 		Word64
#else
#define HTYPE_INTPTR_T 			Int32
#define HTYPE_UINTPTR_T 		Word32
#endif

/* also here: typedef int sig_atomic_t
 */
#if SIZEOF_INT == 8
#define HTYPE_INT 				Int64
#define HTYPE_UNSIGNED_INT 		Word64
#define HTYPE_SIG_ATOMIC_T 		Word64
#else
#define HTYPE_INT 				Int32
#define HTYPE_UNSIGNED_INT 		Word32
#define HTYPE_SIG_ATOMIC_T 		Word32
#endif

#if SIZEOF_LONG == 8
#define HTYPE_LONG 				Int64
#define HTYPE_UNSIGNED_LONG 	Word64
#else
#define HTYPE_LONG 				Int32
#define HTYPE_UNSIGNED_LONG 	Word32
#endif

#if SIZEOF_VOID_P == 8
#define HTYPE_PTRDIFF_T 		Int64
#else
#define HTYPE_PTRDIFF_T 		Int32
#endif

#if SIZEOF_CLOCK_T == 8
#define HTYPE_CLOCK_T 			Word64
#else
#define HTYPE_CLOCK_T 			Word32
#endif

#if SIZEOF_TIME_T == 8
#define HTYPE_TIME_T 			Word64
#else
#define HTYPE_TIME_T 			Word32
#endif

#if SIZEOF_USECONDS_T == 8
#define HTYPE_USECONDS_T 		Word64
#else
#define HTYPE_USECONDS_T 		Word32
#endif

#if SIZEOF_SIZE_T == 8
#define HTYPE_SIZE_T 			Word64
#else
#define HTYPE_SIZE_T 			Word32
#endif

#if SIZEOF_WCHAR_T == 1
#define HTYPE_WCHAR_T 			Word8
#elif SIZEOF_WCHAR_T == 2
#define HTYPE_WCHAR_T 			Word16
#else
#define HTYPE_WCHAR_T 			Word32
#endif



/* Define to Haskell type for char */
#define HTYPE_CHAR Int8

/* Define to Haskell type for double */
#define HTYPE_DOUBLE Double

/* Define to Haskell type for float */
#define HTYPE_FLOAT Float

/* Define to Haskell type for intmax_t */
#define HTYPE_INTMAX_T Int64

/* Define to Haskell type for long long */
#define HTYPE_LONG_LONG Int64

/* Define to Haskell type for short */
#define HTYPE_SHORT Int16

/* Define to Haskell type for signed char */
#define HTYPE_SIGNED_CHAR Int8

/* Define to Haskell type for uintmax_t */
#define HTYPE_UINTMAX_T Word64

/* Define to Haskell type for unsigned char */
#define HTYPE_UNSIGNED_CHAR Word8

/* Define to Haskell type for unsigned long long */
#define HTYPE_UNSIGNED_LONG_LONG Word64

/* Define to Haskell type for unsigned short */
#define HTYPE_UNSIGNED_SHORT Word16



/********************************************************************/
/**** System tupes                                               ****/
/********************************************************************/

#ifdef SIZEOF_DEV_T
# if SIZEOF_DEV_T == 8
#  define HTYPE_DEV_T 			Word64
# elif SIZEOF_DEV_T == 4
#  define HTYPE_DEV_T 			Word32
# elif SIZEOF_DEV_T == 2
#  define HTYPE_DEV_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_INO_T
# if SIZEOF_INO_T == 8
#  define HTYPE_INO_T 			Word64
# elif SIZEOF_INO_T == 4
#  define HTYPE_INO_T 			Word32
# elif SIZEOF_INO_T == 2
#  define HTYPE_INO_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_MODE_T
# if SIZEOF_MODE_T == 8
#  define HTYPE_MODE_T 			Word64
# elif SIZEOF_MODE_T == 4
#  define HTYPE_MODE_T 			Word32
# elif SIZEOF_MODE_T == 2
#  define HTYPE_MODE_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_OFF_T
# if SIZEOF_OFF_T == 8
#  define HTYPE_OFF_T 			Word64
# elif SIZEOF_OFF_T == 4
#  define HTYPE_OFF_T 			Word32
# elif SIZEOF_OFF_T == 2
#  define HTYPE_OFF_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_PID_T
# if SIZEOF_PID_T == 8
#  define HTYPE_PID_T 			Word64
# elif SIZEOF_PID_T == 4
#  define HTYPE_PID_T 			Word32
# elif SIZEOF_PID_T == 2
#  define HTYPE_PID_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_SSIZE_T
# if SIZEOF_SSIZE_T == 8
#  define HTYPE_SSIZE_T 			Word64
# elif SIZEOF_SSIZE_T == 4
#  define HTYPE_SSIZE_T 			Word32
# elif SIZEOF_SSIZE_T == 2
#  define HTYPE_SSIZE_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_GID_T
# if SIZEOF_GID_T == 8
#  define HTYPE_GID_T 			Word64
# elif SIZEOF_GID_T == 4
#  define HTYPE_GID_T 			Word32
# elif SIZEOF_GID_T == 2
#  define HTYPE_GID_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_NLINK_T
# if SIZEOF_NLINK_T == 8
#  define HTYPE_NLINK_T 			Word64
# elif SIZEOF_NLINK_T == 4
#  define HTYPE_NLINK_T 			Word32
# elif SIZEOF_NLINK_T == 2
#  define HTYPE_NLINK_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_UID_T
# if SIZEOF_UID_T == 8
#  define HTYPE_UID_T 			Word64
# elif SIZEOF_UID_T == 4
#  define HTYPE_UID_T 			Word32
# elif SIZEOF_UID_T == 2
#  define HTYPE_UID_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_CC_T
# if SIZEOF_CC_T == 8
#  define HTYPE_CC_T 			Word64
# elif SIZEOF_CC_T == 4
#  define HTYPE_CC_T 			Word32
# elif SIZEOF_CC_T == 2
#  define HTYPE_CC_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_SPEED_T
# if SIZEOF_SPEED_T == 8
#  define HTYPE_SPEED_T 			Word64
# elif SIZEOF_SPEED_T == 4
#  define HTYPE_SPEED_T 			Word32
# elif SIZEOF_SPEED_T == 2
#  define HTYPE_SPEED_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_TCFLAG_T
# if SIZEOF_TCFLAG_T == 8
#  define HTYPE_TCFLAG_T 			Word64
# elif SIZEOF_TCFLAG_T == 4
#  define HTYPE_TCFLAG_T 			Word32
# elif SIZEOF_TCFLAG_T == 2
#  define HTYPE_TCFLAG_T 			Word16
# else
# endif
#endif

#ifdef SIZEOF_RLIM_T
# if SIZEOF_RLIM_T == 8
#  define HTYPE_RLIM_T 			Word64
# elif SIZEOF_RLIM_T == 4
#  define HTYPE_RLIM_T 			Word32
# elif SIZEOF_RLIM_T == 2
#  define HTYPE_RLIM_T 			Word16
# else
# endif
#endif

/********************************************************************/
/**** Presence of include files                                  ****/
/********************************************************************/

#define HAVE_DIRENT_H 1
#define HAVE_ERRNO_H 1
#define HAVE_FCNTL_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_LIMITS_H 1
#define HAVE_SIGNAL_H 1
#define HAVE_STDINT_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_SYSCALL_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TIMEB_H 1
/* #undef HAVE_SYS_TIMERS_H */
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UTSNAME_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_TERMIOS_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_UTIME_H 1
/* #undef HAVE_VFORK_H */
#define HAVE_WCTYPE_H 1
/* #undef HAVE_WINDOWS_H */
/* #undef HAVE_WINSOCK_H */

/* #undef HAVE__CHSIZE */
#define HAVE_FTRUNCATE 1
/* #undef HAVE_GETCLOCK */
#define HAVE_GETRUSAGE 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_LSTAT 1
#define HAVE_READDIR_R 1

/********************************************************************/
/**** Runtime types                                              ****/
/********************************************************************/

#ifdef __UHC_BUILDS_O__

#if defined(__UHC_TARGET_BC__) || defined(__UHC_TARGET_C__)
// # define HsBool				Word
// # define HsInt				Word
// # define HsWord64			Word64
// # define HsWord32			Word32
// # define HsWord16			Word16
// # define HsWord8			Word8
// # ifdef __UHC_TARGET_BC__
// #  define HS_BOOL_FALSE		gb_False
// #  define HS_BOOL_TRUE		gb_True
// # else
// #  define HS_BOOL_FALSE		0
// #  define HS_BOOL_TRUE		1
// # endif
#endif /* __UHC_TARGET_BC__ */

#endif /* __UHC_BUILDS_O__ */

/********************************************************************/
/**** Read dir EOF error                                         ****/
/********************************************************************/

/* See remarks in GHC, libraries/base/aclocal.m4: MinGW platform returns non 0 value. We ignore that */
#define	READDIR_ERRNO_EOF	0

/********************************************************************/
/**** C errors                                                   ****/
/********************************************************************/

/* (1) Assume errno.h is already included */
/* (2) Error codes like EADV are not present on all platforms, so a dummy value is provided, this should be done for all codes */

/* The value of E2BIG. */
#if !defined(E2BIG)
#define E2BIG 10000
#endif
#define CONST_E2BIG E2BIG

/* The value of EACCES. */
#if !defined(EACCES)
#define EACCES 10000
#endif
#define CONST_EACCES EACCES

/* The value of EADDRINUSE. */
#if !defined(EADDRINUSE)
#define EADDRINUSE 10000
#endif
#define CONST_EADDRINUSE EADDRINUSE

/* The value of EADDRNOTAVAIL. */
#define CONST_EADDRNOTAVAIL EADDRNOTAVAIL

/* The value of EADV. */
#if !defined(EADV)
#define EADV 10000
#endif
#define CONST_EADV EADV

/* The value of EAFNOSUPPORT. */
#if !defined(EAFNOSUPPORT)
#define EAFNOSUPPORT 10000
#endif
#define CONST_EAFNOSUPPORT EAFNOSUPPORT

/* The value of EAGAIN. */
#if !defined(EAGAIN)
#define EAGAIN 10000
#endif
#define CONST_EAGAIN EAGAIN

/* The value of EALREADY. */
#if !defined(EALREADY)
#define EALREADY 10000
#endif
#define CONST_EALREADY EALREADY

/* The value of EBADF. */
#if !defined(EBADF)
#define EBADF 10000
#endif
#define CONST_EBADF EBADF

/* The value of EBADMSG. */
#if !defined(EBADMSG)
#define EBADMSG 10000
#endif
#define CONST_EBADMSG EBADMSG

/* The value of EBADRPC. */
#if !defined(EBADRPC)
#define EBADRPC 10000
#endif
#define CONST_EBADRPC EBADRPC

/* The value of EBUSY. */
#if !defined(EBUSY)
#define EBUSY 10000
#endif
#define CONST_EBUSY EBUSY

/* The value of ECHILD. */
#if !defined(ECHILD)
#define ECHILD 10000
#endif
#define CONST_ECHILD ECHILD

/* The value of ECOMM. */
#if !defined(ECOMM)
#define ECOMM 10000
#endif
#define CONST_ECOMM ECOMM

/* The value of ECONNABORTED. */
#if !defined(ECONNABORTED)
#define ECONNABORTED 10000
#endif
#define CONST_ECONNABORTED ECONNABORTED

/* The value of ECONNREFUSED. */
#if !defined(ECONNREFUSED)
#define ECONNREFUSED 10000
#endif
#define CONST_ECONNREFUSED ECONNREFUSED

/* The value of ECONNRESET. */
#if !defined(ECONNRESET)
#define ECONNRESET 10000
#endif
#define CONST_ECONNRESET ECONNRESET

/* The value of EDEADLK. */
#if !defined(EDEADLK)
#define EDEADLK 10000
#endif
#define CONST_EDEADLK EDEADLK

/* The value of EDESTADDRREQ. */
#if !defined(EDESTADDRREQ)
#define EDESTADDRREQ 10000
#endif
#define CONST_EDESTADDRREQ EDESTADDRREQ

/* The value of EDIRTY. */
#if !defined(EDIRTY)
#define EDIRTY 10000
#endif
#define CONST_EDIRTY EDIRTY

/* The value of EDOM. */
#if !defined(EDOM)
#define EDOM 10000
#endif
#define CONST_EDOM EDOM

/* The value of EDQUOT. */
#if !defined(EDQUOT)
#define EDQUOT 10000
#endif
#define CONST_EDQUOT EDQUOT

/* The value of EEXIST. */
#if !defined(EEXIST)
#define EEXIST 10000
#endif
#define CONST_EEXIST EEXIST

/* The value of EFAULT. */
#if !defined(EFAULT)
#define EFAULT 10000
#endif
#define CONST_EFAULT EFAULT

/* The value of EFBIG. */
#if !defined(EFBIG)
#define EFBIG 10000
#endif
#define CONST_EFBIG EFBIG

/* The value of EFTYPE. */
#if !defined(EFTYPE)
#define EFTYPE 10000
#endif
#define CONST_EFTYPE EFTYPE

/* The value of EHOSTDOWN. */
#if !defined(EHOSTDOWN)
#define EHOSTDOWN 10000
#endif
#define CONST_EHOSTDOWN EHOSTDOWN

/* The value of EHOSTUNREACH. */
#if !defined(EHOSTUNREACH)
#define EHOSTUNREACH 10000
#endif
#define CONST_EHOSTUNREACH EHOSTUNREACH

/* The value of EIDRM. */
#if !defined(EIDRM)
#define EIDRM 10000
#endif
#define CONST_EIDRM EIDRM

/* The value of EILSEQ. */
#if !defined(EILSEQ)
#define EILSEQ 10000
#endif
#define CONST_EILSEQ EILSEQ

/* The value of EINPROGRESS. */
#if !defined(EINPROGRESS)
#define EINPROGRESS 10000
#endif
#define CONST_EINPROGRESS EINPROGRESS

/* The value of EINTR. */
#if !defined(EINTR)
#define EINTR 10000
#endif
#define CONST_EINTR EINTR

/* The value of EINVAL. */
#if !defined(EINVAL)
#define EINVAL 10000
#endif
#define CONST_EINVAL EINVAL

/* The value of EIO. */
#if !defined(EIO)
#define EIO 10000
#endif
#define CONST_EIO EIO

/* The value of EISCONN. */
#if !defined(EISCONN)
#define EISCONN 10000
#endif
#define CONST_EISCONN EISCONN

/* The value of EISDIR. */
#if !defined(EISDIR)
#define EISDIR 10000
#endif
#define CONST_EISDIR EISDIR

/* The value of ELOOP. */
#if !defined(ELOOP)
#define ELOOP 10000
#endif
#define CONST_ELOOP ELOOP

/* The value of EMFILE. */
#if !defined(EMFILE)
#define EMFILE 10000
#endif
#define CONST_EMFILE EMFILE

/* The value of EMLINK. */
#if !defined(EMLINK)
#define EMLINK 10000
#endif
#define CONST_EMLINK EMLINK

/* The value of EMSGSIZE. */
#if !defined(EMSGSIZE)
#define EMSGSIZE 10000
#endif
#define CONST_EMSGSIZE EMSGSIZE

/* The value of EMULTIHOP. */
#if !defined(EMULTIHOP)
#define EMULTIHOP 10000
#endif
#define CONST_EMULTIHOP EMULTIHOP

/* The value of ENAMETOOLONG. */
#if !defined(ENAMETOOLONG)
#define ENAMETOOLONG 10000
#endif
#define CONST_ENAMETOOLONG ENAMETOOLONG

/* The value of ENETDOWN. */
#if !defined(ENETDOWN)
#define ENETDOWN 10000
#endif
#define CONST_ENETDOWN ENETDOWN

/* The value of ENETRESET. */
#if !defined(ENETRESET)
#define ENETRESET 10000
#endif
#define CONST_ENETRESET ENETRESET

/* The value of ENETUNREACH. */
#if !defined(ENETUNREACH)
#define ENETUNREACH 10000
#endif
#define CONST_ENETUNREACH ENETUNREACH

/* The value of ENFILE. */
#if !defined(ENFILE)
#define ENFILE 10000
#endif
#define CONST_ENFILE ENFILE

/* The value of ENOBUFS. */
#if !defined(ENOBUFS)
#define ENOBUFS 10000
#endif
#define CONST_ENOBUFS ENOBUFS

/* The value of ENOCIGAR. */
#if !defined(ENOCIGAR)
#define ENOCIGAR 10000
#endif
#define CONST_ENOCIGAR ENOCIGAR

/* The value of ENODATA. */
#if !defined(ENODATA)
#define ENODATA 10000
#endif
#define CONST_ENODATA ENODATA

/* The value of ENODEV. */
#if !defined(ENODEV)
#define ENODEV 10000
#endif
#define CONST_ENODEV ENODEV

/* The value of ENOENT. */
#if !defined(ENOENT)
#define ENOENT 10000
#endif
#define CONST_ENOENT ENOENT

/* The value of ENOEXEC. */
#if !defined(ENOEXEC)
#define ENOEXEC 10000
#endif
#define CONST_ENOEXEC ENOEXEC

/* The value of ENOLCK. */
#if !defined(ENOLCK)
#define ENOLCK 10000
#endif
#define CONST_ENOLCK ENOLCK

/* The value of ENOLINK. */
#if !defined(ENOLINK)
#define ENOLINK 10000
#endif
#define CONST_ENOLINK ENOLINK

/* The value of ENOMEM. */
#if !defined(ENOMEM)
#define ENOMEM 10000
#endif
#define CONST_ENOMEM ENOMEM

/* The value of ENOMSG. */
#if !defined(ENOMSG)
#define ENOMSG 10000
#endif
#define CONST_ENOMSG ENOMSG

/* The value of ENONET. */
#if !defined(ENONET)
#define ENONET 10000
#endif
#define CONST_ENONET ENONET

/* The value of ENOPROTOOPT. */
#if !defined(ENOPROTOOPT)
#define ENOPROTOOPT 10000
#endif
#define CONST_ENOPROTOOPT ENOPROTOOPT

/* The value of ENOSPC. */
#if !defined(ENOSPC)
#define ENOSPC 10000
#endif
#define CONST_ENOSPC ENOSPC

/* The value of ENOSR. */
#if !defined(ENOSR)
#define ENOSR 10000
#endif
#define CONST_ENOSR ENOSR

/* The value of ENOSTR. */
#if !defined(ENOSTR)
#define ENOSTR 10000
#endif
#define CONST_ENOSTR ENOSTR

/* The value of ENOSYS. */
#if !defined(ENOSYS)
#define ENOSYS 10000
#endif
#define CONST_ENOSYS ENOSYS

/* The value of ENOTBLK. */
#if !defined(ENOTBLK)
#define ENOTBLK 10000
#endif
#define CONST_ENOTBLK ENOTBLK

/* The value of ENOTCONN. */
#if !defined(ENOTCONN)
#define ENOTCONN 10000
#endif
#define CONST_ENOTCONN ENOTCONN

/* The value of ENOTDIR. */
#if !defined(ENOTDIR)
#define ENOTDIR 10000
#endif
#define CONST_ENOTDIR ENOTDIR

/* The value of ENOTEMPTY. */
#if !defined(ENOTEMPTY)
#define ENOTEMPTY 10000
#endif
#define CONST_ENOTEMPTY ENOTEMPTY

/* The value of ENOTSOCK. */
#if !defined(ENOTSOCK)
#define ENOTSOCK 10000
#endif
#define CONST_ENOTSOCK ENOTSOCK

/* The value of ENOTTY. */
#if !defined(ENOTTY)
#define ENOTTY 10000
#endif
#define CONST_ENOTTY ENOTTY

/* The value of ENXIO. */
#if !defined(ENXIO)
#define ENXIO 10000
#endif
#define CONST_ENXIO ENXIO

/* The value of EOPNOTSUPP. */
#if !defined(EOPNOTSUPP)
#define EOPNOTSUPP 10000
#endif
#define CONST_EOPNOTSUPP EOPNOTSUPP

/* The value of EPERM. */
#if !defined(EPERM)
#define EPERM 10000
#endif
#define CONST_EPERM EPERM

/* The value of EPFNOSUPPORT. */
#if !defined(EPFNOSUPPORT)
#define EPFNOSUPPORT 10000
#endif
#define CONST_EPFNOSUPPORT EPFNOSUPPORT

/* The value of EPIPE. */
#if !defined(EPIPE)
#define EPIPE 10000
#endif
#define CONST_EPIPE EPIPE

/* The value of EPROCLIM. */
#if !defined(EPROCLIM)
#define EPROCLIM 10000
#endif
#define CONST_EPROCLIM EPROCLIM

/* The value of EPROCUNAVAIL. */
#if !defined(EPROCUNAVAIL)
#define EPROCUNAVAIL 10000
#endif
#define CONST_EPROCUNAVAIL EPROCUNAVAIL

/* The value of EPROGMISMATCH. */
#if !defined(EPROGMISMATCH)
#define EPROGMISMATCH 10000
#endif
#define CONST_EPROGMISMATCH EPROGMISMATCH

/* The value of EPROGUNAVAIL. */
#if !defined(EPROGUNAVAIL)
#define EPROGUNAVAIL 10000
#endif
#define CONST_EPROGUNAVAIL EPROGUNAVAIL

/* The value of EPROTO. */
#if !defined(EPROTO)
#define EPROTO 10000
#endif
#define CONST_EPROTO EPROTO

/* The value of EPROTONOSUPPORT. */
#if !defined(EPROTONOSUPPORT)
#define EPROTONOSUPPORT 10000
#endif
#define CONST_EPROTONOSUPPORT EPROTONOSUPPORT

/* The value of EPROTOTYPE. */
#if !defined(EPROTOTYPE)
#define EPROTOTYPE 10000
#endif
#define CONST_EPROTOTYPE EPROTOTYPE

/* The value of ERANGE. */
#if !defined(ERANGE)
#define ERANGE 10000
#endif
#define CONST_ERANGE ERANGE

/* The value of EREMCHG. */
#if !defined(EREMCHG)
#define EREMCHG 10000
#endif
#define CONST_EREMCHG EREMCHG

/* The value of EREMOTE. */
#if !defined(EREMOTE)
#define EREMOTE 10000
#endif
#define CONST_EREMOTE EREMOTE

/* The value of EROFS. */
#if !defined(EROFS)
#define EROFS 10000
#endif
#define CONST_EROFS EROFS

/* The value of ERPCMISMATCH. */
#if !defined(ERPCMISMATCH)
#define ERPCMISMATCH 10000
#endif
#define CONST_ERPCMISMATCH ERPCMISMATCH

/* The value of ERREMOTE. */
#if !defined(ERREMOTE)
#define ERREMOTE 10000
#endif
#define CONST_ERREMOTE ERREMOTE

/* The value of ESHUTDOWN. */
#if !defined(ESHUTDOWN)
#define ESHUTDOWN 10000
#endif
#define CONST_ESHUTDOWN ESHUTDOWN

/* The value of ESOCKTNOSUPPORT. */
#if !defined(ESOCKTNOSUPPORT)
#define ESOCKTNOSUPPORT 10000
#endif
#define CONST_ESOCKTNOSUPPORT ESOCKTNOSUPPORT

/* The value of ESPIPE. */
#if !defined(ESPIPE)
#define ESPIPE 10000
#endif
#define CONST_ESPIPE ESPIPE

/* The value of ESRCH. */
#if !defined(ESRCH)
#define ESRCH 10000
#endif
#define CONST_ESRCH ESRCH

/* The value of ESRMNT. */
#if !defined(ESRMNT)
#define ESRMNT 10000
#endif
#define CONST_ESRMNT ESRMNT

/* The value of ESTALE. */
#if !defined(ESTALE)
#define ESTALE 10000
#endif
#define CONST_ESTALE ESTALE

/* The value of ETIME. */
#if !defined(ETIME)
#define ETIME 10000
#endif
#define CONST_ETIME ETIME

/* The value of ETIMEDOUT. */
#if !defined(ETIMEDOUT)
#define ETIMEDOUT 10000
#endif
#define CONST_ETIMEDOUT ETIMEDOUT

/* The value of ETOOMANYREFS. */
#if !defined(ETOOMANYREFS)
#define ETOOMANYREFS 10000
#endif
#define CONST_ETOOMANYREFS ETOOMANYREFS

/* The value of ETXTBSY. */
#if !defined(ETXTBSY)
#define ETXTBSY 10000
#endif
#define CONST_ETXTBSY ETXTBSY

/* The value of EUSERS. */
#if !defined(EUSERS)
#define EUSERS 10000
#endif
#define CONST_EUSERS EUSERS

/* The value of EWOULDBLOCK. */
#if !defined(EWOULDBLOCK)
#define EWOULDBLOCK 10000
#endif
#define CONST_EWOULDBLOCK EWOULDBLOCK

/* The value of EXDEV. */
#if !defined(EXDEV)
#define EXDEV 10000
#endif
#define CONST_EXDEV EXDEV

/* The value of O_BINARY. */
#if !defined(O_BINARY)
#define O_BINARY 10000
#endif
#define CONST_O_BINARY O_BINARY

/* The value of SIGINT. */
#if !defined(SIGINT)
#define SIGINT 10000
#endif
#define CONST_SIGINT SIGINT


#endif

