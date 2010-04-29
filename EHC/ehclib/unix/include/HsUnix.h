/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSUNIX_H
#define HSUNIX_H

#include "HsUnixConfig.h"
#include "HsFFI.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#ifdef solaris2_HOST_OS
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#endif
#ifdef HAVE_PTY_H
#include <pty.h>
#endif
#ifdef HAVE_UTMP_H
#include <utmp.h>
#endif

#include <dlfcn.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

extern char **environ;

#ifndef INLINE
#define INLINE static inline
#endif

INLINE int __hsunix_wifexited   (int stat) { return WIFEXITED(stat); }
INLINE int __hsunix_wexitstatus (int stat) { return WEXITSTATUS(stat); }
INLINE int __hsunix_wifsignaled (int stat) { return WIFSIGNALED(stat); }
INLINE int __hsunix_wtermsig    (int stat) { return WTERMSIG(stat); }
INLINE int __hsunix_wifstopped  (int stat) { return WIFSTOPPED(stat); }
INLINE int __hsunix_wstopsig    (int stat) { return WSTOPSIG(stat); }

#ifdef HAVE_RTLDNEXT
INLINE void *__hsunix_rtldNext (void) {return RTLD_NEXT;} 
#endif

#ifdef HAVE_RTLDDEFAULT
INLINE void *__hsunix_rtldDefault (void) {return RTLD_DEFAULT;} 
#endif

/* O_SYNC doesn't exist on Mac OS X and (at least some versions of) FreeBSD,
fall back to O_FSYNC, which should be the same */
#ifndef O_SYNC
#define O_SYNC O_FSYNC
#endif

#ifdef SIGINFO
INLINE int __hsunix_SIGINFO()	{ return SIGINFO; }
#endif
#ifdef SIGWINCH
INLINE int __hsunix_SIGWINCH()	{ return SIGWINCH; }
#endif

// lstat is a macro on some platforms, so we need a wrapper:
INLINE int __hsunix_lstat(const char *path, struct stat *buf) 
{ 
    return lstat(path,buf);
}

// lstat is a macro on some platforms, so we need a wrapper:
INLINE int __hsunix_mknod(const char *pathname, mode_t mode, dev_t dev)
{ 
    return mknod(pathname,mode,dev);
}

#ifdef HAVE_PTSNAME
// I cannot figure out how to make the definitions of the following
// functions visible in <stdlib.h> on Linux.  But these definitions
// follow the POSIX specs, and everything links and runs.

INLINE char *__hsunix_ptsname(int fd)
{
    extern char *ptsname(int);
    return ptsname(fd);
}

INLINE int __hsunix_grantpt(int fd)
{
    extern int grantpt(int);
    return grantpt(fd);
}

INLINE int __hsunix_unlockpt(int fd)
{
    extern int unlockpt(int);
    return unlockpt(fd);
}
#endif

// push a SVR4 STREAMS module; do nothing if STREAMS not available
INLINE int __hsunix_push_module(int fd, const char *module)
{
#if defined(I_PUSH) && !defined(__CYGWIN__) && !defined(HAVE_DEV_PTC)
    return ioctl(fd, I_PUSH, module);
#else
    return 0;
#endif
}

#if !defined(__MINGW32__)
INLINE int __hscore_mkstemp(char *filetemplate) {
    return (mkstemp(filetemplate));
}
#endif

#if !defined(__MINGW32__) && !defined(irix_HOST_OS)
INLINE int __hscore_getrlimit(int resource, struct rlimit *rlim) {
    return (getrlimit(resource, rlim));
}

INLINE int __hscore_setrlimit(int resource, struct rlimit *rlim) {
    return (setrlimit(resource, rlim));
}
#endif

INLINE int __hsunix_unsetenv(const char *name)
{
#ifdef UNSETENV_RETURNS_VOID
    unsetenv(name);
    return 0;
#else
    return unsetenv(name);
#endif
}

/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
INLINE int __hsunix_long_path_size() {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4096;
#endif
}

#endif
