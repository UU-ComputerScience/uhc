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

int __hsunix_wifexited   (int stat);
int __hsunix_wexitstatus (int stat);
int __hsunix_wifsignaled (int stat);
int __hsunix_wtermsig    (int stat);
int __hsunix_wifstopped  (int stat);
int __hsunix_wstopsig    (int stat);

#ifdef HAVE_RTLDNEXT
void *__hsunix_rtldNext (void);
#endif

#ifdef HAVE_RTLDDEFAULT
void *__hsunix_rtldDefault (void);
#endif

/* O_SYNC doesn't exist on Mac OS X and (at least some versions of) FreeBSD,
fall back to O_FSYNC, which should be the same */
#ifndef O_SYNC
#define O_SYNC O_FSYNC
#endif

#ifdef SIGINFO
int __hsunix_SIGINFO();
#endif
#ifdef SIGWINCH
int __hsunix_SIGWINCH();
#endif

// lstat is a macro on some platforms, so we need a wrapper:
int __hsunix_lstat(const char *path, struct stat *buf);

// lstat is a macro on some platforms, so we need a wrapper:
int __hsunix_mknod(const char *pathname, mode_t mode, dev_t dev);

#ifdef HAVE_GETPWENT
// getpwent is a macro on some platforms, so we need a wrapper:
struct passwd *__hsunix_getpwent(void);
#endif

#if HAVE_GETPWNAM_R
// getpwnam_r is a macro on some platforms, so we need a wrapper:
int __hsunix_getpwnam_r(const char *, struct passwd *, char *, size_t,
                        struct passwd **);
#endif

#ifdef HAVE_GETPWUID_R
// getpwuid_r is a macro on some platforms, so we need a wrapper:
int __hsunix_getpwuid_r(uid_t, struct passwd *, char *, size_t,
                        struct passwd **);
#endif

#ifdef HAVE_NANOSLEEP
// nanosleep is a macro on some platforms, so we need a wrapper:
int __hsunix_nanosleep(const struct timespec *, struct timespec *);
#endif

// opendir is a macro on some platforms, so we need a wrapper:
DIR *__hsunix_opendir(const char *);

// time is a macro on some platforms, so we need a wrapper:
time_t __hsunix_time(time_t *);

// times is a macro on some platforms, so we need a wrapper:
clock_t __hsunix_times(struct tms *);

#ifdef HAVE_PTSNAME
// I cannot figure out how to make the definitions of the following
// functions visible in <stdlib.h> on Linux.  But these definitions
// follow the POSIX specs, and everything links and runs.

char *__hsunix_ptsname(int fd);
int __hsunix_grantpt(int fd);
int __hsunix_unlockpt(int fd);
#endif

// push a SVR4 STREAMS module; do nothing if STREAMS not available
int __hsunix_push_module(int fd, const char *module);

#if !defined(__MINGW32__)
int __hscore_mkstemp(char *filetemplate);
#endif

#if !defined(__MINGW32__) && !defined(irix_HOST_OS)
int __hscore_getrlimit(int resource, struct rlimit *rlim);
int __hscore_setrlimit(int resource, struct rlimit *rlim);
#endif

int __hsunix_unsetenv(const char *name);

/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
HsInt __hsunix_long_path_size();

#endif
