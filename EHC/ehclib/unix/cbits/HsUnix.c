/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions for package `unix' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

// Out-of-line versions of all the inline functions from HsUnix.h
#define INLINE  /* nothing */
#include "HsUnix.h"

int __hsunix_wifexited   (int stat) { return WIFEXITED(stat); }
int __hsunix_wexitstatus (int stat) { return WEXITSTATUS(stat); }
int __hsunix_wifsignaled (int stat) { return WIFSIGNALED(stat); }
int __hsunix_wtermsig    (int stat) { return WTERMSIG(stat); }
int __hsunix_wifstopped  (int stat) { return WIFSTOPPED(stat); }
int __hsunix_wstopsig    (int stat) { return WSTOPSIG(stat); }

#ifdef HAVE_RTLDNEXT
void *__hsunix_rtldNext (void) {return RTLD_NEXT;} 
#endif

#ifdef HAVE_RTLDDEFAULT
void *__hsunix_rtldDefault (void) {return RTLD_DEFAULT;} 
#endif

#ifdef SIGINFO
int __hsunix_SIGINFO()	{ return SIGINFO; }
#endif
#ifdef SIGWINCH
int __hsunix_SIGWINCH()	{ return SIGWINCH; }
#endif

// lstat is a macro on some platforms, so we need a wrapper:
int __hsunix_lstat(const char *path, struct stat *buf) 
{ 
    return lstat(path,buf);
}

// mknod is a macro on some platforms, so we need a wrapper:
int __hsunix_mknod(const char *pathname, mode_t mode, dev_t dev)
{ 
    return mknod(pathname,mode,dev);
}

#ifdef HAVE_GETPWENT
// getpwent is a macro on some platforms, so we need a wrapper:
struct passwd *__hsunix_getpwent(void)
{
    return getpwent();
}
#endif

#if HAVE_GETPWNAM_R
// getpwnam_r is a macro on some platforms, so we need a wrapper:
int __hsunix_getpwnam_r(const char *name, struct passwd *pw, char *buffer,
                        size_t buflen, struct passwd **result)
{
    return getpwnam_r(name, pw, buffer, buflen, result);
}
#endif

#ifdef HAVE_GETPWUID_R
// getpwuid_r is a macro on some platforms, so we need a wrapper:
int __hsunix_getpwuid_r(uid_t uid, struct passwd *pw, char *buffer,
                        size_t buflen, struct passwd **result)
{
    return getpwuid_r(uid, pw, buffer, buflen, result);
}
#endif

#ifdef HAVE_NANOSLEEP
// nanosleep is a macro on some platforms, so we need a wrapper:
int __hsunix_nanosleep(const struct timespec *rqtp, struct timespec *rmtp)
{
    return nanosleep(rqtp, rmtp);
}
#endif

// opendir is a macro on some platforms, so we need a wrapper:
DIR *__hsunix_opendir(const char *filename)
{
    return opendir(filename);
}

// time is a macro on some platforms, so we need a wrapper:
time_t __hsunix_time(time_t *tloc)
{
    return time(tloc);
}

// times is a macro on some platforms, so we need a wrapper:
clock_t __hsunix_times(struct tms *tp)
{
    return times(tp);
}

#ifdef HAVE_PTSNAME
// I cannot figure out how to make the definitions of the following
// functions visible in <stdlib.h> on Linux.  But these definitions
// follow the POSIX specs, and everything links and runs.

char *__hsunix_ptsname(int fd)
{
    extern char *ptsname(int);
    return ptsname(fd);
}

int __hsunix_grantpt(int fd)
{
    extern int grantpt(int);
    return grantpt(fd);
}

int __hsunix_unlockpt(int fd)
{
    extern int unlockpt(int);
    return unlockpt(fd);
}
#endif

// push a SVR4 STREAMS module; do nothing if STREAMS not available
int __hsunix_push_module(int fd, const char *module)
{
#if defined(I_PUSH) && !defined(__CYGWIN__) && !defined(HAVE_DEV_PTC)
    return ioctl(fd, I_PUSH, module);
#else
    return 0;
#endif
}

#if !defined(__MINGW32__)
int __hscore_mkstemp(char *filetemplate) {
    return (mkstemp(filetemplate));
}
#endif

#if !defined(__MINGW32__) && !defined(irix_HOST_OS)
int __hscore_getrlimit(int resource, struct rlimit *rlim) {
    return (getrlimit(resource, rlim));
}

int __hscore_setrlimit(int resource, struct rlimit *rlim) {
    return (setrlimit(resource, rlim));
}
#endif

int __hsunix_unsetenv(const char *name)
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
HsInt __hsunix_long_path_size() {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4096;
#endif
}

