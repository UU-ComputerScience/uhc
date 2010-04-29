/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2001-2004
 *
 * Definitions for package `directory' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __HSDIRECTORY_H__
#define __HSDIRECTORY_H__

#include "HsDirectoryConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

//#include "HsFFI.h"

#if defined(__MINGW32__)
#include <shlobj.h>
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
extern int __hscore_getFolderPath(HWND hwndOwner,
                  int nFolder,
                  HANDLE hToken,
                  DWORD dwFlags,
                  char*  pszPath);
#endif

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
# endif
#endif

/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */

INLINE int __hscore_long_path_size() {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4096;
#endif
}

#if defined(__GLASGOW_HASKELL__) || defined(__UHC__)
INLINE int __hscore_R_OK() { return R_OK; }
INLINE int __hscore_W_OK() { return W_OK; }
INLINE int __hscore_X_OK() { return X_OK; }

INLINE mode_t __hscore_S_IRUSR() { return S_IRUSR; }
INLINE mode_t __hscore_S_IWUSR() { return S_IWUSR; }
INLINE mode_t __hscore_S_IXUSR() { return S_IXUSR; }
INLINE mode_t __hscore_S_IFDIR() { return S_IFDIR; }
#endif

#if defined(__MINGW32__)

/* Make sure we've got the reqd CSIDL_ constants in scope;
 * w32api header files are lagging a bit in defining the full set.
 */
#if !defined(CSIDL_APPDATA)
#define CSIDL_APPDATA 0x001a
#endif
#if !defined(CSIDL_PERSONAL)
#define CSIDL_PERSONAL 0x0005
#endif
#if !defined(CSIDL_PROFILE)
#define CSIDL_PROFILE 0x0028
#endif
#if !defined(CSIDL_WINDOWS)
#define CSIDL_WINDOWS 0x0024
#endif

INLINE int __hscore_CSIDL_PROFILE()  { return CSIDL_PROFILE;  }
INLINE int __hscore_CSIDL_APPDATA()  { return CSIDL_APPDATA;  }
INLINE int __hscore_CSIDL_WINDOWS()  { return CSIDL_WINDOWS;  }
INLINE int __hscore_CSIDL_PERSONAL() { return CSIDL_PERSONAL; }
#endif

#endif /* __HSDIRECTORY_H__ */

