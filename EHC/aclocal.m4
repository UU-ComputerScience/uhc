# FP_INCLUDES_MORE
# -----------------------------------------------------------------------------------
# Based on FP_CHECK_HTYPE
# Add additional headers to the default ones (eg. termios.h)

AC_DEFUN([FP_INCLUDES_MORE],
[AC_INCLUDES_DEFAULT
#include <stddef.h>

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_TERMIOS_H
# include <termios.h>
#endif

#if HAVE_STRING_H
# include <string.h>
#endif

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
])# FP_INCLUDES_MORE

# FP_DECL_ALTZONE
# ---------------
# Defines HAVE_DECL_ALTZONE to 1 if declared, 0 otherwise.
#
# Used by base package.
AC_DEFUN([FP_DECL_ALTZONE],
[AC_REQUIRE([AC_HEADER_TIME])dnl
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_DECLS([altzone], [], [],[#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif])
])# FP_DECL_ALTZONE

