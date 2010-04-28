#include "tommath.h"
#include <math.h>
#include <float.h>

#ifdef BN_MP_SET_INT_C
/* LibTomMath, multiple-precision integer library -- Tom St Denis
 *
 * LibTomMath is a library that provides multiple-precision
 * integer arithmetic as well as number theoretic functionality.
 *
 * The library was designed directly after the MPI library by
 * Michael Fromberger but has been written from scratch with
 * additional optimizations in place.
 *
 * The library is free for all purposes without any express
 * guarantee it works.
 *
 * Tom St Denis, tomstdenis@gmail.com, http://math.libtomcrypt.com
 *
 * 20091124 (Atze Dijkstra): added for UHC
 */

/* set a double, for which we know if falls in the range of an int64_t */
int mp_set_double64(mp_int * a, double b)
{
  int     res ;
   
  // printf( "mp_set_double64 %f\n", b ) ;
  int64_t rounded = (int64_t)b ; // llround( b ) ;
  if ((res = mp_set_sint64(a, rounded)) != MP_OKAY) {
    return res;
  }

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_set_int.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
