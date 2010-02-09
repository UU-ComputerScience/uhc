#include "tommath.h"
#ifdef BN_MP_CMP_C
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

/* compare to zero (signed) */
int
mp_sgn (mp_int * a)
{
  /* compare based on sign */
  if (mp_isneg(a) == MP_YES) {
     return MP_LT;
  } else if (mp_iszero(a) == MP_YES) {
     return MP_EQ;
  } else {
     return MP_GT;
  }
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_cmp.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
