#include "tommath.h"
#ifdef BN_MP_NEG_C
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

/* b = ~a */
int mp_com(mp_int * a, mp_int * b)
{
  int     res;
  
  // -x == ~x + 1 (2's complement), hence ~x == -x - 1
  if ((res = mp_neg(a, b)) != MP_OKAY) {
    return res;
  }

  if ((res = mp_sub_d(b, 1, b)) != MP_OKAY) {
    return res;
  }

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_neg.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
