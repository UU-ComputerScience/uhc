#include "tommath.h"
#ifdef BN_MP_AND_C
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
 */

/* AND two ints together */
int
mp_and (mp_int * a, mp_int * b, mp_int * c)
{
  int     res, ix, px;
  mp_int  t, *x;

  if (USED(a) > USED(b)) {
    if ((res = mp_init_copy (&t, a)) != MP_OKAY) {
      return res;
    }
    px = USED(b);
    x = b;
  } else {
    if ((res = mp_init_copy (&t, b)) != MP_OKAY) {
      return res;
    }
    px = USED(a);
    x = a;
  }

  for (ix = 0; ix < px; ix++) {
    AND_DIGIT(&t,ix,DIGIT(x,ix));
  }

  /* zero digits above the last from the smallest mp_int */
  for (; ix < USED(&t); ix++) {
    SET_DIGIT(&t,ix,0);
  }

  mp_clamp (&t);
  mp_exch (c, &t);
  mp_clear (&t);
  return MP_OKAY;
}
#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_and.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
