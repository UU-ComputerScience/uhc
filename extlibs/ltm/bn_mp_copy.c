#include "tommath.h"
#ifdef BN_MP_COPY_C
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

/* copy, b = a */
int
mp_copy (mp_int * a, mp_int * b)
{
  int     res, n;

  /* if dst == src do nothing */
  if (a == b) {
    return MP_OKAY;
  }

  /* grow dest */
  if (ALLOC(b) < USED(a)) {
     if ((res = mp_grow (b, USED(a))) != MP_OKAY) {
        return res;
     }
  }

  /* zero b and copy the parameters over */
  {
    register mp_digit *tmpa, *tmpb;

    /* pointer aliases */

    /* source */
    tmpa = DIGITS(a);

    /* destination */
    tmpb = DIGITS(b);

    /* copy all the digits */
    for (n = 0; n < USED(a); n++) {
      *tmpb++ = *tmpa++;
    }

    /* clear high digits */
    for (; n < USED(b); n++) {
      *tmpb++ = 0;
    }
  }

  /* copy used count and sign */
  SET_USED(b,USED(a));
  SET_SIGN(b,SIGN(a));
  return MP_OKAY;
}
#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_copy.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
