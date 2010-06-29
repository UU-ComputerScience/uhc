#include "tommath.h"
#ifdef BN_MP_DIV_2_C
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

/* b = a/2 */
int mp_div_2(mp_int * a, mp_int * b)
{
  int     x, res, oldused;

  /* copy */
  if (ALLOC(b) < USED(a)) {
    if ((res = mp_grow (b, USED(a))) != MP_OKAY) {
      return res;
    }
  }

  oldused = USED(b);
  SET_USED(b,USED(a));
  {
    register mp_digit r, rr, *tmpa, *tmpb;

    /* source alias */
    tmpa = DIGITS(a) + USED(b) - 1;

    /* dest alias */
    tmpb = DIGITS(b) + USED(b) - 1;

    /* carry */
    r = 0;
    for (x = USED(b) - 1; x >= 0; x--) {
      /* get the carry for the next iteration */
      rr = *tmpa & 1;

      /* shift the current digit, add in carry and store */
      *tmpb-- = (*tmpa-- >> 1) | (r << (DIGIT_BIT - 1));

      /* forward carry to next iteration */
      r = rr;
    }

    /* zero excess digits */
    tmpb = DIGITS(b) + USED(b);
    for (x = USED(b); x < oldused; x++) {
      *tmpb++ = 0;
    }
  }
  SET_SIGN(b,SIGN(a));
  mp_clamp (b);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_div_2.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
