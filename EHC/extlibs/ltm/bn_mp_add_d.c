#include "tommath.h"
#ifdef BN_MP_ADD_D_C
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

/* single digit addition */
int
mp_add_d (mp_int * a, mp_digit b, mp_int * c)
{
  int     res, ix, oldused;
  mp_digit *tmpa, *tmpc, mu;

  /* grow c as required */
  if (ALLOC(c) < USED(a) + 1) {
     if ((res = mp_grow(c, USED(a) + 1)) != MP_OKAY) {
        return res;
     }
  }

  /* if a is negative and |a| >= b, call c = |a| - b */
  if (SIGN(a) == MP_NEG && (USED(a) > 1 || DIGIT(a,0) >= b)) {
     /* temporarily fix sign of a */
     SET_SIGN(a,MP_ZPOS);

     /* c = |a| - b */
     res = mp_sub_d(a, b, c);

     /* fix sign  */
     SET_SIGN(a,MP_NEG);
     SET_SIGN(c,MP_NEG);

     /* clamp */
     mp_clamp(c);

     return res;
  }

  /* old number of used digits in c */
  oldused = USED(c);

  /* sign always positive */
  SET_SIGN(c,MP_ZPOS);

  /* source alias */
  tmpa    = DIGITS(a);

  /* destination alias */
  tmpc    = DIGITS(c);

  /* if a is positive */
  if (SIGN(a) == MP_ZPOS) {
     /* add digit, after this we're propagating
      * the carry.
      */
     *tmpc   = *tmpa++ + b;
     mu      = *tmpc >> DIGIT_BIT;
     *tmpc++ &= MP_MASK;

     /* now handle rest of the digits */
     for (ix = 1; ix < USED(a); ix++) {
        *tmpc   = *tmpa++ + mu;
        mu      = *tmpc >> DIGIT_BIT;
        *tmpc++ &= MP_MASK;
     }
     /* set final carry */
     ix++;
     *tmpc++  = mu;

     /* setup size */
     SET_USED(c,USED(a) + 1);
  } else {
     /* a was negative and |a| < b */
     SET_USED(c,1);

     /* the result is a single digit */
     if (USED(a) == 1) {
        *tmpc++  =  b - DIGIT(a,0);
     } else {
        *tmpc++  =  b;
     }

     /* setup count so the clearing of oldused
      * can fall through correctly
      */
     ix       = 1;
  }

  /* now zero to oldused */
  while (ix++ < oldused) {
     *tmpc++ = 0;
  }
  mp_clamp(c);

  return MP_OKAY;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_add_d.c,v $ */
/* $Revision: 1.4 $ */
/* $Date: 2006/03/31 14:18:44 $ */
