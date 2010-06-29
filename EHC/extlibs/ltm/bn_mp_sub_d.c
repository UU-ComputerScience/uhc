#include "tommath.h"
#ifdef BN_MP_SUB_D_C
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

/* single digit subtraction */
int
mp_sub_d (mp_int * a, mp_digit b, mp_int * c)
{
  mp_digit *tmpa, *tmpc, mu;
  int       res, ix, oldused;

  /* grow c as required */
  if (ALLOC(c) < USED(a) + 1) {
     if ((res = mp_grow(c, USED(a) + 1)) != MP_OKAY) {
        return res;
     }
  }

  /* if a is negative just do an unsigned
   * addition [with fudged signs]
   */
  if (SIGN(a) == MP_NEG) {
     SET_SIGN(a,MP_ZPOS) ;
     res     = mp_add_d(a, b, c);
     SET_SIGN(a,MP_NEG);
     SET_SIGN(c,MP_NEG);

     /* clamp */
     mp_clamp(c);

     return res;
  }

  /* setup regs */
  oldused = USED(c);
  tmpa    = DIGITS(a);
  tmpc    = DIGITS(c);

  /* if a <= b simply fix the single digit */
  if ((USED(a) == 1 && DIGIT(a,0) <= b) || USED(a) == 0) {
     if (USED(a) == 1) {
        *tmpc++ = b - *tmpa;
     } else {
        *tmpc++ = b;
     }
     ix      = 1;

     /* negative/1digit */
     SET_SIGN(c,MP_NEG);
     SET_USED(c,1);
  } else {
     /* positive/size */
     SET_SIGN(c,MP_ZPOS);
     SET_USED(c,USED(a));

     /* subtract first digit */
     *tmpc    = *tmpa++ - b;
     mu       = *tmpc >> (sizeof(mp_digit) * CHAR_BIT - 1);
     *tmpc++ &= MP_MASK;

     /* handle rest of the digits */
     for (ix = 1; ix < USED(a); ix++) {
        *tmpc    = *tmpa++ - mu;
        mu       = *tmpc >> (sizeof(mp_digit) * CHAR_BIT - 1);
        *tmpc++ &= MP_MASK;
     }
  }

  /* zero excess digits */
  while (ix++ < oldused) {
     *tmpc++ = 0;
  }
  mp_clamp(c);
  return MP_OKAY;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_sub_d.c,v $ */
/* $Revision: 1.5 $ */
/* $Date: 2006/03/31 14:18:44 $ */
