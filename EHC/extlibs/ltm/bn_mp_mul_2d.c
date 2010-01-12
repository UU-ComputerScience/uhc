#include "tommath.h"
#ifdef BN_MP_MUL_2D_C
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

/* shift left by a certain bit count */
int mp_mul_2d (mp_int * a, int b, mp_int * c)
{
  mp_digit d;
  int      res;

  /* copy */
  // prLTM(a,"mp_mul_2d a");
  // prLTM(c,"mp_mul_2d c");
  if (a != c) {
     if ((res = mp_copy (a, c)) != MP_OKAY) {
       return res;
     }
  }

  // prLTM(a,"mp_mul_2d A");
  if (ALLOC(c) < (int)(USED(c) + b/DIGIT_BIT + 1)) {
     if ((res = mp_grow (c, USED(c) + b / DIGIT_BIT + 1)) != MP_OKAY) {
       return res;
     }
  }
  // prLTM(a,"mp_mul_2d B");

  /* shift by as many digits in the bit count */
  if (b >= (int)DIGIT_BIT) {
    if ((res = mp_lshd (c, b / DIGIT_BIT)) != MP_OKAY) {
      return res;
    }
  }

  /* shift any bit count < DIGIT_BIT */
  d = (mp_digit) (b % DIGIT_BIT);
  if (d != 0) {
    register mp_digit *tmpc, shift, mask, r, rr;
    register int x;

    /* bitmask for carries */
    mask = (((mp_digit)1) << d) - 1;

    /* shift for msbs */
    shift = DIGIT_BIT - d;

    /* alias */
    tmpc = DIGITS(c);

    /* carry */
    r    = 0;
    for (x = 0; x < USED(c); x++) {
      /* get the higher bits of the current word */
      rr = (*tmpc >> shift) & mask;

      /* shift the current word and OR in the carry */
      *tmpc = ((*tmpc << d) | r) & MP_MASK;
      ++tmpc;

      /* set the carry to the carry bits of the current word */
      r = rr;
    }
    
    /* set final carry */
    if (r != 0) {
       SET_DIGIT(c,USED(c),r);
       INC1_USED(c) ;
    }
  }
  mp_clamp (c);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_mul_2d.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
