#include "tommath.h"
#ifdef BN_MP_MONTGOMERY_REDUCE_C
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

/* computes xR**-1 == x (mod N) via Montgomery Reduction */
int
mp_montgomery_reduce (mp_int * x, mp_int * n, mp_digit rho)
{
  int     ix, res, digs;
  mp_digit mu;

  /* can the fast reduction [comba] method be used?
   *
   * Note that unlike in mul you're safely allowed *less*
   * than the available columns [255 per default] since carries
   * are fixed up in the inner loop.
   */
  digs = USED(n) * 2 + 1;
  if ((digs < MP_WARRAY) &&
      USED(n) <
      (1 << ((CHAR_BIT * sizeof (mp_word)) - (2 * DIGIT_BIT)))) {
    return fast_mp_montgomery_reduce (x, n, rho);
  }

  /* grow the input as required */
  if (ALLOC(x) < digs) {
    if ((res = mp_grow (x, digs)) != MP_OKAY) {
      return res;
    }
  }
  SET_USED(x,digs);

  for (ix = 0; ix < USED(n); ix++) {
    /* mu = ai * rho mod b
     *
     * The value of rho must be precalculated via
     * montgomery_setup() such that
     * it equals -1/n0 mod b this allows the
     * following inner loop to reduce the
     * input one digit at a time
     */
    mu = (mp_digit) (((mp_word)DIGIT(x,ix)) * ((mp_word)rho) & MP_MASK);

    /* a = a + mu * m * b**i */
    {
      register int iy;
      register mp_digit *tmpn, *tmpx, u;
      register mp_word r;

      /* alias for digits of the modulus */
      tmpn = DIGITS(n);

      /* alias for the digits of x [the input] */
      tmpx = DIGITS(x) + ix;

      /* set the carry to zero */
      u = 0;

      /* Multiply and add in place */
      for (iy = 0; iy < USED(n); iy++) {
        /* compute product and sum */
        r       = ((mp_word)mu) * ((mp_word)*tmpn++) +
                  ((mp_word) u) + ((mp_word) * tmpx);

        /* get carry */
        u       = (mp_digit)(r >> ((mp_word) DIGIT_BIT));

        /* fix digit */
        *tmpx++ = (mp_digit)(r & ((mp_word) MP_MASK));
      }
      /* At this point the ix'th digit of x should be zero */


      /* propagate carries upwards as required*/
      while (u) {
        *tmpx   += u;
        u        = *tmpx >> DIGIT_BIT;
        *tmpx++ &= MP_MASK;
      }
    }
  }

  /* at this point the USED(&n)'th least
   * significant digits of x are all zero
   * which means we can shift x to the
   * right by USED(&n) digits and the
   * residue is unchanged.
   */

  /* x = x/b**USED(&n) */
  mp_clamp(x);
  mp_rshd (x, USED(n));

  /* if x >= n then x = x - n */
  if (mp_cmp_mag (x, n) != MP_LT) {
    return s_mp_sub (x, n, x);
  }

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_montgomery_reduce.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
