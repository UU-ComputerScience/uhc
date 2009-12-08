#include "tommath.h"
#ifdef BN_MP_CMP_MAG_C
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

/* compare maginitude of two ints (unsigned) */
int mp_cmp_mag (mp_int * a, mp_int * b)
{
  int     n;
  mp_digit *tmpa, *tmpb;

  /* compare based on # of non-zero digits */
  if (USED(a) > USED(b)) {
    return MP_GT;
  }
  
  if (USED(a) < USED(b)) {
    return MP_LT;
  }

  /* alias for a */
  tmpa = DIGITS(a) + (USED(a) - 1);

  /* alias for b */
  tmpb = DIGITS(b) + (USED(a) - 1);

  /* compare based on digits  */
  for (n = 0; n < USED(a); ++n, --tmpa, --tmpb) {
    if (*tmpa > *tmpb) {
      return MP_GT;
    }

    if (*tmpa < *tmpb) {
      return MP_LT;
    }
  }
  return MP_EQ;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_cmp_mag.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
