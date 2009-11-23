#include "tommath.h"
#ifdef BN_MP_SHRINK_C
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

/* shrink a bignum */
int mp_shrink (mp_int * a)
{
  mp_digit *tmp;
  if (ALLOC(a) != USED(a) && USED(a) > 0) {
    if ((tmp = OPT_CAST(mp_digit) XREALLOC (DIGITS(a), sizeof (mp_digit) * USED(a))) == NULL) {
      return MP_MEM;
    }
    SET_DIGITS(a, tmp);
    SET_ALLOC(a,USED(a));
  }
  return MP_OKAY;
}
#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_shrink.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
