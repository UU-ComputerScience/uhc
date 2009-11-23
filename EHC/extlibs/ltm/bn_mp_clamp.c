#include "tommath.h"
#ifdef BN_MP_CLAMP_C
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

/* trim unused digits 
 *
 * This is used to ensure that leading zero digits are
 * trimed and the leading "used" digit will be non-zero
 * Typically very fast.  Also fixes the sign if there
 * are no more leading digits
 */
void
mp_clamp (mp_int * a)
{
  /* decrease used while the most significant digit is
   * zero.
   */
  while (USED(a) > 0 && DIGIT(a,USED(a) - 1) == 0) {
    DEC1_USED(a);
  }

  /* reset the sign flag if used == 0 */
  if (USED(a) == 0) {
    SET_SIGN(a,MP_ZPOS);
  }
}
#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_clamp.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
