#include "tommath.h"
#ifdef BN_MP_ZERO_C
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

/* set to zero */
void mp_zero (mp_int * a)
{
  int       n;
  mp_digit *tmp;

  // prLTM(a,"mp_zero A");
  SET_SIGN(a,MP_ZPOS);
  // prLTM(a,"mp_zero B");
  SET_USED(a,0);
  // prLTM(a,"mp_zero C");

  tmp = DIGITS(a);
  for (n = 0; n < ALLOC(a); n++) {
     *tmp++ = 0;
  }
  // prLTM(a,"mp_zero D");
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_zero.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
