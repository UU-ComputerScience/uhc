#include "tommath.h"
#ifdef BN_MP_INIT_C
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

/* init a new mp_int */
int mp_init (mp_int * a)
{
  int i;

  /* allocate memory required and clear it */
  SET_DIGITS(a, OPT_CAST(mp_digit) XMALLOC (sizeof (mp_digit) * MP_PREC));
  if (DIGITS(a) == NULL) {
    return MP_MEM;
  }

  /* set the digits to zero */
  for (i = 0; i < MP_PREC; i++) {
      SET_DIGIT(a,i,0);
  }

  /* set the used to zero, allocated digits to the default precision
   * and sign to positive */
  SET_USED(a,0);
  SET_ALLOC(a,MP_PREC);
  SET_SIGN(a,MP_ZPOS);

  return MP_OKAY;
}
#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_init.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
