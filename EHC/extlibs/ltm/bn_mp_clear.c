#include "tommath.h"
#ifdef BN_MP_CLEAR_C
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

/* clear one (frees)  */
int
mp_clear (mp_int * a)
{
#ifdef __UHC_BUILDS_RTS__
  // all allocation is assumed to be done outside library,
  // clearing is therefore a NOP, can be done during garbage collection.
  // printf( "WARNING: mp_clear (%p used=%x alc=%x)\n", a, USED(a), ALLOC(a) ) ;
#else
  int i;

  /* only do anything if a hasn't been freed previously */
  if (DIGITS(a) != NULL) {
    /* first zero the digits */
    for (i = 0; i < USED(a); i++) {
        SET_DIGIT(a,i,0);
    }

    /* free ram */
    XFREE(DIGITS(a));

    /* reset members to make debugging easier */
    SET_DIGITS(a, NULL);
    SET_ALLOC(a,0 );
    SET_USED(a,0 );
    SET_SIGN(a,MP_ZPOS);
  }
#endif

  return MP_OKAY ;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_clear.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
