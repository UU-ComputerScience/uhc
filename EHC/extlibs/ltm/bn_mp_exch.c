#include "tommath.h"
#ifdef BN_MP_EXCH_C
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

/* swap the elements of two integers, for cases where you can't simply swap the 
 * mp_int pointers around
 */
void
mp_exch (mp_int * a, mp_int * b)
{
#ifdef __UHC_BUILDS_RTS__
  // all allocation is assumed to be done outside library,
  // shrinking is therefore a NOP, can be done during garbage collection.
  printf( "WARNING: mp_exch (%p used=%x alc=%x) (%p used=%x alc=%x)\n", a, USED(a), ALLOC(a), b, USED(b), ALLOC(b) ) ;
  // prLTM(a) ;
  // prLTM(b) ;
#else
  mp_int  t;

  t  = *a;
  *a = *b;
  *b = t;
#endif
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_exch.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
