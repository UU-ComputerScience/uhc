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
#ifdef __UHC_BUILDS_RTS__
  // all allocation is assumed to be done outside library,
  /* 20091124 (Atze Dijkstra): ensure USED(a) <= ALLOC(a)
   */
  if (USED(a) > ALLOC(a)) {
    printf( "WARNING: mp_clamp (%p used=%x alc=%x)\n", a, USED(a), ALLOC(a) ) ;
    // prLTM(a) ;
    SET_USED(a,ALLOC(a)) ;
  }
#endif

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
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_clamp.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
