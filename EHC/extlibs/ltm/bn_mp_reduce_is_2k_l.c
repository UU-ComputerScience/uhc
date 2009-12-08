#include "tommath.h"
#ifdef BN_MP_REDUCE_IS_2K_L_C
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

/* determines if reduce_2k_l can be used */
int mp_reduce_is_2k_l(mp_int *a)
{
   int ix, iy;
   
   if (USED(a) == 0) {
      return MP_NO;
   } else if (USED(a) == 1) {
      return MP_YES;
   } else if (USED(a) > 1) {
      /* if more than half of the digits are -1 we're sold */
      for (iy = ix = 0; ix < USED(a); ix++) {
          if (DIGIT(a,ix) == MP_MASK) {
              ++iy;
          }
      }
      return (iy >= (USED(a)/2)) ? MP_YES : MP_NO;
      
   }
   return MP_NO;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_reduce_is_2k_l.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
