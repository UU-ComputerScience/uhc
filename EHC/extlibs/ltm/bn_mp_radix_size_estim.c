#include "tommath.h"
#ifdef BN_MP_RADIX_SIZE_C
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
 *
 * 20091124 (Atze Dijkstra): added for UHC
 */

/* returns size estimate of ASCII reprensentation, based on used size */
int mp_radix_size_estim (mp_int * a, int radix, int *size)
{
  *size = 0;

  /* make sure the radix is in range */
  if (radix < 2 || radix > 64) {
    return MP_VAL;
  }
  
  *size = mp_count_bits (a) ;
  if ( radix > 32 ) {
    // 5 bits per char
    *size /= 5 ;
  } else if ( radix > 16 ) {
    // 4 bits per char
    *size /= 4 ;
  } else if ( radix > 8 ) {
    // 3 bits per char
    *size /= 3 ;
  } else if ( radix > 4 ) {
    // 2 bits per char
    *size /= 2 ;
  } else {
    // 1 bits per char
  }
  
  // sign
  *size += (SIGN(a) == MP_NEG ? 1 : 0) ;
  
  // 1 extra because of round down (not a real entier)
  *size += 1;

  return MP_OKAY;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_radix_size.c,v $ */
/* $Revision: 1.4 $ */
/* $Date: 2006/03/31 14:18:44 $ */
