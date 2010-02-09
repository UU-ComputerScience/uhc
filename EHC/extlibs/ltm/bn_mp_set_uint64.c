#include "tommath.h"
#ifdef BN_MP_SET_INT_C
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

/* set a 64-bit unsigned const */
int mp_set_uint64(mp_int * a, uint64_t b)
{
  int     x, res;

  // prLTM(a,"mp_set_uint64");
  mp_zero (a);
  
  /* set 8 bits at a time */
  for (x = 0; x < 64/8; x++) {
    /* shift the number up 8 bits */
    if ((res = mp_mul_2d (a, 8, a)) != MP_OKAY) {
      return res;
    }
    // prLTM(a,"mp_set_uint64");

    /* OR in the top 8 bits of the source */
    OR_DIGIT(a,0,(b >> (64-8)) & ((1<<8)-1));

    /* shift the source up to the next 8 bits */
    b <<= 8;

    /* ensure that digits are not clamped off */
    INC_USED(a,1);
  }
  mp_clamp (a);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_set_int.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
