#include "tommath.h"

// #ifdef __UHC_BUILDS_RTS__
// #undef BN_MP_TOOM_MUL_C
// #undef BN_MP_KARATSUBA_MUL_C
// #undef BN_FAST_S_MP_MUL_DIGS_C
// #endif

#ifdef BN_MP_MUL_C
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

/* high level multiplication (handles sign) */
int mp_mul (mp_int * a, mp_int * b, mp_int * c)
{
  int     res, neg;
  neg = (SIGN(a) == SIGN(b)) ? MP_ZPOS : MP_NEG;

  /* use Toom-Cook? */
#ifdef BN_MP_TOOM_MUL_C
  if (MIN (USED(a), USED(b)) >= TOOM_MUL_CUTOFF) {
    // printf( "mp_mul mp_toom_mul:\n" ) ;
    res = mp_toom_mul(a, b, c);
  } else 
#endif
#ifdef BN_MP_KARATSUBA_MUL_C
  /* use Karatsuba? */
  if (MIN (USED(a), USED(b)) >= KARATSUBA_MUL_CUTOFF) {
    // printf( "mp_mul mp_karatsuba_mul:\n" ) ;
    res = mp_karatsuba_mul (a, b, c);
  } else 
#endif
  {
    /* can we use the fast multiplier?
     *
     * The fast multiplier can be used if the output will 
     * have less than MP_WARRAY digits and the number of 
     * digits won't affect carry propagation
     */
    int     digs = USED(a) + USED(b) + 1;

#ifdef BN_FAST_S_MP_MUL_DIGS_C
    if ((digs < MP_WARRAY) &&
        MIN(USED(a), USED(b)) <= 
        (1 << ((CHAR_BIT * sizeof (mp_word)) - (2 * DIGIT_BIT)))) {
      // printf( "mp_mul fast_s_mp_mul_digs:\n" ) ;
      res = fast_s_mp_mul_digs (a, b, c, digs);
    } else 
#endif
#ifdef BN_S_MP_MUL_DIGS_C
      // printf( "mp_mul s_mp_mul:\n" ) ;
      res = s_mp_mul (a, b, c); /* uses s_mp_mul_digs */
#else
      // printf( "mp_mul FAIL:\n" ) ;
      res = MP_VAL;
#endif

  }
  SET_SIGN(c,(USED(c) > 0) ? neg : MP_ZPOS);
  return res;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_mul.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
