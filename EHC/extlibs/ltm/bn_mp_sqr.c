#include "tommath.h"
#ifdef BN_MP_SQR_C
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

/* computes b = a*a */
int
mp_sqr (mp_int * a, mp_int * b)
{
  int     res;

#ifdef BN_MP_TOOM_SQR_C
  /* use Toom-Cook? */
  if (USED(a) >= TOOM_SQR_CUTOFF) {
    res = mp_toom_sqr(a, b);
  /* Karatsuba? */
  } else 
#endif
#ifdef BN_MP_KARATSUBA_SQR_C
if (USED(a) >= KARATSUBA_SQR_CUTOFF) {
    res = mp_karatsuba_sqr (a, b);
  } else 
#endif
  {
#ifdef BN_FAST_S_MP_SQR_C
    /* can we use the fast comba multiplier? */
    if ((USED(a) * 2 + 1) < MP_WARRAY && 
         USED(a) < 
         (1 << (sizeof(mp_word) * CHAR_BIT - 2*DIGIT_BIT - 1))) {
      res = fast_s_mp_sqr (a, b);
    } else
#endif
#ifdef BN_S_MP_SQR_C
      res = s_mp_sqr (a, b);
#else
      res = MP_VAL;
#endif
  }
  SET_SIGN(b,MP_ZPOS);
  return res;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_sqr.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
