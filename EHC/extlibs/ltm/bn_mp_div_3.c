#include "tommath.h"
#ifdef BN_MP_DIV_3_C
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

/* divide by three (based on routine from MPI and the GMP manual) */
int
mp_div_3 (mp_int * a, mp_int *c, mp_digit * d)
{
  MP_LOCAL_DEF(q);
  mp_word  w, t;
  mp_digit b;
  int      res, ix;
  
  /* b = 2**DIGIT_BIT / 3 */
  b = (((mp_word)1) << ((mp_word)DIGIT_BIT)) / ((mp_word)3);

  mp_local_init_size(q, USED(a), res) ;
  if (res != MP_OKAY) {
     return res;
  }
  
  SET_USED(MP_LOCAL_REF(q),USED(a));
  SET_SIGN(MP_LOCAL_REF(q),SIGN(a));
  w = 0;
  for (ix = USED(a) - 1; ix >= 0; ix--) {
     w = (w << ((mp_word)DIGIT_BIT)) | ((mp_word)DIGIT(a,ix));

     if (w >= 3) {
        /* multiply w by [1/3] */
        t = (w * ((mp_word)b)) >> ((mp_word)DIGIT_BIT);

        /* now subtract 3 * [w/3] from w, to get the remainder */
        w -= t+t+t;

        /* fixup the remainder as required since
         * the optimization is not exact.
         */
        while (w >= 3) {
           t += 1;
           w -= 3;
        }
      } else {
        t = 0;
      }
      SET_DIGIT(MP_LOCAL_REF(q),ix,(mp_digit)t);
  }

  /* [optional] store the remainder */
  if (d != NULL) {
     *d = (mp_digit)w;
  }

  /* [optional] store the quotient */
  if (c != NULL) {
     mp_clamp(MP_LOCAL_REF(q));
     mp_local_assignfrom(c,q);
  }
  mp_local_clear(q);
  
  return res;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_div_3.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
