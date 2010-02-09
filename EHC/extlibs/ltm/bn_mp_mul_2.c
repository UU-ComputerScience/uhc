#include "tommath.h"
#ifdef BN_MP_MUL_2_C
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

/* b = a*2 */
int mp_mul_2(mp_int * a, mp_int * b)
{
  int     x, res, oldused;

  /* grow to accomodate result */
  if (ALLOC(b) < USED(a) + 1) {
    if ((res = mp_grow (b, USED(a) + 1)) != MP_OKAY) {
      return res;
    }
  }

  oldused = USED(b);
  SET_USED(b,USED(a));

  {
    register mp_digit r, rr, *tmpa, *tmpb;

    /* alias for source */
    tmpa = DIGITS(a);
    
    /* alias for dest */
    tmpb = DIGITS(b);

    /* carry */
    r = 0;
    for (x = 0; x < USED(a); x++) {
    
      /* get what will be the *next* carry bit from the 
       * MSB of the current digit 
       */
      rr = *tmpa >> ((mp_digit)(DIGIT_BIT - 1));
      
      /* now shift up this digit, add in the carry [from the previous] */
      *tmpb++ = ((*tmpa++ << ((mp_digit)1)) | r) & MP_MASK;
      
      /* copy the carry that would be from the source 
       * digit into the next iteration 
       */
      r = rr;
    }

    /* new leading digit? */
    if (r != 0) {
      /* add a MSB which is always 1 at this point */
      *tmpb = 1;
      INC1_USED(b);
    }

    /* now zero any excess digits on the destination 
     * that we didn't write to 
     */
    tmpb = DIGITS(b) + USED(b);
    for (x = USED(b); x < oldused; x++) {
      *tmpb++ = 0;
    }
  }
  SET_SIGN(b,SIGN(a));
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_mul_2.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
