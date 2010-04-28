#include "tommath.h"
#ifdef BN_FAST_S_MP_SQR_C
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

/* the jist of squaring...
 * you do like mult except the offset of the tmpx [one that 
 * starts closer to zero] can't equal the offset of tmpy.  
 * So basically you set up iy like before then you min it with
 * (ty-tx) so that it never happens.  You double all those 
 * you add in the inner loop

After that loop you do the squares and add them in.
*/

int fast_s_mp_sqr (mp_int * a, mp_int * b)
{
  int       olduse, res, pa, ix, iz;
  mp_digit   W[MP_WARRAY], *tmpx;
  mp_word   W1;

  /* grow the destination as required */
  pa = USED(a) + USED(a);
  if (ALLOC(b) < pa) {
    if ((res = mp_grow (b, pa)) != MP_OKAY) {
      return res;
    }
  }

  /* number of output digits to produce */
  W1 = 0;
  for (ix = 0; ix < pa; ix++) { 
      int      tx, ty, iy;
      mp_word  _W;
      mp_digit *tmpy;

      /* clear counter */
      _W = 0;

      /* get offsets into the two bignums */
      ty = MIN(USED(a)-1, ix);
      tx = ix - ty;

      /* setup temp aliases */
      tmpx = DIGITS(a) + tx;
      tmpy = DIGITS(a) + ty;

      /* this is the number of times the loop will iterrate, essentially
         while (tx++ < USED(a) && ty-- >= 0) { ... }
       */
      iy = MIN(USED(a)-tx, ty+1);

      /* now for squaring tx can never equal ty 
       * we halve the distance since they approach at a rate of 2x
       * and we have to round because odd cases need to be executed
       */
      iy = MIN(iy, (ty-tx+1)>>1);

      /* execute loop */
      for (iz = 0; iz < iy; iz++) {
         _W += ((mp_word)*tmpx++)*((mp_word)*tmpy--);
      }

      /* double the inner product and add carry */
      _W = _W + _W + W1;

      /* even columns have the square term in them */
      if ((ix&1) == 0) {
         _W += ((mp_word)DIGIT(a,ix>>1))*((mp_word)DIGIT(a,ix>>1));
      }

      /* store it */
      W[ix] = (mp_digit)(_W & MP_MASK);

      /* make next carry */
      W1 = _W >> ((mp_word)DIGIT_BIT);
  }

  /* setup dest */
  olduse  = USED(b);
  SET_USED(b,USED(a)+USED(a));

  {
    mp_digit *tmpb;
    tmpb = DIGITS(b);
    for (ix = 0; ix < pa; ix++) {
      *tmpb++ = W[ix] & MP_MASK;
    }

    /* clear unused digits [that existed in the old copy of c] */
    for (; ix < olduse; ix++) {
      *tmpb++ = 0;
    }
  }
  mp_clamp (b);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_fast_s_mp_sqr.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
