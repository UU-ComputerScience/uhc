#include "tommath.h"
#ifdef BN_S_MP_MUL_DIGS_C
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

/* multiplies |a| * |b| and only computes upto digs digits of result
 * HAC pp. 595, Algorithm 14.12  Modified so you can control how 
 * many digits of output are created.
 */
int s_mp_mul_digs (mp_int * a, mp_int * b, mp_int * c, int digs)
{
  MP_LOCAL_DEF(t);
  int     res, pa, pb, ix, iy;
  mp_digit u;
  mp_word r;
  mp_digit tmpx, *tmpt, *tmpy;

// #ifndef __UHC_BUILDS_RTS__
  /* can we use the fast multiplier? */
  if (((digs) < MP_WARRAY) &&
      MIN (USED(a), USED(b)) < 
          (1 << ((CHAR_BIT * sizeof (mp_word)) - (2 * DIGIT_BIT)))) {
    return fast_s_mp_mul_digs (a, b, c, digs);
  }
// #endif

  mp_local_init_size(t, digs, res) ;
  if (res != MP_OKAY) {
    return res;
  }
  SET_USED(MP_LOCAL_REF(t),digs);

  /* compute the digits of the product directly */
  pa = USED(a);
  for (ix = 0; ix < pa; ix++) {
    /* set the carry to zero */
    u = 0;

    /* limit ourselves to making digs digits of output */
    pb = MIN (USED(b), digs - ix);

    /* setup some aliases */
    /* copy of the digit from a used within the nested loop */
    tmpx = DIGIT(a,ix);
    
    /* an alias for the destination shifted ix places */
    tmpt = &DIGIT(MP_LOCAL_REF(t),ix);
    
    /* an alias for the digits of b */
    tmpy = DIGITS(b);

    /* compute the columns of the output and propagate the carry */
    for (iy = 0; iy < pb; iy++) {
      /* compute the column as a mp_word */
      r       = ((mp_word)*tmpt) +
                ((mp_word)tmpx) * ((mp_word)*tmpy++) +
                ((mp_word) u);

      /* the new column is the lower part of the result */
      *tmpt++ = (mp_digit) (r & ((mp_word) MP_MASK));

      /* get the carry word from the result */
      u       = (mp_digit) (r >> ((mp_word) DIGIT_BIT));
    }
    /* set carry if it is placed below digs */
    if (ix + iy < digs) {
      *tmpt = u;
    }
  }

  mp_clamp (MP_LOCAL_REF(t));
  mp_local_assignfrom(c,t);

  mp_local_clear(t);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_s_mp_mul_digs.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
