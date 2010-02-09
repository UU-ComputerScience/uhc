#include "tommath.h"
#ifdef BN_S_MP_MUL_HIGH_DIGS_C
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

/* multiplies |a| * |b| and does not compute the lower digs digits
 * [meant to get the higher part of the product]
 */
int
s_mp_mul_high_digs (mp_int * a, mp_int * b, mp_int * c, int digs)
{
  MP_LOCAL_DEF(t);
  int     res, pa, pb, ix, iy;
  mp_digit u;
  mp_word r;
  mp_digit tmpx, *tmpt, *tmpy;

  /* can we use the fast multiplier? */
#ifdef BN_FAST_S_MP_MUL_HIGH_DIGS_C
  if (((USED(a) + USED(b) + 1) < MP_WARRAY)
      && MIN (USED(a), USED(b)) < (1 << ((CHAR_BIT * sizeof (mp_word)) - (2 * DIGIT_BIT)))) {
    return fast_s_mp_mul_high_digs (a, b, c, digs);
  }
#endif

  mp_local_init_size(t, USED(a) + USED(b) + 1, res) ;
  if (res != MP_OKAY) {
    return res;
  }
  SET_USED(MP_LOCAL_REF(t),USED(a) + USED(b) + 1);

  pa = USED(a);
  pb = USED(b);
  for (ix = 0; ix < pa; ix++) {
    /* clear the carry */
    u = 0;

    /* left hand side of A[ix] * B[iy] */
    tmpx = DIGIT(a,ix);

    /* alias to the address of where the digits will be stored */
    tmpt = &(DIGIT(MP_LOCAL_REF(t),digs));

    /* alias for where to read the right hand side from */
    tmpy = DIGITS(b) + (digs - ix);

    for (iy = digs - ix; iy < pb; iy++) {
      /* calculate the double precision result */
      r       = ((mp_word)*tmpt) +
                ((mp_word)tmpx) * ((mp_word)*tmpy++) +
                ((mp_word) u);

      /* get the lower part */
      *tmpt++ = (mp_digit) (r & ((mp_word) MP_MASK));

      /* carry the carry */
      u       = (mp_digit) (r >> ((mp_word) DIGIT_BIT));
    }
    *tmpt = u;
  }
  mp_clamp (MP_LOCAL_REF(t));
  mp_local_assignfrom(c,t);

  mp_local_clear(t);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_s_mp_mul_high_digs.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
