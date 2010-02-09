#include "tommath.h"
#ifdef BN_MP_TORADIX_C
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

/* stores a bignum as a ASCII string in a given radix (2..64) */
int mp_toradix (mp_int * a, char *str, int radix)
{
  int     res, digs;
  MP_LOCAL_DEF(t);
  mp_digit d;
  char   *_s = str;

  /* check range of the radix */
  if (radix < 2 || radix > 64) {
    return MP_VAL;
  }

  /* check out if its zero */
  if ( mp_iszero(a) ) {
     *str++ = '0';
     *str = '\0';
     return MP_OKAY;
  }

  // printf( "mp_toradix mp_local_init_copy 1: a=%p used(a)=%x\n", a, USED(a) ) ;
  mp_local_init_copy(t, a, res, "mp_toradix") ;
  // printf( "mp_toradix mp_local_init_copy 2\n" ) ;
  if (res != MP_OKAY) {
    return res;
  }

  /* if it is negative output a - */
  if (SIGN(MP_LOCAL_REF(t)) == MP_NEG) {
    ++_s;
    *str++ = '-';
    SET_SIGN(MP_LOCAL_REF(t),MP_ZPOS);
  }

  digs = 0;
  while (mp_iszero (MP_LOCAL_REF(t)) == 0) {
    // printf( "mp_toradix mp_div_d\n" ) ;
    if ((res = mp_div_d (MP_LOCAL_REF(t), (mp_digit) radix, MP_LOCAL_REF(t), &d)) != MP_OKAY) {
      mp_local_clear(t);
      return res;
    }
    *str++ = mp_s_rmap[d];
    ++digs;
  }

  /* reverse the digits of the string.  In this case _s points
   * to the first digit [exluding the sign] of the number]
   */
  bn_reverse ((unsigned char *)_s, digs);

  /* append a NULL so the string is properly terminated */
  *str = '\0';

  mp_local_clear(t);
  return MP_OKAY;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_toradix.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
