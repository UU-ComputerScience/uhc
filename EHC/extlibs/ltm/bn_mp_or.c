#include "tommath.h"
#ifdef BN_MP_OR_C
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

/* OR two ints together */
int mp_or (mp_int * a, mp_int * b, mp_int * c)
{
  int     res, ix, px;
  MP_LOCAL_DEF(t) ;
  mp_int  *x;

  if (USED(a) > USED(b)) {
    mp_local_init_copy(t, a, res, "mp_or") ;
    if (res != MP_OKAY) {
      return res;
    }
    px = USED(b);
    x = b;
  } else {
    mp_local_init_copy(t, b, res, "mp_or") ;
    if (res != MP_OKAY) {
      return res;
    }
    px = USED(a);
    x = a;
  }

  for (ix = 0; ix < px; ix++) {
    OR_DIGIT(MP_LOCAL_REF(t),ix,DIGIT(x,ix)) ;
  }
  mp_clamp (MP_LOCAL_REF(t));
  mp_local_assignfrom(c, t);
  mp_local_clear(t);
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_or.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
