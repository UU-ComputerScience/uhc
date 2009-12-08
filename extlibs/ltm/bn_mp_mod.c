#include "tommath.h"
#ifdef BN_MP_MOD_C
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

/* c = a mod b, 0 <= c < b */
int
mp_mod (mp_int * a, mp_int * b, mp_int * c)
{
  MP_LOCAL_DEF(t);
  int     res;

  mp_local_init(t,USED(b),res) ;
  if (res != MP_OKAY) {
    return res;
  }

  if ((res = mp_div (a, b, NULL, MP_LOCAL_REF(t))) != MP_OKAY) {
    mp_local_clear(t);
    return res;
  }

  if (SIGN(MP_LOCAL_REF(t)) != SIGN(b)) {
    res = mp_add (b, MP_LOCAL_REF(t), c);
  } else {
    res = MP_OKAY;
    mp_local_assignfrom(c, t) ;
  }

  mp_local_clear(t);
  return res;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_mod.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
