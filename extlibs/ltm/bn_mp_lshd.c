#include "tommath.h"
#ifdef BN_MP_LSHD_C
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

/* shift left a certain amount of digits */
int mp_lshd (mp_int * a, int b)
{
  int     x, res;

  /* if its less than zero return */
  if (b <= 0) {
    return MP_OKAY;
  }

  /* grow to fit the new digits */
  if (ALLOC(a) < USED(a) + b) {
     if ((res = mp_grow (a, USED(a) + b)) != MP_OKAY) {
       return res;
     }
  }

  {
    register mp_digit *top, *bottom;

    /* increment the used by the shift amount then copy upwards */
    INC_USED(a,b);

    /* top */
    top = DIGITS(a) + USED(a) - 1;

    /* base */
    bottom = DIGITS(a) + USED(a) - 1 - b;

    /* much like mp_rshd this is implemented using a sliding window
     * except the window goes the otherway around.  Copying from
     * the bottom to the top.  see bn_mp_rshd.c for more info.
     */
    for (x = USED(a) - 1; x >= b; x--) {
      *top-- = *bottom--;
    }

    /* zero the lower digits */
    top = DIGITS(a);
    for (x = 0; x < b; x++) {
      *top++ = 0;
    }
  }
  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_lshd.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
