#include "tommath.h"
#include <math.h>
#include <float.h>

#ifdef BN_MP_SET_INT_C
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
 *
 * 20091124 (Atze Dijkstra): added for UHC
 */

/* set a double */
int mp_set_double(mp_int * a, double b)
{
  int     res, exp ;

  double mant = frexp( b, &exp ) ;
  if ( exp == 0 ) {
    // is zero
    mp_zero (a);
    return MP_OKAY ;
  }
  
  // mant is fraction in the interval [1/2, 1),
  // corrected so fraction after the comma can be rounded
  int expinc = exp - MAX(0, exp - DBL_MANT_DIG) ;
  if ( expinc > 0 ) {
    b = ldexp( mant, expinc ) ;
  } else if ( expinc < 0 ) {
    b = mant / ldexp( 1.0, -expinc ) ;
  }
  
  // b should now fit into 64 bits
  if ((res = mp_set_double64(a, b)) != MP_OKAY) {
    return res;
  }
  
  // then correct it by the exponent
  if ((res = mp_mul_2d (a, expinc, a)) != MP_OKAY) {
    return res;
  }

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_set_int.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
