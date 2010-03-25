#include "tommath.h"
#ifdef BN_MP_GET_INT_C
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

/* get the double of an mp_int */
double mp_get_double(mp_int * a) 
{
  int i;
  double res;

  if (USED(a) == 0) {
     return 0.0;
  }

  // printf( "mp_get_double SIGN=%d MP_NEG=%d\n", SIGN(a), MP_NEG ) ;
  /* get number of digits of the lsb we have to read */
  i = USED(a)-1;

  /* get most significant digit of result */
  res = (double)DIGIT(a,i);
   
  // printf( "mp_get_double i=%d dig[i]=%x(%f) res=%f res<<=%f\n", i, DIGIT(a,i), (double)DIGIT(a,i), res, (res * (double)(1 << DIGIT_BIT)) ) ;
  while (--i >= 0) {
    double res2 = (res * (double)(1L << DIGIT_BIT)) + (double)DIGIT(a,i);
  	// printf( "mp_get_double i=%d dig[i]=%x(%f) res=%f res<<=%f res2=%f\n", i, DIGIT(a,i), (double)DIGIT(a,i), res, (res * (double)(1 << DIGIT_BIT)), res2 ) ;
  	res = res2 ;
  }

  // printf( "mp_get_double SIGN=%d MP_NEG=%d\n", SIGN(a), MP_NEG ) ;
  return ( SIGN(a) == MP_NEG ? -res : res ) ;
  // if (SIGN(a) == MP_NEG) {
  // 	res *= -1 ;
  // }
  // printf( "mp_get_double res=%f\n", res ) ;
  // return res ;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_get_int.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
