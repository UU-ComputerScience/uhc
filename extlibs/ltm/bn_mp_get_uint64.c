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

/* get the lower unsigned 64-bits of an mp_int */
uint64_t mp_get_uint64(mp_int * a) 
{
  int i;
  uint64_t res;

  if (USED(a) == 0) {
     return 0;
  }

  /* get number of digits of the lsb we have to read */
  i = MIN(USED(a),(int)((sizeof(uint64_t)*CHAR_BIT+DIGIT_BIT-1)/DIGIT_BIT))-1;

  /* get most significant digit of result */
  res = DIGIT(a,i);
   
  while (--i >= 0) {
    res = (res << DIGIT_BIT) | DIGIT(a,i);
  }

  return res;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_get_int.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
