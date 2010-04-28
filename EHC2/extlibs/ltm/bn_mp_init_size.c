#include "tommath.h"
#ifdef BN_MP_INIT_SIZE_C
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

/* init an mp_init for a given size */
int mp_init_size (mp_int * a, int size)
{
#ifdef __UHC_BUILDS_RTS__
  // all allocation is assumed to be done outside library
  mp_zero(a) ;
  printf( "WARNING: mp_init_size sz=%x (%p used=%x alc=%x)\n", size, a, USED(a), ALLOC(a) ) ;
  // prLTM(a,"mp_init_size") ;
#else
  int x;

  /* pad size so there are always extra digits */
  size += (MP_PREC * 2) - (size % MP_PREC);	
  
  /* alloc mem */
  SET_DIGITS(a, OPT_CAST(mp_digit) XMALLOC (sizeof (mp_digit) * size));
  if (DIGITS(a) == NULL) {
    return MP_MEM;
  }

  /* set the members */
  SET_USED(a,0);
  SET_ALLOC(a,size);
  SET_SIGN(a,MP_ZPOS);

  /* zero the digits */
  for (x = 0; x < size; x++) {
      SET_DIGIT(a,x,0);
  }
#endif

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_init_size.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
