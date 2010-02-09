#include "tommath.h"
#ifdef BN_MP_GROW_C
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

/* grow as required */
int mp_grow (mp_int * a, int size)
{
#ifdef __UHC_BUILDS_RTS__
  if (ALLOC(a) < size) {
    // all allocation is assumed to be done outside library
    printf( "WARNING: mp_grow sz=%x (%p used=%x alc=%x)\n", size, a, USED(a), ALLOC(a) ) ;
    // prLTM(a,"mp_grow") ;
    return MP_MEM ;
  }
#else
  int     i;
  mp_digit *tmp;

  /* if the alloc size is smaller alloc more ram */
  if (ALLOC(a) < size) {
    /* ensure there are always at least MP_PREC digits extra on top */
    size += (MP_PREC * 2) - (size % MP_PREC);

    /* reallocate the array a->dp
     *
     * We store the return in a temporary variable
     * in case the operation failed we don't want
     * to overwrite the dp member of a.
     */
    tmp = OPT_CAST(mp_digit) XREALLOC (DIGITS(a), sizeof (mp_digit) * size);
    if (tmp == NULL) {
      /* reallocation failed but "a" is still valid [can be freed] */
      return MP_MEM;
    }

    /* reallocation succeeded so set a->dp */
    SET_DIGITS(a, tmp);

    /* zero excess digits */
    i        = ALLOC(a);
    SET_ALLOC(a,size);
    for (; i < ALLOC(a); i++) {
      SET_DIGIT(a,i,0);
    }
  }
#endif

  return MP_OKAY;
}
#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_grow.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
