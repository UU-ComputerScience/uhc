#include "tommath.h"
#ifdef BN_MP_DIV_C
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

#ifdef BN_MP_DIV_SMALL

/* slower bit-bang division... also smaller */
int mp_div(mp_int * a, mp_int * b, mp_int * c, mp_int * d)
{
   mp_int ta, tb, tq, q;
   int    res, n, n2;

  /* is divisor zero ? */
  if (mp_iszero (b) == 1) {
    return MP_VAL;
  }

  /* if a < b then q=0, r = a */
  if (mp_cmp_mag (a, b) == MP_LT) {
    if (d != NULL) {
      res = mp_copy (a, d);
    } else {
      res = MP_OKAY;
    }
    if (c != NULL) {
      mp_zero (c);
    }
    return res;
  }
	
  /* init our temps */
  if ((res = mp_init_multi(&ta, &tb, &tq, &q, NULL) != MP_OKAY)) {
     return res;
  }


  mp_set(&tq, 1);
  n = mp_count_bits(a) - mp_count_bits(b);
  if (((res = mp_abs(a, &ta)) != MP_OKAY) ||
      ((res = mp_abs(b, &tb)) != MP_OKAY) || 
      ((res = mp_mul_2d(&tb, n, &tb)) != MP_OKAY) ||
      ((res = mp_mul_2d(&tq, n, &tq)) != MP_OKAY)) {
      goto LBL_ERR;
  }

  while (n-- >= 0) {
     if (mp_cmp(&tb, &ta) != MP_GT) {
        if (((res = mp_sub(&ta, &tb, &ta)) != MP_OKAY) ||
            ((res = mp_add(&q, &tq, &q)) != MP_OKAY)) {
           goto LBL_ERR;
        }
     }
     if (((res = mp_div_2d(&tb, 1, &tb, NULL)) != MP_OKAY) ||
         ((res = mp_div_2d(&tq, 1, &tq, NULL)) != MP_OKAY)) {
           goto LBL_ERR;
     }
  }

  /* now q == quotient and ta == remainder */
  n  = SIGN(a);
  n2 = (SIGN(a) == SIGN(b) ? MP_ZPOS : MP_NEG);
  if (c != NULL) {
     mp_exch(c, &q);
     SET_SIGN(c,(mp_iszero(c) == MP_YES) ? MP_ZPOS : n2);
  }
  if (d != NULL) {
     mp_exch(d, &ta);
     SET_SIGN(d,(mp_iszero(d) == MP_YES) ? MP_ZPOS : n);
  }
LBL_ERR:
   mp_clear_multi(&ta, &tb, &tq, &q, NULL);
   return res;
}

#else

/* integer signed division. 
 * c*b + d == a [e.g. a/b, c=quotient, d=remainder]
 * HAC pp.598 Algorithm 14.20
 *
 * Note that the description in HAC is horribly 
 * incomplete.  For example, it doesn't consider 
 * the case where digits are removed from 'x' in 
 * the inner loop.  It also doesn't consider the 
 * case that y has fewer than three digits, etc..
 *
 * The overall algorithm is as described as 
 * 14.20 from HAC but fixed to treat these cases.
*/
int mp_div (mp_int * a, mp_int * b, mp_int * c, mp_int * d)
{
  mp_int  q, x, y, t1, t2;
  int     res, n, t, i, norm, neg;

  /* is divisor zero ? */
  if (mp_iszero (b) == 1) {
    return MP_VAL;
  }

  /* if a < b then q=0, r = a */
  if (mp_cmp_mag (a, b) == MP_LT) {
    if (d != NULL) {
      res = mp_copy (a, d);
    } else {
      res = MP_OKAY;
    }
    if (c != NULL) {
      mp_zero (c);
    }
    return res;
  }

  if ((res = mp_init_size (&q, USED(a) + 2)) != MP_OKAY) {
    return res;
  }
  SET_USED(&q,USED(a) + 2);

  if ((res = mp_init (&t1)) != MP_OKAY) {
    goto LBL_Q;
  }

  if ((res = mp_init (&t2)) != MP_OKAY) {
    goto LBL_T1;
  }

  if ((res = mp_init_copy (&x, a)) != MP_OKAY) {
    goto LBL_T2;
  }

  if ((res = mp_init_copy (&y, b)) != MP_OKAY) {
    goto LBL_X;
  }

  /* fix the sign */
  neg = (SIGN(a) == SIGN(b)) ? MP_ZPOS : MP_NEG;
  SET_SIGN(&x,MP_ZPOS) ;
  SET_SIGN(&y,MP_ZPOS) ;

  /* normalize both x and y, ensure that y >= b/2, [b == 2**DIGIT_BIT] */
  norm = mp_count_bits(&y) % DIGIT_BIT;
  if (norm < (int)(DIGIT_BIT-1)) {
     norm = (DIGIT_BIT-1) - norm;
     if ((res = mp_mul_2d (&x, norm, &x)) != MP_OKAY) {
       goto LBL_Y;
     }
     if ((res = mp_mul_2d (&y, norm, &y)) != MP_OKAY) {
       goto LBL_Y;
     }
  } else {
     norm = 0;
  }

  /* note hac does 0 based, so if used==5 then its 0,1,2,3,4, e.g. use 4 */
  n = USED(&x) - 1;
  t = USED(&y) - 1;

  /* while (x >= y*b**n-t) do { q[n-t] += 1; x -= y*b**{n-t} } */
  if ((res = mp_lshd (&y, n - t)) != MP_OKAY) { /* y = y*b**{n-t} */
    goto LBL_Y;
  }

  while (mp_cmp (&x, &y) != MP_LT) {
    ++(DIGIT(&q,n - t));
    if ((res = mp_sub (&x, &y, &x)) != MP_OKAY) {
      goto LBL_Y;
    }
  }

  /* reset y by shifting it back down */
  mp_rshd (&y, n - t);

  /* step 3. for i from n down to (t + 1) */
  for (i = n; i >= (t + 1); i--) {
    if (i > USED(&x)) {
      continue;
    }

    /* step 3.1 if xi == yt then set q{i-t-1} to b-1, 
     * otherwise set q{i-t-1} to (xi*b + x{i-1})/yt */
    if (DIGIT(&x,i) == DIGIT(&y,t)) {
      SET_DIGIT(&q,i - t - 1,((((mp_digit)1) << DIGIT_BIT) - 1));
    } else {
      mp_word tmp;
      tmp = ((mp_word) DIGIT(&x,i)) << ((mp_word) DIGIT_BIT);
      tmp |= ((mp_word) DIGIT(&x,i - 1));
      tmp /= ((mp_word) DIGIT(&y,t));
      if (tmp > (mp_word) MP_MASK)
        tmp = MP_MASK;
      SET_DIGIT(&q,i - t - 1,(mp_digit) (tmp & (mp_word) (MP_MASK)));
    }

    /* while (q{i-t-1} * (yt * b + y{t-1})) > 
             xi * b**2 + xi-1 * b + xi-2 
     
       do q{i-t-1} -= 1; 
    */
    SET_DIGIT(&q,i - t - 1,(DIGIT(&q,i - t - 1) + 1) & MP_MASK);
    do {
      SET_DIGIT(&q,i - t - 1,(DIGIT(&q,i - t - 1) - 1) & MP_MASK);

      /* find left hand */
      mp_zero (&t1);
      SET_DIGIT(&t1,0,(t - 1 < 0) ? 0 : DIGIT(&y,t - 1));
      SET_DIGIT(&t1,1,DIGIT(&y,t));
      SET_USED(&t1,2);
      if ((res = mp_mul_d (&t1, DIGIT(&q,i - t - 1), &t1)) != MP_OKAY) {
        goto LBL_Y;
      }

      /* find right hand */
      SET_DIGIT(&t2,0,(i - 2 < 0) ? 0 : DIGIT(&x,i - 2));
      SET_DIGIT(&t2,1,(i - 1 < 0) ? 0 : DIGIT(&x,i - 1));
      SET_DIGIT(&t2,2,DIGIT(&x,i));
      SET_USED(&t2,3);
    } while (mp_cmp_mag(&t1, &t2) == MP_GT);

    /* step 3.3 x = x - q{i-t-1} * y * b**{i-t-1} */
    if ((res = mp_mul_d (&y, DIGIT(&q,i - t - 1), &t1)) != MP_OKAY) {
      goto LBL_Y;
    }

    if ((res = mp_lshd (&t1, i - t - 1)) != MP_OKAY) {
      goto LBL_Y;
    }

    if ((res = mp_sub (&x, &t1, &x)) != MP_OKAY) {
      goto LBL_Y;
    }

    /* if x < 0 then { x = x + y*b**{i-t-1}; q{i-t-1} -= 1; } */
    if (SIGN(&x) == MP_NEG) {
      if ((res = mp_copy (&y, &t1)) != MP_OKAY) {
        goto LBL_Y;
      }
      if ((res = mp_lshd (&t1, i - t - 1)) != MP_OKAY) {
        goto LBL_Y;
      }
      if ((res = mp_add (&x, &t1, &x)) != MP_OKAY) {
        goto LBL_Y;
      }

      SET_DIGIT(&q,i - t - 1,(DIGIT(&q,i - t - 1) - 1UL) & MP_MASK);
    }
  }

  /* now q is the quotient and x is the remainder 
   * [which we have to normalize] 
   */
  
  /* get sign before writing to c */
  SET_SIGN(&x,USED(&x) == 0 ? MP_ZPOS : SIGN(a));

  if (c != NULL) {
    mp_clamp (&q);
    mp_exch (&q, c);
    SET_SIGN(c,neg);
  }

  if (d != NULL) {
    mp_div_2d (&x, norm, &x, NULL);
    mp_exch (&x, d);
  }

  res = MP_OKAY;

LBL_Y:mp_clear (&y);
LBL_X:mp_clear (&x);
LBL_T2:mp_clear (&t2);
LBL_T1:mp_clear (&t1);
LBL_Q:mp_clear (&q);
  return res;
}

#endif

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_div.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
