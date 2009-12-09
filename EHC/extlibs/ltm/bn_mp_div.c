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
#ifdef __UHC_BUILDS_RTS__
// 20091207 AD: non small seems to be buggy
// #define BN_MP_DIV_SMALL
#endif

#ifdef BN_MP_DIV_SMALL

/* slower bit-bang division... also smaller */
int mp_div(mp_int * a, mp_int * b, mp_int * c, mp_int * d)
{
   // mp_int ta, tb, tq, q;
   MP_LOCAL_DEF(ta) ;
   MP_LOCAL_DEF(tb) ;
   MP_LOCAL_DEF(tq) ;
   MP_LOCAL_DEF(q) ;
   int    res, n, n2;
   
   // prLTM(a,"mp_div a") ;
   // prLTM(b,"mp_div b") ;
   // prLTM(c,"mp_div c") ;
   // prLTM(d,"mp_div d") ;

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
  int sz = (MAX(USED(a),USED(b)) << 1) + 1 ;
  mp_local_init( ta, sz, res ) ;
  if (res != MP_OKAY) {
    return res; 
  }
  mp_local_init( tb, sz, res ) ;
  if (res != MP_OKAY) {
    goto LBL_TA; 
  }
  mp_local_init( tq, sz, res ) ;
  if (res != MP_OKAY) {
    goto LBL_TB; 
  }
  mp_local_init( q, sz, res ) ;
  if (res != MP_OKAY) {
    goto LBL_TQ; 
  }
  
  // if ((res = mp_init_multi(&ta, &tb, &tq, &q, NULL) != MP_OKAY)) {
  //    return res;
  // }


  mp_set(MP_LOCAL_REF(tq), 1);
  n = mp_count_bits(a) - mp_count_bits(b);
  if (((res = mp_abs(a, MP_LOCAL_REF(ta))) != MP_OKAY) ||
      ((res = mp_abs(b, MP_LOCAL_REF(tb))) != MP_OKAY) || 
      ((res = mp_mul_2d(MP_LOCAL_REF(tb), n, MP_LOCAL_REF(tb))) != MP_OKAY) ||
      ((res = mp_mul_2d(MP_LOCAL_REF(tq), n, MP_LOCAL_REF(tq))) != MP_OKAY)) {
      goto LBL_ERR;
  }

  while (n-- >= 0) {
     if (mp_cmp(MP_LOCAL_REF(tb), MP_LOCAL_REF(ta)) != MP_GT) {
        if (((res = mp_sub(MP_LOCAL_REF(ta), MP_LOCAL_REF(tb), MP_LOCAL_REF(ta))) != MP_OKAY) ||
            ((res = mp_add(MP_LOCAL_REF(q), MP_LOCAL_REF(tq), MP_LOCAL_REF(q))) != MP_OKAY)) {
           goto LBL_ERR;
        }
     }
     if (((res = mp_div_2d(MP_LOCAL_REF(tb), 1, MP_LOCAL_REF(tb), NULL)) != MP_OKAY) ||
         ((res = mp_div_2d(MP_LOCAL_REF(tq), 1, MP_LOCAL_REF(tq), NULL)) != MP_OKAY)) {
           goto LBL_ERR;
     }
  }

  /* now q == quotient and ta == remainder */
  n  = SIGN(a);
  n2 = (SIGN(a) == SIGN(b) ? MP_ZPOS : MP_NEG);
  if (c != NULL) {
     mp_local_assignfrom(c, q);
     SET_SIGN(c,(mp_iszero(c) == MP_YES) ? MP_ZPOS : n2);
  }
  if (d != NULL) {
     mp_local_assignfrom(d, ta);
     SET_SIGN(d,(mp_iszero(d) == MP_YES) ? MP_ZPOS : n);
  }
LBL_ERR:
   // mp_clear_multi(&ta, &tb, &tq, &q, NULL);
LBL_Q:mp_local_clear(q);
LBL_TQ:mp_local_clear(tq);
LBL_TB:mp_local_clear(tb);
LBL_TA:mp_local_clear(ta);
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
  MP_LOCAL_DEF(q) ;
  MP_LOCAL_DEF(x) ;
  MP_LOCAL_DEF(y) ;
  MP_LOCAL_DEF(t1) ;
  MP_LOCAL_DEF(t2) ;
  int     res, n, t, i, norm, neg;

   // prLTM(a,"mp_div BEF a") ;
   // prLTM(b,"mp_div BEF b") ;
   // prLTM(c,"mp_div BEF c") ;
   // prLTM(d,"mp_div BEF d") ;

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

  mp_local_init_size( q, ALLOC(c) + 2, res ) ;
  if (res != MP_OKAY) {
    return res;
  }
  SET_USED(MP_LOCAL_REF(q),USED(a) + 2);

  int sz = (MAX(USED(a),USED(b)) << 1) + 2 ;
  mp_local_init( t1, sz, res ) ;
  if (res != MP_OKAY) {
    goto LBL_Q;
  }

  mp_local_init( t2, sz, res ) ;
  if (res != MP_OKAY) {
    goto LBL_T1;
  }

  mp_local_init_copy_extrasize( x, a, res, ALLOC(c), "mp_div" ) ;
  if (res != MP_OKAY) {
    goto LBL_T2;
  }

  mp_local_init_copy_extrasize( y, b, res, ALLOC(c), "mp_div" ) ;
  if (res != MP_OKAY) {
    goto LBL_X;
  }

  /* fix the sign */
  neg = (SIGN(a) == SIGN(b)) ? MP_ZPOS : MP_NEG;
  SET_SIGN(MP_LOCAL_REF(x),MP_ZPOS) ;
  SET_SIGN(MP_LOCAL_REF(y),MP_ZPOS) ;

  /* normalize both x and y, ensure that y >= b/2, [b == 2**DIGIT_BIT] */
  // printf( "mp_div A\n" ) ;
  norm = mp_count_bits(MP_LOCAL_REF(y)) % DIGIT_BIT;
  if (norm < (int)(DIGIT_BIT-1)) {
     norm = (DIGIT_BIT-1) - norm;
     if ((res = mp_mul_2d (MP_LOCAL_REF(x), norm, MP_LOCAL_REF(x))) != MP_OKAY) {
       goto LBL_Y;
     }
     if ((res = mp_mul_2d (MP_LOCAL_REF(y), norm, MP_LOCAL_REF(y))) != MP_OKAY) {
       goto LBL_Y;
     }
  } else {
     norm = 0;
  }

  /* note hac does 0 based, so if used==5 then its 0,1,2,3,4, e.g. use 4 */
  n = USED(MP_LOCAL_REF(x)) - 1;
  t = USED(MP_LOCAL_REF(y)) - 1;

  /* while (x >= y*b**n-t) do { q[n-t] += 1; x -= y*b**{n-t} } */
  // printf( "mp_div B\n" ) ;
  if ((res = mp_lshd (MP_LOCAL_REF(y), n - t)) != MP_OKAY) { /* y = y*b**{n-t} */
    goto LBL_Y;
  }

  // printf( "mp_div C\n" ) ;
  while (mp_cmp (MP_LOCAL_REF(x), MP_LOCAL_REF(y)) != MP_LT) {
    ++(DIGIT(MP_LOCAL_REF(q),n - t));
    if ((res = mp_sub (MP_LOCAL_REF(x), MP_LOCAL_REF(y), MP_LOCAL_REF(x))) != MP_OKAY) {
      goto LBL_Y;
    }
  }

  /* reset y by shifting it back down */
  // printf( "mp_div D\n" ) ;
  mp_rshd (MP_LOCAL_REF(y), n - t);

  /* step 3. for i from n down to (t + 1) */
  for (i = n; i >= (t + 1); i--) {
    if (i > USED(MP_LOCAL_REF(x))) {
      continue;
    }

    /* step 3.1 if xi == yt then set q{i-t-1} to b-1, 
     * otherwise set q{i-t-1} to (xi*b + x{i-1})/yt */
    if (DIGIT(MP_LOCAL_REF(x),i) == DIGIT(MP_LOCAL_REF(y),t)) {
      SET_DIGIT(MP_LOCAL_REF(q),i - t - 1,((((mp_digit)1) << DIGIT_BIT) - 1));
    } else {
      mp_word tmp;
      tmp = ((mp_word) DIGIT(MP_LOCAL_REF(x),i)) << ((mp_word) DIGIT_BIT);
      tmp |= ((mp_word) DIGIT(MP_LOCAL_REF(x),i - 1));
      tmp /= ((mp_word) DIGIT(MP_LOCAL_REF(y),t));
      if (tmp > (mp_word) MP_MASK)
        tmp = MP_MASK;
      SET_DIGIT(MP_LOCAL_REF(q),i - t - 1,(mp_digit) (tmp & (mp_word) (MP_MASK)));
    }

    /* while (q{i-t-1} * (yt * b + y{t-1})) > 
             xi * b**2 + xi-1 * b + xi-2 
     
       do q{i-t-1} -= 1; 
    */
    // printf( "mp_div E\n" ) ;
    SET_DIGIT(MP_LOCAL_REF(q),i - t - 1,(DIGIT(MP_LOCAL_REF(q),i - t - 1) + 1) & MP_MASK);
    do {
      SET_DIGIT(MP_LOCAL_REF(q),i - t - 1,(DIGIT(MP_LOCAL_REF(q),i - t - 1) - 1) & MP_MASK);

      /* find left hand */
      mp_zero (MP_LOCAL_REF(t1));
      SET_DIGIT(MP_LOCAL_REF(t1),0,(t - 1 < 0) ? 0 : DIGIT(MP_LOCAL_REF(y),t - 1));
      SET_DIGIT(MP_LOCAL_REF(t1),1,DIGIT(MP_LOCAL_REF(y),t));
      SET_USED(MP_LOCAL_REF(t1),2);
      if ((res = mp_mul_d (MP_LOCAL_REF(t1), DIGIT(MP_LOCAL_REF(q),i - t - 1), MP_LOCAL_REF(t1))) != MP_OKAY) {
        goto LBL_Y;
      }

      /* find right hand */
      SET_DIGIT(MP_LOCAL_REF(t2),0,(i - 2 < 0) ? 0 : DIGIT(MP_LOCAL_REF(x),i - 2));
      SET_DIGIT(MP_LOCAL_REF(t2),1,(i - 1 < 0) ? 0 : DIGIT(MP_LOCAL_REF(x),i - 1));
      SET_DIGIT(MP_LOCAL_REF(t2),2,DIGIT(MP_LOCAL_REF(x),i));
      SET_USED(MP_LOCAL_REF(t2),3);
    } while (mp_cmp_mag(MP_LOCAL_REF(t1), MP_LOCAL_REF(t2)) == MP_GT);

    /* step 3.3 x = x - q{i-t-1} * y * b**{i-t-1} */
    // printf( "mp_div F\n" ) ;
    if ((res = mp_mul_d (MP_LOCAL_REF(y), DIGIT(MP_LOCAL_REF(q),i - t - 1), MP_LOCAL_REF(t1))) != MP_OKAY) {
      goto LBL_Y;
    }

    // printf( "mp_div G\n" ) ;
    if ((res = mp_lshd (MP_LOCAL_REF(t1), i - t - 1)) != MP_OKAY) {
      goto LBL_Y;
    }

    // printf( "mp_div H\n" ) ;
    if ((res = mp_sub (MP_LOCAL_REF(x), MP_LOCAL_REF(t1), MP_LOCAL_REF(x))) != MP_OKAY) {
      goto LBL_Y;
    }

    /* if x < 0 then { x = x + y*b**{i-t-1}; q{i-t-1} -= 1; } */
    // printf( "mp_div I\n" ) ;
    if (SIGN(MP_LOCAL_REF(x)) == MP_NEG) {
      if ((res = mp_copy (MP_LOCAL_REF(y), MP_LOCAL_REF(t1))) != MP_OKAY) {
        goto LBL_Y;
      }
      if ((res = mp_lshd (MP_LOCAL_REF(t1), i - t - 1)) != MP_OKAY) {
        goto LBL_Y;
      }
      if ((res = mp_add (MP_LOCAL_REF(x), MP_LOCAL_REF(t1), MP_LOCAL_REF(x))) != MP_OKAY) {
        goto LBL_Y;
      }

      SET_DIGIT(MP_LOCAL_REF(q),i - t - 1,(DIGIT(MP_LOCAL_REF(q),i - t - 1) - 1UL) & MP_MASK);
    }
  }

  /* now q is the quotient and x is the remainder 
   * [which we have to normalize] 
   */
  
  /* get sign before writing to c */
  SET_SIGN(MP_LOCAL_REF(x),USED(MP_LOCAL_REF(x)) == 0 ? MP_ZPOS : SIGN(a));

  // printf( "mp_div J\n" ) ;
  if (c != NULL) {
    mp_clamp (MP_LOCAL_REF(q));
    mp_local_assignfrom(c,q);
    SET_SIGN(c,neg);
  }

  if (d != NULL) {
    mp_div_2d (MP_LOCAL_REF(x), norm, MP_LOCAL_REF(x), NULL);
    mp_local_assignfrom(d,x);
  }

  res = MP_OKAY;

   // prLTM(a,"mp_div AFT a") ;
   // prLTM(b,"mp_div AFT b") ;
   // prLTM(c,"mp_div AFT c") ;
   // prLTM(d,"mp_div AFT d") ;
   // prLTM(MP_LOCAL_REF(q),"mp_div AFT q") ;
   // prLTM(MP_LOCAL_REF(x),"mp_div AFT x") ;

LBL_Y:mp_local_clear(y);
LBL_X:mp_local_clear(x);
LBL_T2:mp_local_clear(t2);
LBL_T1:mp_local_clear(t1);
LBL_Q:mp_local_clear(q);
  return res;
}

#endif

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_div.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
