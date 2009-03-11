{- --------------------------------------------------------------------------
// Macros to help make various instances for Int like types (Int8, ...).
//
// --------------------------------------------------------------------------
-}

-- define conversion primitives
#define PRIMS_CONVERSION_INTEGER(tycon,primIntegerTo,primToInteger) \
foreign import prim primIntegerTo :: Integer -> tycon ; \
foreign import prim primToInteger :: tycon -> Integer 

#define PRIMS2_CONVERSION_INTEGER(tycon,primIntegerTo,primIntegerToNm,primToInteger,primToIntegerNm) \
foreign import prim primIntegerToNm primIntegerTo :: Integer -> tycon ; \
foreign import prim primToIntegerNm primToInteger :: tycon -> Integer 

#define PRIMS_CONVERSION_INT(tycon,primIntTo,primToInt) \
foreign import prim primIntTo     :: Int -> tycon ; \
foreign import prim primToInt     :: tycon -> Int 


-- define Eq primitives
#define PRIMS_EQ(tycon,primEq) \
foreign import prim primEq      :: tycon -> tycon -> Bool

#define PRIMS2_EQ(tycon,primEq,primEqNm) \
foreign import prim primEqNm primEq     :: tycon -> tycon -> Bool


-- define Eq instance
#define INSTANCE_EQ(tycon,primEq) \
instance Eq tycon where \
  { (==) = primEq \
  }


-- define Ord primitives
#define PRIMS_ORD1(tycon,primCmp) \
foreign import prim primCmp     :: tycon -> tycon -> Ordering 

#define PRIMS2_ORD1(tycon,primCmp,primCmpNm) \
foreign import prim primCmpNm primCmp     :: tycon -> tycon -> Ordering 

#define PRIMS_ORD2(tycon,primCmp,primLt,primGt) \
PRIMS_ORD1(tycon,primCmp) ; \
foreign import prim primLt      :: tycon -> tycon -> Bool ; \
foreign import prim primGt      :: tycon -> tycon -> Bool 

#define PRIMS2_ORD2(tycon,primCmp,primCmpNm,primLt,primLtNm,primGt,primGtNm) \
PRIMS2_ORD1(tycon,primCmp,primCmpNm) ; \
foreign import prim primLtNm primLt      :: tycon -> tycon -> Bool ; \
foreign import prim primGtNm primGt      :: tycon -> tycon -> Bool 


-- define Ord instance
#define INSTANCE_ORD1(tycon,primCmp) \
instance Ord tycon where \
  { compare = primCmp \
  }

#define INSTANCE_ORD2(tycon,primCmp,primLt,primGt) \
instance Ord tycon where \
  { compare = primCmp \
  ; (<) = primLt \
  ; (>) = primGt \
  }


-- define Bounded primitives
#define PRIMS_BOUNDED(tycon,primMin,primMax) \
foreign import prim primMin     :: tycon ; \
foreign import prim primMax     :: tycon 


-- define Bounded instance
#define INSTANCE_BOUNDED(tycon,primMin,primMax) \
instance Bounded tycon where \
  { minBound = primMin \
  ; maxBound = primMax \
  }


-- define Num primitives
#define PRIMS_NUM(tycon,primAdd,primSub,primMul,primNeg) \
foreign import prim primAdd       :: tycon -> tycon -> tycon ; \
foreign import prim primSub       :: tycon -> tycon -> tycon ; \
foreign import prim primMul       :: tycon -> tycon -> tycon ; \
foreign import prim primNeg       :: tycon -> tycon 

#define PRIMS2_NUM(tycon,primAdd,primAddNm,primSub,primSubNm,primMul,primMulNm,primNeg,primNegNm) \
foreign import prim primAddNm 			primAdd       :: tycon -> tycon -> tycon ; \
foreign import prim primSubNm 			primSub       :: tycon -> tycon -> tycon ; \
foreign import prim primMulNm 			primMul       :: tycon -> tycon -> tycon ; \
foreign import prim primNegNm 			primNeg       :: tycon -> tycon 


-- define Num instance
#define INSTANCE_NUM(tycon,primAdd,primSub,primMul,primNeg,primIntegerTo,primIntTo) \
instance Num tycon where \
  { (+)         = primAdd \
  ; (-)         = primSub \
  ; (*)         = primMul \
  ; negate      = primNeg \
  ; fromInteger = primIntegerTo \
  ; fromInt     = primIntTo \
  ; abs         = absReal \
  ; signum      = signumReal \
  }


-- define Enum instance
#define INSTANCE_ENUM(tycon,primToEnum,primFromEnum) \
instance Enum tycon where \
  { succ           = boundedSucc \
  ; pred           = boundedPred \
  ; toEnum         = primToEnum \
  ; fromEnum       = primFromEnum \
  ; enumFrom       = boundedEnumFrom \
  ; enumFromTo     = boundedEnumFromTo \
  ; enumFromThen   = boundedEnumFromThen \
  ; enumFromThenTo = boundedEnumFromThenTo \
  }


-- define Real instance
#define INSTANCE_REAL(tycon) \
instance Real tycon where \
  { toRational x = toInteger x % 1 \
  }


-- define Integral primitives
#define PRIMS_INTEGRAL1(tycon,primDiv,primMod,primQuot,primRem) \
foreign import prim primDiv       :: tycon -> tycon -> tycon ; \
foreign import prim primMod       :: tycon -> tycon -> tycon ; \
foreign import prim primQuot      :: tycon -> tycon -> tycon ; \
foreign import prim primRem       :: tycon -> tycon -> tycon 

#define PRIMS2_INTEGRAL1(tycon,primDiv,primDivNm,primMod,primModNm,primQuot,primQuotNm,primRem,primRemNm) \
foreign import prim primDivNm 			primDiv       :: tycon -> tycon -> tycon ; \
foreign import prim primModNm 			primMod       :: tycon -> tycon -> tycon ; \
foreign import prim primQuotNm 			primQuot      :: tycon -> tycon -> tycon ; \
foreign import prim primRemNm 			primRem       :: tycon -> tycon -> tycon 

#define PRIMS_INTEGRAL2(tycon,primDiv,primMod,primDivMod,primQuot,primRem,primQuotRem) \
PRIMS_INTEGRAL1(tycon,primDiv,primMod,primQuot,primRem) ; \
foreign import prim primDivMod    :: tycon -> tycon -> (tycon,tycon) ; \
foreign import prim primQuotRem   :: tycon -> tycon -> (tycon,tycon) 

#define PRIMS2_INTEGRAL2(tycon,primDiv,primDivNm,primMod,primModNm,primDivMod,primDivModNm,primQuot,primQuotNm,primRem,primRemNm,primQuotRem,primQuotRemNm) \
PRIMS2_INTEGRAL1(tycon,primDiv,primDivNm,primMod,primModNm,primQuot,primQuotNm,primRem,primQuotRemNm) ; \
foreign import prim primDivModNm 	primDivMod    :: tycon -> tycon -> (tycon,tycon) ; \
foreign import prim primQuotRemNm 	primQuotRem   :: tycon -> tycon -> (tycon,tycon) 


-- define Integral instance, assume default for divMod
#define INSTANCE_INTEGRAL1(tycon,primDiv,primMod,primQuot,primRem,primToInteger,primToInt) \
instance Integral tycon where \
  { quotRem d m \
              = (primQuot d m, primRem d m) \
  ; div       = primDiv \
  ; quot      = primQuot \
  ; rem       = primRem \
  ; mod       = primMod \
  ; toInteger = primToInteger \
  ; toInt     = primToInt \
  }

#define INSTANCE_INTEGRAL2(tycon,primDiv,primMod,primDivMod,primQuot,primRem,primQuotRem,primToInteger,primToInt) \
instance Integral tycon where \
  { divMod    = primDivMod \
  ; quotRem   = primQuotRem \
  ; div       = primDiv \
  ; quot      = primQuot \
  ; rem       = primRem \
  ; mod       = primMod \
  ; toInteger = primToInteger \
  ; toInt     = primToInt \
  }


-- define Show instance
#define INSTANCE_SHOW(tycon) \
instance Show tycon where \
  { show = showIntegral \
  }


