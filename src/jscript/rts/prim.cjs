Data constructors must be in lexicographical ordering, using _tag_ (field name must coincide with codegen)

%%[[8
PrimDataOrdering_EQ = {_tag_ : 0}
PrimDataOrdering_GT = {_tag_ : 1}
PrimDataOrdering_LT = {_tag_ : 2}

PrimDataBool_False = {_tag_ : 0}
PrimDataBool_True  = {_tag_ : 1}

PrimMkBool = function(x) { return ( (x) ? PrimDataBool_True : PrimDataBool_False ) ; }
%%]]

%%[[8
// signed, int
primAddInt = function(x,y) {return x+y ;}
primSubInt = function(x,y) {return x-y ;}
primMulInt = function(x,y) {return x*y ;}

primDivInt = function(x,y) {var r = x/y ; return ( (r<0) ? r-1 : r ) ;}
primModInt = function(x,y) {var r = x%y ; return ( (r > 0 && y < 0 || r < 0 && y > 0) ? r+y : r ) ;}
primDivModInt = function(x,y) {return [primDivInt (x,y), primModInt(x,y)] ;}

primQuotInt = function(x,y) {return x/y ;}
primRemInt = function(x,y) {return x%y ;}
primQuotRemInt = function(x,y) {return [x/y, x%y] ;}

primNegInt = function(x) {return -x ;}
primComplementInt = function(x) {return ~x ;}

primShiftLeftInt = function(x,y) {return x<<y ;}
primShiftRightInt = function(x,y) {return x>>y ;}

primEqInt = function(x,y) {return PrimMkBool(x==y) ;}
primNeInt = function(x,y) {return PrimMkBool(x!=y) ;}
primLtInt = function(x,y) {return PrimMkBool(x< y) ;}
primGtInt = function(x,y) {return PrimMkBool(x> y) ;}
primLeInt = function(x,y) {return PrimMkBool(x<=y) ;}
primGeInt = function(x,y) {return PrimMkBool(x>=y) ;}

primCmpInt = function(x,y) {return ( (x>y) ? PrimDataOrdering_GT : ( (x<y) ? PrimDataOrdering_LT : PrimDataOrdering_EQ ) ) ;}

primMinInt = function() {return -(1<<31) ;}
primMaxInt = function() {return (1<<31)-1 ;}

primUnsafeId = function(x) { return x ;}

primIntegerToInt = function(x) { return ToInt32(x) ;}
primIntToInteger = primUnsafeId ;

primAndInt = function(x,y) {return x&y ;}
primOrInt  = function(x,y) {return x|y ;}
primXorInt = function(x,y) {return x^y ;}

// unsigned specific
primMinWord = function() {return 0 ;}
primMaxWord = function() {return (1<<32)-1 ;}

primIntegerToWord = function(x) { return ToUint32(x) ;}

// float, double
primRecipDouble = function(x) { return 1/x ; }
primRationalToDouble = function(x) { return x._1 / x._2 ; }

primSinDouble  = function(x) { return Math.sin(x) ; }
primCosDouble  = function(x) { return Math.cos(x) ; }
primTanDouble  = function(x) { return Math.tan(x) ; }
primAsinDouble = function(x) { return Math.asin(x) ; }
primAcosDouble = function(x) { return Math.acos(x) ; }
primAtanDouble = function(x) { return Math.atan(x) ; }
primExpDouble  = function(x) { return Math.exp(x) ; }
primLogDouble  = function(x) { return Math.log(x) ; }
primSqrtDouble = function(x) { return Math.sqrt(x) ; }
primSinhDouble = function(x) { return (Math.exp(x) - Math.exp(-x))/2 ; }
primCoshDouble = function(x) { return (Math.exp(x) + Math.exp(-x))/2 ; }
primTanhDouble = function(x) { return primSinhDouble(x) / primCoshDouble(x) ; }

primAtan2Double = function(x,y) { return Math.atan2(x,y) ; }

primIsIEEE = function() { return PrimDataBool_True ; }
primRadixDoubleFloat = function() { return 2 ; }

primIsNaNDouble          = function(x) { return PrimMkBool(x==Number.NaN) ;}
primIsNegativeZeroDouble = function(x) { return PrimMkBool(x==-0.0) ;}
primIsDenormalizedDouble = function(x) { return PrimDataBool_False ; }
primIsInfiniteDouble     = function(x) { return PrimMkBool(x==Number.POSITIVE_INFINITY || x==Number.NEGATIVE_INFINITY) ;}
primDigitsDouble         = function() { return 53 ; }
primMaxExpDouble         = function() { return 1024 ; }
primMinExpDouble         = function() { return -1021 ; }
// primDecodeDouble        
// primEncodeDouble        

%%]]

%%[[8
primCharIsUpper = function(x) { return x > 64 && x < 91 ; }
primCharIsLower = function(x) { return x > 96 && x < 123 ; }
%%]]

Represent packed strings as Javascript strings

%%[[8
primPackedStringNull = function(x) { return PrimMkBool(x.length == 0) ; }
primPackedStringHead = function(x) { return x.charCodeAt(0) ; }
primPackedStringTail = function(x) { return x.slice(1) ; }
primPackedStringToInteger = function(x) { return parseInt(x) ; }
%%]]

Represent bytearrays as Javascript strings

%%[[8
primByteArrayLength = function(x) { return x.length ; }
primByteArrayToPackedString = primUnsafeId ;
%%]]

%%[[8
primThrowException = function(x) { throw x ; }
primExitWith = function(x) { throw "EXIT:" + x ; }
%%]]

%%[[8
primShowInteger = function(x) { return x.toString() ; }
primShowDouble = primShowInteger
primShowFloat  = primShowInteger
%%]]

%%[[8
primHPutChar = function(h,c) {
 switch(c) {
  case 10 :
   document.write("<br/>") ;
   break ;
  default :
   document.write(String.fromCharCode(c)) ;
   break ;
 }
 return [] ;
}
%%]

