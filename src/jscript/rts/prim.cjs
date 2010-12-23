Data constructors must be in lexicographical ordering, using _tag_ (field name must coincide with codegen)

%%[8
PrimDataOrdering_EQ = {_tag_ : 0}
PrimDataOrdering_GT = {_tag_ : 1}
PrimDataOrdering_LT = {_tag_ : 2}

PrimDataBool_False = {_tag_ : 0}
PrimDataBool_True  = {_tag_ : 1}

PrimDataList_Nil = {_tag_ : 1}
PrimDataList_Cons  = {_tag_ : 0}

PrimMkBool = function(x) { return ( (x) ? PrimDataBool_True : PrimDataBool_False ) ; }
%%]

%%[8
// signed, int
primAddInt = function(x,y) {return x+y ;}
primSubInt = function(x,y) {return x-y ;}
primMulInt = function(x,y) {return x*y ;}

// primDivInt = function(x,y) {var r = primQuotInt(x,y) ; return ( (r<0) ? r-1 : r ) ;}
primDivInt = function(x,y) {return Math.floor(x/y) ;}
primModInt = function(x,y) {var r = x%y ; return ( (r > 0 && y < 0 || r < 0 && y > 0) ? r+y : r ) ;}
primDivModInt = function(x,y) {return [primDivInt (x,y), primModInt(x,y)] ;}

// primQuotInt = function(x,y) {return Math.floor(x/y) ;}
primQuotInt = function(x,y) {var r = primDivInt(x,y) ; return ( (r<0) ? r+1 : r ) ;}
primRemInt = function(x,y) {return x%y ;}
primQuotRemInt = function(x,y) {return [primQuotInt(x,y), x%y] ;}

primNegInt = function(x) {return -x ;}
primComplementInt = function(x) {return ~x ;}

primShiftLeftInt  = function(x,y) {return x<<y ;}
primShiftRightInt = function(x,y) {return x>>y ;}

primRotateLeftInt  = function(x,y) {var s = (x<0 ? -1 : 1) ; x=x*s ; return s * ((x << y) | (x >> (31 - y))) ;}
primRotateRightInt = function(x,y) {var s = (x<0 ? -1 : 1) ; x=x*s ; return s * ((x >> y) | (x << (31 - y))) ;}

primEqInt = function(x,y) {return PrimMkBool(x==y) ;}
primNeInt = function(x,y) {return PrimMkBool(x!=y) ;}
primLtInt = function(x,y) {return PrimMkBool(x< y) ;}
primGtInt = function(x,y) {return PrimMkBool(x> y) ;}
primLeInt = function(x,y) {return PrimMkBool(x<=y) ;}
primGeInt = function(x,y) {return PrimMkBool(x>=y) ;}

primCmpInt = function(x,y) {return ( (x>y) ? PrimDataOrdering_GT : ( (x<y) ? PrimDataOrdering_LT : PrimDataOrdering_EQ ) ) ;}

/*
primMinInt = function() {return -(1<<31) ;}
primMaxInt = function() {return (1<<31)-1 ;}
*/
primMinInt = function() {return -(1<<30) ;}
primMaxInt = function() {return (1<<30)-1 ;}

primUnsafeId = function(x) { return x ;}

primIntegerToInt = function(x) { return x.intValue() ;}
primIntToInteger = function(x) { var r = nbi(); r.fromDouble(x); return r; }
// primIntToInteger = nbv ;

primAndInt = function(x,y) {return x&y ;}
primOrInt  = function(x,y) {return x|y ;}
primXorInt = function(x,y) {return x^y ;}

// Integer
primEqInteger = function(x,y) {return PrimMkBool(x.compareTo(y) == 0) ;}
primNeInteger = function(x,y) {return PrimMkBool(x.compareTo(y) != 0) ;}
primLtInteger = function(x,y) {return PrimMkBool(x.compareTo(y) <  0) ;}
primGtInteger = function(x,y) {return PrimMkBool(x.compareTo(y) >  0) ;}
primLeInteger = function(x,y) {return PrimMkBool(x.compareTo(y) <= 0) ;}
primGeInteger = function(x,y) {return PrimMkBool(x.compareTo(y) >= 0) ;}

primCmpInteger = function(x,y) {var c=x.compareTo(y) ; return ( (c>0) ? PrimDataOrdering_GT : ( (c<0) ? PrimDataOrdering_LT : PrimDataOrdering_EQ ) ) ;}
primQuotRemInteger = function(x,y) {var q = nbi() ; var r = nbi() ; x.divRemTo(y,q,r) ; return [q,r] ;}

primDivInteger = function(  v1,  v2 ) {
	var r = v1.divide(v2) ;
	if ( r.signum() < 0 )
		return r.subtract( BigInteger.ONE ) ;
	return r ;
}
primModInteger = function(  v1,  v2 ) {
	return ( v2.signum() < 0 ? v1.mod(v2.negate()).add(v2) : v1.mod(v2) ) ;
}
primDivModInteger = function(x,y) {return [primDivInteger (x,y), primModInteger(x,y)] ;}

primAndInteger = function(x,y) {return x.and(y) ;}
primOrInteger  = function(x,y) {return x.or(y) ;}
primXorInteger = function(x,y) {return x.xor(y) ;}

primComplementInteger = function(x) {return x.not() ;}

primShiftLeftInteger = function(x,y) {return x.shiftLeft(y) ;}
primShiftRightInteger = function(x,y) {return x.shiftRight(y) ;}

primRotateLeftWord  = function(x,y) {return (x << y) | (x >> (32 - y)) ;}
primRotateRightWord = function(x,y) {return (x >> y) | (x << (32 - y)) ;}

primComplementWord = primComplementInt ;

// unsigned specific
primMinWord = function() {return 0 ;}
primMaxWord = function() {return (1<<32)-1 ;}

primAndWord = primAndInt ;
primOrWord  = primOrInt  ;
primXorWord = primXorInt ;

primShiftLeftWord  = primShiftLeftInt  ;
primShiftRightWord = primShiftRightInt ;

/// TODO: sign
primIntegerToWord = primIntegerToInt ;

// float, double
primDivideDouble = function(x,y) { return x/y ; }
primRecipDouble = function(x) { return 1/x ; }
primRationalToDouble = function(x) { return _e_(x._1).doubleValue() / _e_(x._2).doubleValue() ; }

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

%%]

%%[8
primCharIsUpper = function(x) { return PrimMkBool(x > 64 && x < 91 ) ; }
primCharIsLower = function(x) { return PrimMkBool(x > 96 && x < 123) ; }
%%]

Represent packed strings as Javascript strings

%%[8
primPackedStringNull = function(x) { return PrimMkBool(x.length == 0) ; }
primPackedStringHead = function(x) { return x.charCodeAt(0) ; }
primPackedStringTail = function(x) { return x.slice(1) ; }
// primPackedStringToInteger = function(x) { return parseInt(x) ; }
primPackedStringToInteger = function(x) { return new BigInteger(x,10) ; }
primStringToPackedString = function(l) {
	var pos = 0 ;
	var a = new Array() ;
	while (l._tag_ != PrimDataList_Nil._tag_) {
		a[pos] = _e_(l._1) ;
		++pos ;
		l = _e_(l._2) ;
	}
	return String.fromCharCode.apply(null,a) ;
}
%%]

Array

%%[8
primNewArray = function(len,x) {
	var a = new Array() ;
	for (var i = 0 ; i < len ; i++ ) {
		a[i] = x ;
	}
	return a ;
}
primIndexArray = function(a,i) { return a[i] ; }
primWriteArray = function(a,i,x) { a[i] = x ; return [] ; }
primSameArray = function(x,y) { return PrimMkBool(x===y) ; }
%%]

Represent bytearrays as Javascript strings

%%[8
primByteArrayLength = function(x) { return x.length ; }
primByteArrayToPackedString = primUnsafeId ;
%%]

%%[8
primThrowException = function(x) { throw x ; }
primExitWith = function(x) { throw "EXIT:" + x ; }
%%]

%%[8
// primShowIntegerToPackedString = function(x) { return x.toString() ; }

primShowDoubleToPackedString = function(x) { return x.toString() ; }
primShowFloatToPackedString = primShowDoubleToPackedString ;

// TODO:
// primShowDoubleToPackedString = primShowIntegerToPackedString
// primShowFloatToPackedString  = primShowIntegerToPackedString
%%]

%%[8
// decode a double for a radix b, into (non fractional) Integer and exponent
function decodeFloat(d,b,logb) {
	var sign = 1 ;
	if ( d < 0 ) {
		sign = -1 ;
		d = -d ;
	}
	if ( d == 0 ) {
		return [primIntToInteger(d),0] ;
	}
	var m = Math.floor(d) ;
	var r = d - m ;
	var e = 0 ;
	if ( r > 0 ) {
		// scale up until no fractional part remains
		var d2 = d ;
		do {
			d = d * b ;
			e = e - logb ;
			m = Math.floor(d) ;
			r = d - m ;
		} while ( r > 0 ) ;
		// d = primIntToInteger(sign * d2).shiftLeft(logb).add( primIntToInteger(sign * r * b) ) ;
		d = primIntToInteger(d) ;
	} else {
		// scale down until a fractional part arises
		var d2, e2 ;
		do {
			d2 = d ;
			e2 = e ;
			d = d / b ;
			e = e + logb ;
			m = Math.floor(d) ;
			r = d - m ;
		} while ( r == 0 )
		d = primIntToInteger(d2) ;
		e = e2 ;
	}
	var shift = 53 - d.bitLength() ;
	if ( shift ) {
		d = d.shiftLeft(shift) ;
		e = e - shift ;
	}
	return [sign < 0 ? d.negate() : d, e] ;
}

primDecodeDouble        = function(d) { var x = decodeFloat(d,2,1) ; return x ; }
primEncodeDouble        = function(d,e) { return d.doubleValue() * Math.pow(2,e) ; }

primIsIEEE = function() { return PrimDataBool_True ; }
primRadixDoubleFloat = function() { return 2 ; }

primIsNaNDouble          = function(x) { return PrimMkBool(x==Number.NaN) ;}
primIsNegativeZeroDouble = function(x) { return PrimMkBool(x==-0.0) ;}
primIsDenormalizedDouble = function(x) { return PrimDataBool_False ; }
primIsInfiniteDouble     = function(x) { return PrimMkBool(x==Number.POSITIVE_INFINITY || x==Number.NEGATIVE_INFINITY) ;}
primDigitsDouble         = function() { return 53 ; }
primMaxExpDouble         = function() { return 1024 ; }
primMinExpDouble         = function() { return -1021 ; }

%%]

%%[8
_MutVar_id_ = 0 ;
_MutVar_.prototype = {
	// identity, a global variable for all MutVar's, used for checking identity equality because this is not offered by javascript
	_id_ : 0
}
function _MutVar_(a) {
	this._val_ = a ;
	this._id_ = ++_MutVar_id_ ;
	// this should be the _id_ of the proto, but I do something wrong:
	// this._id_ = ++this.prototype._id_ ;
}
primNewMutVar 	= function(a,s) 	{return [s,new _MutVar_(a)];}
primReadMutVar 	= function(m,s) 	{return [s,m._val_];}
primWriteMutVar = function(m,a,s) 	{m._val_ = a; return s;}
primSameMutVar 	= function(m1,m2) 	{return PrimMkBool(m1._id_ === m2._id_);}
%%]

%%[8
primHPutChar = function(h,c) {
 switch(c) {
  case 10 :
   document.writeln("") ;
   break ;
  default :
   document.write(String.fromCharCode(c)) ;
   break ;
 }
 return [] ;
}
%%]

