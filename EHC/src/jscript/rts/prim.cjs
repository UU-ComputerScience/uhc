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

%%[8
// Primitive functions for dealing with JS objects

// primMkCtor :: String -> IO (JSFunPtr c)
primMkCtor = function(nm) {
  if (typeof(window[nm]) !== 'function') {
    primSetCtor(nm, new Function());
  }
  return window[nm];
}

// primMkAnonObj :: IO (JSPtr c)
primMkAnonObj = function() { return {} }

// primMkObj :: JSString -> IO (JSPtr c)
primMkObj     = function(nm) { return new primGetCtor(nm); }

// Alias to primMkCtor
primGetCtor   = primMkCtor;

// primSetCtor :: JSString -> JSFunPtr c -> IO ()
primSetCtor   = function(nm, fn) { window[nm] = fn; }

// primGetAttr :: JSString -> JSPtr c -> a
primGetAttr   = function(attr, obj) { return obj[attr]; }

// primSetAttr :: JSString -> a -> JSPtr c -> IO (JSPtr c)
primSetAttr   = function(attr, val, obj) { obj[attr] = val; return obj; }

// primPureSetAttr :: JSString -> a -> JSPtr c -> JSPtr c
primPureSetAttr = function(attr, val, obj) {
  var clone = primClone(obj);
  primSetAttr(attr, val, clone);
  return clone;
}

// primModAttr :: JSString -> (a -> b) -> JSPtr c -> IO (JSPtr c)
primModAttr   = function (attr, f, obj) {
  primSetAttr(attr, _e_(new _A_(f, [primGetAttr(attr, obj)])), obj);
  return obj;
}

// primPureModAttr :: JSString -> (a -> b) -> JSPtr c -> JSPtr c
primPureModAttr   = function (attr, f, obj) {
  var clone = primClone(obj);
  primModAttr(attr, f, clone);
  return clone;
}


// primGetProtoAttr :: JSString -> JSString -> IO a
primGetProtoAttr = function(attr, cls) {
  primMkCtor(cls);
  return window[cls].prototype[attr];
}

// primSetProtoAttr :: JSString -> a -> JSString -> IO ()
primSetProtoAttr = function(attr, val, cls) {
  primMkCtor(cls);
  window[cls].prototype[attr] = val;
}

// primModProtoAttr :: JSString -> (a -> b) -> JSString -> IO ()
primModProtoAttr = function(attr, f, cls) {
  primSetProtoAttr(attr, _e_(new _A_(f, [primGetProtoAttr(attr, cls)])), cls);
}

// Object cloning facilities

// Clones a JS object
// primClone :: JSPtr a -> JSPtr a
primClone = function(obj) {
  var cloneAlg = function(name, target, copy) {
    target[ name ] = copy;
  };
  return foldObj(cloneAlg, {}, obj);
}

// Converts a UHC JS datatype object to a plain JS object
// primToPlainObj :: JSPtr a -> JSPtr b
primToPlainObj = function ( obj ) {
  var toPlainAlg = function(name, target, copy) {
    if (name != "_tag_") {
      target[name] = _e_(copy);
    }
  };
  return foldObj(toPlainAlg, {}, obj);
};

foldObj = function (alg, target, original ) {
  var name, src, copy, copyIsArray, clone;

  // Extend the base object
  for ( name in original ) {
    src = target[ name ];
    copy = original[ name ];

    // Prevent never-ending loop
    if ( target === copy ) {
      continue;
    }

    // Recurse if we're merging plain objects or arrays
    if ( copy && ( isPlainObject(copy) || (copyIsArray = isArray(copy)) ) ) {
      if ( copyIsArray ) {
        copyIsArray = false;
        clone = src && isArray(src) ? src : [];
      } else {
        clone = src && isPlainObject(src) ? src : {};
      }

      // Never move original objects, clone them
      target[ name ] = foldObj(alg, clone, copy );

    // Don't bring in undefined values
    } else if ( copy !== undefined ) {
      alg(name, target, copy);
    }
  }

  // Return the modified object
  return target;
};

type = function( obj ) {
  return obj == null ? String( obj ) : "object";
};

isArray = Array.isArray || function( obj ) {
  return type(obj) === "array";
};

isWindow = function( obj ) {
  return obj && typeof obj === "object" && "setInterval" in obj;
};

isPlainObject = function( obj ) {
  // Must be an Object.
  // Because of IE, we also have to check the presence of the constructor property.
  // Make sure that DOM nodes and window objects don't pass through, as well
  if ( !obj || type(obj) !== "object" || obj.nodeType || isWindow( obj ) ) {
    return false;
  }

  try {
    // Not own constructor property must be Object
    if ( obj.constructor &&
      !hasOwn.call(obj, "constructor") &&
      !hasOwn.call(obj.constructor.prototype, "isPrototypeOf") ) {
      return false;
    }
  } catch ( e ) {
    // IE8,9 Will throw exceptions on certain host objects #9897
    return false;
  }

  // Own properties are enumerated firstly, so to speed up,
  // if last one is own, then all properties are own.

  var key;
  for ( key in obj ) {}

  return key === undefined || hasOwn.call( obj, key );
}

%%]
