
%%[(8 jscript).debugStuff
var traceOn = false ;

function trace( m, s ) {
  if ( traceOn ) {
    console.log(m + ": " + s) ;
  }
}

var evalCounter = 0 ;
var nodeCounter = 0 ;
%%]

%%[(100 jscript) -8.debugStuff
%%]

%%[(8 jscript)
function evaluatable(x) {
  return x !== undefined && x !== null && x.__eOrV__ !== undefined;
}
%%]

// interface to eval
%%[(8 jscript)
function _e_( x ) {
%%[[8
  trace( "> _e_", x ) ;
%%][100
%%]]
  if (evaluatable(x)) {
    var x_ = x ;
    do {
      if ( typeof x.__eOrV__ == 'function' ) {
%%[[8
        trace( ">> _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x ) ;
%%][100
%%]]
        var xx = x.__eOrV__() ;
        x.__eOrV__ = xx ;
        x = xx ;
%%[[8
        if (evaluatable(x)) {
          trace( "<< _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x ) ;
        } else {
          trace( "<< _e_()", typeof x + ":" + x ) ;
        }
%%][100
%%]]
      } else {
%%[[8
        trace( ">> _e_", typeof x + "/" + typeof x.__eOrV__ + ":" + x ) ;
%%][100
%%]]
        x = x.__eOrV__ ;
%%[[8
        if (evaluatable(x)) {
          trace( "<< _e_()", typeof x + "/" + typeof x.__eOrV__ + ":" + x ) ;
        } else {
          trace( "<< _e_()", typeof x + ":" + x ) ;
        }
%%][100
%%]]
      }
    } while (evaluatable(x)) ;
    while (evaluatable(x_)) {
      var x_next = x_.__eOrV__ ;
      x_.__eOrV__ = x ;
      x_ = x_next ;
    }
  }
%%[[8
  ++evalCounter ;
  trace( "< _e_", x ) ;
%%][100
%%]]
  return x ;
}

// Apply node, not enough args
_A_undersat_.prototype = {
  __aN__ : function ( args ) {
    var needs = this.needsNrArgs() ;
    if ( args.length < needs ) {
      return new _A_undersat_( this, args ) ;
    } else if ( args.length == needs ) {
%%[[8
      trace("> _A_undersat_.__aN__(=sat)", this + "(|args#" + args.length + "=(|" + args + "|)|)") ;
%%][100
%%]]
      return this.fun.__aN__( this.args.concat( args ) ) ;
    } else {
%%[[8
      trace("> _A_undersat_.__aN__(>sat)", this + "(|args#" + args.length + "=(|" + args.slice( 0, needs ) + "|)+(|" + args.slice( needs ) + "|)|)") ;
%%][100
%%]]
      var fun = _e_( this.__aN__( args.slice( 0, needs ) ) ) ;
      return {
        __eOrV__ : function() {
          return fun.__aN__( args.slice( needs ) ) ;
        } } ;
    }
  } ,
  needsNrArgs : function() {
    return this.fun.needsNrArgs() - this.args.length ;
  } ,
%%[[8
  getName : function () {
    return "A-" + this.needsNrArgs() + "#" + this.nodeId + "'" ;
  } ,
  toString : function () {
    return "(" + this.getName() + "=" + this.fun + "@" + this.args + ")" ;
  }
%%][100
%%]]
}
function _A_undersat_( fun, args ) {
  // this.needs = fun.needs - args.length ;
  this.fun = fun ;
  this.args = args ;
%%[[8
  this.nodeId = ++nodeCounter ;
%%][100
%%]]
}

// Apply node, unknown how much is missing or too much
_A_.prototype = {
  __aN__ : function ( args ) {
    var fun = _e_(this) ;
    return {
      __eOrV__ : function() {
        return fun.__aN__( args ) ;
      } } ;
  } ,
%%[[8
  getName : function () {
    return "A" + this.args.length + "#" + this.nodeId + "'" + this.fun.getName() ;
  } ,
  getVal : function () {
    return "V#" + this.nodeId + "'" + this.__eOrV__ ;
  } ,
  toString : function () {
    if ( typeof this.__eOrV__ == 'function' ) {
      return "(" + this.getName() + "@args#" + this.args.length + "=(|" + this.args + "|))" ;
    } else {
      return "(" + this.getVal() + ")" ;
    }
  }
%%][100
%%]]
}
function _A_( fun, args ) {
  this.__eOrV__ = function() {
%%[[8
    trace("> _A_.__eOrV__", fun + "(|args#" + args.length + "=" + args + "|)") ;
%%][100
%%]]
    var x = fun.__aN__( args ) ;
%%[[8
    trace("< _A_.__eOrV__", fun + "(|args#" + args.length + "=" + args + "|)") ;
    trace("<   ->", this + " -> " + x) ;
%%][100
%%]]
    return x ;
  }
%%[[8
  this.fun = fun ;
  this.args = args ;
  this.nodeId = ++nodeCounter ;
%%][100
%%]]
}

// Function node
_F_.prototype = {
  __aN__ : function ( args ) {
    if ( args.length < this.needs ) {
      return new _A_undersat_( this, args ) ;
    } else if ( args.length == this.needs ) {
%%[[8
      trace( "> _F_.__aN__(=sat)", this + "(|args#" + args.length + "=" + args + "|)") ;
%%][100
%%]]
      var x = this.__evN__.apply( null, args ) ;
%%[[8
      trace( "< _F_.__aN__(=sat)", this + "(|args#" + args.length + "=" + args + "|)") ;
      trace( "<   ->", x) ;
%%][100
%%]]
      return x ;
    } else {
%%[[8
      trace( "> _F_.__aN__(>sat)", this + "(|needs#" + this.needs + "args#" + args.length + "=" + args + "|)") ;
%%][100
%%]]
      var fun = _e_( this.__evN__.apply( null, args.slice( 0, this.needs ) ) ) ;
      var remargs = args.slice( this.needs ) ;
%%[[8
      trace( "< _F_.__aN__(>sat)", fun + "(|needs#" + this.needs + "remargs#" + remargs.length + "=" + remargs + "|)") ;
      trace( "<   ->", fun) ;
%%][100
%%]]
      return {
        __eOrV__ : function() {
          return fun.__aN__( remargs ) ;
        } } ;
    }
  } ,
  needsNrArgs : function() {
    return this.needs ;
  } ,
%%[[8
  getName : function () {
    return "F" + this.needs + "#" + this.nodeId + "'" + this.name ;
  } ,
  toString : function () {
    return "(" + this.getName() + ")" ;
  }
%%][100
%%]]
}
%%[[8
function _F_( name, evalN ) {
%%][100
function _F_( evalN ) {
%%]]
  this.needs = evalN.length ;
  this.__evN__ = evalN ;
%%[[8
  this.name = name ;
  this.nodeId = ++nodeCounter ;
%%][100
%%]]
}
%%]

%%[8
%%]
// function construction wrappers
function _f_(f) {
  return new _F_(f) ;
}

%%[8
%%]
// strict application wrappers
function _e1_(f,a) {
  return _e_( f.__aN__([a]) ) ;
}

function _e2_(f,a,b) {
  return _e_( f.__aN__([a,b]) ) ;
}

function _e3_(f,a,b,c) {
  return _e_( f.__aN__([a,b,c]) ) ;
}

function _e4_(f,a,b,c,d) {
  return _e_( f.__aN__([a,b,c,d]) ) ;
}

function _e5_(f,a,b,c,d,e) {
  return _e_( f.__aN__([a,b,c,d,e]) ) ;
}

function _eN_(f,a) {
  return _e_( f.__aN__(a) ) ;
}

%%[8
// lazy application wrappers
function _a0_(f) {
  return new _A_(f,[]) ;
}
%%]

function _a1_(f,a) {
  return new _A_(f,[a]) ;
}

function _a2_(f,a,b) {
  return new _A_(f,[a,b]) ;
}

function _a3_(f,a,b,c) {
  return new _A_(f,[a,b,c]) ;
}

function _a4_(f,a,b,c,d) {
  return new _A_(f,[a,b,c,d]) ;
}

function _a5_(f,a,b,c,d,e) {
  return new _A_(f,[a,b,c,d,e]) ;
}

function _aN_(f,a) {
  return new _A_(f,a) ;
}

%%[8
// indirection
function _i_() {
  return new _A_(new _F_("_i_", function(){throw "_i_: attempt to prematurely evaluate indirection";}),[]) ; 
}

function _i_set_(i,x) {
  i.__eOrV__ = x ;
}
%%]

// setup
function init() {
}

%%[8
if ( typeof document != 'object' ) {
  document = {
    write   : function(x) {return write(x) ;},
    writeln : function(x) {return writeln(x) ;}
  }
}
%%]

function cons(x,y) { return [0,x,y]; }
function head(l) { return l[1]; }
function tail(l) { return l[2]; }
var nil = [1] ;
function isNil(x) { return x[0] == 1 ; }


function show( x ) {
  var x = _e_(x) ;
  document.write( ""+_e_(x) ) ;
}

function showList( l ) {
  var list = _e_(l) ;
  switch (list[0]) {
    case 0 :
      document.write( _e_(head(list)) + ":" ) ;
      showList( tail(list) ) ;
      break ;
    case 1 :
      document.write( "[]" ) ;
      break ;
  }
}

// test: sieve
function testSieve() {
  var id = _f_( function(a) {
    // trace( "id: " + a ) ;
    return a ;
  } ) ;
  var even = _f_( function(a) {
    // return __e__(a[0]) % 2 == 0 ;
    return _a2_( eq, _a2_( mod, a, 2 ), 0 ) ;
  } ) ;
  var eq = _f_( function(a,b) {
    return _e_(a) == _e_(b) ;
  } ) ;
  var ne = _f_( function(a,b) {
    return _e_(a) != _e_(b) ;
  } ) ;
  var add = _f_( function(a,b) {
    return _e_(a) + _e_(b) ;
  } ) ;
  var sub = _f_( function(a,b) {
    return _e_(a) - _e_(b) ;
  } ) ;
  var mul = _f_( function(a,b) {
    return _e_(a) * _e_(b) ;
  } ) ;
  var div = _f_( function(a,b) {
    return Math.floor ( _e_(a) / _e_(b) ) ;
  } ) ;
  var mod = _f_( function(a,b) {
    return ( _e_(a) % _e_(b) ) ;
  } ) ;
  var from = _f_( function(a) {
    return cons( a, _a1_( from, _a2_( add, a, 1 ) ) ) ;
  } ) ;
  var last = _f_( function(a) {
    var list = _e_(a) ;
    switch (list[0]) {
      case 0 :
        var list2 = _e_(tail(list)) ;
        switch (list2[0]) {
          case 0 :
            return _a1_( last, tail(list) ) ;
          case 1 :
            return head(list) ;
        }
      case 1 :
        return undefined ;
    }
  } ) ;
  var take = _f_( function(a,b) {
    var len  = _e_(a) ;
    var list = _e_(b) ;
    if ( len <= 0 || isNil(list) ) {
      return nil ;
    } else {
      return cons( head(list), _a2_( take, _a2_( sub, len, 1 ), tail(list) ) ) ;
    }
  } ) ;
  var filter = _f_( function(a,b) {
    var list = _e_(b) ;
    var test = _e1_( a, head(list) ) ;
    if ( test ) {
      return cons( head(list), _a2_( filter, a, tail(list) ) ) ;
    } else {
      return _a2_( filter, a, tail(list) ) ;
    }
  } ) ;
  var notMultiple = _f_( function(a,b) {
    return _a2_( ne, _a2_( mul, _a2_( div, b, a), a ), b ) ;
  } ) ;
  var notMultiple2 = _f_( function(a,b) {
    var x = _e_(a) ;
    var y = _e_(b) ;
    return (Math.floor(y / x) * x) != y ;
  } ) ;
  var sieve = _f_( function(a) {
    var list = _e_(a) ;
    return cons( head(list), _a1_( sieve, _a2_( filter, _a1_( notMultiple2, head(list) ), tail(list) ) ) ) ;
  } ) ;
  var sieve2 = _f_( function(nmz,a) {
    var list = _e_(a) ;
    return cons( head(list), _a2_( sieve2, _a1_( id, nmz ), _a2_( filter, _a1_( nmz, head(list) ), tail(list) ) ) ) ;
  } ) ;
  var mainSieve = _a2_( take, 1000, _a1_( sieve, _a1_( from, 2 ) ) ) ;
  var mainSieve2 = _a2_( take, 500, _a2_( sieve2, _a1_( id, notMultiple2 ), _a1_( from, 2 ) ) ) ;
  
  // running it...
  evalCounter = 0 ;
  var d = new Date() ;
  var t1 = d.getTime() ;
  // showList( mainSieve ) ;
  show( _a1_( last, mainSieve ) ) ;
  d = new Date() ;
  var t2 = d.getTime() - t1 ;
  document.write("<hr/>time= " + t2 + " ms" + ((evalCounter>0) ? ", nreval= " + evalCounter + ", ms/ev= " + (t2/evalCounter) : "") + "<br/>") ;
}

function testMisc()  {
  trace("load & init ok") ;
  var plus = _f_( function(a,b){return _e_(a)+_e_(b);}) ;
  trace("plus: " + plus) ;
  var inc1 = _f_( function(a){trace("inc: " + a) ; var x = _e_(a) ; return x+1;}) ;
  trace("inc1: " + inc1) ;
  var inc2 = plus.__aN__([10]) ;
  trace("inc2: " + inc2) ;
  var two1 = 2 ;
  // var two2 = new AppN_WHNF(2) ;
  var two3 = new _A_(new _F_(0,function(){return 2;}),[]) ;
  var arr = [two1] ;
  // trace("two2: " + two2) ;
  trace("two3: " + two3) ;
  trace("two3 eval: " + _e_(two3)) ;
  trace("two3: " + two3) ;
  trace("two3 eval: " + _e_(two3)) ;
  trace("arr: " + arr) ;
  var x1 = inc2.__aN__( arr ) ;
  trace("inc 2: " + x1) ;
  var x2 = new _A_( inc2, arr ) ;
  trace("inc del 2: " + x2) ;
  trace("inc del 2 eval: " + _e_(x2)) ;
}

function tryOut() {
  var f = function(a,b) {} ;
  var l = cons(1,nil) ;
  // trace(ToPropertyDescriptor(f)) ;
  // trace(ToPropertyDescriptor(Function)) ;
  // trace(ToPropertyDescriptor("a")) ;
  // trace(ToPropertyDescriptor(String)) ;
  trace("f "+f.length) ;
}

function main() {
  init() ;
  // testMisc() ;
  // tryOut() ;
  testSieve() ;
}
