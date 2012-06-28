
// interface to eval
function _e_(x) {
  var x_, xx, x_next;
  if (x !== undefined && x !== null && x.__eOrV__ !== undefined) {
    x_ = x;
    do {
      if (typeof x.__eOrV__ === 'function' && !x.fe) {
        xx = x.__eOrV__();
        x.__eOrV__ = xx;
        x = xx;
      } else {
        x = x.__eOrV__;
      }
    } while (x !== undefined && x !== null && x.__eOrV__ !== undefined);
    while (x_ !== undefined && x_ !== null && x_.__eOrV__ !== undefined) {
      x_next = x_.__eOrV__;
      x_.__eOrV__ = x;
      x_.fe = true;
      x_ = x_next;
    }
  }
  return x;
}

function _A_undersat_(fun, args) {
  // this.needs = fun.needs - args.length;
  this.fun = fun;
  this.args = args;
}

// Apply node, not enough args
_A_undersat_.prototype = {
  __aN__ : function (args) {
    var needs, fun;
    needs = this.needsNrArgs();
    if (args.length < needs) {
      return new _A_undersat_(this, args);
    } else if (args.length === needs) {
      return this.fun.__aN__(this.args.concat(args));
    } else {
      fun = _e_(this.__aN__(args.slice(0, needs)));
      return {
        __eOrV__ : function () {
          return fun.__aN__(args.slice(needs));
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.fun.needsNrArgs() - this.args.length;
  },
};

// Apply node, unknown how much is missing or too much
_A_.prototype = {
  __aN__ : function (args) {
    var fun = _e_(this);
    return {
      __eOrV__ : function () {
        return fun.__aN__(args);
      }
    };
  },
};
function _A_(fun, args) {
  this.__eOrV__ = function () {
    var x = fun.__aN__(args);
    return x;
  };
  this.fe = false;
}

function _F_(evalN) {
  //this.needs = evalN.length;
  this.__evN__ = evalN;
}

// Function node
_F_.prototype = {
  __aN__ : function (args) {
    var x, fun, remargs;
    if (args.length < this.__evN__.length) {
      return new _A_undersat_(this, args);
    } else if (args.length === this.__evN__.length) {
      x = this.__evN__.apply(null, args);
      return x;
    } else {
      fun = _e_(this.__evN__.apply(null, args.slice(0, this.__evN__.length)));
      remargs = args.slice(this.__evN__.length);
      return {
        __eOrV__ : function () {
          return fun.__aN__(remargs);
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.__evN__.length;
  },
}

// lazy application wrappers
function _a0_(f) {
  return new _A_(f, []);
}

// indirection
function _i_() {
  return new _A_(new _F_(
    function () {throw "_i_: attempt to prematurely evaluate indirection"; }
  ), []);
}

function _i_set_(i, x) {
  i.__eOrV__ = x;
}

if (typeof document !== 'object') {
  document = {
    write   : function (x) {return write(x); },
    writeln : function (x) {return writeln(x); }
  };
};

