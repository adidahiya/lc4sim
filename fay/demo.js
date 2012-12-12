/** @constructor
*/
var Main = function(){
var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
  while (thunkish instanceof $) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof $? _(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function $(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

function Fay$$seq(x) {
  return function(y) {
    _(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  _(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new $(function(){
      var monad = _(m,true);
      return f(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
    return new $(function(){
      var monad = _(m,true);
      return f(monad.value);
    });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new $(function(){
      _(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  switch(base){
    case "action": {
      // A nullary monadic action. Should become a nullary JS function.
      // Fay () -> function(){ return ... }
      jsObj = function(){
        return Fay$$fayToJs(args[0],_(fayObj,true).value);
      };
      break;
    }
    case "function": {
      // A proper function.
      jsObj = function(){
        var fayFunc = fayObj;
        var return_type = args[args.length-1];
        var len = args.length;
        // If some arguments.
        if (len > 1) {
          // Apply to all the arguments.
          fayFunc = _(fayFunc,true);
          // TODO: Perhaps we should throw an error when JS
          // passes more arguments than Haskell accepts.
          for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
            // Unserialize the JS values to Fay for the Fay callback.
            fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
          }
          // Finally, serialize the Fay return value back to JS.
          var return_base = return_type[0];
          var return_args = return_type[1];
          // If it's a monadic return value, get the value instead.
          if(return_base == "action") {
            return Fay$$fayToJs(return_args[0],fayFunc.value);
          }
          // Otherwise just serialize the value direct.
          else {
            return Fay$$fayToJs(return_type,fayFunc);
          }
        } else {
          throw new Error("Nullary function?");
        }
      };
      break;
    }
    case "string": {
      // Serialize Fay string to JavaScript string.
      var str = "";
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        str += fayObj.car;
        fayObj = _(fayObj.cdr);
      }
      jsObj = str;
      break;
    }
    case "list": {
      // Serialize Fay list to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[0],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "tuple": {
      // Serialize Fay tuple to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      var i = 0;
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[i++],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "defined": {
      fayObj = _(fayObj);
      if (fayObj instanceof $_Language$Fay$Stdlib$Undefined) {
        jsObj = undefined;
      } else {
        jsObj = Fay$$fayToJs(args[0],fayObj["slot1"]);
      }
      break;
    }
    case "double": {
      // Serialize double, just force the argument. Doubles are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "int": {
      // Serialize int, just force the argument. Ints are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "bool": {
      // Bools are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "unknown":
    case "user": {
      if(fayObj instanceof $)
        fayObj = _(fayObj);
      jsObj = Fay$$fayToJsUserDefined(type,fayObj);
      break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  switch(base){
    case "action": {
      // Unserialize a "monadic" JavaScript return value into a monadic value.
      fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
      break;
    }
    case "string": {
      // Unserialize a JS string into Fay list (String).
      fayObj = Fay$$list(jsObj);
      break;
    }
    case "list": {
      // Unserialize a JS array into a Fay list ([a]).
      var serializedList = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedList);
      break;
    }
    case "tuple": {
      // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
      var serializedTuple = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedTuple);
      break;
    }
    case "defined": {
      if (jsObj === undefined) {
        fayObj = new $_Language$Fay$Stdlib$Undefined();
      } else {
        fayObj = new $_Language$Fay$Stdlib$Defined(Fay$$jsToFay(args[0],jsObj));
      }
      break;
    }
    case "double": {
      // Doubles are unboxed, so there's nothing to do.
      fayObj = jsObj;
      break;
    }
    case "int": {
      // Int are unboxed, so there's no forcing to do.
      // But we can do validation that the int has no decimal places.
      // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
      fayObj = Math.round(jsObj);
      if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
      break;
    }
    case "bool": {
      // Bools are unboxed.
      fayObj = jsObj;
      break;
    }
    case "unknown":
    case "user": {
      if (jsObj && jsObj['instance']) {
        fayObj = Fay$$jsToFayUserDefined(type,jsObj);
      }
      else
        fayObj = jsObj;
      break;
    }
  default: throw new Error("Unhandled JS->Fay translation type: " + base);
  }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
function Fay$$index(index){
  return function(list){
    for(var i = 0; i < index; i++) {
      list = _(list).cdr;
    }
    return list.car;
  };
}

// List length.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = _(list).cdr;
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new $(function(){
      return _(x) * _(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

    return new $(function(){
      return _(x) * _(y);
    });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new $(function(){
      return _(x) + _(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

    return new $(function(){
      return _(x) + _(y);
    });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new $(function(){
      return _(x) - _(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

    return new $(function(){
      return _(x) - _(y);
    });

}

// Built-in /.
function Fay$$div(x){
  return function(y){
    return new $(function(){
      return _(x) / _(y);
    });
  };
}

// Built-in /.
function Fay$$div$36$uncurried(x,y){

    return new $(function(){
      return _(x) / _(y);
    });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = _(lit1);
  lit2 = _(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new $(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

    return new $(function(){
      return Fay$$equal(x,y);
    });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new $(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

    return new $(function(){
      return !(Fay$$equal(x,y));
    });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new $(function(){
      return _(x) > _(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

    return new $(function(){
      return _(x) > _(y);
    });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new $(function(){
      return _(x) < _(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

    return new $(function(){
      return _(x) < _(y);
    });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new $(function(){
      return _(x) >= _(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

    return new $(function(){
      return _(x) >= _(y);
    });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new $(function(){
      return _(x) <= _(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

    return new $(function(){
      return _(x) <= _(y);
    });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new $(function(){
      return _(x) && _(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

    return new $(function(){
      return _(x) && _(y);
    });
 ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new $(function(){
      return _(x) || _(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

    return new $(function(){
      return _(x) || _(y);
    });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$Stdlib$error = function($p1){
  return new $(function(){
    var str = $p1;
    return (function($tmp1){
      if (_($tmp1) === 0) {
        return __(Language$Fay$Stdlib$error,str);
      }
      return __(Language$Fay$Stdlib$error,str);
    })(__(Language$Fay$Stdlib$error$39$,str));
  });
};
var Language$Fay$Stdlib$error$39$ = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["int"],(function() { throw Fay$$fayToJs(["string"],$p1) })());
  });
};
var Language$Fay$Stdlib$$_undefined = new $(function(){
  return __(Language$Fay$Stdlib$error,Fay$$list("Prelude.undefined"));
});
var Language$Fay$Stdlib$show = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["unknown"],$p1)));
  });
};
var $_Language$Fay$Stdlib$Undefined = function(){
};
var Language$Fay$Stdlib$Undefined = new $(function(){
  return new $_Language$Fay$Stdlib$Undefined();
});
var $_Language$Fay$Stdlib$Defined = function(slot1){
  this.slot1 = slot1;
};
var Language$Fay$Stdlib$Defined = function(slot1){
  return new $(function(){
    return new $_Language$Fay$Stdlib$Defined(slot1);
  });
};
var $_Language$Fay$Stdlib$Left = function(slot1){
  this.slot1 = slot1;
};
var Language$Fay$Stdlib$Left = function(slot1){
  return new $(function(){
    return new $_Language$Fay$Stdlib$Left(slot1);
  });
};
var $_Language$Fay$Stdlib$Right = function(slot1){
  this.slot1 = slot1;
};
var Language$Fay$Stdlib$Right = function(slot1){
  return new $(function(){
    return new $_Language$Fay$Stdlib$Right(slot1);
  });
};
var Language$Fay$Stdlib$either = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) instanceof $_Language$Fay$Stdlib$Left) {
          var a = _($p3).slot1;
          var f = $p1;
          return __(f,a);
        }
        if (_($p3) instanceof $_Language$Fay$Stdlib$Right) {
          var b = _($p3).slot1;
          var g = $p2;
          return __(g,b);
        }
        throw ["unhandled case in either",[$p1,$p2,$p3]];
      });
    };
  };
};
var Language$Fay$Stdlib$fromInteger = function($p1){
  return new $(function(){
    var x = $p1;
    return x;
  });
};
var Language$Fay$Stdlib$fromRational = function($p1){
  return new $(function(){
    var x = $p1;
    return x;
  });
};
var Language$Fay$Stdlib$negate = function($p1){
  return new $(function(){
    var x = $p1;
    return (-(_(x)));
  });
};
var Language$Fay$Stdlib$abs = function($p1){
  return new $(function(){
    var x = $p1;
    return _(_(Fay$$lt)(_(x))(0)) ? __(Language$Fay$Stdlib$negate,x) : x;
  });
};
var Language$Fay$Stdlib$signum = function($p1){
  return new $(function(){
    var x = $p1;
    return _(_(Fay$$gt)(_(x))(0)) ? 1 : _(__(Fay$$eq,x,0)) ? 0 : (-(1));
  });
};
var Language$Fay$Stdlib$pi = new $(function(){
  return Fay$$jsToFay(["double"],Math.PI);
});
var Language$Fay$Stdlib$exp = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.exp(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$sqrt = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.sqrt(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$log = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.log(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$$42$$42$ = new $(function(){
  return Language$Fay$Stdlib$unsafePow;
});
var Language$Fay$Stdlib$$94$$94$ = new $(function(){
  return Language$Fay$Stdlib$unsafePow;
});
var Language$Fay$Stdlib$unsafePow = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Language$Fay$Stdlib$$94$ = function($p1){
  return function($p2){
    return new $(function(){
      var b = $p2;
      var a = $p1;
      if (_(_(Fay$$lt)(_(b))(0))) {
        return __(Language$Fay$Stdlib$error,Fay$$list("(^): negative exponent"));
      } else {if (_(__(Fay$$eq,b,0))) {
          return 1;
        } else {if (_(__(Language$Fay$Stdlib$even,b))) {
            return (function(){
              var x = new $(function(){
                return __(Language$Fay$Stdlib$$94$,a,__(Language$Fay$Stdlib$quot,b,2));
              });
              return _(Fay$$mult)(_(x))(_(x));
            })();
          }
        }
      }
      var b = $p2;
      var a = $p1;
      return _(Fay$$mult)(_(a))(_(__(Language$Fay$Stdlib$$94$,a,_(Fay$$sub)(_(b))(1))));
    });
  };
};
var Language$Fay$Stdlib$logBase = function($p1){
  return function($p2){
    return new $(function(){
      var x = $p2;
      var b = $p1;
      return _(Fay$$div)(_(__(Language$Fay$Stdlib$log,x)))(_(__(Language$Fay$Stdlib$log,b)));
    });
  };
};
var Language$Fay$Stdlib$sin = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.sin(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$tan = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.tan(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$cos = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.cos(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$asin = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.asin(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$atan = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.atan(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$acos = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Math.acos(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$sinh = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$div)(_(_(Fay$$sub)(_(__(Language$Fay$Stdlib$exp,x)))(_(__(Language$Fay$Stdlib$exp,(-(_(x))))))))(2);
  });
};
var Language$Fay$Stdlib$tanh = function($p1){
  return new $(function(){
    var x = $p1;
    return (function(){
      var a = new $(function(){
        return __(Language$Fay$Stdlib$exp,x);
      });
      var b = new $(function(){
        return __(Language$Fay$Stdlib$exp,(-(_(x))));
      });
      return _(Fay$$div)(_(_(Fay$$sub)(_(a))(_(b))))(_(_(Fay$$add)(_(a))(_(b))));
    })();
  });
};
var Language$Fay$Stdlib$cosh = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$div)(_(_(Fay$$add)(_(__(Language$Fay$Stdlib$exp,x)))(_(__(Language$Fay$Stdlib$exp,(-(_(x))))))))(2);
  });
};
var Language$Fay$Stdlib$asinh = function($p1){
  return new $(function(){
    var x = $p1;
    return __(Language$Fay$Stdlib$log,_(Fay$$add)(_(x))(_(__(Language$Fay$Stdlib$sqrt,_(Fay$$add)(_(__(Language$Fay$Stdlib$$42$$42$,x,2)))(1)))));
  });
};
var Language$Fay$Stdlib$atanh = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$div)(_(__(Language$Fay$Stdlib$log,_(Fay$$div)(_(_(Fay$$add)(1)(_(x))))(_(_(Fay$$sub)(1)(_(x)))))))(2);
  });
};
var Language$Fay$Stdlib$acosh = function($p1){
  return new $(function(){
    var x = $p1;
    return __(Language$Fay$Stdlib$log,_(Fay$$add)(_(x))(_(__(Language$Fay$Stdlib$sqrt,_(Fay$$sub)(_(__(Language$Fay$Stdlib$$42$$42$,x,2)))(1)))));
  });
};
var Language$Fay$Stdlib$properFraction = function($p1){
  return new $(function(){
    var x = $p1;
    return (function(){
      var a = new $(function(){
        return __(Language$Fay$Stdlib$truncate,x);
      });
      return Fay$$list([a,_(Fay$$sub)(_(x))(_(__(Language$Fay$Stdlib$fromIntegral,a)))]);
    })();
  });
};
var Language$Fay$Stdlib$truncate = function($p1){
  return new $(function(){
    var x = $p1;
    return _(_(Fay$$lt)(_(x))(0)) ? __(Language$Fay$Stdlib$ceiling,x) : __(Language$Fay$Stdlib$floor,x);
  });
};
var Language$Fay$Stdlib$round = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["int"],Math.round(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$ceiling = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["int"],Math.ceil(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$floor = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["int"],Math.floor(Fay$$fayToJs(["double"],$p1)));
  });
};
var Language$Fay$Stdlib$subtract = new $(function(){
  return __(Language$Fay$Stdlib$flip,Fay$$sub);
});
var Language$Fay$Stdlib$even = function($p1){
  return new $(function(){
    var x = $p1;
    return __(Fay$$eq,__(Language$Fay$Stdlib$rem,x,2),0);
  });
};
var Language$Fay$Stdlib$odd = function($p1){
  return new $(function(){
    var x = $p1;
    return __(Language$Fay$Stdlib$not,__(Language$Fay$Stdlib$even,x));
  });
};
var Language$Fay$Stdlib$gcd = function($p1){
  return function($p2){
    return new $(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new $(function(){
              if (_($p2) === 0) {
                var x = $p1;
                return x;
              }
              var y = $p2;
              var x = $p1;
              return __(go,y,__(Language$Fay$Stdlib$rem,x,y));
            });
          };
        };
        return __(go,__(Language$Fay$Stdlib$abs,a),__(Language$Fay$Stdlib$abs,b));
      })();
    });
  };
};
var Language$Fay$Stdlib$lcm = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === 0) {
        return 0;
      }
      if (_($p1) === 0) {
        return 0;
      }
      var b = $p2;
      var a = $p1;
      return __(Language$Fay$Stdlib$abs,_(Fay$$mult)(_(__(Language$Fay$Stdlib$quot,a,__(Language$Fay$Stdlib$gcd,a,b))))(_(b)));
    });
  };
};
var Language$Fay$Stdlib$curry = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return __(f,Fay$$list([x,y]));
      });
    };
  };
};
var Language$Fay$Stdlib$uncurry = function($p1){
  return function($p2){
    return new $(function(){
      var p = $p2;
      var f = $p1;
      return (function($tmp1){
        if (Fay$$listLen(_($tmp1),2)) {
          var x = Fay$$index(0)(_($tmp1));
          var y = Fay$$index(1)(_($tmp1));
          return __(f,x,y);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(p);
    });
  };
};
var Language$Fay$Stdlib$snd = function($p1){
  return new $(function(){
    if (Fay$$listLen(_($p1),2)) {
      var x = Fay$$index(1)(_($p1));
      return x;
    }
    throw ["unhandled case in snd",[$p1]];
  });
};
var Language$Fay$Stdlib$fst = function($p1){
  return new $(function(){
    if (Fay$$listLen(_($p1),2)) {
      var x = Fay$$index(0)(_($p1));
      return x;
    }
    throw ["unhandled case in fst",[$p1]];
  });
};
var Language$Fay$Stdlib$find = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(__(p,x)) ? __(Language$Fay$Stdlib$Just,x) : __(Language$Fay$Stdlib$find,p,xs);
      }
      if (_($p2) === null) {
        return Language$Fay$Stdlib$Nothing;
      }
      throw ["unhandled case in find",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$filter = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(__(p,x)) ? __(Fay$$cons,x,__(Language$Fay$Stdlib$filter,p,xs)) : __(Language$Fay$Stdlib$filter,p,xs);
      }
      if (_($p2) === null) {
        return null;
      }
      throw ["unhandled case in filter",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$not = function($p1){
  return new $(function(){
    var p = $p1;
    return _(p) ? false : true;
  });
};
var Language$Fay$Stdlib$$_null = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return true;
    }
    return false;
  });
};
var Language$Fay$Stdlib$map = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return __(Fay$$cons,__(f,x),__(Language$Fay$Stdlib$map,f,xs));
      }
      throw ["unhandled case in map",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$nub = function($p1){
  return new $(function(){
    var ls = $p1;
    return __(Language$Fay$Stdlib$nub$39$,ls,null);
  });
};
var Language$Fay$Stdlib$nub$39$ = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p1) === null) {
        return null;
      }
      var ls = $p2;
      var $tmp1 = _($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return _(__(Language$Fay$Stdlib$elem,x,ls)) ? __(Language$Fay$Stdlib$nub$39$,xs,ls) : __(Fay$$cons,x,__(Language$Fay$Stdlib$nub$39$,xs,__(Fay$$cons,x,ls)));
      }
      throw ["unhandled case in nub'",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$elem = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var y = $tmp1.car;
        var ys = $tmp1.cdr;
        var x = $p1;
        return _(Fay$$or)(_(__(Fay$$eq,x,y)))(_(__(Language$Fay$Stdlib$elem,x,ys)));
      }
      if (_($p2) === null) {
        return false;
      }
      throw ["unhandled case in elem",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$notElem = function($p1){
  return function($p2){
    return new $(function(){
      var ys = $p2;
      var x = $p1;
      return __(Language$Fay$Stdlib$not,__(Language$Fay$Stdlib$elem,x,ys));
    });
  };
};
var $_Language$Fay$Stdlib$GT = function(){
};
var Language$Fay$Stdlib$GT = new $(function(){
  return new $_Language$Fay$Stdlib$GT();
});
var $_Language$Fay$Stdlib$LT = function(){
};
var Language$Fay$Stdlib$LT = new $(function(){
  return new $_Language$Fay$Stdlib$LT();
});
var $_Language$Fay$Stdlib$EQ = function(){
};
var Language$Fay$Stdlib$EQ = new $(function(){
  return new $_Language$Fay$Stdlib$EQ();
});
var Language$Fay$Stdlib$sort = new $(function(){
  return __(Language$Fay$Stdlib$sortBy,Language$Fay$Stdlib$compare);
});
var Language$Fay$Stdlib$compare = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      return _(_(Fay$$gt)(_(x))(_(y))) ? Language$Fay$Stdlib$GT : _(_(Fay$$lt)(_(x))(_(y))) ? Language$Fay$Stdlib$LT : Language$Fay$Stdlib$EQ;
    });
  };
};
var Language$Fay$Stdlib$sortBy = function($p1){
  return new $(function(){
    var cmp = $p1;
    return __(Language$Fay$Stdlib$foldr,__(Language$Fay$Stdlib$insertBy,cmp),null);
  });
};
var Language$Fay$Stdlib$insertBy = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) === null) {
          var x = $p2;
          return Fay$$list([x]);
        }
        var ys = $p3;
        var x = $p2;
        var cmp = $p1;
        return (function($tmp1){
          if (_($tmp1) === null) {
            return Fay$$list([x]);
          }
          var $tmp2 = _($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var y = $tmp2.car;
            var ys$39$ = $tmp2.cdr;
            return (function($tmp2){
              if (_($tmp2) instanceof $_Language$Fay$Stdlib$GT) {
                return __(Fay$$cons,y,__(Language$Fay$Stdlib$insertBy,cmp,x,ys$39$));
              }
              return __(Fay$$cons,x,ys);
            })(__(cmp,x,y));
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(ys);
      });
    };
  };
};
var Language$Fay$Stdlib$when = function($p1){
  return function($p2){
    return new $(function(){
      var m = $p2;
      var p = $p1;
      return _(p) ? __(Fay$$then,m,__(Fay$$$_return,Fay$$unit)) : __(Fay$$$_return,Fay$$unit);
    });
  };
};
var Language$Fay$Stdlib$succ = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$add)(_(x))(1);
  });
};
var Language$Fay$Stdlib$pred = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$sub)(_(x))(1);
  });
};
var Language$Fay$Stdlib$enumFrom = function($p1){
  return new $(function(){
    var i = $p1;
    return __(Fay$$cons,i,__(Language$Fay$Stdlib$enumFrom,_(Fay$$add)(_(i))(1)));
  });
};
var Language$Fay$Stdlib$enumFromTo = function($p1){
  return function($p2){
    return new $(function(){
      var n = $p2;
      var i = $p1;
      return _(_(Fay$$gt)(_(i))(_(n))) ? null : __(Fay$$cons,i,__(Language$Fay$Stdlib$enumFromTo,_(Fay$$add)(_(i))(1),n));
    });
  };
};
var Language$Fay$Stdlib$enumFromBy = function($p1){
  return function($p2){
    return new $(function(){
      var by = $p2;
      var fr = $p1;
      return __(Fay$$cons,fr,__(Language$Fay$Stdlib$enumFromBy,_(Fay$$add)(_(fr))(_(by)),by));
    });
  };
};
var Language$Fay$Stdlib$enumFromThen = function($p1){
  return function($p2){
    return new $(function(){
      var th = $p2;
      var fr = $p1;
      return __(Language$Fay$Stdlib$enumFromBy,fr,_(Fay$$sub)(_(th))(_(fr)));
    });
  };
};
var Language$Fay$Stdlib$enumFromByTo = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var to = $p3;
        var by = $p2;
        var fr = $p1;
        return (function(){
          var neg = function($p1){
            return new $(function(){
              var x = $p1;
              return _(_(Fay$$lt)(_(x))(_(to))) ? null : __(Fay$$cons,x,__(neg,_(Fay$$add)(_(x))(_(by))));
            });
          };
          var pos = function($p1){
            return new $(function(){
              var x = $p1;
              return _(_(Fay$$gt)(_(x))(_(to))) ? null : __(Fay$$cons,x,__(pos,_(Fay$$add)(_(x))(_(by))));
            });
          };
          return _(_(Fay$$lt)(_(by))(0)) ? __(neg,fr) : __(pos,fr);
        })();
      });
    };
  };
};
var Language$Fay$Stdlib$enumFromThenTo = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var to = $p3;
        var th = $p2;
        var fr = $p1;
        return __(Language$Fay$Stdlib$enumFromByTo,fr,_(Fay$$sub)(_(th))(_(fr)),to);
      });
    };
  };
};
var Language$Fay$Stdlib$zipWith = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var $tmp1 = _($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var b = $tmp1.car;
          var bs = $tmp1.cdr;
          var $tmp1 = _($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var a = $tmp1.car;
            var as = $tmp1.cdr;
            var f = $p1;
            return __(Fay$$cons,__(f,a,b),__(Language$Fay$Stdlib$zipWith,f,as,bs));
          }
        }
        return null;
      });
    };
  };
};
var Language$Fay$Stdlib$zipWith3 = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new $(function(){
          var $tmp1 = _($p4);
          if ($tmp1 instanceof Fay$$Cons) {
            var c = $tmp1.car;
            var cs = $tmp1.cdr;
            var $tmp1 = _($p3);
            if ($tmp1 instanceof Fay$$Cons) {
              var b = $tmp1.car;
              var bs = $tmp1.cdr;
              var $tmp1 = _($p2);
              if ($tmp1 instanceof Fay$$Cons) {
                var a = $tmp1.car;
                var as = $tmp1.cdr;
                var f = $p1;
                return __(Fay$$cons,__(f,a,b,c),__(Language$Fay$Stdlib$zipWith3,f,as,bs,cs));
              }
            }
          }
          return null;
        });
      };
    };
  };
};
var Language$Fay$Stdlib$zip = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = _($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          return __(Fay$$cons,Fay$$list([a,b]),__(Language$Fay$Stdlib$zip,as,bs));
        }
      }
      return null;
    });
  };
};
var Language$Fay$Stdlib$zip3 = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var $tmp1 = _($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var c = $tmp1.car;
          var cs = $tmp1.cdr;
          var $tmp1 = _($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var b = $tmp1.car;
            var bs = $tmp1.cdr;
            var $tmp1 = _($p1);
            if ($tmp1 instanceof Fay$$Cons) {
              var a = $tmp1.car;
              var as = $tmp1.cdr;
              return __(Fay$$cons,Fay$$list([a,b,c]),__(Language$Fay$Stdlib$zip3,as,bs,cs));
            }
          }
        }
        return null;
      });
    };
  };
};
var Language$Fay$Stdlib$unzip = function($p1){
  return new $(function(){
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(_($tmp1.car),2)) {
        var x = Fay$$index(0)(_($tmp1.car));
        var y = Fay$$index(1)(_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(_($tmp1),2)) {
            var xs = Fay$$index(0)(_($tmp1));
            var ys = Fay$$index(1)(_($tmp1));
            return Fay$$list([__(Fay$$cons,x,xs),__(Fay$$cons,y,ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(__(Language$Fay$Stdlib$unzip,ps));
      }
    }
    if (_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
var Language$Fay$Stdlib$unzip3 = function($p1){
  return new $(function(){
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(_($tmp1.car),3)) {
        var x = Fay$$index(0)(_($tmp1.car));
        var y = Fay$$index(1)(_($tmp1.car));
        var z = Fay$$index(2)(_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(_($tmp1),3)) {
            var xs = Fay$$index(0)(_($tmp1));
            var ys = Fay$$index(1)(_($tmp1));
            var zs = Fay$$index(2)(_($tmp1));
            return Fay$$list([__(Fay$$cons,x,xs),__(Fay$$cons,y,ys),__(Fay$$cons,z,zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(__(Language$Fay$Stdlib$unzip3,ps));
      }
    }
    if (_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
var Language$Fay$Stdlib$lines = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new $(function(){
          var c = $p1;
          return _(Fay$$or)(_(__(Fay$$eq,c,"\r")))(_(__(Fay$$eq,c,"\n")));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(_($tmp1),2)) {
          var a = Fay$$index(0)(_($tmp1));
          if (_(Fay$$index(1)(_($tmp1))) === null) {
            return Fay$$list([a]);
          }
          var a = Fay$$index(0)(_($tmp1));
          var $tmp2 = _(Fay$$index(1)(_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return __(Fay$$cons,a,__(Language$Fay$Stdlib$lines,cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(__(Language$Fay$Stdlib$$_break,isLineBreak,s));
    })();
  });
};
var Language$Fay$Stdlib$unlines = new $(function(){
  return __(Language$Fay$Stdlib$intercalate,Fay$$list("\n"));
});
var Language$Fay$Stdlib$words = function($p1){
  return new $(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new $(function(){
          if (_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(_($tmp1),2)) {
              var a = Fay$$index(0)(_($tmp1));
              var b = Fay$$index(1)(_($tmp1));
              return __(Fay$$cons,a,__(Language$Fay$Stdlib$words,b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(__(Language$Fay$Stdlib$$_break,isSpace,s));
        });
      };
      var isSpace = function($p1){
        return new $(function(){
          var c = $p1;
          return __(Language$Fay$Stdlib$elem,c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return __(words$39$,__(Language$Fay$Stdlib$dropWhile,isSpace,str));
    })();
  });
};
var Language$Fay$Stdlib$unwords = new $(function(){
  return __(Language$Fay$Stdlib$intercalate,Fay$$list(" "));
});
var Language$Fay$Stdlib$flip = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return __(f,y,x);
      });
    };
  };
};
var Language$Fay$Stdlib$maybe = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) instanceof $_Language$Fay$Stdlib$Nothing) {
          var m = $p1;
          return m;
        }
        if (_($p3) instanceof $_Language$Fay$Stdlib$Just) {
          var x = _($p3).slot1;
          var f = $p2;
          return __(f,x);
        }
        throw ["unhandled case in maybe",[$p1,$p2,$p3]];
      });
    };
  };
};
var Language$Fay$Stdlib$$46$ = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return __(f,__(g,x));
      });
    };
  };
};
var Language$Fay$Stdlib$$43$$43$ = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      return __(Language$Fay$Stdlib$conc,x,y);
    });
  };
};
var Language$Fay$Stdlib$$36$ = function($p1){
  return function($p2){
    return new $(function(){
      var x = $p2;
      var f = $p1;
      return __(f,x);
    });
  };
};
var Language$Fay$Stdlib$conc = function($p1){
  return function($p2){
    return new $(function(){
      var ys = $p2;
      var $tmp1 = _($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return __(Fay$$cons,x,__(Language$Fay$Stdlib$conc,xs,ys));
      }
      var ys = $p2;
      if (_($p1) === null) {
        return ys;
      }
      throw ["unhandled case in conc",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$concat = new $(function(){
  return __(Language$Fay$Stdlib$foldr,Language$Fay$Stdlib$conc,null);
});
var Language$Fay$Stdlib$concatMap = function($p1){
  return new $(function(){
    var f = $p1;
    return __(Language$Fay$Stdlib$foldr,__(Language$Fay$Stdlib$$46$,Language$Fay$Stdlib$$43$$43$,f),null);
  });
};
var Language$Fay$Stdlib$foldr = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = _($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return __(f,x,__(Language$Fay$Stdlib$foldr,f,z,xs));
        }
        throw ["unhandled case in foldr",[$p1,$p2,$p3]];
      });
    };
  };
};
var Language$Fay$Stdlib$foldr1 = function($p1){
  return function($p2){
    return new $(function(){
      if (Fay$$listLen(_($p2),1)) {
        var x = Fay$$index(0)(_($p2));
        return x;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return __(f,x,__(Language$Fay$Stdlib$foldr1,f,xs));
      }
      if (_($p2) === null) {
        return __(Language$Fay$Stdlib$error,Fay$$list("foldr1: empty list"));
      }
      throw ["unhandled case in foldr1",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$foldl = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = _($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return __(Language$Fay$Stdlib$foldl,f,__(f,z,x),xs);
        }
        throw ["unhandled case in foldl",[$p1,$p2,$p3]];
      });
    };
  };
};
var Language$Fay$Stdlib$foldl1 = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return __(Language$Fay$Stdlib$foldl,f,x,xs);
      }
      if (_($p2) === null) {
        return __(Language$Fay$Stdlib$error,Fay$$list("foldl1: empty list"));
      }
      throw ["unhandled case in foldl1",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$and = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return true;
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return _(Fay$$and)(_(x))(_(__(Language$Fay$Stdlib$and,xs)));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
var Language$Fay$Stdlib$or = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return false;
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return _(Fay$$or)(_(x))(_(__(Language$Fay$Stdlib$or,xs)));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
var Language$Fay$Stdlib$any = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return false;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(Fay$$or)(_(__(p,x)))(_(__(Language$Fay$Stdlib$any,p,xs)));
      }
      throw ["unhandled case in any",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$all = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return true;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(Fay$$and)(_(__(p,x)))(_(__(Language$Fay$Stdlib$all,p,xs)));
      }
      throw ["unhandled case in all",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$maximum = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("maximum: empty list"));
    }
    var xs = $p1;
    return __(Language$Fay$Stdlib$foldl1,Language$Fay$Stdlib$max,xs);
  });
};
var Language$Fay$Stdlib$minimum = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("minimum: empty list"));
    }
    var xs = $p1;
    return __(Language$Fay$Stdlib$foldl1,Language$Fay$Stdlib$min,xs);
  });
};
var Language$Fay$Stdlib$product = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("product: empty list"));
    }
    var xs = $p1;
    return __(Language$Fay$Stdlib$foldl,Fay$$mult,1,xs);
  });
};
var Language$Fay$Stdlib$sum = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("sum: empty list"));
    }
    var xs = $p1;
    return __(Language$Fay$Stdlib$foldl,Fay$$add,0,xs);
  });
};
var Language$Fay$Stdlib$scanl = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var l = $p3;
        var z = $p2;
        var f = $p1;
        return __(Fay$$cons,z,(function($tmp1){
          if (_($tmp1) === null) {
            return null;
          }
          var $tmp2 = _($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var x = $tmp2.car;
            var xs = $tmp2.cdr;
            return __(Language$Fay$Stdlib$scanl,f,__(f,z,x),xs);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(l));
      });
    };
  };
};
var Language$Fay$Stdlib$scanl1 = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return __(Language$Fay$Stdlib$scanl,f,x,xs);
      }
      throw ["unhandled case in scanl1",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$scanr = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        if (_($p3) === null) {
          var z = $p2;
          return Fay$$list([z]);
        }
        var $tmp1 = _($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return (function($tmp1){
            var $tmp2 = _($tmp1);
            if ($tmp2 instanceof Fay$$Cons) {
              var h = $tmp2.car;
              var t = $tmp2.cdr;
              return __(Fay$$cons,__(f,x,h),__(Fay$$cons,h,t));
            }
            return Language$Fay$Stdlib$$_undefined;
          })(__(Language$Fay$Stdlib$scanr,f,z,xs));
        }
        throw ["unhandled case in scanr",[$p1,$p2,$p3]];
      });
    };
  };
};
var Language$Fay$Stdlib$scanr1 = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      if (Fay$$listLen(_($p2),1)) {
        var x = Fay$$index(0)(_($p2));
        return Fay$$list([x]);
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return (function($tmp1){
          var $tmp2 = _($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var h = $tmp2.car;
            var t = $tmp2.cdr;
            return __(Fay$$cons,__(f,x,h),__(Fay$$cons,h,t));
          }
          return Language$Fay$Stdlib$$_undefined;
        })(__(Language$Fay$Stdlib$scanr1,f,xs));
      }
      throw ["unhandled case in scanr1",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$lookup = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        var _key = $p1;
        return Language$Fay$Stdlib$Nothing;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        if (Fay$$listLen(_($tmp1.car),2)) {
          var x = Fay$$index(0)(_($tmp1.car));
          var y = Fay$$index(1)(_($tmp1.car));
          var xys = $tmp1.cdr;
          var key = $p1;
          return _(__(Fay$$eq,key,x)) ? __(Language$Fay$Stdlib$Just,y) : __(Language$Fay$Stdlib$lookup,key,xys);
        }
      }
      throw ["unhandled case in lookup",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$intersperse = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return __(Fay$$cons,x,__(Language$Fay$Stdlib$prependToAll,sep,xs));
      }
      throw ["unhandled case in intersperse",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$prependToAll = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return __(Fay$$cons,sep,__(Fay$$cons,x,__(Language$Fay$Stdlib$prependToAll,sep,xs)));
      }
      throw ["unhandled case in prependToAll",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$intercalate = function($p1){
  return function($p2){
    return new $(function(){
      var xss = $p2;
      var xs = $p1;
      return __(Language$Fay$Stdlib$concat,__(Language$Fay$Stdlib$intersperse,xs,xss));
    });
  };
};
var Language$Fay$Stdlib$forM_ = function($p1){
  return function($p2){
    return new $(function(){
      var m = $p2;
      var $tmp1 = _($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return __(Fay$$then,__(m,x),__(Language$Fay$Stdlib$forM_,xs,m));
      }
      if (_($p1) === null) {
        return __(Fay$$$_return,Fay$$unit);
      }
      throw ["unhandled case in forM_",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$mapM_ = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var m = $p1;
        return __(Fay$$then,__(m,x),__(Language$Fay$Stdlib$mapM_,m,xs));
      }
      if (_($p2) === null) {
        return __(Fay$$$_return,Fay$$unit);
      }
      throw ["unhandled case in mapM_",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$$_const = function($p1){
  return function($p2){
    return new $(function(){
      var a = $p1;
      return a;
    });
  };
};
var Language$Fay$Stdlib$length = function($p1){
  return new $(function(){
    var xs = $p1;
    return __(Language$Fay$Stdlib$length$39$,0,xs);
  });
};
var Language$Fay$Stdlib$length$39$ = function($p1){
  return function($p2){
    return new $(function(){
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var acc = $p1;
        return __(Language$Fay$Stdlib$length$39$,_(Fay$$add)(_(acc))(1),xs);
      }
      var acc = $p1;
      return acc;
    });
  };
};
var Language$Fay$Stdlib$rem = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      return _(__(Fay$$eq,y,0)) ? __(Language$Fay$Stdlib$error,Fay$$list("Division by zero")) : __(Language$Fay$Stdlib$rem$39$,x,y);
    });
  };
};
var Language$Fay$Stdlib$rem$39$ = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["int"],Fay$$fayToJs(["int"],$p1) % Fay$$fayToJs(["int"],$p2));
    });
  };
};
var Language$Fay$Stdlib$quot = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      return _(__(Fay$$eq,y,0)) ? __(Language$Fay$Stdlib$error,Fay$$list("Division by zero")) : __(Language$Fay$Stdlib$quot$39$,x,y);
    });
  };
};
var Language$Fay$Stdlib$quot$39$ = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["int"],~~(Fay$$fayToJs(["int"],$p1)/Fay$$fayToJs(["int"],$p2)));
    });
  };
};
var Language$Fay$Stdlib$quotRem = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$list([__(Language$Fay$Stdlib$quot,x,y),__(Language$Fay$Stdlib$rem,x,y)]);
    });
  };
};
var Language$Fay$Stdlib$div = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {
        return _(Fay$$sub)(_(__(Language$Fay$Stdlib$quot,_(Fay$$sub)(_(x))(1),y)))(1);
      } else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {
          return _(Fay$$sub)(_(__(Language$Fay$Stdlib$quot,_(Fay$$add)(_(x))(1),y)))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return __(Language$Fay$Stdlib$quot,x,y);
    });
  };
};
var Language$Fay$Stdlib$mod = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {
        return _(Fay$$add)(_(_(Fay$$add)(_(__(Language$Fay$Stdlib$rem,_(Fay$$sub)(_(x))(1),y)))(_(y))))(1);
      } else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {
          return _(Fay$$sub)(_(_(Fay$$add)(_(__(Language$Fay$Stdlib$rem,_(Fay$$add)(_(x))(1),y)))(_(y))))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return __(Language$Fay$Stdlib$rem,x,y);
    });
  };
};
var Language$Fay$Stdlib$divMod = function($p1){
  return function($p2){
    return new $(function(){
      var y = $p2;
      var x = $p1;
      if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {
        return (function($tmp1){
          if (Fay$$listLen(_($tmp1),2)) {
            var q = Fay$$index(0)(_($tmp1));
            var r = Fay$$index(1)(_($tmp1));
            return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$add)(_(_(Fay$$add)(_(r))(_(y))))(1)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(__(Language$Fay$Stdlib$quotRem,_(Fay$$sub)(_(x))(1),y));
      } else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(1))))) {
          return (function($tmp1){
            if (Fay$$listLen(_($tmp1),2)) {
              var q = Fay$$index(0)(_($tmp1));
              var r = Fay$$index(1)(_($tmp1));
              return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$sub)(_(_(Fay$$add)(_(r))(_(y))))(1)]);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(__(Language$Fay$Stdlib$quotRem,_(Fay$$add)(_(x))(1),y));
        }
      }
      var y = $p2;
      var x = $p1;
      return __(Language$Fay$Stdlib$quotRem,x,y);
    });
  };
};
var Language$Fay$Stdlib$min = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Language$Fay$Stdlib$max = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Language$Fay$Stdlib$recip = function($p1){
  return new $(function(){
    var x = $p1;
    return _(Fay$$div)(1)(_(x));
  });
};
var Language$Fay$Stdlib$fromIntegral = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$p1));
  });
};
var Language$Fay$Stdlib$otherwise = true;
var Language$Fay$Stdlib$reverse = function($p1){
  return new $(function(){
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return __(Language$Fay$Stdlib$$43$$43$,__(Language$Fay$Stdlib$reverse,xs),Fay$$list([x]));
    }
    if (_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
var Language$Fay$Stdlib$$61$$60$$60$ = function($p1){
  return function($p2){
    return new $(function(){
      var x = $p2;
      var f = $p1;
      return __(Fay$$bind,x,f);
    });
  };
};
var Language$Fay$Stdlib$sequence = function($p1){
  return new $(function(){
    var ms = $p1;
    return (function(){
      var k = function($p1){
        return function($p2){
          return new $(function(){
            var m$39$ = $p2;
            var m = $p1;
            return __(Fay$$bind,m,function($p1){
              var x = $p1;
              return __(Fay$$bind,m$39$,function($p1){
                var xs = $p1;
                return __(Fay$$$_return,__(Fay$$cons,x,xs));
              });
            });
          });
        };
      };
      return __(Language$Fay$Stdlib$foldr,k,__(Fay$$$_return,null),ms);
    })();
  });
};
var Language$Fay$Stdlib$sequence_ = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Fay$$$_return,Fay$$unit);
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return __(Fay$$then,m,__(Language$Fay$Stdlib$sequence_,ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
var Language$Fay$Stdlib$id = function($p1){
  return new $(function(){
    var x = $p1;
    return x;
  });
};
var Language$Fay$Stdlib$asTypeOf = new $(function(){
  return Language$Fay$Stdlib$$_const;
});
var Language$Fay$Stdlib$until = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        var x = $p3;
        var f = $p2;
        var p = $p1;
        return _(__(p,x)) ? x : __(Language$Fay$Stdlib$until,p,f,__(f,x));
      });
    };
  };
};
var Language$Fay$Stdlib$$36$$33$ = function($p1){
  return function($p2){
    return new $(function(){
      var x = $p2;
      var f = $p1;
      return __(Fay$$seq,x,__(f,x));
    });
  };
};
var Language$Fay$Stdlib$$33$$33$ = function($p1){
  return function($p2){
    return new $(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new $(function(){
              if (_($p1) === null) {
                return __(Language$Fay$Stdlib$error,Fay$$list("(!!): index too large"));
              }
              if (_($p2) === 0) {
                var $tmp1 = _($p1);
                if ($tmp1 instanceof Fay$$Cons) {
                  var h = $tmp1.car;
                  return h;
                }
              }
              var n = $p2;
              var $tmp1 = _($p1);
              if ($tmp1 instanceof Fay$$Cons) {
                var t = $tmp1.cdr;
                return __(go,t,_(Fay$$sub)(_(n))(1));
              }
              throw ["unhandled case in go",[$p1,$p2]];
            });
          };
        };
        return _(_(Fay$$lt)(_(b))(0)) ? __(Language$Fay$Stdlib$error,Fay$$list("(!!): negative index")) : __(go,a,b);
      })();
    });
  };
};
var Language$Fay$Stdlib$head = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("head: empty list"));
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      return h;
    }
    throw ["unhandled case in head",[$p1]];
  });
};
var Language$Fay$Stdlib$tail = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("tail: empty list"));
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return t;
    }
    throw ["unhandled case in tail",[$p1]];
  });
};
var Language$Fay$Stdlib$init = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(_($p1),1)) {
      var a = Fay$$index(0)(_($p1));
      return Fay$$list([a]);
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return __(Fay$$cons,h,__(Language$Fay$Stdlib$init,t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
var Language$Fay$Stdlib$last = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("last: empty list"));
    }
    if (Fay$$listLen(_($p1),1)) {
      var a = Fay$$index(0)(_($p1));
      return a;
    }
    var $tmp1 = _($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return __(Language$Fay$Stdlib$last,t);
    }
    throw ["unhandled case in last",[$p1]];
  });
};
var Language$Fay$Stdlib$iterate = function($p1){
  return function($p2){
    return new $(function(){
      var x = $p2;
      var f = $p1;
      return __(Fay$$cons,x,__(Language$Fay$Stdlib$iterate,f,__(f,x)));
    });
  };
};
var Language$Fay$Stdlib$repeat = function($p1){
  return new $(function(){
    var x = $p1;
    return __(Fay$$cons,x,__(Language$Fay$Stdlib$repeat,x));
  });
};
var Language$Fay$Stdlib$replicate = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p1) === 0) {
        return null;
      }
      var x = $p2;
      var n = $p1;
      return _(_(Fay$$lt)(_(n))(0)) ? __(Language$Fay$Stdlib$error,Fay$$list("replicate: negative length")) : __(Fay$$cons,x,__(Language$Fay$Stdlib$replicate,_(Fay$$sub)(_(n))(1),x));
    });
  };
};
var Language$Fay$Stdlib$cycle = function($p1){
  return new $(function(){
    if (_($p1) === null) {
      return __(Language$Fay$Stdlib$error,Fay$$list("cycle: empty list"));
    }
    var xs = $p1;
    return (function(){
      var xs$39$ = new $(function(){
        return __(Language$Fay$Stdlib$$43$$43$,xs,xs$39$);
      });
      return xs$39$;
    })();
  });
};
var Language$Fay$Stdlib$take = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p1) === 0) {
        return null;
      }
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return _(_(Fay$$lt)(_(n))(0)) ? __(Language$Fay$Stdlib$error,Fay$$list("take: negative length")) : __(Fay$$cons,x,__(Language$Fay$Stdlib$take,_(Fay$$sub)(_(n))(1),xs));
      }
      throw ["unhandled case in take",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$drop = function($p1){
  return function($p2){
    return new $(function(){
      var xs = $p2;
      if (_($p1) === 0) {
        return xs;
      }
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var n = $p1;
        return _(_(Fay$$lt)(_(n))(0)) ? __(Language$Fay$Stdlib$error,Fay$$list("drop: negative length")) : __(Language$Fay$Stdlib$drop,_(Fay$$sub)(_(n))(1),xs);
      }
      throw ["unhandled case in drop",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$splitAt = function($p1){
  return function($p2){
    return new $(function(){
      var xs = $p2;
      if (_($p1) === 0) {
        return Fay$$list([null,xs]);
      }
      if (_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return _(_(Fay$$lt)(_(n))(0)) ? __(Language$Fay$Stdlib$error,Fay$$list("splitAt: negative length")) : (function($tmp1){
          if (Fay$$listLen(_($tmp1),2)) {
            var a = Fay$$index(0)(_($tmp1));
            var b = Fay$$index(1)(_($tmp1));
            return Fay$$list([__(Fay$$cons,x,a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(__(Language$Fay$Stdlib$splitAt,_(Fay$$sub)(_(n))(1),xs));
      }
      throw ["unhandled case in splitAt",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$takeWhile = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(__(p,x)) ? __(Fay$$cons,x,__(Language$Fay$Stdlib$takeWhile,p,xs)) : null;
      }
      throw ["unhandled case in takeWhile",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$dropWhile = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return null;
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(__(p,x)) ? __(Language$Fay$Stdlib$dropWhile,p,xs) : __(Fay$$cons,x,xs);
      }
      throw ["unhandled case in dropWhile",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$span = function($p1){
  return function($p2){
    return new $(function(){
      if (_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = _($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return _(__(p,x)) ? (function($tmp1){
          if (Fay$$listLen(_($tmp1),2)) {
            var a = Fay$$index(0)(_($tmp1));
            var b = Fay$$index(1)(_($tmp1));
            return Fay$$list([__(Fay$$cons,x,a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(__(Language$Fay$Stdlib$span,p,xs)) : Fay$$list([null,__(Fay$$cons,x,xs)]);
      }
      throw ["unhandled case in span",[$p1,$p2]];
    });
  };
};
var Language$Fay$Stdlib$$_break = function($p1){
  return new $(function(){
    var p = $p1;
    return __(Language$Fay$Stdlib$span,__(Language$Fay$Stdlib$$46$,Language$Fay$Stdlib$not,p));
  });
};
var Language$Fay$Stdlib$print = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["unknown"],$p1)));
  });
};
var Language$Fay$Stdlib$putStrLn = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["string"],$p1)));
  });
};
var $_Language$Fay$Stdlib$Just = function(slot1){
  this.slot1 = slot1;
};
var Language$Fay$Stdlib$Just = function(slot1){
  return new $(function(){
    return new $_Language$Fay$Stdlib$Just(slot1);
  });
};
var $_Language$Fay$Stdlib$Nothing = function(){
};
var Language$Fay$Stdlib$Nothing = new $(function(){
  return new $_Language$Fay$Stdlib$Nothing();
});
var JQuery$getThis = new $(function(){
  return Fay$$jsToFay(["action",[["user","JQuery",[]]]],this);
});
var JQuery$addClass = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).addClass(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$addClassWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).addClass(Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$p1)));
    });
  };
};
var JQuery$getAttr = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).attr(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setAttr = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).attr(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$setAttrWith = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).attr(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$p2)));
      });
    };
  };
};
var JQuery$hasClass = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["bool"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).hasClass(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$getHtml = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).html());
  });
};
var JQuery$setHtml = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).html(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setHtmlWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).html(Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$getProp = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prop(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setProp = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).prop(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$setPropWith = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).prop(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$p2)));
      });
    };
  };
};
var JQuery$removeAttr = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).removeAttr(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$removeClass = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).removeClass(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$removeClassWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).removeClass(Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$removeProp = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).removeProp(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$toggleClass = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).toggleClass(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$toggleClassBool = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).toggleClass(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["bool"],$p2)));
      });
    };
  };
};
var JQuery$toggleAllClasses = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).toggleClass(Fay$$fayToJs(["bool"],$p1)));
    });
  };
};
var JQuery$toggleClassWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).toggleClass(Fay$$fayToJs(["function",[["double"],["string"],["bool"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$toggleClassBoolWith = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).toggleClass(Fay$$fayToJs(["function",[["double"],["string"],["bool"],["action",[["user","JQuery",[]]]]]],$p1), Fay$$fayToJs(["bool"],$p2)));
      });
    };
  };
};
var JQuery$getVal = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).val());
  });
};
var JQuery$setVal = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).val(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setValWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).val(Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$setText = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).text(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setTextWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).text(Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$getText = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).text());
  });
};
var JQuery$holdReady = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery.holdReady(Fay$$fayToJs(["bool"],$p1)));
  });
};
var JQuery$selectElement = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery(Fay$$fayToJs(["user","Element",[]],$p1)));
  });
};
var JQuery$selectObject = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery(Fay$$fayToJs(["unknown"],$p1)));
  });
};
var JQuery$select = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery(Fay$$fayToJs(["string"],$p1)));
  });
};
var JQuery$selectEmpty = new $(function(){
  return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery());
});
var JQuery$createJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var JQuery$ready = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],jQuery(Fay$$fayToJs(["action",[["unknown"]]],$p1)));
  });
};
var JQuery$noConflict = new $(function(){
  return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery.noConflict());
});
var JQuery$noConflictBool = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],jQuery.noConflict(Fay$$fayToJs(["bool"],$p1)));
  });
};
var JQuery$getCss = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).css(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$setCss = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).css(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$setCssWith = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).css(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$p2)));
      });
    };
  };
};
var JQuery$getHeight = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).height());
  });
};
var JQuery$setHeight = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).height(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$setHeightWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).height(Fay$$fayToJs(["function",[["double"],["double"],["action",[["double"]]]]],$p1)));
    });
  };
};
var JQuery$getInnerHeight = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).innerHeight());
  });
};
var JQuery$getInnerWidth = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).innerWidth());
  });
};
var JQuery$getOuterHeight = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).outerHeight());
  });
};
var JQuery$getOuterHeightBool = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).outerHeight(Fay$$fayToJs(["bool"],$p1)));
    });
  };
};
var JQuery$getOuterWidth = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).outerWidth());
  });
};
var JQuery$getOuterWidthBool = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).outerWidth(Fay$$fayToJs(["bool"],$p1)));
    });
  };
};
var JQuery$getScrollLeft = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).scrollLeft());
  });
};
var JQuery$setScrollLeft = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).scrollLeft(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$getScrollTop = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).scrollTop());
  });
};
var JQuery$setScrollTop = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).scrollTop(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$getWidth = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$p1).width());
  });
};
var JQuery$setWidth = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).width(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$setWidthWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).width(Fay$$fayToJs(["function",[["double"],["double"],["action",[["double"]]]]],$p1)));
    });
  };
};
var JQuery$resize = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).resize(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$scroll = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).scroll(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$load = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).load(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$documentReady = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],jQuery(Fay$$fayToJs(["user","Document",[]],$p2)).ready(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$unload = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],jQuery(Fay$$fayToJs(["user","Window",[]],$p2)).unload(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$click = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).click(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$dblclick = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).dblclick(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$focusin = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).focusin(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$focusout = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).focusout(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$hover = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).hover(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mousedown = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mousedown(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mouseenter = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mouseenter(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mouseleave = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mouseleave(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mousemove = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mousemove(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mouseout = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mouseout(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mouseover = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mouseover(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$mouseup = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).mouseup(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$toggle = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).toggle.apply(Fay$$fayToJs(["user","JQuery",[]],$p2), Fay$$fayToJs(["list",[["function",[["user","Event",[]],["action",[["unknown"]]]]]]],$p1)));
    });
  };
};
var JQuery$bind = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p3).bind(Fay$$fayToJs(["user","EventType",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2)));
      });
    };
  };
};
var JQuery$bindPreventBubble = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p3).bind(Fay$$fayToJs(["user","EventType",[]],$p1),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2),false));
      });
    };
  };
};
var JQuery$on = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p3).on(Fay$$fayToJs(["user","EventType",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2)));
      });
    };
  };
};
var JQuery$onDelegate = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new $(function(){
          return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p4).on(Fay$$fayToJs(["user","EventType",[]],$p1),Fay$$fayToJs(["user","Selector",[]],$p2),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p3)));
        });
      };
    };
  };
};
var JQuery$one = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p3).one(Fay$$fayToJs(["user","EventType",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2)));
      });
    };
  };
};
var JQuery$trigger = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).trigger(Fay$$fayToJs(["user","EventType",[]],$p1)));
    });
  };
};
var JQuery$triggerHandler = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).triggerHandler(Fay$$fayToJs(["user","EventType",[]],$p1)));
    });
  };
};
var JQuery$delegateTarget = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","Element",[]]]],jQuery(Fay$$fayToJs(["user","Event",[]],$p1).delegateTarget));
  });
};
var JQuery$isDefaultPrevented = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["bool"]]],Fay$$fayToJs(["user","Event",[]],$p1).isDefaultPrevented());
  });
};
var JQuery$isImmediatePropagationStopped = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["bool"]]],Fay$$fayToJs(["user","Event",[]],$p1).isImmediatePropagationStopped());
  });
};
var JQuery$isPropagationStopped = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","Element",[]]]],Fay$$fayToJs(["user","Event",[]],$p1).isPropagationStopped());
  });
};
var JQuery$namespace = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","Event",[]],$p1).namespace);
  });
};
var JQuery$pageX = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Event",[]],$p1).pageX);
  });
};
var JQuery$pageY = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Event",[]],$p1).pageY);
  });
};
var JQuery$preventDefault = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Event",[]],$p1).preventDefault());
  });
};
var JQuery$target = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","Element",[]]]],Fay$$fayToJs(["user","Event",[]],$p1).target);
  });
};
var JQuery$timeStamp = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Event",[]],$p1).timeStamp);
  });
};
var JQuery$eventType = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","Event",[]],$p1).type);
  });
};
var JQuery$which = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["int"]]],Fay$$fayToJs(["user","Event",[]],$p1).which);
  });
};
var JQuery$blur = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).blur(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$change = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).change(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$focus = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).focus(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$onselect = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).select(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$submit = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).submit(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$keydown = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).keydown(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$keypress = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).keypress(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$keyup = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","JQuery",[]],$p2).keyup(Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p1)));
    });
  };
};
var JQuery$after = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).after(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$afterWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).after(Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$append = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).append(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$appendJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).append(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$appendWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).append(Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$appendTo = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).appendTo(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$appendToJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).appendTo(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$before = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).before(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$beforeWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).before(Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var $_JQuery$WithoutDataAndEvents = function(){
};
var JQuery$WithoutDataAndEvents = new $(function(){
  return new $_JQuery$WithoutDataAndEvents();
});
var $_JQuery$WithDataAndEvents = function(){
};
var JQuery$WithDataAndEvents = new $(function(){
  return new $_JQuery$WithDataAndEvents();
});
var $_JQuery$DeepWithDataAndEvents = function(){
};
var JQuery$DeepWithDataAndEvents = new $(function(){
  return new $_JQuery$DeepWithDataAndEvents();
});
var JQuery$clone = function($p1){
  return new $(function(){
    if (_($p1) instanceof $_JQuery$WithoutDataAndEvents) {
      return __(JQuery$ffi,Fay$$list("%2.clone(false)"));
    }
    if (_($p1) instanceof $_JQuery$WithDataAndEvents) {
      return __(JQuery$ffi,Fay$$list("%2.clone(true, false)"));
    }
    if (_($p1) instanceof $_JQuery$DeepWithDataAndEvents) {
      return __(JQuery$ffi,Fay$$list("%2.clone(true, true)"));
    }
    throw ["unhandled case in clone",[$p1]];
  });
};
var JQuery$detach = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).detach());
  });
};
var JQuery$detachSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).detach(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$empty = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).empty());
  });
};
var JQuery$insertAfter = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).insertAfter(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$insertBefore = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).insertBefore(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$prepend = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prepend(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$prependWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prepend(Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$prependTo = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prependTo(Fay$$fayToJs(["unknown"],$p1)));
    });
  };
};
var JQuery$remove = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).remove());
  });
};
var JQuery$removeSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).remove(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$replaceAll = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).replaceAll(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$replaceWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).replaceWith(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$replaceWithJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).replaceWith(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$replaceWithWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).replaceWith(Fay$$fayToJs(["action",[["user","JQuery",[]]]],$p1)));
    });
  };
};
var JQuery$unwrap = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).unwrap());
  });
};
var JQuery$wrap = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrap(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$wrapWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrap(Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$wrapAllHtml = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapAll(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$wrapAllSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapAll(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$wrapAllElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapAll(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$wrapInnerHtml = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapInner(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$wrapInnerSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapInner(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$wrapInnerElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).wrapInner(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$addSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).add(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$addElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).add(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$addHtml = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).add(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$add = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).add(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$addSelectorWithContext = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).add(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["user","JQuery",[]],$p2)));
      });
    };
  };
};
var JQuery$andSelf = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).andSelf());
  });
};
var JQuery$children = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).children());
  });
};
var JQuery$childrenMatching = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).children(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$closestSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).closest(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$closestWithContext = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).closest(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$closest = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).closest(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$closestElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).closest(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$contents = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).contents());
  });
};
var JQuery$each = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).each(Fay$$fayToJs(["function",[["double"],["user","Element",[]],["action",[["bool"]]]]],$p1)));
    });
  };
};
var JQuery$end = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).end());
  });
};
var JQuery$eq = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).eq(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$filter = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).filter(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$filterWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).filter(Fay$$fayToJs(["function",[["double"],["action",[["bool"]]]]],$p1)));
    });
  };
};
var JQuery$filterElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).filter(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$filterJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).filter(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$findSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).find(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$findJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).find(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$findElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).find(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$first = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).first());
  });
};
var JQuery$has = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).has(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$hasElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).has(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$is = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).is(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$isWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).is(Fay$$fayToJs(["function",[["double"],["bool"]]],$p1)));
    });
  };
};
var JQuery$isJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).is(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$isElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).is(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$last = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).last());
  });
};
var JQuery$jQueryMap = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).map(Fay$$fayToJs(["function",[["double"],["user","Element",[]],["action",[["user","JQuery",[]]]]]],$p1)));
    });
  };
};
var JQuery$next = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).next());
  });
};
var JQuery$nextSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).next(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$nextAll = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).nextAll());
  });
};
var JQuery$nextAllSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).nextAll(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$nextUntil = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).nextUntil(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$nextUntilFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).nextUntil(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$nextUntilElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).nextUntil(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$nextUntilElementFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).nextUntil(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$not = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).not(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$notElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).not(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$notElements = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).not(Fay$$fayToJs(["list",[["user","Element",[]]]],$p1)));
    });
  };
};
var JQuery$notWith = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).not(Fay$$fayToJs(["function",[["double"],["bool"]]],$p1)));
    });
  };
};
var JQuery$notJQuery = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).not(Fay$$fayToJs(["user","JQuery",[]],$p1)));
    });
  };
};
var JQuery$offsetParent = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).offsetParent());
  });
};
var JQuery$parent = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).parent());
  });
};
var JQuery$parentSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).parent(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$parents = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).parents());
  });
};
var JQuery$parentsSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).parents(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$parentsUntil = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).parentsUntil(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$parentsUntilFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).parentsUntil(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$parentsUntilElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).parentsUntil(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$parentsUntilElementFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).parentsUntil(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$prev = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).prev());
  });
};
var JQuery$prevSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prev(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$prevAll = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).prevAll());
  });
};
var JQuery$prevAllSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prevAll(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$prevUntil = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prevUntil(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$prevUntilFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).prevUntil(Fay$$fayToJs(["string"],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$prevUntilElement = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).prevUntil(Fay$$fayToJs(["user","Element",[]],$p1)));
    });
  };
};
var JQuery$prevUntilElementFiltered = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).prevUntil(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["string"],$p2)));
      });
    };
  };
};
var JQuery$siblings = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p1).siblings());
  });
};
var JQuery$siblingsSelector = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).siblings(Fay$$fayToJs(["string"],$p1)));
    });
  };
};
var JQuery$slice = function($p1){
  return function($p2){
    return new $(function(){
      return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p2).slice(Fay$$fayToJs(["double"],$p1)));
    });
  };
};
var JQuery$sliceFromTo = function($p1){
  return function($p2){
    return function($p3){
      return new $(function(){
        return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$p3).slice(Fay$$fayToJs(["double"],$p1), Fay$$fayToJs(["double"],$p2)));
      });
    };
  };
};
var Main$alert = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],window.alert(Fay$$fayToJs(["unknown"],$p1)));
  });
};
var $_Main$Attr = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var Main$Attr = function(slot1){
  return function(slot2){
    return new $(function(){
      return new $_Main$Attr(slot1,slot2);
    });
  };
};
var $_Main$Elem = function(slot1,slot2,slot3){
  this.slot1 = slot1;
  this.slot2 = slot2;
  this.slot3 = slot3;
};
var Main$Elem = function(slot1){
  return function(slot2){
    return function(slot3){
      return new $(function(){
        return new $_Main$Elem(slot1,slot2,slot3);
      });
    };
  };
};
var $_Main$CData = function(slot1){
  this.slot1 = slot1;
};
var Main$CData = function(slot1){
  return new $(function(){
    return new $_Main$CData(slot1);
  });
};
var Main$buildAttr = function($p1){
  return new $(function(){
    if (_($p1) instanceof $_Main$Attr) {
      var k = _($p1).slot1;
      var v = _($p1).slot2;
      return __(Language$Fay$Stdlib$$43$$43$,Fay$$list(" "),__(Language$Fay$Stdlib$$43$$43$,k,__(Language$Fay$Stdlib$$43$$43$,Fay$$list("='"),__(Language$Fay$Stdlib$$43$$43$,v,Fay$$list("'")))));
    }
    throw ["unhandled case in buildAttr",[$p1]];
  });
};
var Main$buildElem = function($p1){
  return new $(function(){
    if (_($p1) instanceof $_Main$CData) {
      var s = _($p1).slot1;
      return s;
    }
    if (_($p1) instanceof $_Main$Elem) {
      var tag = _($p1).slot1;
      var attrs = _($p1).slot2;
      var childs = _($p1).slot3;
      return __(Language$Fay$Stdlib$$43$$43$,Fay$$list("<"),__(Language$Fay$Stdlib$$43$$43$,tag,__(Language$Fay$Stdlib$$43$$43$,__(Language$Fay$Stdlib$concatMap,Main$buildAttr,attrs),__(Language$Fay$Stdlib$$43$$43$,Fay$$list(">"),__(Language$Fay$Stdlib$$43$$43$,__(Language$Fay$Stdlib$concatMap,Main$buildElem,childs),__(Language$Fay$Stdlib$$43$$43$,Fay$$list("</"),__(Language$Fay$Stdlib$$43$$43$,tag,Fay$$list(">"))))))));
    }
    throw ["unhandled case in buildElem",[$p1]];
  });
};
var Main$writeRaw = function($p1){
  return new $(function(){
    return Fay$$jsToFay(["action",[["unknown"]]],document.write(Fay$$fayToJs(["string"],$p1)));
  });
};
var Main$writeElem = new $(function(){
  return __(Language$Fay$Stdlib$$46$,Main$writeRaw,Main$buildElem);
});
var Main$printElem = function($p1){
  return new $(function(){
    var f = $p1;
    return __(Fay$$bind,__(Fay$$bind,f,JQuery$getHtml),Language$Fay$Stdlib$putStrLn);
  });
};
var Main$safePrint = function($p1){
  return new $(function(){
    if (Fay$$equal($p1,Fay$$list(""))) {
      return __(Fay$$$_return,Fay$$unit);
    }
    var s = $p1;
    return __(Language$Fay$Stdlib$putStrLn,s);
  });
};
var Main$test = function($p1){
  return new $(function(){
    return __(Fay$$then,__(Language$Fay$Stdlib$putStrLn,Fay$$list("Entered main...")),__(Fay$$bind,__(JQuery$select,Fay$$list(".left")),function($p1){
      var left = $p1;
      return __(Fay$$bind,__(JQuery$select,Fay$$list(".right")),function($p1){
        var right = $p1;
        return __(Fay$$bind,__(JQuery$getHtml,right),function($p1){
          var rightContents = $p1;
          return __(Fay$$then,__(Language$Fay$Stdlib$putStrLn,rightContents),__(Fay$$then,__(JQuery$setHtml,rightContents,left),__(Language$Fay$Stdlib$putStrLn,Fay$$list("...finished main."))));
        });
      });
    }));
  });
};
var Main$theDocument = new $(function(){
  return Fay$$jsToFay(["user","Document",[]],window.document);
});
var Main$main = new $(function(){
  return __(JQuery$documentReady,Main$test,Main$theDocument);
});
var Fay$$fayToJsUserDefined = function(type,obj){
  var _obj = _(obj);
  var argTypes = type[2];
  if (_obj instanceof $_Main$CData) {
    var obj_ = {"instance": "CData"};
    var obj_slot1 = Fay$$fayToJs(["string"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Main$Elem) {
    var obj_ = {"instance": "Elem"};
    var obj_slot1 = Fay$$fayToJs(["string"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["list",[["user","Attr",[]]]],_(_obj.slot2));
    if (undefined !== obj_slot2) {
      obj_.slot2 = obj_slot2;
    }
    var obj_slot3 = Fay$$fayToJs(["list",[["user","Elem",[]]]],_(_obj.slot3));
    if (undefined !== obj_slot3) {
      obj_.slot3 = obj_slot3;
    }
    return obj_;
  }
  if (_obj instanceof $_Main$Attr) {
    var obj_ = {"instance": "Attr"};
    var obj_slot1 = Fay$$fayToJs(["string"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["string"],_(_obj.slot2));
    if (undefined !== obj_slot2) {
      obj_.slot2 = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_JQuery$DeepWithDataAndEvents) {
    var obj_ = {"instance": "DeepWithDataAndEvents"};
    return obj_;
  }
  if (_obj instanceof $_JQuery$WithDataAndEvents) {
    var obj_ = {"instance": "WithDataAndEvents"};
    return obj_;
  }
  if (_obj instanceof $_JQuery$WithoutDataAndEvents) {
    var obj_ = {"instance": "WithoutDataAndEvents"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Nothing) {
    var obj_ = {"instance": "Nothing"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Just) {
    var obj_ = {"instance": "Just"};
    var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$EQ) {
    var obj_ = {"instance": "EQ"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$LT) {
    var obj_ = {"instance": "LT"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$GT) {
    var obj_ = {"instance": "GT"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Right) {
    var obj_ = {"instance": "Right"};
    var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Left) {
    var obj_ = {"instance": "Left"};
    var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Defined) {
    var obj_ = {"instance": "Defined"};
    var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));
    if (undefined !== obj_slot1) {
      obj_.slot1 = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$Stdlib$Undefined) {
    var obj_ = {"instance": "Undefined"};
    return obj_;
  }
  return obj;
};
var Fay$$jsToFayUserDefined = function(type,obj){
  if (obj["instance"] === "CData") {
    return new $_Main$CData(Fay$$jsToFay(["string"],obj["slot1"]));
  }
  if (obj["instance"] === "Elem") {
    return new $_Main$Elem(Fay$$jsToFay(["string"],obj["slot1"]),Fay$$jsToFay(["list",[["user","Attr",[]]]],obj["slot2"]),Fay$$jsToFay(["list",[["user","Elem",[]]]],obj["slot3"]));
  }
  if (obj["instance"] === "Attr") {
    return new $_Main$Attr(Fay$$jsToFay(["string"],obj["slot1"]),Fay$$jsToFay(["string"],obj["slot2"]));
  }
  if (obj["instance"] === "DeepWithDataAndEvents") {
    return new $_JQuery$DeepWithDataAndEvents();
  }
  if (obj["instance"] === "WithDataAndEvents") {
    return new $_JQuery$WithDataAndEvents();
  }
  if (obj["instance"] === "WithoutDataAndEvents") {
    return new $_JQuery$WithoutDataAndEvents();
  }
  if (obj["instance"] === "Nothing") {
    return new $_Language$Fay$Stdlib$Nothing();
  }
  if (obj["instance"] === "Just") {
    return new $_Language$Fay$Stdlib$Just(Fay$$jsToFay(["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "EQ") {
    return new $_Language$Fay$Stdlib$EQ();
  }
  if (obj["instance"] === "LT") {
    return new $_Language$Fay$Stdlib$LT();
  }
  if (obj["instance"] === "GT") {
    return new $_Language$Fay$Stdlib$GT();
  }
  if (obj["instance"] === "Right") {
    return new $_Language$Fay$Stdlib$Right(Fay$$jsToFay(["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Left") {
    return new $_Language$Fay$Stdlib$Left(Fay$$jsToFay(["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Defined") {
    return new $_Language$Fay$Stdlib$Defined(Fay$$jsToFay(["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Undefined") {
    return new $_Language$Fay$Stdlib$Undefined();
  }
  return obj;
};

// Exports
this.Main$main = Main$main;
this.Main$CData = Main$CData;
this.Main$Elem = Main$Elem;
this.Main$Attr = Main$Attr;

// Built-ins
this._ = _;
this.$           = $;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new Main();
main._(main.Main$main);

