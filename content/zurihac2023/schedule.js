(() => {
  // output/Main/foreign.js
  var innerText = function(element) {
    return function() {
      return element.innerText;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map8 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map8($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure13 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply3(pure13(f))(a);
      };
    };
  };
  var applicativeArray = {
    pure: function(x) {
      return [x];
    },
    Apply0: function() {
      return applyArray;
    }
  };

  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count2) {
    return function(value12) {
      if (count2 < 1) {
        return [];
      }
      var result = new Array(count2);
      return result.fill(value12);
    };
  };
  var replicatePolyfill = function(count2) {
    return function(value12) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count2; i++) {
        result[n++] = value12;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count2 = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count2++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr2) {
      return function(xs) {
        return listToArray(foldr2(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var concat = function(xss) {
    if (xss.length <= 1e4) {
      return Array.prototype.concat.apply([], xss);
    }
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
      var xs = xss[i];
      for (var j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
  var sortByImpl = function() {
    function mergeFromTo(compare5, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare5, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare5, fromOrdering, xs2, xs1, mid, to);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare5(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare5) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare5, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label4) {
    return function(rec) {
      return rec[label4];
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind4 = bind(dictMonad.Bind1());
    var pure4 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind4(f)(function(f$prime) {
          return bind4(a)(function(a$prime) {
            return pure4(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq6) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq6 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a) {
      return sub1(zero2)(a);
    };
  };

  // output/Data.Ord/index.js
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var comparing = function(dictOrd) {
    var compare33 = compare(dictOrd);
    return function(f) {
      return function(x) {
        return function(y) {
          return compare33(f(x))(f(y));
        };
      };
    };
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare33 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare33(a1)(a2);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var lessThanOrEq = function(dictOrd) {
    var compare33 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare33(a1)(a2);
        if (v instanceof GT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var max = function(dictOrd) {
    var compare33 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare33(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare33 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare33(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };
  var abs = function(dictOrd) {
    var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
    return function(dictRing) {
      var zero2 = zero(dictRing.Semiring0());
      var negate1 = negate(dictRing);
      return function(x) {
        var $99 = greaterThanOrEq1(x)(zero2);
        if ($99) {
          return x;
        }
        ;
        return negate1(x);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showCharImpl = function(c) {
    var code = c.charCodeAt(0);
    if (code < 32 || code === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty4 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty4;
      }
    ) + '"';
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show14 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key = reflectSymbol2($$Proxy.value);
              var focus2 = unsafeGet(key)(record);
              return " " + (key + (": " + (show14(focus2) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show14 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key = reflectSymbol2($$Proxy.value);
            var focus2 = unsafeGet(key)(record);
            return " " + (key + (": " + (show14(focus2) + " ")));
          };
        }
      };
    };
  };

  // output/Data.Generic.Rep/index.js
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a) {
      return function(f) {
        return f(a);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go2(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare5, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare5, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare5, fromOrdering, xs2, xs1, mid, to);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare5(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare5) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare5, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var eq12 = /* @__PURE__ */ eq(eqOrdering);
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure4 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure4(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var minimumBy = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(cmp) {
      var min$prime = function(v) {
        return function(v1) {
          if (v instanceof Nothing) {
            return new Just(v1);
          }
          ;
          if (v instanceof Just) {
            return new Just(function() {
              var $307 = eq12(cmp(v.value0)(v1))(LT.value);
              if ($307) {
                return v.value0;
              }
              ;
              return v1;
            }());
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 454, column 3 - line 454, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return foldl22(min$prime)(Nothing.value);
    };
  };
  var minimum = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(dictFoldable) {
      return minimumBy(dictFoldable)(compare5);
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append3 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr2(function(x) {
          return function(acc) {
            return append3(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map8) {
        return function(pure4) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure4([]);
                  case 1:
                    return map8(array1)(f(array[bot]));
                  case 2:
                    return apply3(map8(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map8(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply3(map8(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value12 = b;
              while (true) {
                var maybe2 = f(value12);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value12 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value12 = b;
              while (true) {
                var tuple = f(value12);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value12 = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var take = function(n) {
    return function(xs) {
      var $149 = n < 1;
      if ($149) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 870, column 31 - line 873, column 11): " + [v.constructor.name]);
    });
  };
  var sortWith = function(dictOrd) {
    var comparing2 = comparing(dictOrd);
    return function(f) {
      return sortBy(comparing2(f));
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $191 = maybe([])(singleton2);
      return function($192) {
        return $191(f($192));
      };
    }());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Data.Date/foreign.js
  var createDate = function(y, m, d) {
    var date2 = new Date(Date.UTC(y, m, d));
    if (y >= 0 && y < 100) {
      date2.setUTCFullYear(y);
    }
    return date2;
  };
  function canonicalDateImpl(ctor, y, m, d) {
    var date2 = createDate(y, m - 1, d);
    return ctor(date2.getUTCFullYear())(date2.getUTCMonth() + 1)(date2.getUTCDate());
  }
  function calcWeekday(y, m, d) {
    return createDate(y, m - 1, d).getUTCDay();
  }

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum12 = toEnum(dictBoundedEnum);
    var fromEnum13 = fromEnum(dictBoundedEnum);
    var bottom23 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum12(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum13(bottom23);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.Date.Component/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Monday = /* @__PURE__ */ function() {
    function Monday2() {
    }
    ;
    Monday2.value = new Monday2();
    return Monday2;
  }();
  var Tuesday = /* @__PURE__ */ function() {
    function Tuesday2() {
    }
    ;
    Tuesday2.value = new Tuesday2();
    return Tuesday2;
  }();
  var Wednesday = /* @__PURE__ */ function() {
    function Wednesday2() {
    }
    ;
    Wednesday2.value = new Wednesday2();
    return Wednesday2;
  }();
  var Thursday = /* @__PURE__ */ function() {
    function Thursday2() {
    }
    ;
    Thursday2.value = new Thursday2();
    return Thursday2;
  }();
  var Friday = /* @__PURE__ */ function() {
    function Friday2() {
    }
    ;
    Friday2.value = new Friday2();
    return Friday2;
  }();
  var Saturday = /* @__PURE__ */ function() {
    function Saturday2() {
    }
    ;
    Saturday2.value = new Saturday2();
    return Saturday2;
  }();
  var Sunday = /* @__PURE__ */ function() {
    function Sunday2() {
    }
    ;
    Sunday2.value = new Sunday2();
    return Sunday2;
  }();
  var January = /* @__PURE__ */ function() {
    function January2() {
    }
    ;
    January2.value = new January2();
    return January2;
  }();
  var February = /* @__PURE__ */ function() {
    function February2() {
    }
    ;
    February2.value = new February2();
    return February2;
  }();
  var March = /* @__PURE__ */ function() {
    function March2() {
    }
    ;
    March2.value = new March2();
    return March2;
  }();
  var April = /* @__PURE__ */ function() {
    function April2() {
    }
    ;
    April2.value = new April2();
    return April2;
  }();
  var May = /* @__PURE__ */ function() {
    function May2() {
    }
    ;
    May2.value = new May2();
    return May2;
  }();
  var June = /* @__PURE__ */ function() {
    function June2() {
    }
    ;
    June2.value = new June2();
    return June2;
  }();
  var July = /* @__PURE__ */ function() {
    function July2() {
    }
    ;
    July2.value = new July2();
    return July2;
  }();
  var August = /* @__PURE__ */ function() {
    function August2() {
    }
    ;
    August2.value = new August2();
    return August2;
  }();
  var September = /* @__PURE__ */ function() {
    function September2() {
    }
    ;
    September2.value = new September2();
    return September2;
  }();
  var October = /* @__PURE__ */ function() {
    function October2() {
    }
    ;
    October2.value = new October2();
    return October2;
  }();
  var November = /* @__PURE__ */ function() {
    function November2() {
    }
    ;
    November2.value = new November2();
    return November2;
  }();
  var December = /* @__PURE__ */ function() {
    function December2() {
    }
    ;
    December2.value = new December2();
    return December2;
  }();
  var showWeekday = {
    show: function(v) {
      if (v instanceof Monday) {
        return "Monday";
      }
      ;
      if (v instanceof Tuesday) {
        return "Tuesday";
      }
      ;
      if (v instanceof Wednesday) {
        return "Wednesday";
      }
      ;
      if (v instanceof Thursday) {
        return "Thursday";
      }
      ;
      if (v instanceof Friday) {
        return "Friday";
      }
      ;
      if (v instanceof Saturday) {
        return "Saturday";
      }
      ;
      if (v instanceof Sunday) {
        return "Sunday";
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 184, column 1 - line 191, column 25): " + [v.constructor.name]);
    }
  };
  var showMonth = {
    show: function(v) {
      if (v instanceof January) {
        return "January";
      }
      ;
      if (v instanceof February) {
        return "February";
      }
      ;
      if (v instanceof March) {
        return "March";
      }
      ;
      if (v instanceof April) {
        return "April";
      }
      ;
      if (v instanceof May) {
        return "May";
      }
      ;
      if (v instanceof June) {
        return "June";
      }
      ;
      if (v instanceof July) {
        return "July";
      }
      ;
      if (v instanceof August) {
        return "August";
      }
      ;
      if (v instanceof September) {
        return "September";
      }
      ;
      if (v instanceof October) {
        return "October";
      }
      ;
      if (v instanceof November) {
        return "November";
      }
      ;
      if (v instanceof December) {
        return "December";
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 101, column 1 - line 113, column 29): " + [v.constructor.name]);
    }
  };
  var ordYear = ordInt;
  var ordDay = ordInt;
  var eqYear = eqInt;
  var eqWeekday = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Monday && y instanceof Monday) {
          return true;
        }
        ;
        if (x instanceof Tuesday && y instanceof Tuesday) {
          return true;
        }
        ;
        if (x instanceof Wednesday && y instanceof Wednesday) {
          return true;
        }
        ;
        if (x instanceof Thursday && y instanceof Thursday) {
          return true;
        }
        ;
        if (x instanceof Friday && y instanceof Friday) {
          return true;
        }
        ;
        if (x instanceof Saturday && y instanceof Saturday) {
          return true;
        }
        ;
        if (x instanceof Sunday && y instanceof Sunday) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordWeekday = {
    compare: function(x) {
      return function(y) {
        if (x instanceof Monday && y instanceof Monday) {
          return EQ.value;
        }
        ;
        if (x instanceof Monday) {
          return LT.value;
        }
        ;
        if (y instanceof Monday) {
          return GT.value;
        }
        ;
        if (x instanceof Tuesday && y instanceof Tuesday) {
          return EQ.value;
        }
        ;
        if (x instanceof Tuesday) {
          return LT.value;
        }
        ;
        if (y instanceof Tuesday) {
          return GT.value;
        }
        ;
        if (x instanceof Wednesday && y instanceof Wednesday) {
          return EQ.value;
        }
        ;
        if (x instanceof Wednesday) {
          return LT.value;
        }
        ;
        if (y instanceof Wednesday) {
          return GT.value;
        }
        ;
        if (x instanceof Thursday && y instanceof Thursday) {
          return EQ.value;
        }
        ;
        if (x instanceof Thursday) {
          return LT.value;
        }
        ;
        if (y instanceof Thursday) {
          return GT.value;
        }
        ;
        if (x instanceof Friday && y instanceof Friday) {
          return EQ.value;
        }
        ;
        if (x instanceof Friday) {
          return LT.value;
        }
        ;
        if (y instanceof Friday) {
          return GT.value;
        }
        ;
        if (x instanceof Saturday && y instanceof Saturday) {
          return EQ.value;
        }
        ;
        if (x instanceof Saturday) {
          return LT.value;
        }
        ;
        if (y instanceof Saturday) {
          return GT.value;
        }
        ;
        if (x instanceof Sunday && y instanceof Sunday) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqWeekday;
    }
  };
  var eqMonth = {
    eq: function(x) {
      return function(y) {
        if (x instanceof January && y instanceof January) {
          return true;
        }
        ;
        if (x instanceof February && y instanceof February) {
          return true;
        }
        ;
        if (x instanceof March && y instanceof March) {
          return true;
        }
        ;
        if (x instanceof April && y instanceof April) {
          return true;
        }
        ;
        if (x instanceof May && y instanceof May) {
          return true;
        }
        ;
        if (x instanceof June && y instanceof June) {
          return true;
        }
        ;
        if (x instanceof July && y instanceof July) {
          return true;
        }
        ;
        if (x instanceof August && y instanceof August) {
          return true;
        }
        ;
        if (x instanceof September && y instanceof September) {
          return true;
        }
        ;
        if (x instanceof October && y instanceof October) {
          return true;
        }
        ;
        if (x instanceof November && y instanceof November) {
          return true;
        }
        ;
        if (x instanceof December && y instanceof December) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordMonth = {
    compare: function(x) {
      return function(y) {
        if (x instanceof January && y instanceof January) {
          return EQ.value;
        }
        ;
        if (x instanceof January) {
          return LT.value;
        }
        ;
        if (y instanceof January) {
          return GT.value;
        }
        ;
        if (x instanceof February && y instanceof February) {
          return EQ.value;
        }
        ;
        if (x instanceof February) {
          return LT.value;
        }
        ;
        if (y instanceof February) {
          return GT.value;
        }
        ;
        if (x instanceof March && y instanceof March) {
          return EQ.value;
        }
        ;
        if (x instanceof March) {
          return LT.value;
        }
        ;
        if (y instanceof March) {
          return GT.value;
        }
        ;
        if (x instanceof April && y instanceof April) {
          return EQ.value;
        }
        ;
        if (x instanceof April) {
          return LT.value;
        }
        ;
        if (y instanceof April) {
          return GT.value;
        }
        ;
        if (x instanceof May && y instanceof May) {
          return EQ.value;
        }
        ;
        if (x instanceof May) {
          return LT.value;
        }
        ;
        if (y instanceof May) {
          return GT.value;
        }
        ;
        if (x instanceof June && y instanceof June) {
          return EQ.value;
        }
        ;
        if (x instanceof June) {
          return LT.value;
        }
        ;
        if (y instanceof June) {
          return GT.value;
        }
        ;
        if (x instanceof July && y instanceof July) {
          return EQ.value;
        }
        ;
        if (x instanceof July) {
          return LT.value;
        }
        ;
        if (y instanceof July) {
          return GT.value;
        }
        ;
        if (x instanceof August && y instanceof August) {
          return EQ.value;
        }
        ;
        if (x instanceof August) {
          return LT.value;
        }
        ;
        if (y instanceof August) {
          return GT.value;
        }
        ;
        if (x instanceof September && y instanceof September) {
          return EQ.value;
        }
        ;
        if (x instanceof September) {
          return LT.value;
        }
        ;
        if (y instanceof September) {
          return GT.value;
        }
        ;
        if (x instanceof October && y instanceof October) {
          return EQ.value;
        }
        ;
        if (x instanceof October) {
          return LT.value;
        }
        ;
        if (y instanceof October) {
          return GT.value;
        }
        ;
        if (x instanceof November && y instanceof November) {
          return EQ.value;
        }
        ;
        if (x instanceof November) {
          return LT.value;
        }
        ;
        if (y instanceof November) {
          return GT.value;
        }
        ;
        if (x instanceof December && y instanceof December) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqMonth;
    }
  };
  var eqDay = eqInt;
  var boundedYear = /* @__PURE__ */ function() {
    return {
      bottom: -271820 | 0,
      top: 275759,
      Ord0: function() {
        return ordYear;
      }
    };
  }();
  var boundedWeekday = /* @__PURE__ */ function() {
    return {
      bottom: Monday.value,
      top: Sunday.value,
      Ord0: function() {
        return ordWeekday;
      }
    };
  }();
  var boundedMonth = /* @__PURE__ */ function() {
    return {
      bottom: January.value,
      top: December.value,
      Ord0: function() {
        return ordMonth;
      }
    };
  }();
  var boundedEnumYear = {
    cardinality: 547580,
    toEnum: function(n) {
      if (n >= (-271820 | 0) && n <= 275759) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedYear;
    },
    Enum1: function() {
      return $lazy_enumYear(0);
    }
  };
  var $lazy_enumYear = /* @__PURE__ */ $runtime_lazy2("enumYear", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $55 = toEnum(boundedEnumYear);
        var $56 = fromEnum(boundedEnumYear);
        return function($57) {
          return $55(function(v) {
            return v + 1 | 0;
          }($56($57)));
        };
      }(),
      pred: function() {
        var $58 = toEnum(boundedEnumYear);
        var $59 = fromEnum(boundedEnumYear);
        return function($60) {
          return $58(function(v) {
            return v - 1 | 0;
          }($59($60)));
        };
      }(),
      Ord0: function() {
        return ordYear;
      }
    };
  });
  var boundedEnumWeekday = {
    cardinality: 7,
    toEnum: function(v) {
      if (v === 1) {
        return new Just(Monday.value);
      }
      ;
      if (v === 2) {
        return new Just(Tuesday.value);
      }
      ;
      if (v === 3) {
        return new Just(Wednesday.value);
      }
      ;
      if (v === 4) {
        return new Just(Thursday.value);
      }
      ;
      if (v === 5) {
        return new Just(Friday.value);
      }
      ;
      if (v === 6) {
        return new Just(Saturday.value);
      }
      ;
      if (v === 7) {
        return new Just(Sunday.value);
      }
      ;
      return Nothing.value;
    },
    fromEnum: function(v) {
      if (v instanceof Monday) {
        return 1;
      }
      ;
      if (v instanceof Tuesday) {
        return 2;
      }
      ;
      if (v instanceof Wednesday) {
        return 3;
      }
      ;
      if (v instanceof Thursday) {
        return 4;
      }
      ;
      if (v instanceof Friday) {
        return 5;
      }
      ;
      if (v instanceof Saturday) {
        return 6;
      }
      ;
      if (v instanceof Sunday) {
        return 7;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 175, column 14 - line 182, column 16): " + [v.constructor.name]);
    },
    Bounded0: function() {
      return boundedWeekday;
    },
    Enum1: function() {
      return $lazy_enumWeekday(0);
    }
  };
  var $lazy_enumWeekday = /* @__PURE__ */ $runtime_lazy2("enumWeekday", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $61 = toEnum(boundedEnumWeekday);
        var $62 = fromEnum(boundedEnumWeekday);
        return function($63) {
          return $61(function(v) {
            return v + 1 | 0;
          }($62($63)));
        };
      }(),
      pred: function() {
        var $64 = toEnum(boundedEnumWeekday);
        var $65 = fromEnum(boundedEnumWeekday);
        return function($66) {
          return $64(function(v) {
            return v - 1 | 0;
          }($65($66)));
        };
      }(),
      Ord0: function() {
        return ordWeekday;
      }
    };
  });
  var boundedEnumMonth = {
    cardinality: 12,
    toEnum: function(v) {
      if (v === 1) {
        return new Just(January.value);
      }
      ;
      if (v === 2) {
        return new Just(February.value);
      }
      ;
      if (v === 3) {
        return new Just(March.value);
      }
      ;
      if (v === 4) {
        return new Just(April.value);
      }
      ;
      if (v === 5) {
        return new Just(May.value);
      }
      ;
      if (v === 6) {
        return new Just(June.value);
      }
      ;
      if (v === 7) {
        return new Just(July.value);
      }
      ;
      if (v === 8) {
        return new Just(August.value);
      }
      ;
      if (v === 9) {
        return new Just(September.value);
      }
      ;
      if (v === 10) {
        return new Just(October.value);
      }
      ;
      if (v === 11) {
        return new Just(November.value);
      }
      ;
      if (v === 12) {
        return new Just(December.value);
      }
      ;
      return Nothing.value;
    },
    fromEnum: function(v) {
      if (v instanceof January) {
        return 1;
      }
      ;
      if (v instanceof February) {
        return 2;
      }
      ;
      if (v instanceof March) {
        return 3;
      }
      ;
      if (v instanceof April) {
        return 4;
      }
      ;
      if (v instanceof May) {
        return 5;
      }
      ;
      if (v instanceof June) {
        return 6;
      }
      ;
      if (v instanceof July) {
        return 7;
      }
      ;
      if (v instanceof August) {
        return 8;
      }
      ;
      if (v instanceof September) {
        return 9;
      }
      ;
      if (v instanceof October) {
        return 10;
      }
      ;
      if (v instanceof November) {
        return 11;
      }
      ;
      if (v instanceof December) {
        return 12;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
    },
    Bounded0: function() {
      return boundedMonth;
    },
    Enum1: function() {
      return $lazy_enumMonth(0);
    }
  };
  var $lazy_enumMonth = /* @__PURE__ */ $runtime_lazy2("enumMonth", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $67 = toEnum(boundedEnumMonth);
        var $68 = fromEnum(boundedEnumMonth);
        return function($69) {
          return $67(function(v) {
            return v + 1 | 0;
          }($68($69)));
        };
      }(),
      pred: function() {
        var $70 = toEnum(boundedEnumMonth);
        var $71 = fromEnum(boundedEnumMonth);
        return function($72) {
          return $70(function(v) {
            return v - 1 | 0;
          }($71($72)));
        };
      }(),
      Ord0: function() {
        return ordMonth;
      }
    };
  });
  var boundedDay = {
    bottom: 1,
    top: 31,
    Ord0: function() {
      return ordDay;
    }
  };
  var boundedEnumDay = {
    cardinality: 31,
    toEnum: function(n) {
      if (n >= 1 && n <= 31) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedDay;
    },
    Enum1: function() {
      return $lazy_enumDay(0);
    }
  };
  var $lazy_enumDay = /* @__PURE__ */ $runtime_lazy2("enumDay", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $73 = toEnum(boundedEnumDay);
        var $74 = fromEnum(boundedEnumDay);
        return function($75) {
          return $73(function(v) {
            return v + 1 | 0;
          }($74($75)));
        };
      }(),
      pred: function() {
        var $76 = toEnum(boundedEnumDay);
        var $77 = fromEnum(boundedEnumDay);
        return function($78) {
          return $76(function(v) {
            return v - 1 | 0;
          }($77($78)));
        };
      }(),
      Ord0: function() {
        return ordDay;
      }
    };
  });

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  function fromStringImpl(str, isFinite2, just, nothing) {
    var num = parseFloat(str);
    if (isFinite2(num)) {
      return just(num);
    } else {
      return nothing;
    }
  }
  var floor = Math.floor;

  // output/Data.Number/index.js
  var fromString = function(str) {
    return fromStringImpl(str, isFiniteImpl, Just.create, Nothing.value);
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Data.Date/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumWeekday);
  var eq13 = /* @__PURE__ */ eq(eqYear);
  var eq2 = /* @__PURE__ */ eq(eqMonth);
  var eq3 = /* @__PURE__ */ eq(eqDay);
  var compare2 = /* @__PURE__ */ compare(ordYear);
  var compare12 = /* @__PURE__ */ compare(ordMonth);
  var compare22 = /* @__PURE__ */ compare(ordDay);
  var toEnum22 = /* @__PURE__ */ toEnum(boundedEnumMonth);
  var $$Date = /* @__PURE__ */ function() {
    function $$Date2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    $$Date2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new $$Date2(value0, value1, value22);
        };
      };
    };
    return $$Date2;
  }();
  var year = function(v) {
    return v.value0;
  };
  var weekday = function(v) {
    var n = calcWeekday(v.value0, fromEnum2(v.value1), v.value2);
    var $86 = n === 0;
    if ($86) {
      return fromJust4(toEnum2(7));
    }
    ;
    return fromJust4(toEnum2(n));
  };
  var month = function(v) {
    return v.value1;
  };
  var eqDate = {
    eq: function(x) {
      return function(y) {
        return eq13(x.value0)(y.value0) && eq2(x.value1)(y.value1) && eq3(x.value2)(y.value2);
      };
    }
  };
  var eq4 = /* @__PURE__ */ eq(eqDate);
  var ordDate = {
    compare: function(x) {
      return function(y) {
        var v = compare2(x.value0)(y.value0);
        if (v instanceof LT) {
          return LT.value;
        }
        ;
        if (v instanceof GT) {
          return GT.value;
        }
        ;
        var v1 = compare12(x.value1)(y.value1);
        if (v1 instanceof LT) {
          return LT.value;
        }
        ;
        if (v1 instanceof GT) {
          return GT.value;
        }
        ;
        return compare22(x.value2)(y.value2);
      };
    },
    Eq0: function() {
      return eqDate;
    }
  };
  var day = function(v) {
    return v.value2;
  };
  var canonicalDate = function(y) {
    return function(m) {
      return function(d) {
        var mkDate = function(y$prime) {
          return function(m$prime) {
            return function(d$prime) {
              return new $$Date(y$prime, fromJust4(toEnum22(m$prime)), d$prime);
            };
          };
        };
        return canonicalDateImpl(mkDate, y, fromEnum2(m), d);
      };
    };
  };
  var exactDate = function(y) {
    return function(m) {
      return function(d) {
        var dt = new $$Date(y, m, d);
        var $144 = eq4(canonicalDate(y)(m)(d))(dt);
        if ($144) {
          return new Just(dt);
        }
        ;
        return Nothing.value;
      };
    };
  };

  // output/Data.Time.Component/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var ordSecond = ordInt;
  var ordMinute = ordInt;
  var ordMillisecond = ordInt;
  var ordHour = ordInt;
  var eqSecond = eqInt;
  var eqMinute = eqInt;
  var eqMillisecond = eqInt;
  var eqHour = eqInt;
  var boundedSecond = {
    bottom: 0,
    top: 59,
    Ord0: function() {
      return ordSecond;
    }
  };
  var boundedMinute = {
    bottom: 0,
    top: 59,
    Ord0: function() {
      return ordMinute;
    }
  };
  var boundedMillisecond = {
    bottom: 0,
    top: 999,
    Ord0: function() {
      return ordMillisecond;
    }
  };
  var boundedHour = {
    bottom: 0,
    top: 23,
    Ord0: function() {
      return ordHour;
    }
  };
  var boundedEnumSecond = {
    cardinality: 60,
    toEnum: function(n) {
      if (n >= 0 && n <= 59) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedSecond;
    },
    Enum1: function() {
      return $lazy_enumSecond(0);
    }
  };
  var $lazy_enumSecond = /* @__PURE__ */ $runtime_lazy3("enumSecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $36 = toEnum(boundedEnumSecond);
        var $37 = fromEnum(boundedEnumSecond);
        return function($38) {
          return $36(function(v) {
            return v + 1 | 0;
          }($37($38)));
        };
      }(),
      pred: function() {
        var $39 = toEnum(boundedEnumSecond);
        var $40 = fromEnum(boundedEnumSecond);
        return function($41) {
          return $39(function(v) {
            return v - 1 | 0;
          }($40($41)));
        };
      }(),
      Ord0: function() {
        return ordSecond;
      }
    };
  });
  var boundedEnumMinute = {
    cardinality: 60,
    toEnum: function(n) {
      if (n >= 0 && n <= 59) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedMinute;
    },
    Enum1: function() {
      return $lazy_enumMinute(0);
    }
  };
  var $lazy_enumMinute = /* @__PURE__ */ $runtime_lazy3("enumMinute", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $42 = toEnum(boundedEnumMinute);
        var $43 = fromEnum(boundedEnumMinute);
        return function($44) {
          return $42(function(v) {
            return v + 1 | 0;
          }($43($44)));
        };
      }(),
      pred: function() {
        var $45 = toEnum(boundedEnumMinute);
        var $46 = fromEnum(boundedEnumMinute);
        return function($47) {
          return $45(function(v) {
            return v - 1 | 0;
          }($46($47)));
        };
      }(),
      Ord0: function() {
        return ordMinute;
      }
    };
  });
  var boundedEnumMillisecond = {
    cardinality: 1e3,
    toEnum: function(n) {
      if (n >= 0 && n <= 999) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedMillisecond;
    },
    Enum1: function() {
      return $lazy_enumMillisecond(0);
    }
  };
  var $lazy_enumMillisecond = /* @__PURE__ */ $runtime_lazy3("enumMillisecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $48 = toEnum(boundedEnumMillisecond);
        var $49 = fromEnum(boundedEnumMillisecond);
        return function($50) {
          return $48(function(v) {
            return v + 1 | 0;
          }($49($50)));
        };
      }(),
      pred: function() {
        var $51 = toEnum(boundedEnumMillisecond);
        var $52 = fromEnum(boundedEnumMillisecond);
        return function($53) {
          return $51(function(v) {
            return v - 1 | 0;
          }($52($53)));
        };
      }(),
      Ord0: function() {
        return ordMillisecond;
      }
    };
  });
  var boundedEnumHour = {
    cardinality: 24,
    toEnum: function(n) {
      if (n >= 0 && n <= 23) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedHour;
    },
    Enum1: function() {
      return $lazy_enumHour(0);
    }
  };
  var $lazy_enumHour = /* @__PURE__ */ $runtime_lazy3("enumHour", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $54 = toEnum(boundedEnumHour);
        var $55 = fromEnum(boundedEnumHour);
        return function($56) {
          return $54(function(v) {
            return v + 1 | 0;
          }($55($56)));
        };
      }(),
      pred: function() {
        var $57 = toEnum(boundedEnumHour);
        var $58 = fromEnum(boundedEnumHour);
        return function($59) {
          return $57(function(v) {
            return v - 1 | 0;
          }($58($59)));
        };
      }(),
      Ord0: function() {
        return ordHour;
      }
    };
  });

  // output/Data.Time/index.js
  var eq5 = /* @__PURE__ */ eq(eqHour);
  var eq14 = /* @__PURE__ */ eq(eqMinute);
  var eq22 = /* @__PURE__ */ eq(eqSecond);
  var eq32 = /* @__PURE__ */ eq(eqMillisecond);
  var compare3 = /* @__PURE__ */ compare(ordHour);
  var compare13 = /* @__PURE__ */ compare(ordMinute);
  var compare23 = /* @__PURE__ */ compare(ordSecond);
  var compare32 = /* @__PURE__ */ compare(ordMillisecond);
  var Time = /* @__PURE__ */ function() {
    function Time2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Time2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Time2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Time2;
  }();
  var second = function(v) {
    return v.value2;
  };
  var minute = function(v) {
    return v.value1;
  };
  var millisecond = function(v) {
    return v.value3;
  };
  var hour = function(v) {
    return v.value0;
  };
  var eqTime = {
    eq: function(x) {
      return function(y) {
        return eq5(x.value0)(y.value0) && eq14(x.value1)(y.value1) && eq22(x.value2)(y.value2) && eq32(x.value3)(y.value3);
      };
    }
  };
  var ordTime = {
    compare: function(x) {
      return function(y) {
        var v = compare3(x.value0)(y.value0);
        if (v instanceof LT) {
          return LT.value;
        }
        ;
        if (v instanceof GT) {
          return GT.value;
        }
        ;
        var v1 = compare13(x.value1)(y.value1);
        if (v1 instanceof LT) {
          return LT.value;
        }
        ;
        if (v1 instanceof GT) {
          return GT.value;
        }
        ;
        var v2 = compare23(x.value2)(y.value2);
        if (v2 instanceof LT) {
          return LT.value;
        }
        ;
        if (v2 instanceof GT) {
          return GT.value;
        }
        ;
        return compare32(x.value3)(y.value3);
      };
    },
    Eq0: function() {
      return eqTime;
    }
  };

  // output/Data.DateTime/index.js
  var DateTime = /* @__PURE__ */ function() {
    function DateTime2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DateTime2.create = function(value0) {
      return function(value1) {
        return new DateTime2(value0, value1);
      };
    };
    return DateTime2;
  }();
  var time = function(v) {
    return v.value1;
  };
  var date = function(v) {
    return v.value0;
  };

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index4) {
            return function(str) {
              var length10 = str.length;
              if (index4 < 0 || index4 >= length10)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i = index4; ; --i) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index4)(str);
            };
          };
        };
      };
    };
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i = 0; i < n; ++i) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a) {
    return a.join("");
  };
  var singleton3 = function(c) {
    return c;
  };
  var length3 = function(s) {
    return s.length;
  };
  var drop = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.Common/foreign.js
  var trim = function(s) {
    return s.trim();
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map4 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare4 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum3(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum3(charAt(1)(s));
    var cu0 = fromEnum3(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map4(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum3(charAt(0)(s));
    var $47 = isLead(cu0) && length3(s) > 1;
    if ($47) {
      var cu1 = fromEnum3(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length4 = function($74) {
    return length(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton3($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton4 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons(v1);
      if (v2 instanceof Just) {
        return singleton4(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take3 = /* @__PURE__ */ _take(takeFallback);
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare4(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var drop2 = function(n) {
    return function(s) {
      return drop(length3(take3(n)(s)))(s);
    };
  };
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum3($77));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons(s);
        if (v instanceof Just) {
          var $66 = n === 0;
          if ($66) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy4("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var isDecDigit = function(c) {
    var diff2 = fromEnum4(c) - toCharCode2("0") | 0;
    return diff2 <= 9 && diff2 >= 0;
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate2 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsArgument = function(dictShow) {
    var show5 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show5(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate2(" ")(append2([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShow = function(dictGeneric) {
    var from3 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x) {
        return genericShow$prime1(from3(x));
      };
    };
  };

  // output/Parsing/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var show2 = /* @__PURE__ */ show(showString);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ParseState2(value0, value1, value22);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift2(value0) {
      this.value0 = value0;
    }
    ;
    Lift2.create = function(value0) {
      return new Lift2(value0);
    };
    return Lift2;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var genericPosition_ = {
    to: function(x) {
      return x;
    },
    from: function(x) {
      return x;
    }
  };
  var genericShow2 = /* @__PURE__ */ genericShow(genericPosition_)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "column";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "index";
    }
  })(/* @__PURE__ */ showRecordFieldsConsNil({
    reflectSymbol: function() {
      return "line";
    }
  })(showInt))(showInt))(showInt))))({
    reflectSymbol: function() {
      return "Position";
    }
  }));
  var showPosition = {
    show: function(x) {
      return genericShow2(x);
    }
  };
  var show1 = /* @__PURE__ */ show(showPosition);
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a) {
              return more(function(v2) {
                return done(state22, f(a));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state22, f) {
              return more(function(v3) {
                return v1(state22, more, lift1, $$throw2, function(state3, a) {
                  return more(function(v4) {
                    return done(state3, f(a));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a) {
              return more(function(v2) {
                var v3 = next(a);
                return v3(state22, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadRecParserT = {
    tailRecM: function(next) {
      return function(initArg) {
        return function(state1, more, lift1, $$throw2, done) {
          var $lazy_loop = $runtime_lazy5("loop", "Parsing", function() {
            return function(state22, arg, gas) {
              var v = next(arg);
              return v(state22, more, lift1, $$throw2, function(state3, step3) {
                if (step3 instanceof Loop) {
                  var $206 = gas === 0;
                  if ($206) {
                    return more(function(v1) {
                      return $lazy_loop(277)(state3, step3.value0, 30);
                    });
                  }
                  ;
                  return $lazy_loop(279)(state3, step3.value0, gas - 1 | 0);
                }
                ;
                if (step3 instanceof Done) {
                  return done(state3, step3.value0);
                }
                ;
                throw new Error("Failed pattern match at Parsing (line 273, column 39 - line 281, column 43): " + [step3.constructor.name]);
              });
            };
          });
          var loop2 = $lazy_loop(270);
          return loop2(state1, initArg, 30);
        };
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError2 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k) {
    return function(state1, v, v1, v2, done) {
      var v3 = k(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var showParseError = {
    show: function(v) {
      return "(ParseError " + (show2(v.value0) + (" " + (show1(v.value1) + ")")));
    }
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map8 = map(Monad0.Bind1().Apply0().Functor0());
    var pure13 = pure(Monad0.Applicative0());
    var tailRecM3 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go2 = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step3) {
            var v1 = step3(unit);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift) {
              $tco_done = true;
              return map8(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure13(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM3(go2)(function(v1) {
          return v(state1, More.create, Lift.create, function(state22, err) {
            return new Stop(state22, new Left(err));
          }, function(state22, res) {
            return new Stop(state22, new Right(res));
          });
        });
      };
    };
  };
  var position = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map8 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p) {
        var initialState = new ParseState(s, initialPos, false);
        return map8(fst)(runParserT$prime1(initialState)(p));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $281 = runParserT1(s);
    return function($282) {
      return unwrap2($281($282));
    };
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError2(new ParseError(message2, pos));
    };
  };
  var fail = function(message2) {
    return bindFlipped2(failWithPosition(message2))(position);
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };

  // output/Data.List/index.js
  var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var manyRec = function(dictMonadRec) {
    var bind13 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM3 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt6 = alt(Alt0);
      var map12 = map(Alt0.Functor0());
      var pure4 = pure(dictAlternative.Applicative0());
      return function(p) {
        var go2 = function(acc) {
          return bind13(alt6(map12(Loop.create)(p))(pure4(new Done(unit))))(function(aa) {
            return pure4(bimap2(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse2(acc);
            })(aa));
          });
        };
        return tailRecM3(go2)(Nil.value);
      };
    };
  };
  var fromFoldable = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };

  // output/Data.List.NonEmpty/index.js
  var cons$prime = function(x) {
    return function(xs) {
      return new NonEmpty(x, xs);
    };
  };

  // output/Parsing.Combinators/index.js
  var alt2 = /* @__PURE__ */ alt(altParserT);
  var pure2 = /* @__PURE__ */ pure(applicativeParserT);
  var map5 = /* @__PURE__ */ map(functorParserT);
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var apply2 = /* @__PURE__ */ apply(applyParserT);
  var empty2 = /* @__PURE__ */ empty(plusParserT);
  var withErrorMessage = function(p) {
    return function(msg) {
      return alt2(p)(fail("Expected " + msg));
    };
  };
  var option = function(a) {
    return function(p) {
      return alt2(p)(pure2(a));
    };
  };
  var many1 = function(p) {
    return apply2(map5(cons$prime)(p))(manyRec2(p));
  };
  var choice = function(dictFoldable) {
    var go2 = function(p1) {
      return function(v) {
        if (v instanceof Nothing) {
          return new Just(p1);
        }
        ;
        if (v instanceof Just) {
          return new Just(alt2(p1)(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Combinators (line 358, column 11 - line 360, column 32): " + [v.constructor.name]);
      };
    };
    var $95 = fromMaybe(empty2);
    var $96 = foldr(dictFoldable)(go2)(Nothing.value);
    return function($97) {
      return $95($96($97));
    };
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply3) {
      return function(map8) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply3(map8(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map8(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map8(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Function.Uncurried/foreign.js
  var mkFn5 = function(fn) {
    return function(a, b, c, d, e) {
      return fn(a)(b)(c)(d)(e);
    };
  };

  // output/Parsing.String/index.js
  var fromEnum5 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toEnum3 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show22 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum5(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum5(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum5(v3.value0.head);
                var $85 = cp < 0 || cp > 65535;
                if ($85) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust5(toEnum3(cp));
                var $86 = f(ch);
                if ($86) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var $$char = function(c) {
    return withErrorMessage(satisfy(function(v) {
      return v === c;
    }))(show22(c));
  };

  // output/Data.DateTime.Parsing/index.js
  var bind2 = /* @__PURE__ */ bind(bindMaybe);
  var toEnum1 = /* @__PURE__ */ toEnum(boundedEnumMillisecond);
  var bind1 = /* @__PURE__ */ bind(bindParserT);
  var choice2 = /* @__PURE__ */ choice(foldableArray);
  var pure1 = /* @__PURE__ */ pure(applicativeParserT);
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList)(monoidArray);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeParserT);
  var Plus = /* @__PURE__ */ function() {
    function Plus2() {
    }
    ;
    Plus2.value = new Plus2();
    return Plus2;
  }();
  var Minus = /* @__PURE__ */ function() {
    function Minus2() {
    }
    ;
    Minus2.value = new Minus2();
    return Minus2;
  }();
  var Zulu = /* @__PURE__ */ function() {
    function Zulu2() {
    }
    ;
    Zulu2.value = new Zulu2();
    return Zulu2;
  }();
  var Offset = /* @__PURE__ */ function() {
    function Offset2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Offset2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Offset2(value0, value1, value22);
        };
      };
    };
    return Offset2;
  }();
  var FullDateTime = /* @__PURE__ */ function() {
    function FullDateTime2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    FullDateTime2.create = function(value0) {
      return function(value1) {
        return new FullDateTime2(value0, value1);
      };
    };
    return FullDateTime2;
  }();
  var parseZuluOffset = function(dictMonad) {
    return bind1(choice2([$$char("Z"), $$char("z")]))(function() {
      return pure1(Zulu.value);
    });
  };
  var parseDirection = function(dictMonad) {
    return bind1(choice2([$$char("+"), $$char("-")]))(function(direction) {
      if (direction === "+") {
        return pure1(Plus.value);
      }
      ;
      if (direction === "-") {
        return pure1(Minus.value);
      }
      ;
      return fail("Bad direction");
    });
  };
  var maybeFail = function(dictMonad) {
    return function(str) {
      return maybe(fail(str))(pure1);
    };
  };
  var digit = function(dictMonad) {
    return satisfy(function($146) {
      return isDecDigit(codePointFromChar($146));
    });
  };
  var parseSecondFraction = function(dictMonad) {
    var digit1 = digit(dictMonad);
    var maybeFail1 = maybeFail(dictMonad);
    return bind1($$char("."))(function() {
      return bind1(many1(digit1))(function(digits) {
        var arrayOfDigits = concat(function(x) {
          return [["0", "."], x];
        }(take(3)(foldMap2(singleton2)(toList(digits)))));
        var maybeN = fromString(fromCharArray(arrayOfDigits));
        var millisecond2 = bind2(maybeN)(function(n) {
          return toEnum1(floor2(n * 1e3));
        });
        return maybeFail1("bad millisecond")(millisecond2);
      });
    });
  };
  var count = function(dictMonad) {
    return function(n) {
      return function(p) {
        return sequence2(replicate(n)(p));
      };
    };
  };
  var parseDigits = function(dictMonad) {
    var count1 = count(dictMonad);
    var digit1 = digit(dictMonad);
    return function(n) {
      return bind1(count1(n)(digit1))(function(digits) {
        return maybe(fail("bad number"))(function($147) {
          return pure1(floor2($147));
        })(fromString(fromCharArray(digits)));
      });
    };
  };
  var parseDateTimeSegment = function(dictMonad) {
    var parseDigits1 = parseDigits(dictMonad);
    var maybeFail1 = maybeFail(dictMonad);
    return function(dictBoundedEnum) {
      var toEnum4 = toEnum(dictBoundedEnum);
      return function(errorMessage) {
        return function(n) {
          return bind1(parseDigits1(n))(function(digits) {
            return maybeFail1(errorMessage)(toEnum4(digits));
          });
        };
      };
    };
  };
  var parseDay = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumDay)("bad day")(2);
  };
  var parseHour = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumHour)("bad hour")(2);
  };
  var parseMinute = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumMinute)("bad minute")(2);
  };
  var parseNumOffset = function(dictMonad) {
    var parseHour1 = parseHour(dictMonad);
    var parseMinute1 = parseMinute(dictMonad);
    return bind1(parseDirection(dictMonad))(function(direction) {
      return bind1(parseHour1)(function(hour2) {
        return bind1($$char(":"))(function() {
          return bind1(parseMinute1)(function(minute2) {
            return pure1(new Offset(direction, hour2, minute2));
          });
        });
      });
    });
  };
  var parseOffset = function(dictMonad) {
    return choice2([parseZuluOffset(dictMonad), parseNumOffset(dictMonad)]);
  };
  var parseMonth = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumMonth)("bad month")(2);
  };
  var parseSecond = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumSecond)("bad second")(2);
  };
  var parsePartialTime = function(dictMonad) {
    var parseMinute1 = parseMinute(dictMonad);
    var parseSecond1 = parseSecond(dictMonad);
    var maybeFail1 = maybeFail(dictMonad);
    var parseSecondFraction1 = parseSecondFraction(dictMonad);
    return bind1(parseHour(dictMonad))(function(hh) {
      return bind1($$char(":"))(function() {
        return bind1(parseMinute1)(function(mm) {
          return bind1($$char(":"))(function() {
            return bind1(parseSecond1)(function(ss) {
              return bind1(maybeFail1("bad default millisecond")(toEnum1(0)))(function(default_mss) {
                return bind1(option(default_mss)(parseSecondFraction1))(function(mss) {
                  return pure1(new Time(hh, mm, ss, mss));
                });
              });
            });
          });
        });
      });
    });
  };
  var parseYear = function(dictMonad) {
    return parseDateTimeSegment(dictMonad)(boundedEnumYear)("bad year")(4);
  };
  var parseDate = function(dictMonad) {
    var parseMonth1 = parseMonth(dictMonad);
    var parseDay1 = parseDay(dictMonad);
    var maybeFail1 = maybeFail(dictMonad);
    return bind1(parseYear(dictMonad))(function(year2) {
      return bind1($$char("-"))(function() {
        return bind1(parseMonth1)(function(month2) {
          return bind1($$char("-"))(function() {
            return bind1(parseDay1)(function(day2) {
              return maybeFail1("bad date")(exactDate(year2)(month2)(day2));
            });
          });
        });
      });
    });
  };
  var parseFullDateTime = function(dictMonad) {
    var parsePartialTime1 = parsePartialTime(dictMonad);
    var parseOffset1 = parseOffset(dictMonad);
    return bind1(parseDate(dictMonad))(function(date2) {
      return bind1(choice2([$$char("T"), $$char("t")]))(function() {
        return bind1(parsePartialTime1)(function(time2) {
          return bind1(parseOffset1)(function(offset) {
            return pure1(new FullDateTime(new DateTime(date2, time2), offset));
          });
        });
      });
    });
  };
  var parseFullDateTime1 = /* @__PURE__ */ parseFullDateTime(monadIdentity);
  var fromString2 = function(s) {
    return runParser(s)(parseFullDateTime1);
  };

  // output/Data.DateTime.Instant/foreign.js
  var createDateTime = function(y, m, d, h, mi, s, ms) {
    var dateTime3 = new Date(Date.UTC(y, m, d, h, mi, s, ms));
    if (y >= 0 && y < 100) {
      dateTime3.setUTCFullYear(y);
    }
    return dateTime3;
  };
  function fromDateTimeImpl(y, mo, d, h, mi, s, ms) {
    return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
  }

  // output/Data.DateTime.Instant/index.js
  var fromEnum6 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var unInstant = function(v) {
    return v;
  };
  var fromDateTime = function(v) {
    return fromDateTimeImpl(year(v.value0), fromEnum6(month(v.value0)), day(v.value0), hour(v.value1), minute(v.value1), second(v.value1), millisecond(v.value1));
  };

  // output/Data.Formatter.DateTime/index.js
  var show3 = /* @__PURE__ */ show(showInt);
  var foldMap3 = /* @__PURE__ */ foldMap(foldableList);
  var foldMap12 = /* @__PURE__ */ foldMap3(monoidString);
  var abs3 = /* @__PURE__ */ abs(ordInt)(ringInt);
  var fromEnum7 = /* @__PURE__ */ fromEnum(boundedEnumYear);
  var show12 = /* @__PURE__ */ show(showMonth);
  var fromEnum1 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromEnum22 = /* @__PURE__ */ fromEnum(boundedEnumDay);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var fromEnum32 = /* @__PURE__ */ fromEnum(boundedEnumWeekday);
  var show23 = /* @__PURE__ */ show(showWeekday);
  var fromEnum42 = /* @__PURE__ */ fromEnum(boundedEnumHour);
  var mod4 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromEnum52 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
  var fromEnum62 = /* @__PURE__ */ fromEnum(boundedEnumSecond);
  var fromEnum72 = /* @__PURE__ */ fromEnum(boundedEnumMillisecond);
  var div1 = /* @__PURE__ */ div(euclideanRingInt);
  var YearFull = /* @__PURE__ */ function() {
    function YearFull2() {
    }
    ;
    YearFull2.value = new YearFull2();
    return YearFull2;
  }();
  var YearTwoDigits = /* @__PURE__ */ function() {
    function YearTwoDigits2() {
    }
    ;
    YearTwoDigits2.value = new YearTwoDigits2();
    return YearTwoDigits2;
  }();
  var YearAbsolute = /* @__PURE__ */ function() {
    function YearAbsolute2() {
    }
    ;
    YearAbsolute2.value = new YearAbsolute2();
    return YearAbsolute2;
  }();
  var MonthFull = /* @__PURE__ */ function() {
    function MonthFull2() {
    }
    ;
    MonthFull2.value = new MonthFull2();
    return MonthFull2;
  }();
  var MonthShort = /* @__PURE__ */ function() {
    function MonthShort2() {
    }
    ;
    MonthShort2.value = new MonthShort2();
    return MonthShort2;
  }();
  var MonthTwoDigits = /* @__PURE__ */ function() {
    function MonthTwoDigits2() {
    }
    ;
    MonthTwoDigits2.value = new MonthTwoDigits2();
    return MonthTwoDigits2;
  }();
  var DayOfMonthTwoDigits = /* @__PURE__ */ function() {
    function DayOfMonthTwoDigits2() {
    }
    ;
    DayOfMonthTwoDigits2.value = new DayOfMonthTwoDigits2();
    return DayOfMonthTwoDigits2;
  }();
  var DayOfMonth = /* @__PURE__ */ function() {
    function DayOfMonth2() {
    }
    ;
    DayOfMonth2.value = new DayOfMonth2();
    return DayOfMonth2;
  }();
  var UnixTimestamp = /* @__PURE__ */ function() {
    function UnixTimestamp2() {
    }
    ;
    UnixTimestamp2.value = new UnixTimestamp2();
    return UnixTimestamp2;
  }();
  var DayOfWeek = /* @__PURE__ */ function() {
    function DayOfWeek2() {
    }
    ;
    DayOfWeek2.value = new DayOfWeek2();
    return DayOfWeek2;
  }();
  var DayOfWeekName = /* @__PURE__ */ function() {
    function DayOfWeekName2() {
    }
    ;
    DayOfWeekName2.value = new DayOfWeekName2();
    return DayOfWeekName2;
  }();
  var DayOfWeekNameShort = /* @__PURE__ */ function() {
    function DayOfWeekNameShort2() {
    }
    ;
    DayOfWeekNameShort2.value = new DayOfWeekNameShort2();
    return DayOfWeekNameShort2;
  }();
  var Hours24 = /* @__PURE__ */ function() {
    function Hours242() {
    }
    ;
    Hours242.value = new Hours242();
    return Hours242;
  }();
  var Hours12 = /* @__PURE__ */ function() {
    function Hours122() {
    }
    ;
    Hours122.value = new Hours122();
    return Hours122;
  }();
  var Meridiem = /* @__PURE__ */ function() {
    function Meridiem2() {
    }
    ;
    Meridiem2.value = new Meridiem2();
    return Meridiem2;
  }();
  var Minutes = /* @__PURE__ */ function() {
    function Minutes2() {
    }
    ;
    Minutes2.value = new Minutes2();
    return Minutes2;
  }();
  var MinutesTwoDigits = /* @__PURE__ */ function() {
    function MinutesTwoDigits2() {
    }
    ;
    MinutesTwoDigits2.value = new MinutesTwoDigits2();
    return MinutesTwoDigits2;
  }();
  var Seconds = /* @__PURE__ */ function() {
    function Seconds2() {
    }
    ;
    Seconds2.value = new Seconds2();
    return Seconds2;
  }();
  var SecondsTwoDigits = /* @__PURE__ */ function() {
    function SecondsTwoDigits2() {
    }
    ;
    SecondsTwoDigits2.value = new SecondsTwoDigits2();
    return SecondsTwoDigits2;
  }();
  var Milliseconds = /* @__PURE__ */ function() {
    function Milliseconds2() {
    }
    ;
    Milliseconds2.value = new Milliseconds2();
    return Milliseconds2;
  }();
  var MillisecondsShort = /* @__PURE__ */ function() {
    function MillisecondsShort2() {
    }
    ;
    MillisecondsShort2.value = new MillisecondsShort2();
    return MillisecondsShort2;
  }();
  var MillisecondsTwoDigits = /* @__PURE__ */ function() {
    function MillisecondsTwoDigits2() {
    }
    ;
    MillisecondsTwoDigits2.value = new MillisecondsTwoDigits2();
    return MillisecondsTwoDigits2;
  }();
  var Placeholder = /* @__PURE__ */ function() {
    function Placeholder2(value0) {
      this.value0 = value0;
    }
    ;
    Placeholder2.create = function(value0) {
      return new Placeholder2(value0);
    };
    return Placeholder2;
  }();
  var printShortMonth = function(v) {
    if (v instanceof January) {
      return "Jan";
    }
    ;
    if (v instanceof February) {
      return "Feb";
    }
    ;
    if (v instanceof March) {
      return "Mar";
    }
    ;
    if (v instanceof April) {
      return "Apr";
    }
    ;
    if (v instanceof May) {
      return "May";
    }
    ;
    if (v instanceof June) {
      return "Jun";
    }
    ;
    if (v instanceof July) {
      return "Jul";
    }
    ;
    if (v instanceof August) {
      return "Aug";
    }
    ;
    if (v instanceof September) {
      return "Sep";
    }
    ;
    if (v instanceof October) {
      return "Oct";
    }
    ;
    if (v instanceof November) {
      return "Nov";
    }
    ;
    if (v instanceof December) {
      return "Dec";
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 489, column 19 - line 501, column 22): " + [v.constructor.name]);
  };
  var padSingleDigit = function(i) {
    if (i < 0) {
      return "-" + padSingleDigit(-i | 0);
    }
    ;
    if (i < 10) {
      return "0" + show3(i);
    }
    ;
    if (otherwise) {
      return show3(i);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 194, column 1 - line 194, column 32): " + [i.constructor.name]);
  };
  var padQuadrupleDigit = function(i) {
    if (i < 0) {
      return "-" + padQuadrupleDigit(-i | 0);
    }
    ;
    if (i < 10) {
      return "000" + show3(i);
    }
    ;
    if (i < 100) {
      return "00" + show3(i);
    }
    ;
    if (i < 1e3) {
      return "0" + show3(i);
    }
    ;
    if (otherwise) {
      return show3(i);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 207, column 1 - line 207, column 35): " + [i.constructor.name]);
  };
  var padDoubleDigit = function(i) {
    if (i < 0) {
      return "-" + padDoubleDigit(-i | 0);
    }
    ;
    if (i < 10) {
      return "00" + show3(i);
    }
    ;
    if (i < 100) {
      return "0" + show3(i);
    }
    ;
    if (otherwise) {
      return show3(i);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 200, column 1 - line 200, column 32): " + [i.constructor.name]);
  };
  var formatYearTwoDigits = function(i) {
    var dateString = show3(abs3(i));
    var dateLength = length4(dateString);
    if (dateLength === 1) {
      return "0" + dateString;
    }
    ;
    if (dateLength === 2) {
      return dateString;
    }
    ;
    return drop2(dateLength - 2 | 0)(dateString);
  };
  var fix12 = function(h) {
    var $618 = h === 0;
    if ($618) {
      return 12;
    }
    ;
    return h;
  };
  var formatCommand = function(v) {
    return function(v1) {
      if (v1 instanceof YearFull) {
        return padQuadrupleDigit(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof YearTwoDigits) {
        return formatYearTwoDigits(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof YearAbsolute) {
        return show3(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof MonthFull) {
        return show12(month(v.value0));
      }
      ;
      if (v1 instanceof MonthShort) {
        return printShortMonth(month(v.value0));
      }
      ;
      if (v1 instanceof MonthTwoDigits) {
        return padSingleDigit(fromEnum1(month(v.value0)));
      }
      ;
      if (v1 instanceof DayOfMonthTwoDigits) {
        return padSingleDigit(fromEnum22(day(v.value0)));
      }
      ;
      if (v1 instanceof DayOfMonth) {
        return show3(fromEnum22(day(v.value0)));
      }
      ;
      if (v1 instanceof UnixTimestamp) {
        return show3(floor2(function(v2) {
          return v2 / 1e3;
        }(unwrap3(unInstant(fromDateTime(v))))));
      }
      ;
      if (v1 instanceof DayOfWeek) {
        return show3(fromEnum32(weekday(v.value0)));
      }
      ;
      if (v1 instanceof DayOfWeekName) {
        return show23(weekday(v.value0));
      }
      ;
      if (v1 instanceof DayOfWeekNameShort) {
        return take3(3)(show23(weekday(v.value0)));
      }
      ;
      if (v1 instanceof Hours24) {
        return padSingleDigit(fromEnum42(hour(v.value1)));
      }
      ;
      if (v1 instanceof Hours12) {
        return padSingleDigit(fix12(mod4(fromEnum42(hour(v.value1)))(12)));
      }
      ;
      if (v1 instanceof Meridiem) {
        var $621 = fromEnum42(hour(v.value1)) >= 12;
        if ($621) {
          return "PM";
        }
        ;
        return "AM";
      }
      ;
      if (v1 instanceof Minutes) {
        return show3(fromEnum52(minute(v.value1)));
      }
      ;
      if (v1 instanceof MinutesTwoDigits) {
        return padSingleDigit(fromEnum52(minute(v.value1)));
      }
      ;
      if (v1 instanceof Seconds) {
        return show3(fromEnum62(second(v.value1)));
      }
      ;
      if (v1 instanceof SecondsTwoDigits) {
        return padSingleDigit(fromEnum62(second(v.value1)));
      }
      ;
      if (v1 instanceof Milliseconds) {
        return padDoubleDigit(fromEnum72(millisecond(v.value1)));
      }
      ;
      if (v1 instanceof MillisecondsShort) {
        return show3(function(v2) {
          return div1(v2)(100);
        }(fromEnum72(millisecond(v.value1))));
      }
      ;
      if (v1 instanceof MillisecondsTwoDigits) {
        return padSingleDigit(function(v2) {
          return div1(v2)(10);
        }(fromEnum72(millisecond(v.value1))));
      }
      ;
      if (v1 instanceof Placeholder) {
        return v1.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Formatter.DateTime (line 169, column 38 - line 192, column 21): " + [v1.constructor.name]);
    };
  };
  var format = function(f) {
    return function(d) {
      return foldMap12(formatCommand(d))(f);
    };
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var singleton7 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var toUnfoldable2 = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(m) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof Leaf) {
              $copy_v = v.value1;
              return;
            }
            ;
            if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), v.value1));
            }
            ;
            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), new Cons(v.value0.value3, v.value1)));
            }
            ;
            if (v.value0 instanceof Two) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton7(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, v.value1)));
              return;
            }
            ;
            if (v.value0 instanceof Three) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton7(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, new Cons(singleton7(v.value0.value4)(v.value0.value5), new Cons(v.value0.value6, v.value1)))));
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 624, column 18 - line 633, column 71): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 623, column 3 - line 623, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return unfoldr3(go2)(new Cons(m, Nil.value));
    };
  };
  var lookup = function(dictOrd) {
    var compare5 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare5(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare5(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare5(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var functorMap = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v1 instanceof Two) {
          return new Two(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Three) {
          return new Three(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), map(functorMap)(v)(v1.value6));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 116, column 1 - line 119, column 110): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert2 = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare5 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare5(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare5(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare5(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var empty3 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();

  // output/Web.DOM.ChildNode/foreign.js
  function remove(node) {
    return function() {
      return node.remove();
    };
  }

  // output/Web.DOM.Document/foreign.js
  var getEffProp = function(name15) {
    return function(doc) {
      return function() {
        return doc[name15];
      };
    };
  };
  var url = getEffProp("URL");
  var documentURI = getEffProp("documentURI");
  var origin = getEffProp("origin");
  var compatMode = getEffProp("compatMode");
  var characterSet = getEffProp("characterSet");
  var contentType = getEffProp("contentType");
  var _documentElement = getEffProp("documentElement");
  function createElement(localName2) {
    return function(doc) {
      return function() {
        return doc.createElement(localName2);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name15, value12) {
    if (typeof window !== "undefined") {
      var ty = window[name15];
      if (ty != null && value12 instanceof ty) {
        return just(value12);
      }
    }
    var obj = value12;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name15) {
        return just(value12);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name15) {
    return function(value12) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
    };
  };

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function setAttribute(name15) {
    return function(value12) {
      return function(element) {
        return function() {
          element.setAttribute(name15, value12);
        };
      };
    };
  }

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp2("children");
  var _firstElementChild = getEffProp2("firstElementChild");
  var _lastElementChild = getEffProp2("lastElementChild");
  var childElementCount = getEffProp2("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }
  function querySelectorAll(selector) {
    return function(node) {
      return function() {
        return node.querySelectorAll(selector);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var map6 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map6(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.DOM.Element/index.js
  var toParentNode = unsafeCoerce2;
  var toNode = unsafeCoerce2;
  var toChildNode = unsafeCoerce2;
  var fromNode = /* @__PURE__ */ unsafeReadProtoTagged("Element");

  // output/Web.DOM.HTMLCollection/foreign.js
  function toArray(list) {
    return function() {
      return [].slice.call(list);
    };
  }

  // output/Web.DOM.Node/foreign.js
  var getEffProp3 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp3("baseURI");
  var _ownerDocument = getEffProp3("ownerDocument");
  var _parentNode = getEffProp3("parentNode");
  var _parentElement = getEffProp3("parentElement");
  var childNodes = getEffProp3("childNodes");
  var _firstChild = getEffProp3("firstChild");
  var _lastChild = getEffProp3("lastChild");
  var _previousSibling = getEffProp3("previousSibling");
  var _nextSibling = getEffProp3("nextSibling");
  var _nodeValue = getEffProp3("nodeValue");
  var textContent = getEffProp3("textContent");
  function setTextContent(value12) {
    return function(node) {
      return function() {
        node.textContent = value12;
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }

  // output/Web.DOM.NodeList/foreign.js
  function toArray2(list) {
    return function() {
      return [].slice.call(list);
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var toParentNode2 = unsafeCoerce2;
  var toDocument = unsafeCoerce2;

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value12) {
    var tag = Object.prototype.toString.call(value12);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value12);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var fromNode2 = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Main/index.js
  var max6 = /* @__PURE__ */ max(ordInt);
  var fromEnum8 = /* @__PURE__ */ fromEnum(boundedEnumHour);
  var fromEnum12 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
  var fromFoldable3 = /* @__PURE__ */ fromFoldable(foldableArray);
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var show4 = /* @__PURE__ */ show(showInt);
  var show13 = /* @__PURE__ */ show(showNumber);
  var bottom3 = /* @__PURE__ */ bottom(boundedHour);
  var bottom12 = /* @__PURE__ */ bottom(boundedMinute);
  var bottom22 = /* @__PURE__ */ bottom(boundedSecond);
  var bottom32 = /* @__PURE__ */ bottom(boundedMillisecond);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect);
  var for_1 = /* @__PURE__ */ for_2(foldableMaybe);
  var show24 = /* @__PURE__ */ show(showParseError);
  var bind12 = /* @__PURE__ */ bind(bindEither);
  var pure12 = /* @__PURE__ */ pure(applicativeEither);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var map7 = /* @__PURE__ */ map(functorEffect);
  var minimum2 = /* @__PURE__ */ minimum(ordTime)(foldableMap);
  var map1 = /* @__PURE__ */ map(functorMap);
  var sequence3 = /* @__PURE__ */ sequence(traversableArray)(applicativeEffect);
  var bind22 = /* @__PURE__ */ bind(bindArray);
  var sortWith2 = /* @__PURE__ */ sortWith(ordDate);
  var toUnfoldable3 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var pure22 = /* @__PURE__ */ pure(applicativeArray);
  var lookup2 = /* @__PURE__ */ lookup(ordDate);
  var insert3 = /* @__PURE__ */ insert2(ordDate);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var for_22 = /* @__PURE__ */ for_2(foldableArray);
  var Single = /* @__PURE__ */ function() {
    function Single2(value0) {
      this.value0 = value0;
    }
    ;
    Single2.create = function(value0) {
      return new Single2(value0);
    };
    return Single2;
  }();
  var After = /* @__PURE__ */ function() {
    function After2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    After2.create = function(value0) {
      return function(value1) {
        return new After2(value0, value1);
      };
    };
    return After2;
  }();
  var Parallel = /* @__PURE__ */ function() {
    function Parallel2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Parallel2.create = function(value0) {
      return function(value1) {
        return new Parallel2(value0, value1);
      };
    };
    return Parallel2;
  }();
  var scheduleStart = function(dictOrd) {
    var min5 = min(dictOrd);
    return function(v) {
      if (v instanceof Single) {
        return v.value0.start;
      }
      ;
      if (v instanceof After) {
        return scheduleStart(dictOrd)(v.value0);
      }
      ;
      if (v instanceof Parallel) {
        return min5(scheduleStart(dictOrd)(v.value0))(scheduleStart(dictOrd)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Main (line 55, column 1 - line 55, column 56): " + [v.constructor.name]);
    };
  };
  var scheduleStart1 = /* @__PURE__ */ scheduleStart(ordTime);
  var scheduleParallelism = function(v) {
    if (v instanceof Single) {
      return 1;
    }
    ;
    if (v instanceof After) {
      return max6(scheduleParallelism(v.value0))(scheduleParallelism(v.value1));
    }
    ;
    if (v instanceof Parallel) {
      return scheduleParallelism(v.value0) + scheduleParallelism(v.value1) | 0;
    }
    ;
    throw new Error("Failed pattern match at Main (line 65, column 1 - line 65, column 55): " + [v.constructor.name]);
  };
  var scheduleEnd = function(dictOrd) {
    var max1 = max(dictOrd);
    return function(v) {
      if (v instanceof Single) {
        return v.value0.end;
      }
      ;
      if (v instanceof After) {
        return scheduleEnd(dictOrd)(v.value1);
      }
      ;
      if (v instanceof Parallel) {
        return max1(scheduleEnd(dictOrd)(v.value0))(scheduleEnd(dictOrd)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Main (line 60, column 1 - line 60, column 54): " + [v.constructor.name]);
    };
  };
  var scheduleEnd1 = /* @__PURE__ */ scheduleEnd(ordTime);
  var scheduleInsert = function(dictOrd) {
    var greaterThanOrEq2 = greaterThanOrEq(dictOrd);
    var scheduleEnd2 = scheduleEnd(dictOrd);
    var lessThanOrEq2 = lessThanOrEq(dictOrd);
    var scheduleStart2 = scheduleStart(dictOrd);
    return function(entry) {
      return function(schedule) {
        if (greaterThanOrEq2(entry.start)(scheduleEnd2(schedule))) {
          return new After(schedule, new Single(entry));
        }
        ;
        if (lessThanOrEq2(entry.end)(scheduleStart2(schedule))) {
          return new After(new Single(entry), schedule);
        }
        ;
        if (otherwise) {
          if (schedule instanceof Single) {
            return new Parallel(new Single(schedule.value0), new Single(entry));
          }
          ;
          if (schedule instanceof After && (schedule.value0 instanceof After && greaterThanOrEq2(entry.start)(scheduleEnd2(schedule.value0.value0)))) {
            return new After(schedule.value0.value0, scheduleInsert(dictOrd)(entry)(new After(schedule.value0.value1, schedule.value1)));
          }
          ;
          if (schedule instanceof After) {
            if (greaterThanOrEq2(entry.start)(scheduleEnd2(schedule.value0))) {
              return new After(schedule.value0, scheduleInsert(dictOrd)(entry)(schedule.value1));
            }
            ;
            if (lessThanOrEq2(entry.end)(scheduleStart2(schedule.value1))) {
              return new After(scheduleInsert(dictOrd)(entry)(schedule.value0), schedule.value1);
            }
            ;
            if (otherwise) {
              return new Parallel(new After(schedule.value0, schedule.value1), new Single(entry));
            }
            ;
          }
          ;
          if (schedule instanceof Parallel) {
            var y$prime = scheduleInsert(dictOrd)(entry)(schedule.value1);
            var x$prime = scheduleInsert(dictOrd)(entry)(schedule.value0);
            var r = scheduleParallelism(schedule.value0) + scheduleParallelism(y$prime) | 0;
            var l = scheduleParallelism(x$prime) + scheduleParallelism(schedule.value1) | 0;
            var $122 = l < r;
            if ($122) {
              return new Parallel(x$prime, schedule.value1);
            }
            ;
            return new Parallel(schedule.value0, y$prime);
          }
          ;
          throw new Error("Failed pattern match at Main (line 78, column 19 - line 94, column 59): " + [schedule.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Main (line 72, column 1 - line 72, column 81): " + [entry.constructor.name, schedule.constructor.name]);
      };
    };
  };
  var scheduleInsert1 = /* @__PURE__ */ scheduleInsert(ordTime);
  var scheduleRender = function(doc) {
    return function(date2) {
      return function(mbEarliest) {
        return function(schedule0) {
          var zero2 = scheduleStart1(schedule0);
          var timeFmt = [Hours24.value, new Placeholder(":"), MinutesTwoDigits.value];
          var ticks = function(t) {
            return (fromEnum8(hour(t)) * 60 | 0) + fromEnum12(minute(t)) | 0;
          };
          var renderTime = function(t) {
            return format(fromFoldable3(timeFmt))(new DateTime(date2, t));
          };
          var go2 = function(container) {
            return function(left) {
              return function(width8) {
                return function(schedule) {
                  if (schedule instanceof Single) {
                    var y = ticks(schedule.value0.start) - ticks(zero2) | 0;
                    var height8 = ticks(schedule.value0.end) - ticks(schedule.value0.start) | 0;
                    return function __do2() {
                      var div12 = function() {
                        var v = trim(schedule.value0.content.link);
                        if (v === "") {
                          return createElement("div")(doc)();
                        }
                        ;
                        var a = createElement("a")(doc)();
                        setAttribute("href")(v)(a)();
                        return a;
                      }();
                      setAttribute("class")("entry " + schedule.value0.content.kind)(div12)();
                      setAttribute("style")("position: absolute;" + ("top: " + (show4(y) + ("px;" + ("left: " + (show13(left) + ("%; " + ("width: " + (show13(width8) + ("%; " + ("height: " + (show4(height8) + "px;"))))))))))))(div12)();
                      var timeSpan = createElement("span")(doc)();
                      setAttribute("class")("time")(timeSpan)();
                      var startStr = renderTime(schedule.value0.start);
                      var endStr = renderTime(schedule.value0.end);
                      setTextContent(startStr + (" - " + (endStr + ":")))(toNode(timeSpan))();
                      appendChild(toNode(timeSpan))(toNode(div12))();
                      var titleSpan = createElement("span")(doc)();
                      setAttribute("class")("title")(titleSpan)();
                      setTextContent(schedule.value0.content.title)(toNode(titleSpan))();
                      appendChild(toNode(titleSpan))(toNode(div12))();
                      return appendChild(toNode(div12))(toNode(container))();
                    };
                  }
                  ;
                  if (schedule instanceof After) {
                    return function __do2() {
                      go2(container)(left)(width8)(schedule.value0)();
                      return go2(container)(left)(width8)(schedule.value1)();
                    };
                  }
                  ;
                  if (schedule instanceof Parallel) {
                    var yp = toNumber(scheduleParallelism(schedule.value1));
                    var xp = toNumber(scheduleParallelism(schedule.value0));
                    var unitWidth = width8 / (xp + yp);
                    return function __do2() {
                      go2(container)(left)(xp * unitWidth)(schedule.value0)();
                      return go2(container)(left + xp * unitWidth)(yp * unitWidth)(schedule.value1)();
                    };
                  }
                  ;
                  throw new Error("Failed pattern match at Main (line 153, column 40 - line 198, column 68): " + [schedule.constructor.name]);
                };
              };
            };
          };
          var dateFmt = [DayOfWeekName.value, new Placeholder(" "), DayOfMonth.value, new Placeholder(" "), MonthFull.value];
          var renderDate = function(d) {
            return format(fromFoldable3(dateFmt))(new DateTime(d, new Time(bottom3, bottom12, bottom22, bottom32)));
          };
          return function __do2() {
            var container = createElement("div")(doc)();
            setAttribute("class")("day")(container)();
            var div12 = createElement("div")(doc)();
            setAttribute("class")("date")(div12)();
            setTextContent(renderDate(date2))(toNode(div12))();
            appendChild(toNode(div12))(toNode(container))();
            var start2 = ticks(scheduleStart1(schedule0));
            for_1(mbEarliest)(function(earliest) {
              var padding = start2 - ticks(earliest) | 0;
              return function __do3() {
                var morning = createElement("div")(doc)();
                setAttribute("class")("morning")(morning)();
                setAttribute("style")("height: " + (show4(padding) + "px;"))(morning)();
                return appendChild(toNode(morning))(toNode(container))();
              };
            })();
            var entries = createElement("div")(doc)();
            var end = ticks(scheduleEnd1(schedule0));
            var height8 = end - start2 | 0;
            setAttribute("class")("entries")(entries)();
            setAttribute("style")("position: relative; height: " + (show4(height8) + "px;"))(entries)();
            go2(entries)(0)(100)(schedule0)();
            appendChild(toNode(entries))(toNode(container))();
            return container;
          };
        };
      };
    };
  };
  var parseEntry = function(element) {
    var parseDateTime = function(txt) {
      return either(function($168) {
        return $$throw(show24($168));
      })(pure3)(bind12(fromString2(txt))(function(v) {
        return pure12(v.value0);
      }));
    };
    return function __do2() {
      var cells2 = bind3(bind3(bind3(querySelectorAll("td")(toParentNode(element)))(toArray2))(function() {
        var $169 = mapMaybe(fromNode2);
        return function($170) {
          return pure3($169($170));
        };
      }()))(traverse2(innerText))();
      if (cells2.length === 5) {
        var start2 = parseDateTime(cells2[0])();
        var end = parseDateTime(cells2[1])();
        return new Just({
          start: start2,
          end,
          content: {
            kind: cells2[4],
            link: cells2[3],
            title: cells2[2]
          }
        });
      }
      ;
      return Nothing.value;
    };
  };
  var parseEntries = function(schedule) {
    return function __do2() {
      var trs = bind3(querySelectorAll("tr")(toParentNode(schedule)))(toArray2)();
      return map7(catMaybes)(traverse2(parseEntry)(mapMaybe(fromNode)(trs)))();
    };
  };
  var calendarRender = function(doc) {
    return function(calendar) {
      var earliest = minimum2(map1(scheduleStart1)(calendar));
      return sequence3(bind22(sortWith2(fst)(toUnfoldable3(calendar)))(function(v) {
        return pure22(scheduleRender(doc)(v.value0)(earliest)(v.value1));
      }));
    };
  };
  var calendarInsert = function(entry) {
    return function(calendar) {
      var time2 = {
        start: time(entry.start),
        end: time(entry.end),
        content: entry.content
      };
      var date2 = date(entry.start);
      var v = lookup2(date2)(calendar);
      if (v instanceof Nothing) {
        return insert3(date2)(new Single(time2))(calendar);
      }
      ;
      if (v instanceof Just) {
        return insert3(date2)(scheduleInsert1(time2)(v.value0))(calendar);
      }
      ;
      throw new Error("Failed pattern match at Main (line 205, column 33 - line 207, column 73): " + [v.constructor.name]);
    };
  };
  var calendarFromEntries = /* @__PURE__ */ foldl2(/* @__PURE__ */ flip(calendarInsert))(empty3);
  var main = function __do() {
    var window2 = windowImpl();
    var doc = document(window2)();
    var schedule = bind3(querySelector(".schedule")(toParentNode2(doc)))(maybe($$throw("no schedule"))(pure3))();
    var calendar = map7(calendarFromEntries)(parseEntries(schedule))();
    bind3(bind3(children(toParentNode(schedule)))(toArray))(traverse_2(function($171) {
      return remove(toChildNode($171));
    }))();
    var rendered = calendarRender(toDocument(doc))(calendar)();
    return for_22(rendered)(function(c) {
      return appendChild(toNode(c))(toNode(schedule));
    })();
  };

  // <stdin>
  main();
})();
