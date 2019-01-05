// Generated by purs bundle 0.12.0
var PS = {};
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var Control_Category = PS["Control.Category"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ring = PS["Data.Ring"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];                 
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };                                                                                             
  var functorArray = new Functor($foreign.arrayMap);
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
  exports["functorArray"] = functorArray;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];                 
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
    "use strict";

  //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.length = function (xs) {
    return xs.length;
  };

  //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
        };
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var fromJust = function (dictPartial) {
      return function (v) {
          var $__unused = function (dictPartial1) {
              return function ($dollar35) {
                  return $dollar35;
              };
          };
          return $__unused(dictPartial)((function () {
              if (v instanceof Just) {
                  return v.value0;
              };
              throw new Error("Failed pattern match at Data.Maybe line 268, column 1 - line 268, column 46: " + [ v.constructor.name ]);
          })());
      };
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["fromJust"] = fromJust;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Data.Array"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Lazy = PS["Control.Lazy"];
  var Control_Monad_Rec_Class = PS["Control.Monad.Rec.Class"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var Control_Monad_ST_Internal = PS["Control.Monad.ST.Internal"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Array_NonEmpty_Internal = PS["Data.Array.NonEmpty.Internal"];
  var Data_Array_ST = PS["Data.Array.ST"];
  var Data_Array_ST_Iterator = PS["Data.Array.ST.Iterator"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Unfoldable = PS["Data.Unfoldable"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["index"] = index;
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Effect"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Prelude = PS["Prelude"];                 
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applyEffect"] = applyEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
    "use strict";

  exports.new = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.read = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports["modify'"] = function (f) {
    return function (ref) {
      return function () {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Effect.Ref"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Effect = PS["Effect"];
  var Prelude = PS["Prelude"];                 
  var modify = function (f) {
      return $foreign["modify'"](function (s) {
          var s$prime = f(s);
          return {
              state: s$prime,
              value: s$prime
          };
      });
  };
  var modify_ = function (f) {
      return function (s) {
          return Data_Functor["void"](Effect.functorEffect)(modify(f)(s));
      };
  };
  exports["modify"] = modify;
  exports["modify_"] = modify_;
  exports["new"] = $foreign["new"];
  exports["read"] = $foreign.read;
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setStrokeStyle = function(ctx) {
      return function(style) {
          return function() {
              ctx.strokeStyle = style;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
      };
  };

  exports.stroke = function(ctx) {
      return function() {
          ctx.stroke();
      };
  };

  exports.lineTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.lineTo(x, y);
              };
          };
      };
  };

  exports.moveTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.moveTo(x, y);
              };
          };
      };
  };

  exports.clearRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.clearRect(r.x, r.y, r.width, r.height);
          };
      };
  };

  exports.translate = function(ctx) {
      return function(t) {
          return function() {
              ctx.translate(t.translateX, t.translateY);
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Effect = PS["Effect"];
  var Effect_Exception_Unsafe = PS["Effect.Exception.Unsafe"];
  var Prelude = PS["Prelude"];
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["setStrokeStyle"] = $foreign.setStrokeStyle;
  exports["beginPath"] = $foreign.beginPath;
  exports["stroke"] = $foreign.stroke;
  exports["lineTo"] = $foreign.lineTo;
  exports["moveTo"] = $foreign.moveTo;
  exports["clearRect"] = $foreign.clearRect;
  exports["translate"] = $foreign.translate;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {var obj = {};


  exports["get"] = function(key) {
      return function(){
          return obj[key];
      }
  }

  exports["set"] = function(key)
  {
      return function(value)
      {
          return function(){
              obj[key] = value
          }
      }    
  }

  exports.addEventListener = function (ctx){
      return function (eventType) {
          return function(callback) {
              function eventHandler(e) {
                  callback(e)();
              }
              canvas.addEventListener(eventType, eventHandler);
              return function () {}
          }
      }
  }
})(PS["Main"] = PS["Main"] || {});
(function(exports) {
    "use strict";          

  exports.cos = Math.cos;    

  exports.sin = Math.sin;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Math"];
  exports["cos"] = $foreign.cos;
  exports["sin"] = $foreign.sin;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.12.0
  "use strict";
  var $foreign = PS["Main"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Array = PS["Data.Array"];
  var Data_EuclideanRing = PS["Data.EuclideanRing"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Unit = PS["Data.Unit"];
  var Effect = PS["Effect"];
  var Effect_Console = PS["Effect.Console"];
  var Effect_Ref = PS["Effect.Ref"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var $$Math = PS["Math"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];                 
  var rotateY = function (theta) {
      return function (vertices) {
          var rotateXZ = function (v) {
              var sina = $$Math.sin(theta);
              var cosa = $$Math.cos(theta);
              return {
                  x: v.x * cosa - v.z * sina,
                  y: v.y,
                  z: v.z * cosa + v.x * sina
              };
          };
          return Data_Functor.map(Data_Functor.functorArray)(function (v) {
              return rotateXZ(v);
          })(vertices);
      };
  };
  var rotateX = function (dictFunctor) {
      return function (theta) {
          return function (vertices) {
              var rotateYZ = function (v) {
                  var sina = $$Math.sin(theta);
                  var cosa = $$Math.cos(theta);
                  return {
                      x: v.x,
                      y: v.y * cosa - v.z * sina,
                      z: v.z * cosa + v.y * sina
                  };
              };
              return Data_Functor.map(dictFunctor)(function (v) {
                  return rotateYZ(v);
              })(vertices);
          };
      };
  };
  var project = function (vertex) {
      return function (ctx) {
          var drawLine = function (v1) {
              return function (v2) {
                  return function __do() {
                      Graphics_Canvas.moveTo(ctx)(v1.x)(v1.y)();
                      return Graphics_Canvas.lineTo(ctx)(v2.x)(v2.y)();
                  };
              };
          };
          return function __do() {
              Graphics_Canvas.setStrokeStyle(ctx)("#6233ff")();
              Graphics_Canvas.beginPath(ctx)();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(0)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(1)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(0)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(2)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(0)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(4)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(1)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(5)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(1)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(3)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(2)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(3)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(2)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(6)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(4)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(6)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(3)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(7)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(6)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(7)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(4)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(5)))();
              drawLine(Data_Maybe.fromJust()(Data_Array.index(vertex)(5)))(Data_Maybe.fromJust()(Data_Array.index(vertex)(7)))();
              return Graphics_Canvas.stroke(ctx)();
          };
      };
  };
  var onMouseMove = function (ref) {
      return function (ctx) {
          return function (event) {
              return function __do() {
                  var v = $foreign.get("x")();
                  var v1 = $foreign.get("y")();
                  var v2 = Effect_Ref.read(ref)();
                  var v3 = rotateX(Data_Functor.functorArray)((event.clientY - v1) / 1000.0)(v2);
                  var v4 = rotateY((event.clientX - v) / 1000.0)(v3);
                  Graphics_Canvas.clearRect(ctx)({
                      x: -500.0,
                      y: -500.0,
                      width: 1000.0,
                      height: 1000.0
                  })();
                  project(v4)(ctx)();
                  return Effect_Ref.modify_(function (v5) {
                      return v4;
                  })(ref)();
              };
          };
      };
  };
  var onMouseDown = function (event) {
      return function __do() {
          $foreign.set("x")(event.clientX)();
          return $foreign.set("y")(event.clientY)();
      };
  };
  var initialVertices = [ {
      x: -100.0,
      y: -100.0,
      z: -100.0
  }, {
      x: -100.0,
      y: -100.0,
      z: 100.0
  }, {
      x: -100.0,
      y: 100.0,
      z: -100.0
  }, {
      x: -100.0,
      y: 100.0,
      z: 100.0
  }, {
      x: 100.0,
      y: -100.0,
      z: -100.0
  }, {
      x: 100.0,
      y: -100.0,
      z: 100.0
  }, {
      x: 100.0,
      y: 100.0,
      z: -100.0
  }, {
      x: 100.0,
      y: 100.0,
      z: 100.0
  } ];
  var main = Data_Functor["void"](Effect.functorEffect)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      var $__unused = function (dictPartial1) {
          return function ($dollar37) {
              return $dollar37;
          };
      };
      return $__unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  Graphics_Canvas.translate(v1)({
                      translateX: 200.0,
                      translateY: 200.0
                  })();
                  var v2 = rotateX(Data_Functor.functorArray)(60.0)(initialVertices);
                  var v3 = rotateY(60.0)(v2);
                  project(v3)(v1)();
                  var v4 = Effect_Ref["new"](v3)();
                  $foreign.addEventListener(v.value0)("mousedown")(onMouseDown)();
                  $foreign.addEventListener(v.value0)("mousemove")(onMouseMove(v4)(v1))();
                  return Data_Unit.unit;
              };
          };
          throw new Error("Failed pattern match at Main line 33, column 5 - line 34, column 5: " + [ v.constructor.name ]);
      })())();
  });
  exports["initialVertices"] = initialVertices;
  exports["main"] = main;
  exports["onMouseDown"] = onMouseDown;
  exports["onMouseMove"] = onMouseMove;
  exports["rotateX"] = rotateX;
  exports["rotateY"] = rotateY;
  exports["project"] = project;
  exports["addEventListener"] = $foreign.addEventListener;
  exports["set"] = $foreign.set;
  exports["get"] = $foreign.get;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();