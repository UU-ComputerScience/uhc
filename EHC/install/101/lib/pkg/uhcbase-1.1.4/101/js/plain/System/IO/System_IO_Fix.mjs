// System.IO.Fix
var $System=
 ($System ? $System : {});
$System.$IO=
 ($System.$IO ? $System.$IO : {});
$System.$IO.$Fix=
 ($System.$IO.$Fix ? $System.$IO.$Fix : {});
$System.$IO.$Fix.$_24okUNQ6=
 new _F_(function($k,$_24x)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__4=
           new _A_($System.$IO.$Unsafe.$unsafeInterleaveIO,[$__]);
          var $__5=
           new _A_($System.$IO.$Fix.$_24okUNQ12,[$k,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$System.$IO.$Fix.$_24okUNQ12=
 new _F_(function($k,$_24x,$_24x3)
         {var $__=
           new _A_($k,[$_24x3]);
          var $__5=
           new _A_($System.$IO.$Fix.$_24okUNQ19,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$System.$IO.$Fix.$_24okUNQ19=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x2]);
          var $__4=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$System.$IO.$Fix.$fixIO=
 new _F_(function($k)
         {var $__=
           new _A_($UHC.$Base.$throw,[$UHC.$Base.$NonTermination__]);
          var $__3=
           new _A_($UHC.$IOBase.$newIORef,[$__]);
          var $__4=
           new _A_($System.$IO.$Fix.$_24okUNQ6,[$k]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
