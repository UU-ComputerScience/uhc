// Debug.Trace
var $Debug=
 ($Debug ? $Debug : {});
$Debug.$Trace=
 ($Debug.$Trace ? $Debug.$Trace : {});
$Debug.$Trace.$putTraceMsg=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$OldIO.$hPutStrLn,[$UHC.$OldIO.$stderr]);}),[]);
$Debug.$Trace.$trace=
 new _F_(function($string,$expr)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$expr]);
          var $__4=
           new _A_($Debug.$Trace.$putTraceMsg,[$string]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);
          return new _A_($UHC.$Base.$_24,[$UHC.$IOBase.$unsafePerformIO,$__5]);});
$Debug.$Trace.$traceShow=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$show,[$__]);
          return new _A_($UHC.$Base.$_2e,[$Debug.$Trace.$trace,$__2]);});
