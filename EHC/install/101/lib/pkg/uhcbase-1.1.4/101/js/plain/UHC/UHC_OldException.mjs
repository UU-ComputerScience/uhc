// UHC.OldException
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$OldException=
 ($UHC.$OldException ? $UHC.$OldException : {});
$UHC.$OldException.$unblock=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$OldException.$throwIO=
 new _F_(function($e,$s)
         {return new _A_($UHC.$Base.$throw,[$e]);});
$UHC.$OldException.$catchAny=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$catchException;}),[]);
$UHC.$OldException.$block=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$OldException.$assert=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW0__0;
          switch($x13._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$packedStringToString,["Assertion failed"]);
             var $__5=
              new _A_($UHC.$Base.$error,[$__]);
             $__swJSW0__0=
              $__5;
             break;
            case 1:
             $__swJSW0__0=
              $x2;
             break;}
          return $__swJSW0__0;});
$UHC.$OldException.$_24okUNQ41=
 new _F_(function($after,$m,$_24x)
         {var $__=
           new _A_($m,[$_24x]);
          var $__5=
           new _A_($UHC.$IOBase.$try,[$__]);
          var $__6=
           new _A_($UHC.$OldException.$_24okUNQ48,[$after,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__6]);});
$UHC.$OldException.$_24okUNQ48=
 new _F_(function($after,$_24x,$_24x3)
         {var $__=
           new _A_($UHC.$OldException.$__198__35NEW11,[$_24x3]);
          var $__5=
           new _A_($after,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$UHC.$OldException.$__198__35NEW11=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW1__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$IOBase.$ioError,[$__._1]);
             $__swJSW1__0=
              $__4;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__._1]);
             $__swJSW1__0=
              $__6;
             break;}
          return $__swJSW1__0;});
$UHC.$OldException.$bracket=
 new _F_(function($before,$after,$m)
         {var $__=
           new _A_($UHC.$OldException.$_24okUNQ41,[$after,$m]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$before,$__]);});
$UHC.$OldException.$_24okUNQ19=
 new _F_(function($after,$m,$_24x)
         {var $__=
           new _A_($UHC.$IOBase.$try,[$m]);
          var $__5=
           new _A_($UHC.$OldException.$_24okUNQ26,[$after,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$UHC.$OldException.$_24okUNQ26=
 new _F_(function($after,$_24x,$_24x3)
         {var $__=
           new _A_($UHC.$OldException.$__198__65NEW22,[$_24x3]);
          var $__5=
           new _A_($after,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$UHC.$OldException.$__198__65NEW22=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW2__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$IOBase.$ioError,[$__._1]);
             $__swJSW2__0=
              $__4;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__._1]);
             $__swJSW2__0=
              $__6;
             break;}
          return $__swJSW2__0;});
$UHC.$OldException.$bracket__=
 new _F_(function($before,$after,$m)
         {var $__=
           new _A_($UHC.$OldException.$_24okUNQ19,[$after,$m]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$before,$__]);});
$UHC.$OldException.$__198__84__0=
 new _F_(function($what,$e)
         {var $__=
           new _A_($UHC.$OldException.$throwIO,[$e]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$what,$__]);});
$UHC.$OldException.$onException=
 new _F_(function($io,$what)
         {var $__=
           new _A_($UHC.$OldException.$__198__84__0,[$what]);
          return new _A_($UHC.$OldException.$catchAny,[$io,$__]);});
