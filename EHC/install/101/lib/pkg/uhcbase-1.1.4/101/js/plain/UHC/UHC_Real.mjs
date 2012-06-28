// UHC.Real
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$Real=
 ($UHC.$Real ? $UHC.$Real : {});
$UHC.$Real.$__130__115__2__0NEW1UNQ6=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Real.$__130__105__2__0NEW4UNQ5=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Real.$__132__9__0=
 new _F_(function($__,$__2,$showPos,$p,$x)
         {var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3c,[$__,$x,$__7]);
          var $__9=
           _e_($__8);
          var $__swJSW2__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW3__0;
             switch($__10._tag_)
              {case 0:
                var $__11=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 129_0_0"]);
                var $__12=
                 new _A_($UHC.$Base.$error,[$__11]);
                $__swJSW3__0=
                 $__12;
                break;
               case 1:
                var $__13=
                 new _A_($showPos,[$x]);
                $__swJSW3__0=
                 $__13;
                break;}
             $__swJSW2__0=
              $__swJSW3__0;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$negate,[$__2,$x]);
             var $__15=
              new _A_($showPos,[$__14]);
             var $__16=
              new _A_($UHC.$Base.$showChar,[45]);
             var $__17=
              new _A_($UHC.$Base.$_2e,[$__16,$__15]);
             var $__18=
              new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$p,6]);
             var $__19=
              new _A_($UHC.$Base.$showParen,[$__18,$__17]);
             $__swJSW2__0=
              $__19;
             break;}
          return $__swJSW2__0;});
$UHC.$Real.$showSigned=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Real.$__130__115__2__0NEW1UNQ6,[$__]);
          var $__3=
           new _A_($UHC.$Real.$__130__105__2__0NEW4UNQ5,[$__]);
          return new _A_($UHC.$Real.$__132__9__0,[$__3,$__2]);});
