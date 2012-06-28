// UHC.Char
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$Char=
 ($UHC.$Char ? $UHC.$Char : {});
$UHC.$Char.$intToDigit=
 new _F_(function($i)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__91__0,$i,9]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$i,0]);
          var $__4=
           new _A_($UHC.$Base.$_26_26,[$__3,$__]);
          var $__5=
           _e_($__4);
          var $__swJSW0__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__91__0,$i,15]);
             var $__7=
              new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$i,10]);
             var $__8=
              new _A_($UHC.$Base.$_26_26,[$__7,$__6]);
             var $__9=
              _e_($__8);
             var $__swJSW1__0;
             switch($__9._tag_)
              {case 0:
                var $__10=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW2__0;
                switch($__10._tag_)
                 {case 0:
                   var $__11=
                    new _A_($UHC.$Base.$packedStringToString,["FAIL 93_0_0"]);
                   var $__12=
                    new _A_($UHC.$Base.$error,[$__11]);
                   $__swJSW2__0=
                    $__12;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$packedStringToString,["Char.intToDigit: not a digit"]);
                   var $__14=
                    new _A_($UHC.$Base.$error,[$__13]);
                   $__swJSW2__0=
                    $__14;
                   break;}
                $__swJSW1__0=
                 $__swJSW2__0;
                break;
               case 1:
                var $__15=
                 new _A_($UHC.$Base.$fromEnum,[$UHC.$Base.$Enum__DCT74__60__0,97]);
                var $__16=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__15,$i]);
                var $__17=
                 new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__16,10]);
                var $__18=
                 new _A_($UHC.$Base.$toEnum,[$UHC.$Base.$Enum__DCT74__60__0,$__17]);
                $__swJSW1__0=
                 $__18;
                break;}
             $__swJSW0__0=
              $__swJSW1__0;
             break;
            case 1:
             var $__19=
              new _A_($UHC.$Base.$fromEnum,[$UHC.$Base.$Enum__DCT74__60__0,48]);
             var $__20=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__19,$i]);
             var $__21=
              new _A_($UHC.$Base.$toEnum,[$UHC.$Base.$Enum__DCT74__60__0,$__20]);
             $__swJSW0__0=
              $__21;
             break;}
          return $__swJSW0__0;});
