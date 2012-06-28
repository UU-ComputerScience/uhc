// Data.Char
var $Data=
 ($Data ? $Data : {});
$Data.$Char=
 ($Data.$Char ? $Data.$Char : {});
$Data.$Char.$toUpper=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCharToUpper($__2);});
$Data.$Char.$toLower=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCharToLower($__2);});
$Data.$Char.$toTitle=
 new _A_(new _F_(function()
                 {return $Data.$Char.$toUpper;}),[]);
$Data.$Char.$isPrint=
 new _F_(function($c)
         {return new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,32]);});
$Data.$Char.$digitToInt=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$isDigit,[$c]);
          var $__3=
           _e_($__);
          var $__swJSW0__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,102]);
             var $__5=
              new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,97]);
             var $__6=
              new _A_($UHC.$Base.$_26_26,[$__5,$__4]);
             var $__7=
              _e_($__6);
             var $__swJSW1__0;
             switch($__7._tag_)
              {case 0:
                var $__8=
                 new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,70]);
                var $__9=
                 new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,65]);
                var $__10=
                 new _A_($UHC.$Base.$_26_26,[$__9,$__8]);
                var $__11=
                 _e_($__10);
                var $__swJSW2__0;
                switch($__11._tag_)
                 {case 0:
                   var $__12=
                    _e_($UHC.$Base.$otherwise);
                   var $__swJSW3__0;
                   switch($__12._tag_)
                    {case 0:
                      var $__13=
                       new _A_($UHC.$Base.$packedStringToString,["FAIL 309_0_0"]);
                      var $__14=
                       new _A_($UHC.$Base.$error,[$__13]);
                      $__swJSW3__0=
                       $__14;
                      break;
                     case 1:
                      var $__15=
                       new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__65__0,$c]);
                      var $__16=
                       new _A_($UHC.$Base.$packedStringToString,["Char.digitToInt: not a digit "]);
                      var $__17=
                       new _A_($UHC.$Base.$_2b_2b,[$__16,$__15]);
                      var $__18=
                       new _A_($UHC.$Base.$error,[$__17]);
                      $__swJSW3__0=
                       $__18;
                      break;}
                   $__swJSW2__0=
                    $__swJSW3__0;
                   break;
                  case 1:
                   var $__19=
                    new _A_($UHC.$Base.$ord,[65]);
                   var $__20=
                    new _A_($UHC.$Base.$ord,[$c]);
                   var $__21=
                    new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__20,$__19]);
                   var $__22=
                    new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__21,10]);
                   $__swJSW2__0=
                    $__22;
                   break;}
                $__swJSW1__0=
                 $__swJSW2__0;
                break;
               case 1:
                var $__23=
                 new _A_($UHC.$Base.$ord,[97]);
                var $__24=
                 new _A_($UHC.$Base.$ord,[$c]);
                var $__25=
                 new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__24,$__23]);
                var $__26=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__25,10]);
                $__swJSW1__0=
                 $__26;
                break;}
             $__swJSW0__0=
              $__swJSW1__0;
             break;
            case 1:
             var $__27=
              new _A_($UHC.$Base.$ord,[48]);
             var $__28=
              new _A_($UHC.$Base.$ord,[$c]);
             var $__29=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__28,$__27]);
             $__swJSW0__0=
              $__29;
             break;}
          return $__swJSW0__0;});
$Data.$Char.$isAscii=
 new _F_(function($c)
         {return new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__58__0,$c,128]);});
$Data.$Char.$isAsciiLower=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,122]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,97]);
          return new _A_($UHC.$Base.$_26_26,[$__3,$__]);});
$Data.$Char.$isAsciiUpper=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,90]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,65]);
          return new _A_($UHC.$Base.$_26_26,[$__3,$__]);});
$Data.$Char.$isControl=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,159]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,127]);
          var $__4=
           new _A_($UHC.$Base.$_26_26,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__58__0,$c,32]);
          return new _A_($UHC.$Base.$_7c_7c,[$__5,$__4]);});
$Data.$Char.$isLatin1=
 new _F_(function($c)
         {return new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,255]);});
