// Data.Ratio
var $Data=
 ($Data ? $Data : {});
$Data.$Ratio=
 ($Data.$Ratio ? $Data.$Ratio : {});
$Data.$Ratio.$__134__8__2__6NEW1UNQ6=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$Ratio.$__134__921__8__0NEW4UNQ11=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$Ratio.$__134__8__2__2NEW7UNQ4=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._1;});
$Data.$Ratio.$__134__8__2__1NEW10UNQ3=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Ratio.$__134__921__2__0NEW13UNQ9=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$Ratio.$__136__18__0=
 new _F_(function($__,$__2,$__3,$__4,$rat,$eps)
         {var $__7=
           new _A_($UHC.$Base.$Num__DCT74__271__0,[$UHC.$Base.$Integral__DCT74__143__0]);
          var $__8=
           new _A_($UHC.$Base.$_2b,[$__,$rat,$eps]);
          var $__9=
           new _A_($UHC.$Base.$_2d,[$__,$rat,$eps]);
          return new _A_($Data.$Ratio.$simplestUNQ66,[$__7,$__3,$__,$__4,$__2,$__9,$__8]);});
$Data.$Ratio.$simplest_27UNQ53=
 new _F_(function($__,$__2,$__3,$n,$d,$n_27,$d_27)
         {var $__8=
           new _A_($UHC.$Base.$quotRem,[$__2,$n_27,$d_27]);
          var $q_27=
           new _A_($Data.$Ratio.$q_27NEW19UNQ98,[$__8]);
          var $r_27=
           new _A_($Data.$Ratio.$r_27NEW22UNQ99,[$__8]);
          var $__11=
           new _A_($UHC.$Base.$quotRem,[$__2,$n,$d]);
          var $q=
           new _A_($Data.$Ratio.$qNEW26UNQ95,[$__11]);
          var $r=
           new _A_($Data.$Ratio.$rNEW29UNQ96,[$__11]);
          var $nd_27_27=
           new _A_($Data.$Ratio.$simplest_27UNQ53,[$__,$__2,$__3,$d_27,$r_27,$d,$r]);
          var $d_27_27=
           new _A_($UHC.$Base.$denominator,[$__2,$nd_27_27]);
          var $n_27_27=
           new _A_($UHC.$Base.$numerator,[$__2,$nd_27_27]);
          var $__17=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__18=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__17]);
          var $__19=
           new _A_($UHC.$Base.$_3d_3d,[$__,$r,$__18]);
          var $__20=
           _e_($__19);
          var $__swJSW5__0;
          switch($__20._tag_)
           {case 0:
             var $__21=
              new _A_($UHC.$Base.$_2f_3d,[$__,$q,$q_27]);
             var $__22=
              _e_($__21);
             var $__swJSW6__0;
             switch($__22._tag_)
              {case 0:
                var $__23=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW7__0;
                switch($__23._tag_)
                 {case 0:
                   var $__24=
                    new _A_($UHC.$Base.$packedStringToString,["FAIL 133_8_0"]);
                   var $__25=
                    new _A_($UHC.$Base.$error,[$__24]);
                   $__swJSW7__0=
                    $__25;
                   break;
                  case 1:
                   var $__26=
                    new _A_($UHC.$Base.$_2a,[$__3,$q,$n_27_27]);
                   var $__27=
                    new _A_($UHC.$Base.$_2b,[$__3,$__26,$d_27_27]);
                   var $__28=
                    new _A_($UHC.$Base.$_3a_25,[$__27,$n_27_27]);
                   $__swJSW7__0=
                    $__28;
                   break;}
                $__swJSW6__0=
                 $__swJSW7__0;
                break;
               case 1:
                var $__29=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__30=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__29]);
                var $__31=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__32=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__31]);
                var $__33=
                 new _A_($UHC.$Base.$_2b,[$__3,$q,$__32]);
                var $__34=
                 new _A_($UHC.$Base.$_3a_25,[$__33,$__30]);
                $__swJSW6__0=
                 $__34;
                break;}
             $__swJSW5__0=
              $__swJSW6__0;
             break;
            case 1:
             var $__35=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__36=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__35]);
             var $__37=
              new _A_($UHC.$Base.$_3a_25,[$q,$__36]);
             $__swJSW5__0=
              $__37;
             break;}
          return $__swJSW5__0;});
$Data.$Ratio.$q_27NEW19UNQ98=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$Ratio.$r_27NEW22UNQ99=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$Ratio.$qNEW26UNQ95=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$Ratio.$rNEW29UNQ96=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$Ratio.$simplestUNQ66=
 new _F_(function($__,$__2,$__3,$__4,$__5,$x,$y)
         {var $xr=
           new _A_($UHC.$Base.$toRational,[$__5,$x]);
          var $nd_27=
           new _A_($UHC.$Base.$toRational,[$__5,$y]);
          var $d=
           new _A_($UHC.$Base.$denominator,[$UHC.$Base.$Integral__DCT74__143__0,$xr]);
          var $n=
           new _A_($UHC.$Base.$numerator,[$UHC.$Base.$Integral__DCT74__143__0,$xr]);
          var $d_27=
           new _A_($UHC.$Base.$denominator,[$UHC.$Base.$Integral__DCT74__143__0,$nd_27]);
          var $n_27=
           new _A_($UHC.$Base.$numerator,[$UHC.$Base.$Integral__DCT74__143__0,$nd_27]);
          var $__14=
           new _A_($UHC.$Base.$_3c,[$__4,$y,$x]);
          var $__15=
           _e_($__14);
          var $__swJSW12__0;
          switch($__15._tag_)
           {case 0:
             var $__16=
              new _A_($UHC.$Base.$_3d_3d,[$__2,$x,$y]);
             var $__17=
              _e_($__16);
             var $__swJSW13__0;
             switch($__17._tag_)
              {case 0:
                var $__18=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__19=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__18]);
                var $__20=
                 new _A_($UHC.$Base.$_3e,[$__4,$x,$__19]);
                var $__21=
                 _e_($__20);
                var $__swJSW14__0;
                switch($__21._tag_)
                 {case 0:
                   var $__22=
                    new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                   var $__23=
                    new _A_($UHC.$Base.$fromInteger,[$__3,$__22]);
                   var $__24=
                    new _A_($UHC.$Base.$_3c,[$__4,$y,$__23]);
                   var $__25=
                    _e_($__24);
                   var $__swJSW15__0;
                   switch($__25._tag_)
                    {case 0:
                      var $__26=
                       _e_($UHC.$Base.$otherwise);
                      var $__swJSW16__0;
                      switch($__26._tag_)
                       {case 0:
                         var $__27=
                          new _A_($UHC.$Base.$packedStringToString,["FAIL 133_1_0"]);
                         var $__28=
                          new _A_($UHC.$Base.$error,[$__27]);
                         $__swJSW16__0=
                          $__28;
                         break;
                        case 1:
                         var $__29=
                          new _A_($UHC.$Base.$primIntToInteger,[1]);
                         var $__30=
                          new _A_($UHC.$Base.$primIntToInteger,[0]);
                         var $__31=
                          new _A_($UHC.$Base.$_3a_25,[$__30,$__29]);
                         $__swJSW16__0=
                          $__31;
                         break;}
                      $__swJSW15__0=
                       $__swJSW16__0;
                      break;
                     case 1:
                      var $__32=
                       new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__134__0,$n]);
                      var $__33=
                       new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__134__0,$n_27]);
                      var $__34=
                       new _A_($Data.$Ratio.$simplest_27UNQ53,[$UHC.$Base.$Eq__DCT74__130__0,$UHC.$Base.$Integral__DCT74__143__0,$UHC.$Base.$Num__DCT74__134__0,$__33,$d_27,$__32,$d]);
                      var $__35=
                       new _A_($UHC.$Base.$negate,[$__,$__34]);
                      $__swJSW15__0=
                       $__35;
                      break;}
                   $__swJSW14__0=
                    $__swJSW15__0;
                   break;
                  case 1:
                   var $__36=
                    new _A_($Data.$Ratio.$simplest_27UNQ53,[$UHC.$Base.$Eq__DCT74__130__0,$UHC.$Base.$Integral__DCT74__143__0,$UHC.$Base.$Num__DCT74__134__0,$n,$d,$n_27,$d_27]);
                   $__swJSW14__0=
                    $__36;
                   break;}
                $__swJSW13__0=
                 $__swJSW14__0;
                break;
               case 1:
                $__swJSW13__0=
                 $xr;
                break;}
             $__swJSW12__0=
              $__swJSW13__0;
             break;
            case 1:
             var $__37=
              new _A_($Data.$Ratio.$simplestUNQ66,[$__,$__2,$__3,$__4,$__5,$y,$x]);
             $__swJSW12__0=
              $__37;
             break;}
          return $__swJSW12__0;});
$Data.$Ratio.$approxRational=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Ratio.$__134__8__2__6NEW1UNQ6,[$__]);
          var $__3=
           new _A_($Data.$Ratio.$__134__921__8__0NEW4UNQ11,[$__2]);
          var $__4=
           new _A_($Data.$Ratio.$__134__8__2__2NEW7UNQ4,[$__]);
          var $__5=
           new _A_($Data.$Ratio.$__134__8__2__1NEW10UNQ3,[$__4]);
          var $__6=
           new _A_($Data.$Ratio.$__134__921__2__0NEW13UNQ9,[$__5]);
          return new _A_($Data.$Ratio.$__136__18__0,[$__5,$__2,$__6,$__3]);});
