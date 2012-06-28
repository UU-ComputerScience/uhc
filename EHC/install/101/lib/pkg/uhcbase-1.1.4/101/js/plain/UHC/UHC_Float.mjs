// UHC.Float
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$Float=
 ($UHC.$Float ? $UHC.$Float : {});
$UHC.$Float.$expt=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0]);}),[]);
$UHC.$Float.$__106__69__2__19NEW2UNQ11=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$UHC.$Float.$__106__69__2__18NEW5UNQ10=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$UHC.$Float.$__106__175__2__0NEW8UNQ15=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Float.$__106__167__1__0NEW11UNQ16=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Float.$__108__18__0=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $__6=
           new _A_($UHC.$Float.$__106__166__0NEW15UNQ91CCN,[$__,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__7]);
          var $x29=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__8,$x2]));
          var $__swJSW4__0;
          switch($x29._tag_)
           {case 0:
             $__swJSW4__0=
              $__6;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$_3a,[0,$UHC.$Base.$_5b_5d]);
             var $__11=
              [$__10,0];
             $__swJSW4__0=
              $__11;
             break;}
          return $__swJSW4__0;});
$UHC.$Float.$__106__166__0NEW15UNQ91CCN=
 new _F_(function($__,$x1,$x2)
         {var $p=
           new _A_($UHC.$Base.$floatDigits,[$__,$x2]);
          var $b=
           new _A_($UHC.$Base.$floatRadix,[$__,$x2]);
          var $__6=
           new _A_($UHC.$Base.$decodeFloat,[$__,$x2]);
          var $e0=
           new _A_($UHC.$Float.$e0NEW22UNQ113,[$__6]);
          var $f0=
           new _A_($UHC.$Float.$f0NEW25UNQ112,[$__6]);
          var $__9=
           new _A_($UHC.$Base.$floatRange,[$__,$x2]);
          var $minExp0=
           new _A_($UHC.$Float.$minExp0NEW29UNQ108,[$__9]);
          var $minExp=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$minExp0,$p]);
          var $__12=
           new _A_($UHC.$Float.$__106__889__0NEW66UNQ120,[$b,$e0,$f0,$minExp]);
          var $e=
           new _A_($UHC.$Float.$eNEW79UNQ122,[$__12]);
          var $f=
           new _A_($UHC.$Float.$fNEW82UNQ121,[$__12]);
          var $__15=
           new _A_($UHC.$Float.$__106__1056__0NEW85UNQ167,[$b,$e,$f,$minExp,$p]);
          var $mDn=
           new _A_($UHC.$Float.$mDnNEW136UNQ171,[$__15]);
          var $mUp=
           new _A_($UHC.$Float.$mUpNEW139UNQ170,[$__15]);
          var $r=
           new _A_($UHC.$Float.$rNEW142UNQ168,[$__15]);
          var $s=
           new _A_($UHC.$Float.$sNEW145UNQ169,[$__15]);
          var $k=
           new _A_($UHC.$Float.$kNEW148UNQ172,[$x1,$b,$e,$e0,$f,$mUp,$p,$r,$s]);
          var $rds=
           new _A_($UHC.$Float.$rdsNEW206UNQ179,[$x1,$k,$mDn,$mUp,$r,$s]);
          var $__22=
           new _A_($UHC.$Base.$reverse,[$rds]);
          var $__23=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__143__0,$UHC.$Base.$Num__DCT74__101__0]);
          var $__24=
           new _A_($UHC.$Base.$map,[$__23,$__22]);
          return [$__24,$k];});
$UHC.$Float.$e0NEW22UNQ113=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Float.$f0NEW25UNQ112=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Float.$minExp0NEW29UNQ108=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Float.$genUNQ104=
 new _F_(function($x1,$ds,$rn,$sN,$mUpN,$mDnN)
         {var $__=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$rn,$x1]);
          var $__8=
           new _A_($UHC.$Base.$divMod,[$UHC.$Base.$Integral__DCT74__143__0,$__,$sN]);
          var $dn=
           new _A_($UHC.$Float.$dnNEW36UNQ207,[$__8]);
          var $rn_27=
           new _A_($UHC.$Float.$rn_27NEW39UNQ208,[$__8]);
          var $mUpN_27=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$mUpN,$x1]);
          var $mDnN_27=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$mDnN,$x1]);
          var $__13=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$rn_27,$mUpN_27]);
          var $__14=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__132__0,$__13,$sN]);
          var $__15=
           new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__132__0,$rn_27,$mDnN_27]);
          var $__16=
           [$__15,$__14];
          var $__17=
           _e_($__16);
          var $__20=
           _e_($__17[0]);
          var $__swJSW9__0;
          switch($__20._tag_)
           {case 0:
             var $__21=
              _e_($__17[1]);
             var $__swJSW10__0;
             switch($__21._tag_)
              {case 0:
                var $__22=
                 new _A_($UHC.$Base.$_3a,[$dn,$ds]);
                var $__23=
                 new _A_($UHC.$Float.$genUNQ104,[$x1,$__22,$rn_27,$sN,$mUpN_27,$mDnN_27]);
                $__swJSW10__0=
                 $__23;
                break;
               case 1:
                var $__24=
                 new _A_($UHC.$Base.$primIntToInteger,[1]);
                var $__25=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$dn,$__24]);
                var $__26=
                 new _A_($UHC.$Base.$_3a,[$__25,$ds]);
                $__swJSW10__0=
                 $__26;
                break;}
             $__swJSW9__0=
              $__swJSW10__0;
             break;
            case 1:
             var $__27=
              _e_($__17[1]);
             var $__swJSW11__0;
             switch($__27._tag_)
              {case 0:
                var $__28=
                 new _A_($UHC.$Base.$_3a,[$dn,$ds]);
                $__swJSW11__0=
                 $__28;
                break;
               case 1:
                var $__29=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__30=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$rn_27,$__29]);
                var $__31=
                 new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__132__0,$__30,$sN]);
                var $__32=
                 _e_($__31);
                var $__swJSW12__0;
                switch($__32._tag_)
                 {case 0:
                   var $__33=
                    new _A_($UHC.$Base.$primIntToInteger,[1]);
                   var $__34=
                    new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$dn,$__33]);
                   var $__35=
                    new _A_($UHC.$Base.$_3a,[$__34,$ds]);
                   $__swJSW12__0=
                    $__35;
                   break;
                  case 1:
                   var $__36=
                    new _A_($UHC.$Base.$_3a,[$dn,$ds]);
                   $__swJSW12__0=
                    $__36;
                   break;}
                $__swJSW11__0=
                 $__swJSW12__0;
                break;}
             $__swJSW9__0=
              $__swJSW11__0;
             break;}
          return $__swJSW9__0;});
$UHC.$Float.$dnNEW36UNQ207=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Float.$rn_27NEW39UNQ208=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Float.$__106__889__0NEW66UNQ120=
 new _F_(function($b,$e0,$f0,$minExp)
         {var $n=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$minExp,$e0]);
          var $__=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$n,0]);
          var $__7=
           _e_($__);
          var $__swJSW15__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              [$f0,$e0];
             $__swJSW15__0=
              $__8;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$e0,$n]);
             var $__10=
              new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$n]);
             var $__11=
              new _A_($UHC.$Base.$div,[$UHC.$Base.$Integral__DCT74__143__0,$f0,$__10]);
             var $__12=
              [$__11,$__9];
             $__swJSW15__0=
              $__12;
             break;}
          return $__swJSW15__0;});
$UHC.$Float.$eNEW79UNQ122=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Float.$fNEW82UNQ121=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Float.$__106__1056__0NEW85UNQ167=
 new _F_(function($b,$e,$f,$minExp,$p)
         {var $__=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$e,0]);
          var $__7=
           _e_($__);
          var $__swJSW18__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$p,1]);
             var $__9=
              new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$__8]);
             var $__10=
              new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__130__0,$f,$__9]);
             var $__11=
              new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$e,$minExp]);
             var $__12=
              new _A_($UHC.$Base.$_26_26,[$__11,$__10]);
             var $__13=
              _e_($__12);
             var $__swJSW19__0;
             switch($__13._tag_)
              {case 0:
                var $__14=
                 new _A_($UHC.$Base.$primIntToInteger,[1]);
                var $__15=
                 new _A_($UHC.$Base.$primIntToInteger,[1]);
                var $__16=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__17=
                 new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$e]);
                var $__18=
                 new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$__17]);
                var $__19=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__18,$__16]);
                var $__20=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__21=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$f,$__20]);
                var $__22=
                 [$__21,$__19,$__15,$__14];
                $__swJSW19__0=
                 $__22;
                break;
               case 1:
                var $__23=
                 new _A_($UHC.$Base.$primIntToInteger,[1]);
                var $__24=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__25=
                 new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$e]);
                var $__26=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__25,1]);
                var $__27=
                 new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$__26]);
                var $__28=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__27,$__24]);
                var $__29=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__30=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$f,$b]);
                var $__31=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__30,$__29]);
                var $__32=
                 [$__31,$__28,$b,$__23];
                $__swJSW19__0=
                 $__32;
                break;}
             $__swJSW18__0=
              $__swJSW19__0;
             break;
            case 1:
             var $be=
              new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$e]);
             var $__34=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$p,1]);
             var $__35=
              new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$__34]);
             var $__36=
              new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__130__0,$f,$__35]);
             var $__37=
              _e_($__36);
             var $__swJSW20__0;
             switch($__37._tag_)
              {case 0:
                var $__38=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__39=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__40=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$f,$be]);
                var $__41=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__40,$__39]);
                var $__42=
                 [$__41,$__38,$be,$be];
                $__swJSW20__0=
                 $__42;
                break;
               case 1:
                var $__43=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$be,$b]);
                var $__44=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__45=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__44,$b]);
                var $__46=
                 new _A_($UHC.$Base.$primIntToInteger,[2]);
                var $__47=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$f,$be]);
                var $__48=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__47,$b]);
                var $__49=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__48,$__46]);
                var $__50=
                 [$__49,$__45,$__43,$b];
                $__swJSW20__0=
                 $__50;
                break;}
             $__swJSW18__0=
              $__swJSW20__0;
             break;}
          return $__swJSW18__0;});
$UHC.$Float.$mDnNEW136UNQ171=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[3];});
$UHC.$Float.$mUpNEW139UNQ170=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[2];});
$UHC.$Float.$rNEW142UNQ168=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Float.$sNEW145UNQ169=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Float.$kNEW148UNQ172=
 new _F_(function($x1,$b,$e,$e0,$f,$mUp,$p,$r,$s)
         {var $k0=
           new _A_($UHC.$Float.$k0NEW158UNQ367,[$x1,$b,$e,$e0,$f,$p]);
          return new _A_($UHC.$Float.$fixupUNQ340,[$x1,$mUp,$r,$s,$k0]);});
$UHC.$Float.$k0NEW158UNQ367=
 new _F_(function($x1,$b,$e,$e0,$f,$p)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[10]);
          var $__8=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__130__0,$x1,$__]);
          var $__9=
           new _A_($UHC.$Base.$primIntToInteger,[2]);
          var $__10=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__130__0,$b,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_26_26,[$__10,$__8]);
          var $__12=
           _e_($__11);
          var $__swJSW25__0;
          switch($__12._tag_)
           {case 0:
             var $__13=
              new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$x1]);
             var $__14=
              new _A_($UHC.$Base.$log,[$UHC.$Base.$Floating__DCT74__212__0,$__13]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$b]);
             var $__16=
              new _A_($UHC.$Base.$log,[$UHC.$Base.$Floating__DCT74__212__0,$__15]);
             var $__17=
              new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0,$e]);
             var $__18=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__177__0,$__17,$__16]);
             var $__19=
              new _A_($UHC.$Base.$primIntToInteger,[1]);
             var $__20=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$f,$__19]);
             var $__21=
              new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__20]);
             var $__22=
              new _A_($UHC.$Base.$log,[$UHC.$Base.$Floating__DCT74__212__0,$__21]);
             var $__23=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__177__0,$__22,$__18]);
             var $__24=
              new _A_($UHC.$Base.$_2f,[$UHC.$Base.$Fractional__DCT74__197__0,$__23,$__14]);
             var $__25=
              new _A_($UHC.$Base.$ceiling,[$UHC.$Base.$RealFrac__DCT74__227__0,$UHC.$Base.$Integral__DCT74__110__0,$__24]);
             $__swJSW25__0=
              $__25;
             break;
            case 1:
             var $__26=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$p,1]);
             var $__27=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__26,$e0]);
             var $__28=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,$__27,3]);
             var $__29=
              new _A_($UHC.$Base.$div,[$UHC.$Base.$Integral__DCT74__110__0,$__28,10]);
             $__swJSW25__0=
              $__29;
             break;}
          return $__swJSW25__0;});
$UHC.$Float.$fixupUNQ340=
 new _F_(function($x1,$mUp,$r,$s,$n)
         {var $__=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$n,0]);
          var $__7=
           _e_($__);
          var $__swJSW26__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$r,$mUp]);
             var $__9=
              new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$n]);
             var $__10=
              new _A_($UHC.$Float.$expt,[$x1,$__9]);
             var $__11=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__10,$__8]);
             var $__12=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__132__0,$__11,$s]);
             var $__13=
              _e_($__12);
             var $__swJSW27__0;
             switch($__13._tag_)
              {case 0:
                var $__14=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$n,1]);
                var $__15=
                 new _A_($UHC.$Float.$fixupUNQ340,[$x1,$mUp,$r,$s,$__14]);
                $__swJSW27__0=
                 $__15;
                break;
               case 1:
                $__swJSW27__0=
                 $n;
                break;}
             $__swJSW26__0=
              $__swJSW27__0;
             break;
            case 1:
             var $__16=
              new _A_($UHC.$Float.$expt,[$x1,$n]);
             var $__17=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$__16,$s]);
             var $__18=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$r,$mUp]);
             var $__19=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__132__0,$__18,$__17]);
             var $__20=
              _e_($__19);
             var $__swJSW28__0;
             switch($__20._tag_)
              {case 0:
                var $__21=
                 new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$n,1]);
                var $__22=
                 new _A_($UHC.$Float.$fixupUNQ340,[$x1,$mUp,$r,$s,$__21]);
                $__swJSW28__0=
                 $__22;
                break;
               case 1:
                $__swJSW28__0=
                 $n;
                break;}
             $__swJSW26__0=
              $__swJSW28__0;
             break;}
          return $__swJSW26__0;});
$UHC.$Float.$rdsNEW206UNQ179=
 new _F_(function($x1,$k,$mDn,$mUp,$r,$s)
         {var $__=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$k,0]);
          var $__8=
           _e_($__);
          var $__swJSW29__0;
          switch($__8._tag_)
           {case 0:
             var $__9=
              new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$k]);
             var $bk=
              new _A_($UHC.$Float.$expt,[$x1,$__9]);
             var $__11=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$mDn,$bk]);
             var $__12=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$mUp,$bk]);
             var $__13=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$r,$bk]);
             $__swJSW29__0=
              new _A_($UHC.$Float.$genUNQ104,[$x1,$UHC.$Base.$_5b_5d,$__13,$s,$__12,$__11]);
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Float.$expt,[$x1,$k]);
             var $__15=
              new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$s,$__14]);
             var $__16=
              new _A_($UHC.$Float.$genUNQ104,[$x1,$UHC.$Base.$_5b_5d,$r,$__15,$mUp,$mDn]);
             $__swJSW29__0=
              $__16;
             break;}
          return $__swJSW29__0;});
$UHC.$Float.$floatToDigits=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Float.$__106__69__2__19NEW2UNQ11,[$__]);
          var $__3=
           new _A_($UHC.$Float.$__106__69__2__18NEW5UNQ10,[$__2]);
          var $__4=
           new _A_($UHC.$Float.$__106__175__2__0NEW8UNQ15,[$__3]);
          var $__5=
           new _A_($UHC.$Float.$__106__167__1__0NEW11UNQ16,[$__4]);
          return new _A_($UHC.$Float.$__108__18__0,[$__,$__5,$__4]);});
