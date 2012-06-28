// UHC.Show
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$Show=
 ($UHC.$Show ? $UHC.$Show : {});
$UHC.$Show.$__156__10__0=
 new _F_(function($s,$r)
         {var $__=
           new _A_($UHC.$Base.$showChar,[44]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$__,$r]);
          return new _A_($UHC.$Base.$_2e,[$s,$__4]);});
$UHC.$Show.$showTuple=
 new _F_(function($ss)
         {var $__=
           new _A_($UHC.$Base.$showChar,[41]);
          var $__3=
           new _A_($UHC.$Base.$foldr1,[$UHC.$Show.$__156__10__0,$ss]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$showChar,[40]);
          return new _A_($UHC.$Base.$_2e,[$__5,$__4]);});
$UHC.$Show.$appPrec1=
 new _A_(new _F_(function()
                 {return 11;}),[]);
$UHC.$Show.$appPrec=
 new _A_(new _F_(function()
                 {return 10;}),[]);
$UHC.$Show.$showIntegral=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$toInteger,[$__]);
          var $__3=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__157__0]);
          return new _A_($UHC.$Base.$_2e,[$__3,$__2]);});
$UHC.$Show.$fUNQ143=
 new _F_(function($base,$b2,$x1,$x2)
         {var $n=
           new _A_($UHC.$Show.$nNEW16UNQ172CCN,[$base,$b2,$x1,$x2]);
          var $x26=
           _e_($x2);
          var $__swJSW0__0;
          switch($x26._tag_)
           {case 0:
             $__swJSW0__0=
              $n;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$replicate,[$x1,0]);
             var $__10=
              [0,$__];
             $__swJSW0__0=
              $__10;
             break;}
          return $__swJSW0__0;});
$UHC.$Show.$nNEW16UNQ172CCN=
 new _F_(function($base,$b2,$x1,$x2)
         {var $__=
           new _A_($UHC.$Show.$__154__805__0NEW21UNQ173CCN,[$base,$b2,$x1,$x2]);
          var $x16=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,0,$x1]));
          var $__swJSW1__0;
          switch($x16._tag_)
           {case 0:
             $__swJSW1__0=
              $__;
             break;
            case 1:
             var $x27=
              _e_($x2);
             var $__swJSW2__0;
             switch($x27._tag_)
              {case 0:
                var $__10=
                 new _A_($UHC.$Show.$__156__90NEW45,[$b2,$x27._1]);
                var $__11=
                 [$__10,$UHC.$Base.$_5b_5d];
                $__swJSW2__0=
                 $__11;
                break;
               case 1:
                $__swJSW2__0=
                 $__;
                break;}
             $__swJSW1__0=
              $__swJSW2__0;
             break;}
          return $__swJSW1__0;});
$UHC.$Show.$__154__805__0NEW21UNQ173CCN=
 new _F_(function($base,$b2,$x1,$x2)
         {var $x25=
           _e_($x2);
          var $__swJSW3__0;
          switch($x25._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$x1,1]);
             var $__9=
              new _A_($UHC.$Show.$fUNQ143,[$base,$b2,$__,$x25._2]);
             var $c=
              new _A_($UHC.$Show.$cNEW29UNQ183,[$__9]);
             var $ds=
              new _A_($UHC.$Show.$dsNEW32UNQ184,[$__9]);
             var $i_27=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$c,$x25._1]);
             var $__13=
              new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$i_27,$base]);
             var $__14=
              _e_($__13);
             var $__swJSW4__0;
             switch($__14._tag_)
              {case 0:
                var $__15=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW5__0;
                switch($__15._tag_)
                 {case 0:
                   $__swJSW5__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__16=
                    new _A_($UHC.$Base.$_3a,[$i_27,$ds]);
                   var $__17=
                    [0,$__16];
                   $__swJSW5__0=
                    $__17;
                   break;}
                $__swJSW4__0=
                 $__swJSW5__0;
                break;
               case 1:
                var $__18=
                 new _A_($UHC.$Base.$_3a,[0,$ds]);
                var $__19=
                 [1,$__18];
                $__swJSW4__0=
                 $__19;
                break;}
             $__swJSW3__0=
              $__swJSW4__0;
             break;
            case 1:
             $__swJSW3__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW3__0;});
$UHC.$Show.$cNEW29UNQ183=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Show.$dsNEW32UNQ184=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Show.$__156__90NEW45=
 new _F_(function($b2,$x)
         {var $__=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$x,$b2]);
          var $__4=
           _e_($__);
          var $__swJSW8__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW8__0=
              0;
             break;
            case 1:
             $__swJSW8__0=
              1;
             break;}
          return $__swJSW8__0;});
$UHC.$Show.$roundTo=
 new _F_(function($base,$d,$is)
         {var $b2=
           new _A_($UHC.$Base.$div,[$UHC.$Base.$Integral__DCT74__110__0,$base,2]);
          var $__=
           new _A_($UHC.$Show.$fUNQ143,[$base,$b2,$d,$is]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["roundTo: bad Value"]);
          var $x=
           new _A_($UHC.$Base.$error,[$__6]);
          var $__8=
           _e_($__);
          var $__11=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,0,$__8[0]]));
          var $__swJSW10__0;
          switch($__11._tag_)
           {case 0:
             var $__12=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,1,$__8[0]]));
             var $__swJSW11__0;
             switch($__12._tag_)
              {case 0:
                $__swJSW11__0=
                 $x;
                break;
               case 1:
                var $__13=
                 new _A_($UHC.$Base.$_3a,[1,$__8[1]]);
                var $__14=
                 [1,$__13];
                $__swJSW11__0=
                 $__14;
                break;}
             $__swJSW10__0=
              $__swJSW11__0;
             break;
            case 1:
             $__swJSW10__0=
              $__8;
             break;}
          return $__swJSW10__0;});
$UHC.$Show.$Show__NEW64UNQ658DCT152__1__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           new _A_($UHC.$Show.$Show__NEW67UNQ660EVLDCT152__1__0RDC,[$Show__,$Show__2]);
          return $Show__3;});
$UHC.$Show.$Show__NEW67UNQ660EVLDCT152__1__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__7=
           {_tag_:0,_1:$Show__2,_2:$Show__3._2,_3:$Show__3._3};
          return $__7;});
$UHC.$Show.$Show__DCT152__1__0=
 new _F_(function($__)
         {var $Show__DCT152__1__0DFLUHC_2eBase_2eshow=
           new _A_($UHC.$Show.$showIntegral,[$__]);
          var $Show__=
           _i_();
          _i_set_($Show__,new _A_($UHC.$Show.$Show__NEW64UNQ658DCT152__1__0RDC,[$Show__,$Show__DCT152__1__0DFLUHC_2eBase_2eshow]));
          return $Show__;});
$UHC.$Show.$showInt=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$Show__DCT152__1__0,[$__]);
          return new _A_($UHC.$Base.$shows,[$__2]);});
$UHC.$Show.$__154__147__2__4NEW76UNQ52=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Show.$__154__487__2__0NEW79UNQ44=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Show.$__154__497__2__0NEW82UNQ43=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Show.$__154__361__1__0NEW85UNQ50=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Show.$__156__160__0=
 new _F_(function($__,$__2,$__3,$__4,$__5,$base,$toChr,$n0,$r0)
         {var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__4,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_3c_3d,[$__3,$base,$__11]);
          var $__13=
           _e_($__12);
          var $__swJSW17__0;
          switch($__13._tag_)
           {case 0:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__4,$__14]);
             var $__16=
              new _A_($UHC.$Base.$_3c,[$__3,$n0,$__15]);
             var $__17=
              _e_($__16);
             var $__swJSW18__0;
             switch($__17._tag_)
              {case 0:
                var $__18=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW19__0;
                switch($__18._tag_)
                 {case 0:
                   var $__19=
                    new _A_($UHC.$Base.$packedStringToString,["FAIL 153_6_0"]);
                   var $__20=
                    new _A_($UHC.$Base.$error,[$__19]);
                   $__swJSW19__0=
                    $__20;
                   break;
                  case 1:
                   var $__21=
                    new _A_($UHC.$Base.$quotRem,[$__,$n0,$base]);
                   var $__22=
                    new _A_($UHC.$Show.$showItUNQ94,[$__,$__2,$__4,$base,$toChr,$__21,$r0]);
                   $__swJSW19__0=
                    $__22;
                   break;}
                $__swJSW18__0=
                 $__swJSW19__0;
                break;
               case 1:
                var $__23=
                 new _A_($UHC.$Base.$show,[$__5,$n0]);
                var $__24=
                 new _A_($UHC.$Base.$packedStringToString,["Numeric.showIntAtBase: applied to negative number "]);
                var $__25=
                 new _A_($UHC.$Base.$_2b_2b,[$__24,$__23]);
                var $__26=
                 new _A_($UHC.$Base.$error,[$__25]);
                $__swJSW18__0=
                 $__26;
                break;}
             $__swJSW17__0=
              $__swJSW18__0;
             break;
            case 1:
             var $__27=
              new _A_($UHC.$Base.$show,[$__5,$base]);
             var $__28=
              new _A_($UHC.$Base.$packedStringToString,["Numeric.showIntAtBase: applied to unsupported base "]);
             var $__29=
              new _A_($UHC.$Base.$_2b_2b,[$__28,$__27]);
             var $__30=
              new _A_($UHC.$Base.$error,[$__29]);
             $__swJSW17__0=
              $__30;
             break;}
          return $__swJSW17__0;});
$UHC.$Show.$showItUNQ94=
 new _F_(function($__,$__2,$__3,$base,$toChr,$__6)
         {var $__7=
           _e_($__6);
          return new _A_($UHC.$Show.$__156__173__0,[$__,$__2,$__3,$base,$toChr,$__7[1],$__7[0]]);});
$UHC.$Show.$__156__173__0=
 new _F_(function($__,$__2,$__3,$base,$toChr,$d,$n,$r)
         {var $__9=
           new _A_($UHC.$Base.$fromIntegral,[$__,$UHC.$Base.$Num__DCT74__101__0,$d]);
          var $c=
           new _A_($toChr,[$__9]);
          var $r_27=
           new _A_($UHC.$Base.$_3a,[$c,$r]);
          var $__12=
           new _A_($UHC.$Show.$__156__186NEW95,[$__,$__2,$__3,$base,$toChr,$n,$r_27]);
          var $__13=
           new _A_($UHC.$Base.$seq,[$c]);
          return new _A_($UHC.$Base.$_24,[$__13,$__12]);});
$UHC.$Show.$__156__186NEW95=
 new _F_(function($__,$__2,$__3,$base,$toChr,$n,$r_27)
         {var $__8=
           new _A_($UHC.$Base.$quotRem,[$__,$n,$base]);
          var $__9=
           new _A_($UHC.$Show.$showItUNQ94,[$__,$__2,$__3,$base,$toChr,$__8,$r_27]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
          var $__12=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__11,$n]));
          var $__swJSW21__0;
          switch($__12._tag_)
           {case 0:
             $__swJSW21__0=
              $__9;
             break;
            case 1:
             $__swJSW21__0=
              $r_27;
             break;}
          return $__swJSW21__0;});
$UHC.$Show.$showIntAtBase=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$Show__DCT152__1__0,[$__]);
          var $__3=
           new _A_($UHC.$Show.$__154__147__2__4NEW76UNQ52,[$__]);
          var $__4=
           new _A_($UHC.$Show.$__154__487__2__0NEW79UNQ44,[$__3]);
          var $__5=
           new _A_($UHC.$Show.$__154__497__2__0NEW82UNQ43,[$__3]);
          var $__6=
           new _A_($UHC.$Show.$__154__361__1__0NEW85UNQ50,[$__5]);
          return new _A_($UHC.$Show.$__156__160__0,[$__,$__6,$__4,$__5,$__2]);});
$UHC.$Show.$__154__3709__2__1NEW131UNQ641=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Show.$__154__3737__2__0NEW134UNQ640=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Show.$showHex=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$__154__3709__2__1NEW131UNQ641,[$__]);
          var $__3=
           new _A_($UHC.$Show.$__154__3737__2__0NEW134UNQ640,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["16"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__4]);
          return new _A_($UHC.$Show.$showIntAtBase,[$__,$__5,$UHC.$Char.$intToDigit]);});
$UHC.$Show.$__154__3644__2__1NEW140UNQ622=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Show.$__154__3672__2__0NEW143UNQ621=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Show.$showOct=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$__154__3644__2__1NEW140UNQ622,[$__]);
          var $__3=
           new _A_($UHC.$Show.$__154__3672__2__0NEW143UNQ621,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["8"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__4]);
          return new _A_($UHC.$Show.$showIntAtBase,[$__,$__5,$UHC.$Char.$intToDigit]);});
$UHC.$Show.$Show__DCT152__0__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($p,$__)
         {var $__3=
           _e_($__);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["()"]);
          var $__5=
           new _A_($UHC.$Base.$showString,[$__4]);
          return $__5;});
$UHC.$Show.$Show__NEW153UNQ665DCT152__0__0RDC=
 new _F_(function($Show__)
         {var $Show__2=
           new _A_($UHC.$Show.$Show__NEW155UNQ666EVLDCT152__0__0RDC,[$Show__]);
          return $Show__2;});
$UHC.$Show.$Show__NEW155UNQ666EVLDCT152__0__0RDC=
 new _F_(function($Show__)
         {var $Show__2=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__6=
           {_tag_:0,_1:$Show__2._1,_2:$Show__2._2,_3:$UHC.$Show.$Show__DCT152__0__0DFLUHC_2eBase_2eshowsPrec};
          return $__6;});
$UHC.$Show.$Show__UNQ665DCT152__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$Show__NEW153UNQ665DCT152__0__0RDC,[$UHC.$Show.$Show__UNQ665DCT152__0__0RDC]);}),[]);
$UHC.$Show.$Show__DCT152__0__0=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$Show__UNQ665DCT152__0__0RDC;}),[]);
$UHC.$Show.$Show__DCT152__25__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$shows,[$__2,$__5[1]]);
          var $__9=
           new _A_($UHC.$Base.$_3a,[$__8,$UHC.$Base.$_5b_5d]);
          var $__10=
           new _A_($UHC.$Base.$shows,[$__,$__5[0]]);
          var $__11=
           new _A_($UHC.$Base.$_3a,[$__10,$__9]);
          var $__12=
           new _A_($UHC.$Show.$showTuple,[$__11]);
          return $__12;});
$UHC.$Show.$Show__NEW167UNQ676DCT152__25__0RDC=
 new _F_(function($__,$Show__,$__3)
         {var $Show__4=
           new _A_($UHC.$Show.$Show__NEW171UNQ679EVLDCT152__25__0RDC,[$__,$Show__,$__3]);
          return $Show__4;});
$UHC.$Show.$Show__NEW171UNQ679EVLDCT152__25__0RDC=
 new _F_(function($__,$Show__,$__3)
         {var $Show__4=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__8=
           new _A_($UHC.$Show.$Show__DCT152__25__0DFLUHC_2eBase_2eshowsPrec,[$__,$__3]);
          var $__9=
           {_tag_:0,_1:$Show__4._1,_2:$Show__4._2,_3:$__8};
          return $__9;});
$UHC.$Show.$Show__DCT152__25__0=
 new _F_(function($__,$__2)
         {var $Show__=
           _i_();
          _i_set_($Show__,new _A_($UHC.$Show.$Show__NEW167UNQ676DCT152__25__0RDC,[$__,$Show__,$__2]));
          return $Show__;});
$UHC.$Show.$Show__DCT152__28__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7)
         {var $__8=
           _e_($__7);
          var $__14=
           new _A_($UHC.$Base.$shows,[$__5,$__8[4]]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$__14,$UHC.$Base.$_5b_5d]);
          var $__16=
           new _A_($UHC.$Base.$shows,[$__4,$__8[3]]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[$__16,$__15]);
          var $__18=
           new _A_($UHC.$Base.$shows,[$__3,$__8[2]]);
          var $__19=
           new _A_($UHC.$Base.$_3a,[$__18,$__17]);
          var $__20=
           new _A_($UHC.$Base.$shows,[$__2,$__8[1]]);
          var $__21=
           new _A_($UHC.$Base.$_3a,[$__20,$__19]);
          var $__22=
           new _A_($UHC.$Base.$shows,[$__,$__8[0]]);
          var $__23=
           new _A_($UHC.$Base.$_3a,[$__22,$__21]);
          var $__24=
           new _A_($UHC.$Show.$showTuple,[$__23]);
          return $__24;});
$UHC.$Show.$Show__NEW191UNQ701DCT152__28__0RDC=
 new _F_(function($__,$__2,$__3,$__4,$Show__,$__6)
         {var $Show__7=
           new _A_($UHC.$Show.$Show__NEW198UNQ707EVLDCT152__28__0RDC,[$__,$__2,$__3,$__4,$Show__,$__6]);
          return $Show__7;});
$UHC.$Show.$Show__NEW198UNQ707EVLDCT152__28__0RDC=
 new _F_(function($__,$__2,$__3,$__4,$Show__,$__6)
         {var $Show__7=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__11=
           new _A_($UHC.$Show.$Show__DCT152__28__0DFLUHC_2eBase_2eshowsPrec,[$__,$__2,$__3,$__4,$__6]);
          var $__12=
           {_tag_:0,_1:$Show__7._1,_2:$Show__7._2,_3:$__11};
          return $__12;});
$UHC.$Show.$Show__DCT152__28__0=
 new _F_(function($__,$__2,$__3,$__4,$__5)
         {var $Show__=
           _i_();
          _i_set_($Show__,new _A_($UHC.$Show.$Show__NEW191UNQ701DCT152__28__0RDC,[$__,$__2,$__3,$__4,$Show__,$__5]));
          return $Show__;});
$UHC.$Show.$Show__DCT152__27__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6)
         {var $__7=
           _e_($__6);
          var $__12=
           new _A_($UHC.$Base.$shows,[$__4,$__7[3]]);
          var $__13=
           new _A_($UHC.$Base.$_3a,[$__12,$UHC.$Base.$_5b_5d]);
          var $__14=
           new _A_($UHC.$Base.$shows,[$__3,$__7[2]]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$__14,$__13]);
          var $__16=
           new _A_($UHC.$Base.$shows,[$__2,$__7[1]]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[$__16,$__15]);
          var $__18=
           new _A_($UHC.$Base.$shows,[$__,$__7[0]]);
          var $__19=
           new _A_($UHC.$Base.$_3a,[$__18,$__17]);
          var $__20=
           new _A_($UHC.$Show.$showTuple,[$__19]);
          return $__20;});
$UHC.$Show.$Show__NEW219UNQ743DCT152__27__0RDC=
 new _F_(function($__,$__2,$__3,$Show__,$__5)
         {var $Show__6=
           new _A_($UHC.$Show.$Show__NEW225UNQ748EVLDCT152__27__0RDC,[$__,$__2,$__3,$Show__,$__5]);
          return $Show__6;});
$UHC.$Show.$Show__NEW225UNQ748EVLDCT152__27__0RDC=
 new _F_(function($__,$__2,$__3,$Show__,$__5)
         {var $Show__6=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__10=
           new _A_($UHC.$Show.$Show__DCT152__27__0DFLUHC_2eBase_2eshowsPrec,[$__,$__2,$__3,$__5]);
          var $__11=
           {_tag_:0,_1:$Show__6._1,_2:$Show__6._2,_3:$__10};
          return $__11;});
$UHC.$Show.$Show__DCT152__27__0=
 new _F_(function($__,$__2,$__3,$__4)
         {var $Show__=
           _i_();
          _i_set_($Show__,new _A_($UHC.$Show.$Show__NEW219UNQ743DCT152__27__0RDC,[$__,$__2,$__3,$Show__,$__4]));
          return $Show__;});
$UHC.$Show.$Show__DCT152__26__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2,$__3,$__4,$__5)
         {var $__6=
           _e_($__5);
          var $__10=
           new _A_($UHC.$Base.$shows,[$__3,$__6[2]]);
          var $__11=
           new _A_($UHC.$Base.$_3a,[$__10,$UHC.$Base.$_5b_5d]);
          var $__12=
           new _A_($UHC.$Base.$shows,[$__2,$__6[1]]);
          var $__13=
           new _A_($UHC.$Base.$_3a,[$__12,$__11]);
          var $__14=
           new _A_($UHC.$Base.$shows,[$__,$__6[0]]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$__14,$__13]);
          var $__16=
           new _A_($UHC.$Show.$showTuple,[$__15]);
          return $__16;});
$UHC.$Show.$Show__NEW243UNQ778DCT152__26__0RDC=
 new _F_(function($__,$__2,$Show__,$__4)
         {var $Show__5=
           new _A_($UHC.$Show.$Show__NEW248UNQ782EVLDCT152__26__0RDC,[$__,$__2,$Show__,$__4]);
          return $Show__5;});
$UHC.$Show.$Show__NEW248UNQ782EVLDCT152__26__0RDC=
 new _F_(function($__,$__2,$Show__,$__4)
         {var $Show__5=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__9=
           new _A_($UHC.$Show.$Show__DCT152__26__0DFLUHC_2eBase_2eshowsPrec,[$__,$__2,$__4]);
          var $__10=
           {_tag_:0,_1:$Show__5._1,_2:$Show__5._2,_3:$__9};
          return $__10;});
$UHC.$Show.$Show__DCT152__26__0=
 new _F_(function($__,$__2,$__3)
         {var $Show__=
           _i_();
          _i_set_($Show__,new _A_($UHC.$Show.$Show__NEW243UNQ778DCT152__26__0RDC,[$__,$__2,$Show__,$__3]));
          return $Show__;});
$UHC.$Show.$FFGeneric__=
 new _A_(new _F_(function()
                 {return {_tag_:2};}),[]);
$UHC.$Show.$FFFixed__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$UHC.$Show.$FFExponent__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$UHC.$Show.$__Rep0FFFormatDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__2=
           _e_($proj__1);
          var $__swJSW36__0;
          switch($proj__2._tag_)
           {case 0:
             var $proj__4=
              _e_($proj__2.unL1);
             $__swJSW36__0=
              $UHC.$Show.$FFExponent__;
             break;
            case 1:
             var $proj__56=
              _e_($proj__2.unR1);
             var $__swJSW38__0;
             switch($proj__56._tag_)
              {case 0:
                var $proj__7=
                 _e_($proj__56.unL1);
                $__swJSW38__0=
                 $UHC.$Show.$FFFixed__;
                break;
               case 1:
                var $proj__9=
                 _e_($proj__56.unR1);
                $__swJSW38__0=
                 $UHC.$Show.$FFGeneric__;
                break;}
             $__swJSW36__0=
              $__swJSW38__0;
             break;}
          return $__swJSW36__0;});
$UHC.$Show.$__Rep0FFFormatDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__swJSW41__0;
          switch($x2._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__4=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__5=
              new _A_($UHC.$Base.$M1__,[$__4]);
             $__swJSW41__0=
              $__5;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__7=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__8=
              new _A_($UHC.$Base.$R1__,[$__7]);
             var $__9=
              new _A_($UHC.$Base.$M1__,[$__8]);
             $__swJSW41__0=
              $__9;
             break;
            case 2:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__11=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__12=
              new _A_($UHC.$Base.$R1__,[$__11]);
             var $__13=
              new _A_($UHC.$Base.$M1__,[$__12]);
             $__swJSW41__0=
              $__13;
             break;}
          return $__swJSW41__0;});
$UHC.$Show.$__Rep0FFFormatNEW278UNQ220SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$__Rep0FFFormatNEW280UNQ221EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$Show.$__Rep0FFFormatNEW280UNQ221EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Show.$__Rep0FFFormatDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$Show.$__Rep0FFFormatDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$Show.$__Rep0FFFormatUNQ220SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$__Rep0FFFormatNEW278UNQ220SDCGENRepresentable0,[$UHC.$Show.$__Rep0FFFormatUNQ220SDCGENRepresentable0]);}),[]);
$UHC.$Show.$__Rep0FFFormatGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$__Rep0FFFormatUNQ220SDCGENRepresentable0;}),[]);
$UHC.$Show.$__154__1303__2__5NEW285UNQ287=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$UHC.$Show.$__154__1303__2__4NEW288UNQ286=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$UHC.$Show.$__154__3279__2__0NEW291UNQ279=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Show.$__154__1303__2__2NEW294UNQ285=
 new _F_(function($__)
         {var $RealFrac__=
           _e_($__);
          return $RealFrac__._2;});
$UHC.$Show.$__154__1303__2__1NEW297UNQ284=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Show.$__154__3269__2__0NEW300UNQ278=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Show.$__156__500__0=
 new _F_(function($__,$__2,$__3,$fmt,$decs,$x)
         {var $__7=
           new _A_($UHC.$Base.$isNaN,[$__,$x]);
          var $__8=
           _e_($__7);
          var $__swJSW49__0;
          switch($__8._tag_)
           {case 0:
             var $__9=
              new _A_($UHC.$Base.$isInfinite,[$__,$x]);
             var $__10=
              _e_($__9);
             var $__swJSW50__0;
             switch($__10._tag_)
              {case 0:
                var $__11=
                 new _A_($UHC.$Base.$isNegativeZero,[$__,$x]);
                var $__12=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__13=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__12]);
                var $__14=
                 new _A_($UHC.$Base.$_3c,[$__2,$x,$__13]);
                var $__15=
                 new _A_($UHC.$Base.$_7c_7c,[$__14,$__11]);
                var $__16=
                 _e_($__15);
                var $__swJSW51__0;
                switch($__16._tag_)
                 {case 0:
                   var $__17=
                    _e_($UHC.$Base.$otherwise);
                   var $__swJSW52__0;
                   switch($__17._tag_)
                    {case 0:
                      var $__18=
                       new _A_($UHC.$Base.$packedStringToString,["FAIL 153_14_0"]);
                      var $__19=
                       new _A_($UHC.$Base.$error,[$__18]);
                      $__swJSW52__0=
                       $__19;
                      break;
                     case 1:
                      var $__20=
                       new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0,10]);
                      var $__21=
                       new _A_($UHC.$Float.$floatToDigits,[$__,$__20,$x]);
                      var $__22=
                       new _A_($UHC.$Show.$doFmtUNQ395,[$decs,$fmt,$__21]);
                      $__swJSW52__0=
                       $__22;
                      break;}
                   $__swJSW51__0=
                    $__swJSW52__0;
                   break;
                  case 1:
                   var $__23=
                    new _A_($UHC.$Base.$negate,[$__3,$x]);
                   var $__24=
                    new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0,10]);
                   var $__25=
                    new _A_($UHC.$Float.$floatToDigits,[$__,$__24,$__23]);
                   var $__26=
                    new _A_($UHC.$Show.$doFmtUNQ395,[$decs,$fmt,$__25]);
                   var $__27=
                    new _A_($UHC.$Base.$_3a,[45,$__26]);
                   $__swJSW51__0=
                    $__27;
                   break;}
                $__swJSW50__0=
                 $__swJSW51__0;
                break;
               case 1:
                var $__28=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__29=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__28]);
                var $__30=
                 new _A_($UHC.$Base.$_3c,[$__2,$x,$__29]);
                var $__31=
                 _e_($__30);
                var $__swJSW53__0;
                switch($__31._tag_)
                 {case 0:
                   var $__32=
                    new _A_($UHC.$Base.$packedStringToString,["Infinity"]);
                   $__swJSW53__0=
                    $__32;
                   break;
                  case 1:
                   var $__33=
                    new _A_($UHC.$Base.$packedStringToString,["-Infinity"]);
                   $__swJSW53__0=
                    $__33;
                   break;}
                $__swJSW50__0=
                 $__swJSW53__0;
                break;}
             $__swJSW49__0=
              $__swJSW50__0;
             break;
            case 1:
             var $__34=
              new _A_($UHC.$Base.$packedStringToString,["NaN"]);
             $__swJSW49__0=
              $__34;
             break;}
          return $__swJSW49__0;});
$UHC.$Show.$doFmtUNQ395=
 new _F_(function($decs,$format,$__)
         {var $__4=
           _e_($__);
          var $ds=
           new _A_($UHC.$Base.$map,[$UHC.$Char.$intToDigit,$__4[0]]);
          var $__8=
           _e_($format);
          var $__swJSW55__0;
          switch($__8._tag_)
           {case 0:
             var $__9=
              _e_($decs);
             var $__swJSW56__0;
             switch($__9._tag_)
              {case 0:
                var $dec_27=
                 new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,$__9._1,1]);
                var $__12=
                 new _A_($UHC.$Show.$__154__1828__0NEW310UNQ422CCN,[$__4[1],$__4[0],$dec_27]);
                var $__13=
                 _e_($__4[0]);
                var $__swJSW57__0;
                switch($__13._tag_)
                 {case 0:
                   var $__16=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,0,$__13._1]));
                   var $__swJSW58__0;
                   switch($__16._tag_)
                    {case 0:
                      $__swJSW58__0=
                       $__12;
                      break;
                     case 1:
                      var $__17=
                       _e_($__13._2);
                      var $__swJSW59__0;
                      switch($__17._tag_)
                       {case 0:
                         $__swJSW59__0=
                          $__12;
                         break;
                        case 1:
                         var $__20=
                          new _A_($UHC.$Base.$packedStringToString,["e0"]);
                         var $__21=
                          new _A_($UHC.$Base.$repeat,[48]);
                         var $__22=
                          new _A_($UHC.$Base.$take,[$dec_27,$__21]);
                         var $__23=
                          new _A_($UHC.$Base.$_2b_2b,[$__22,$__20]);
                         var $__24=
                          new _A_($UHC.$Base.$_3a,[46,$__23]);
                         var $__25=
                          new _A_($UHC.$Base.$_3a,[48,$__24]);
                         $__swJSW59__0=
                          $__25;
                         break;}
                      $__swJSW58__0=
                       $__swJSW59__0;
                      break;}
                   $__swJSW57__0=
                    $__swJSW58__0;
                   break;
                  case 1:
                   $__swJSW57__0=
                    $__12;
                   break;}
                $__swJSW56__0=
                 $__swJSW57__0;
                break;
               case 1:
                var $__26=
                 new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__4[1],1]);
                var $show__e_27=
                 new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$__26]);
                var $__28=
                 _e_($ds);
                var $__swJSW60__0;
                switch($__28._tag_)
                 {case 0:
                   var $__31=
                    new _A_($UHC.$Show.$__154__1648__0NEW353UNQ462CCN,[$show__e_27,$__28._1,$__28._2]);
                   var $__32=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,48,$__28._1]));
                   var $__swJSW61__0;
                   switch($__32._tag_)
                    {case 0:
                      $__swJSW61__0=
                       $__31;
                      break;
                     case 1:
                      var $__33=
                       _e_($__28._2);
                      var $__swJSW62__0;
                      switch($__33._tag_)
                       {case 0:
                         $__swJSW62__0=
                          $__31;
                         break;
                        case 1:
                         var $__36=
                          new _A_($UHC.$Base.$packedStringToString,["0.0e0"]);
                         $__swJSW62__0=
                          $__36;
                         break;}
                      $__swJSW61__0=
                       $__swJSW62__0;
                      break;}
                   $__swJSW60__0=
                    $__swJSW61__0;
                   break;
                  case 1:
                   var $__37=
                    new _A_($UHC.$Base.$packedStringToString,["formatRealFloat/doFmt/FFExponent: []"]);
                   var $__38=
                    new _A_($UHC.$Base.$error,[$__37]);
                   $__swJSW60__0=
                    $__38;
                   break;}
                $__swJSW56__0=
                 $__swJSW60__0;
                break;}
             $__swJSW55__0=
              $__swJSW56__0;
             break;
            case 1:
             var $__39=
              _e_($decs);
             var $__swJSW63__0;
             switch($__39._tag_)
              {case 0:
                var $dec_27=
                 new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,$__39._1,0]);
                var $__42=
                 new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$__4[1],0]);
                var $__43=
                 _e_($__42);
                var $__swJSW64__0;
                switch($__43._tag_)
                 {case 0:
                   var $__44=
                    new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$__4[1]]);
                   var $__45=
                    new _A_($UHC.$Base.$replicate,[$__44,0]);
                   var $__46=
                    new _A_($UHC.$Base.$_2b_2b,[$__45,$__4[0]]);
                   var $__47=
                    new _A_($UHC.$Show.$roundTo,[10,$dec_27,$__46]);
                   var $ei=
                    new _A_($UHC.$Show.$eiNEW382UNQ492,[$__47]);
                   var $is_27=
                    new _A_($UHC.$Show.$is_27NEW385UNQ493,[$__47]);
                   var $__50=
                    new _A_($UHC.$Show.$__156__662NEW388,[$ei,$is_27]);
                   var $__51=
                    new _A_($UHC.$Base.$map,[$UHC.$Char.$intToDigit,$__50]);
                   var $d=
                    new _A_($UHC.$Show.$dNEW395UNQ498,[$__51]);
                   var $ds_27=
                    new _A_($UHC.$Show.$ds_27NEW398UNQ499,[$__51]);
                   var $__54=
                    new _A_($UHC.$Show.$__156__678NEW401,[$ds_27]);
                   $__swJSW64__0=
                    new _A_($UHC.$Base.$_3a,[$d,$__54]);
                   break;
                  case 1:
                   var $__55=
                    new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$dec_27,$__4[1]]);
                   var $__56=
                    new _A_($UHC.$Show.$roundTo,[10,$__55,$__4[0]]);
                   var $ei=
                    new _A_($UHC.$Show.$eiNEW409UNQ521,[$__56]);
                   var $is_27=
                    new _A_($UHC.$Show.$is_27NEW412UNQ522,[$__56]);
                   var $__59=
                    new _A_($UHC.$Base.$map,[$UHC.$Char.$intToDigit,$is_27]);
                   var $__60=
                    new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__4[1],$ei]);
                   var $__61=
                    new _A_($UHC.$Base.$splitAt,[$__60,$__59]);
                   var $ls=
                    new _A_($UHC.$Show.$lsNEW418UNQ525,[$__61]);
                   var $rs=
                    new _A_($UHC.$Show.$rsNEW421UNQ526,[$__61]);
                   var $__64=
                    new _A_($UHC.$Show.$__156__715NEW424,[$rs]);
                   var $__65=
                    new _A_($UHC.$Show.$mk0UNQ474,[$ls]);
                   $__swJSW64__0=
                    new _A_($UHC.$Base.$_2b_2b,[$__65,$__64]);
                   break;}
                $__swJSW63__0=
                 $__swJSW64__0;
                break;
               case 1:
                var $__66=
                 new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__91__0,$__4[1],0]);
                var $__67=
                 _e_($__66);
                var $__swJSW65__0;
                switch($__67._tag_)
                 {case 0:
                   var $__68=
                    _e_($UHC.$Base.$otherwise);
                   var $__swJSW66__0;
                   switch($__68._tag_)
                    {case 0:
                      $__swJSW66__0=
                       $UHC.$Base.$undefined;
                      break;
                     case 1:
                      var $__69=
                       new _A_($UHC.$Base.$packedStringToString,[""]);
                      $__swJSW66__0=
                       new _A_($UHC.$Show.$fUNQ551,[$UHC.$Base.$Eq__DCT74__88__0,$UHC.$Base.$Num__DCT74__101__0,$__4[1],$__69,$ds]);
                      break;}
                   $__swJSW65__0=
                    $__swJSW66__0;
                   break;
                  case 1:
                   var $__70=
                    new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$__4[1]]);
                   var $__71=
                    new _A_($UHC.$Base.$replicate,[$__70,48]);
                   var $__72=
                    new _A_($UHC.$Base.$_2b_2b,[$__71,$ds]);
                   var $__73=
                    new _A_($UHC.$Base.$packedStringToString,["0."]);
                   var $__74=
                    new _A_($UHC.$Base.$_2b_2b,[$__73,$__72]);
                   $__swJSW65__0=
                    $__74;
                   break;}
                $__swJSW63__0=
                 $__swJSW65__0;
                break;}
             $__swJSW55__0=
              $__swJSW63__0;
             break;
            case 2:
             var $__75=
              [$__4[0],$__4[1]];
             var $__76=
              new _A_($UHC.$Show.$__156__803NEW468,[$__4[1]]);
             var $__77=
              new _A_($UHC.$Show.$doFmtUNQ395,[$decs,$__76,$__75]);
             $__swJSW55__0=
              $__77;
             break;}
          return $__swJSW55__0;});
$UHC.$Show.$__154__1828__0NEW310UNQ422CCN=
 new _F_(function($e,$is,$dec_27)
         {var $__=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$dec_27,1]);
          var $__5=
           new _A_($UHC.$Show.$roundTo,[10,$__,$is]);
          var $ei=
           new _A_($UHC.$Show.$eiNEW316UNQ426,[$__5]);
          var $is_27=
           new _A_($UHC.$Show.$is_27NEW319UNQ427,[$__5]);
          var $__8=
           new _A_($UHC.$Show.$__156__541NEW322,[$ei,$is_27]);
          var $__9=
           new _A_($UHC.$Base.$map,[$UHC.$Char.$intToDigit,$__8]);
          var $d=
           new _A_($UHC.$Show.$dNEW329UNQ431,[$__9]);
          var $ds_27=
           new _A_($UHC.$Show.$ds_27NEW332UNQ432,[$__9]);
          var $__12=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$e,1]);
          var $__13=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__12,$ei]);
          var $__14=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$__13]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[101,$__14]);
          var $__16=
           new _A_($UHC.$Base.$_2b_2b,[$ds_27,$__15]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[46,$__16]);
          return new _A_($UHC.$Base.$_3a,[$d,$__17]);});
$UHC.$Show.$eiNEW316UNQ426=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Show.$is_27NEW319UNQ427=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Show.$__156__541NEW322=
 new _F_(function($ei,$is_27)
         {var $__=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$ei,0]);
          var $__4=
           _e_($__);
          var $__swJSW69__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW69__0=
              $is_27;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$init,[$is_27]);
             $__swJSW69__0=
              $__5;
             break;}
          return $__swJSW69__0;});
$UHC.$Show.$dNEW329UNQ431=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW70__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW70__0=
              $__2._1;
             break;
            case 1:
             $__swJSW70__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW70__0;});
$UHC.$Show.$ds_27NEW332UNQ432=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW71__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW71__0=
              $__2._2;
             break;
            case 1:
             $__swJSW71__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW71__0;});
$UHC.$Show.$__154__1648__0NEW353UNQ462CCN=
 new _F_(function($show__e_27,$__,$__3)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToString,["e"]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$__4,$show__e_27]);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$__3,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[46,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__,$__7]);
          var $__9=
           _e_($__3);
          var $__swJSW72__0;
          switch($__9._tag_)
           {case 0:
             $__swJSW72__0=
              $__8;
             break;
            case 1:
             var $__12=
              new _A_($UHC.$Base.$packedStringToString,[".0e"]);
             var $__13=
              new _A_($UHC.$Base.$_2b_2b,[$__12,$show__e_27]);
             var $__14=
              new _A_($UHC.$Base.$_3a,[$__,$__13]);
             $__swJSW72__0=
              $__14;
             break;}
          return $__swJSW72__0;});
$UHC.$Show.$mk0UNQ474=
 new _F_(function($ls)
         {var $__=
           _e_($ls);
          var $__swJSW73__0;
          switch($__._tag_)
           {case 0:
             $__swJSW73__0=
              $ls;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$packedStringToString,["0"]);
             $__swJSW73__0=
              $__5;
             break;}
          return $__swJSW73__0;});
$UHC.$Show.$eiNEW382UNQ492=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Show.$is_27NEW385UNQ493=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Show.$__156__662NEW388=
 new _F_(function($ei,$is_27)
         {var $__=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$ei,0]);
          var $__4=
           _e_($__);
          var $__swJSW76__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$_3a,[0,$is_27]);
             $__swJSW76__0=
              $__5;
             break;
            case 1:
             $__swJSW76__0=
              $is_27;
             break;}
          return $__swJSW76__0;});
$UHC.$Show.$dNEW395UNQ498=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW77__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW77__0=
              $__2._1;
             break;
            case 1:
             $__swJSW77__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW77__0;});
$UHC.$Show.$ds_27NEW398UNQ499=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW78__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW78__0=
              $__2._2;
             break;
            case 1:
             $__swJSW78__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW78__0;});
$UHC.$Show.$__156__678NEW401=
 new _F_(function($ds_27)
         {var $__=
           new _A_($UHC.$Base.$null,[$ds_27]);
          var $__3=
           _e_($__);
          var $__swJSW79__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3a,[46,$ds_27]);
             $__swJSW79__0=
              $__4;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             $__swJSW79__0=
              $__5;
             break;}
          return $__swJSW79__0;});
$UHC.$Show.$eiNEW409UNQ521=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Show.$is_27NEW412UNQ522=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Show.$lsNEW418UNQ525=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Show.$rsNEW421UNQ526=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Show.$__156__715NEW424=
 new _F_(function($rs)
         {var $__=
           new _A_($UHC.$Base.$null,[$rs]);
          var $__3=
           _e_($__);
          var $__swJSW84__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3a,[46,$rs]);
             $__swJSW84__0=
              $__4;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             $__swJSW84__0=
              $__5;
             break;}
          return $__swJSW84__0;});
$UHC.$Show.$fUNQ551=
 new _F_(function($__,$__2,$x1,$x2,$x3)
         {var $__6=
           new _A_($UHC.$Show.$__154__2380__0NEW435UNQ587CCN,[$__,$__2,$x1,$x2,$x3]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__7]);
          var $x19=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__,$__8,$x1]));
          var $__swJSW85__0;
          switch($x19._tag_)
           {case 0:
             $__swJSW85__0=
              $__6;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Show.$mk0UNQ474,[$x3]);
             var $__11=
              new _A_($UHC.$Base.$_3a,[46,$__10]);
             var $__12=
              new _A_($UHC.$Base.$reverse,[$x2]);
             var $__13=
              new _A_($UHC.$Show.$mk0UNQ474,[$__12]);
             var $__14=
              new _A_($UHC.$Base.$_2b_2b,[$__13,$__11]);
             $__swJSW85__0=
              $__14;
             break;}
          return $__swJSW85__0;});
$UHC.$Show.$__154__2380__0NEW435UNQ587CCN=
 new _F_(function($__,$__2,$x1,$x2,$x3)
         {var $x36=
           _e_($x3);
          var $__swJSW86__0;
          switch($x36._tag_)
           {case 0:
             var $__9=
              new _A_($UHC.$Base.$_3a,[$x36._1,$x2]);
             var $__10=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__11=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__10]);
             var $__12=
              new _A_($UHC.$Base.$_2d,[$__2,$x1,$__11]);
             var $__13=
              new _A_($UHC.$Show.$fUNQ551,[$__,$__2,$__12,$__9,$x36._2]);
             $__swJSW86__0=
              $__13;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             var $__15=
              new _A_($UHC.$Base.$_3a,[48,$x2]);
             var $__16=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__17=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__16]);
             var $__18=
              new _A_($UHC.$Base.$_2d,[$__2,$x1,$__17]);
             var $__19=
              new _A_($UHC.$Show.$fUNQ551,[$__,$__2,$__18,$__15,$__14]);
             $__swJSW86__0=
              $__19;
             break;}
          return $__swJSW86__0;});
$UHC.$Show.$__156__803NEW468=
 new _F_(function($e)
         {var $__=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__91__0,$e,7]);
          var $__3=
           new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__91__0,$e,0]);
          var $__4=
           new _A_($UHC.$Base.$_7c_7c,[$__3,$__]);
          var $__5=
           _e_($__4);
          var $__swJSW87__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW87__0=
              $UHC.$Show.$FFFixed__;
             break;
            case 1:
             $__swJSW87__0=
              $UHC.$Show.$FFExponent__;
             break;}
          return $__swJSW87__0;});
$UHC.$Show.$formatRealFloat=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Show.$__154__1303__2__5NEW285UNQ287,[$__]);
          var $__3=
           new _A_($UHC.$Show.$__154__1303__2__4NEW288UNQ286,[$__2]);
          var $__4=
           new _A_($UHC.$Show.$__154__3279__2__0NEW291UNQ279,[$__3]);
          var $__5=
           new _A_($UHC.$Show.$__154__1303__2__2NEW294UNQ285,[$__]);
          var $__6=
           new _A_($UHC.$Show.$__154__1303__2__1NEW297UNQ284,[$__5]);
          var $__7=
           new _A_($UHC.$Show.$__154__3269__2__0NEW300UNQ278,[$__6]);
          return new _A_($UHC.$Show.$__156__500__0,[$__,$__7,$__4]);});
$UHC.$Show.$showFloat=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Show.$formatRealFloat,[$__,$UHC.$Show.$FFGeneric__,$UHC.$Base.$Nothing__,$x]);
          return new _A_($UHC.$Base.$showString,[$__3]);});
$UHC.$Show.$_24D__FFFormatDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["UHC.Show"]);});
$UHC.$Show.$_24D__FFFormatDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["FFFormat"]);});
$UHC.$Show.$_24D__FFFormatNEW508UNQ245SDCGENDatatype=
 new _F_(function($_24D__FFFormat)
         {var $_24D__FFFormat2=
           new _A_($UHC.$Show.$_24D__FFFormatNEW510UNQ246EVLSDCGENDatatype,[$_24D__FFFormat]);
          return $_24D__FFFormat2;});
$UHC.$Show.$_24D__FFFormatNEW510UNQ246EVLSDCGENDatatype=
 new _F_(function($_24D__FFFormat)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__FFFormat]));
          var $__5=
           {_tag_:0,_1:$UHC.$Show.$_24D__FFFormatDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$UHC.$Show.$_24D__FFFormatDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$UHC.$Show.$_24D__FFFormatUNQ245SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$_24D__FFFormatNEW508UNQ245SDCGENDatatype,[$UHC.$Show.$_24D__FFFormatUNQ245SDCGENDatatype]);}),[]);
$UHC.$Show.$_24D__FFFormatGENDatatype=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$_24D__FFFormatUNQ245SDCGENDatatype;}),[]);
$UHC.$Show.$_24C__FFGenericDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["FFGeneric"]);});
$UHC.$Show.$_24C__FFGenericNEW516UNQ268SDCGENConstructor=
 new _F_(function($_24C__FFGeneric)
         {var $_24C__FFGeneric2=
           new _A_($UHC.$Show.$_24C__FFGenericNEW518UNQ269EVLSDCGENConstructor,[$_24C__FFGeneric]);
          return $_24C__FFGeneric2;});
$UHC.$Show.$_24C__FFGenericNEW518UNQ269EVLSDCGENConstructor=
 new _F_(function($_24C__FFGeneric)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__FFGeneric]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$UHC.$Show.$_24C__FFGenericDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$UHC.$Show.$_24C__FFGenericUNQ268SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$_24C__FFGenericNEW516UNQ268SDCGENConstructor,[$UHC.$Show.$_24C__FFGenericUNQ268SDCGENConstructor]);}),[]);
$UHC.$Show.$_24C__FFGenericGENConstructor=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$_24C__FFGenericUNQ268SDCGENConstructor;}),[]);
$UHC.$Show.$_24C__FFFixedDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["FFFixed"]);});
$UHC.$Show.$_24C__FFFixedNEW524UNQ260SDCGENConstructor=
 new _F_(function($_24C__FFFixed)
         {var $_24C__FFFixed2=
           new _A_($UHC.$Show.$_24C__FFFixedNEW526UNQ261EVLSDCGENConstructor,[$_24C__FFFixed]);
          return $_24C__FFFixed2;});
$UHC.$Show.$_24C__FFFixedNEW526UNQ261EVLSDCGENConstructor=
 new _F_(function($_24C__FFFixed)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__FFFixed]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$UHC.$Show.$_24C__FFFixedDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$UHC.$Show.$_24C__FFFixedUNQ260SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$_24C__FFFixedNEW524UNQ260SDCGENConstructor,[$UHC.$Show.$_24C__FFFixedUNQ260SDCGENConstructor]);}),[]);
$UHC.$Show.$_24C__FFFixedGENConstructor=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$_24C__FFFixedUNQ260SDCGENConstructor;}),[]);
$UHC.$Show.$_24C__FFExponentDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["FFExponent"]);});
$UHC.$Show.$_24C__FFExponentNEW532UNQ252SDCGENConstructor=
 new _F_(function($_24C__FFExponent)
         {var $_24C__FFExponent2=
           new _A_($UHC.$Show.$_24C__FFExponentNEW534UNQ253EVLSDCGENConstructor,[$_24C__FFExponent]);
          return $_24C__FFExponent2;});
$UHC.$Show.$_24C__FFExponentNEW534UNQ253EVLSDCGENConstructor=
 new _F_(function($_24C__FFExponent)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__FFExponent]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$UHC.$Show.$_24C__FFExponentDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$UHC.$Show.$_24C__FFExponentUNQ252SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Show.$_24C__FFExponentNEW532UNQ252SDCGENConstructor,[$UHC.$Show.$_24C__FFExponentUNQ252SDCGENConstructor]);}),[]);
$UHC.$Show.$_24C__FFExponentGENConstructor=
 new _A_(new _F_(function()
                 {return $UHC.$Show.$_24C__FFExponentUNQ252SDCGENConstructor;}),[]);
