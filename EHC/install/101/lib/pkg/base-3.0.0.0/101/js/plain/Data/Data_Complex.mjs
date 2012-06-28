// Data.Complex
var $Data=
 ($Data ? $Data : {});
$Data.$Complex=
 ($Data.$Complex ? $Data.$Complex : {});
$Data.$Complex.$realPart=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return $__3._1;});
$Data.$Complex.$__56__675__2__2NEW3UNQ300=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__675__2__1NEW6UNQ299=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__744__2__0NEW9UNQ312=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__56__749__1__0NEW12UNQ313=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$Complex.$__58__22__0=
 new _F_(function($__,$__2,$__3,$x1)
         {var $__5=
           _e_($x1);
          var $__8=
           new _A_($UHC.$Base.$atan2,[$__,$__5._2,$__5._1]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__9]);
          var $__11=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__3,$__10,$__5._1]));
          var $__swJSW6__0;
          switch($__11._tag_)
           {case 0:
             $__swJSW6__0=
              $__8;
             break;
            case 1:
             var $__12=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__13=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__12]);
             var $__14=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__3,$__13,$__5._2]));
             var $__swJSW7__0;
             switch($__14._tag_)
              {case 0:
                $__swJSW7__0=
                 $__8;
                break;
               case 1:
                var $__15=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__16=
                 new _A_($UHC.$Base.$fromInteger,[$__2,$__15]);
                $__swJSW7__0=
                 $__16;
                break;}
             $__swJSW6__0=
              $__swJSW7__0;
             break;}
          return $__swJSW6__0;});
$Data.$Complex.$phase=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__675__2__2NEW3UNQ300,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__675__2__1NEW6UNQ299,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__744__2__0NEW9UNQ312,[$__3]);
          var $__5=
           new _A_($Data.$Complex.$__56__749__1__0NEW12UNQ313,[$__4]);
          return new _A_($Data.$Complex.$__58__22__0,[$__,$__4,$__5]);});
$Data.$Complex.$__56__485__2__0NEW27UNQ178=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__299__2__2NEW30UNQ172=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__496__2__0NEW33UNQ181=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__58__61__0=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$exponent,[$__,$__5._2]);
          var $__9=
           new _A_($UHC.$Base.$exponent,[$__,$__5._1]);
          var $k=
           new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,$__9,$__8]);
          var $mk=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$k]);
          var $__12=
           new _A_($UHC.$Base.$scaleFloat,[$__,$mk,$__5._2]);
          var $__13=
           new _A_($Data.$Complex.$sqrUNQ236,[$__3,$__12]);
          var $__14=
           new _A_($UHC.$Base.$scaleFloat,[$__,$mk,$__5._1]);
          var $__15=
           new _A_($Data.$Complex.$sqrUNQ236,[$__3,$__14]);
          var $__16=
           new _A_($UHC.$Base.$_2b,[$__3,$__15,$__13]);
          var $__17=
           new _A_($UHC.$Base.$sqrt,[$__2,$__16]);
          return new _A_($UHC.$Base.$scaleFloat,[$__,$k,$__17]);});
$Data.$Complex.$sqrUNQ236=
 new _F_(function($__,$z)
         {return new _A_($UHC.$Base.$_2a,[$__,$z,$z]);});
$Data.$Complex.$magnitude=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__485__2__0NEW27UNQ178,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__299__2__2NEW30UNQ172,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__496__2__0NEW33UNQ181,[$__3]);
          return new _A_($Data.$Complex.$__58__61__0,[$__,$__2,$__4]);});
$Data.$Complex.$polar=
 new _F_(function($__,$z)
         {var $__3=
           new _A_($Data.$Complex.$phase,[$__,$z]);
          var $__4=
           new _A_($Data.$Complex.$magnitude,[$__,$z]);
          return [$__4,$__3];});
$Data.$Complex.$imagPart=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return $__3._2;});
$Data.$Complex.$__58__122=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Complex"]);}),[]);
$Data.$Complex.$complexTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Data.$Complex.$__58__122]);}),[]);
$Data.$Complex.$__54__2__0DFLUHC_2eBase_2ereadsPrec=
 new _F_(function($__,$d)
         {var $__3=
           new _A_($UHC.$Base.$primGtInt,[$d,6]);
          var $__4=
           new _A_($Data.$Complex.$__58__133__0,[$__]);
          return new _A_($UHC.$Base.$readParen,[$__3,$__4]);});
$Data.$Complex.$__58__133__0=
 new _F_(function($__,$r)
         {var $__3=
           new _A_($UHC.$Base.$readsPrec,[$__,11,$r]);
          var $__4=
           new _A_($Data.$Complex.$__58__137__0,[$__]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__3]);});
$Data.$Complex.$__58__137__0=
 new _F_(function($__,$_24uv__1)
         {var $_24x=
           _e_($_24uv__1);
          var $__6=
           new _A_($UHC.$Base.$lex,[$_24x[1]]);
          var $__7=
           new _A_($Data.$Complex.$__58__143__0,[$__,$_24x[0]]);
          var $__8=
           new _A_($UHC.$Base.$concatMap,[$__7,$__6]);
          return $__8;});
$Data.$Complex.$__58__143__0=
 new _F_(function($__,$_24u__1,$_24uv__2)
         {var $_24x=
           _e_($_24uv__2);
          var $_24l__1=
           _e_($_24x[0]);
          var $__swJSW15__0;
          switch($_24l__1._tag_)
           {case 0:
             var $_24l__110=
              _e_(new _A_($UHC.$Base.$primEqChar,[$_24l__1._1,58]));
             var $__swJSW16__0;
             switch($_24l__110._tag_)
              {case 0:
                $__swJSW16__0=
                 {_tag_:1};
                break;
               case 1:
                var $_24l__211=
                 _e_($_24l__1._2);
                var $__swJSW17__0;
                switch($_24l__211._tag_)
                 {case 0:
                   var $_24l__214=
                    _e_(new _A_($UHC.$Base.$primEqChar,[$_24l__211._1,43]));
                   var $__swJSW18__0;
                   switch($_24l__214._tag_)
                    {case 0:
                      $__swJSW18__0=
                       {_tag_:1};
                      break;
                     case 1:
                      var $_24l__315=
                       _e_($_24l__211._2);
                      var $__swJSW19__0;
                      switch($_24l__315._tag_)
                       {case 0:
                         $__swJSW19__0=
                          {_tag_:1};
                         break;
                        case 1:
                         var $__18=
                          new _A_($UHC.$Base.$readsPrec,[$__,11,$_24x[1]]);
                         var $__19=
                          new _A_($Data.$Complex.$__58__163__0,[$_24u__1]);
                         var $__20=
                          new _A_($UHC.$Base.$concatMap,[$__19,$__18]);
                         $__swJSW19__0=
                          $__20;
                         break;}
                      $__swJSW18__0=
                       $__swJSW19__0;
                      break;}
                   $__swJSW17__0=
                    $__swJSW18__0;
                   break;
                  case 1:
                   $__swJSW17__0=
                    {_tag_:1};
                   break;}
                $__swJSW16__0=
                 $__swJSW17__0;
                break;}
             $__swJSW15__0=
              $__swJSW16__0;
             break;
            case 1:
             $__swJSW15__0=
              {_tag_:1};
             break;}
          return $__swJSW15__0;});
$Data.$Complex.$__58__163__0=
 new _F_(function($_24u__1,$_24uv__3)
         {var $_24x=
           _e_($_24uv__3);
          var $__=
           {_tag_:0,_1:$_24u__1,_2:$_24x[0]};
          var $__7=
           [$__,$_24x[1]];
          var $__8=
           {_tag_:0,_1:$__7,_2:{_tag_:1}};
          return $__8;});
$Data.$Complex.$__54__2__0NEW79UNQ1250RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Complex.$__54__2__0NEW82UNQ1253EVLRDC,[$__,$__2]);
          return $__3;});
$Data.$Complex.$__54__2__0NEW82UNQ1253EVLRDC=
 new _F_(function($__,$__2)
         {var $Read__=
           _e_(new _A_($UHC.$Base.$Read__CLS74__41__0,[$__]));
          var $__6=
           new _A_($Data.$Complex.$__54__2__0DFLUHC_2eBase_2ereadsPrec,[$__2]);
          var $__7=
           {_tag_:0,_1:$Read__._1,_2:$__6};
          return $__7;});
$Data.$Complex.$__54__2__0=
 new _F_(function($__)
         {var $__2=
           _i_();
          _i_set_($__2,new _A_($Data.$Complex.$__54__2__0NEW79UNQ1250RDC,[$__2,$__]));
          return $__2;});
$Data.$Complex.$__54__1__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$d,$x__1)
         {var $x__14=
           _e_($x__1);
          var $__7=
           new _A_($UHC.$Base.$showsPrec,[$__,7,$x__14._2]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,[" :+ "]);
          var $__9=
           new _A_($UHC.$Base.$showString,[$__8]);
          var $__10=
           new _A_($UHC.$Base.$_2e,[$__9,$__7]);
          var $__11=
           new _A_($UHC.$Base.$showsPrec,[$__,7,$x__14._1]);
          var $__12=
           new _A_($UHC.$Base.$_2e,[$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$primGtInt,[$d,6]);
          var $__14=
           new _A_($UHC.$Base.$showParen,[$__13,$__12]);
          return $__14;});
$Data.$Complex.$__54__1__0NEW98UNQ1236RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Complex.$__54__1__0NEW101UNQ1239EVLRDC,[$__,$__2]);
          return $__3;});
$Data.$Complex.$__54__1__0NEW101UNQ1239EVLRDC=
 new _F_(function($__,$__2)
         {var $Show__=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$__]));
          var $__7=
           new _A_($Data.$Complex.$__54__1__0DFLUHC_2eBase_2eshowsPrec,[$__2]);
          var $__8=
           {_tag_:0,_1:$Show__._1,_2:$Show__._2,_3:$__7};
          return $__8;});
$Data.$Complex.$__54__1__0=
 new _F_(function($__)
         {var $__2=
           _i_();
          _i_set_($__2,new _A_($Data.$Complex.$__54__1__0NEW98UNQ1236RDC,[$__2,$__]));
          return $__2;});
$Data.$Complex.$Typeable1__DCT54__6__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Data.$Complex.$complexTc,$UHC.$Base.$_5b_5d]);});
$Data.$Complex.$Typeable1__NEW108UNQ1216DCT54__6__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Data.$Complex.$Typeable1__NEW110UNQ1217EVLDCT54__6__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Data.$Complex.$Typeable1__NEW110UNQ1217EVLDCT54__6__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Data.$Complex.$Typeable1__DCT54__6__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Data.$Complex.$Typeable1__UNQ1216DCT54__6__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Complex.$Typeable1__NEW108UNQ1216DCT54__6__0RDC,[$Data.$Complex.$Typeable1__UNQ1216DCT54__6__0RDC]);}),[]);
$Data.$Complex.$Typeable1__DCT54__6__0=
 new _A_(new _F_(function()
                 {return $Data.$Complex.$Typeable1__UNQ1216DCT54__6__0RDC;}),[]);
$Data.$Complex.$Typeable__NEW116UNQ1210DCT54__7__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Data.$Complex.$Typeable__NEW119UNQ1213EVLDCT54__7__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Data.$Complex.$Typeable__NEW119UNQ1213EVLDCT54__7__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__5=
           {_tag_:0,_1:$Typeable__2};
          return $__5;});
$Data.$Complex.$Typeable__DCT54__7__0=
 new _F_(function($__)
         {var $Typeable__DCT54__7__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Data.$Complex.$Typeable1__DCT54__6__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Data.$Complex.$Typeable__NEW116UNQ1210DCT54__7__0RDC,[$Typeable__,$Typeable__DCT54__7__0DFLData_2eTypeable_2etypeOf]));
          return $Typeable__;});
$Data.$Complex.$_3a_2b=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$Data.$Complex.$__Rep0ComplexDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Data.$Complex.$_3a_2b,[$proj__3._1,$proj__3._2]);
          return $__;});
$Data.$Complex.$__Rep0ComplexDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$K1__,[$x2._2]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2._1]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Data.$Complex.$__Rep0ComplexNEW138UNQ25SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__Rep0ComplexNEW140UNQ26EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Data.$Complex.$__Rep0ComplexNEW140UNQ26EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Data.$Complex.$__Rep0ComplexDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Data.$Complex.$__Rep0ComplexDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Data.$Complex.$__Rep0ComplexUNQ25SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Complex.$__Rep0ComplexNEW138UNQ25SDCGENRepresentable0,[$Data.$Complex.$__Rep0ComplexUNQ25SDCGENRepresentable0]);}),[]);
$Data.$Complex.$__Rep0ComplexGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Data.$Complex.$__Rep0ComplexUNQ25SDCGENRepresentable0;}),[]);
$Data.$Complex.$__54__0__0NEW151UNQ1222RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Complex.$__54__0__0NEW154UNQ1231EVLRDC,[$__,$__2]);
          return $__3;});
$Data.$Complex.$__54__0__0NEW154UNQ1231EVLRDC=
 new _F_(function($__,$__2)
         {var $Eq__=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$__]));
          var $__6=
           {_tag_:0,_1:$Eq__._1,_2:$__2};
          return $__6;});
$Data.$Complex.$__54__0__0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$__]);
          var $__3=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$Eq_27__DCT74__393__0,[$__3,$__3]);
          var $__5=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__5]);
          var $__54__0__0DFLUHC_2eBase_2e_3d_3d=
           new _A_($UHC.$Base.$geqdefault,[$Data.$Complex.$__Rep0ComplexGENRepresentable0,$__6,$UHC.$Base.$undefined]);
          var $__7=
           _i_();
          _i_set_($__7,new _A_($Data.$Complex.$__54__0__0NEW151UNQ1222RDC,[$__7,$__54__0__0DFLUHC_2eBase_2e_3d_3d]));
          return $__7;});
$Data.$Complex.$__56__3620__2__1NEW160UNQ991=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__4202__2__0NEW163UNQ970=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__3721__2__0NEW166UNQ1015=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__56__4104__1__0NEW169UNQ1029=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$Complex.$__56__3618__2__4NEW172UNQ969=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__3618__2__3NEW175UNQ968=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__3618__2__2NEW178UNQ967=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__56__3618__2__1NEW181UNQ966=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2enegate=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$negate,[$__,$__3._2]);
          var $__7=
           new _A_($UHC.$Base.$negate,[$__,$__3._1]);
          var $__8=
           new _A_($Data.$Complex.$_3a_2b,[$__7,$__6]);
          return $__8;});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2efromInteger=
 new _F_(function($__,$n)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$n]);
          return new _A_($Data.$Complex.$_3a_2b,[$__5,$__4]);});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2eabs=
 new _F_(function($__,$__2,$z)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__4]);
          var $__6=
           new _A_($Data.$Complex.$magnitude,[$__,$z]);
          return new _A_($Data.$Complex.$_3a_2b,[$__6,$__5]);});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2d=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Complex.$__58__368__0,[$__,$__3._1,$__3._2]);});
$Data.$Complex.$__58__368__0=
 new _F_(function($__,$x,$y,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$_2d,[$__,$y,$__5._2]);
          var $__9=
           new _A_($UHC.$Base.$_2d,[$__,$x,$__5._1]);
          var $__10=
           new _A_($Data.$Complex.$_3a_2b,[$__9,$__8]);
          return $__10;});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2b=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Complex.$__58__385__0,[$__,$__3._1,$__3._2]);});
$Data.$Complex.$__58__385__0=
 new _F_(function($__,$x,$y,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$_2b,[$__,$y,$__5._2]);
          var $__9=
           new _A_($UHC.$Base.$_2b,[$__,$x,$__5._1]);
          var $__10=
           new _A_($Data.$Complex.$_3a_2b,[$__9,$__8]);
          return $__10;});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2a=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Complex.$__58__402__0,[$__,$__3._1,$__3._2]);});
$Data.$Complex.$__58__402__0=
 new _F_(function($__,$x,$y,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$_2a,[$__,$y,$__5._1]);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__5._2]);
          var $__10=
           new _A_($UHC.$Base.$_2b,[$__,$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_2a,[$__,$y,$__5._2]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__5._1]);
          var $__13=
           new _A_($UHC.$Base.$_2d,[$__,$__12,$__11]);
          var $__14=
           new _A_($Data.$Complex.$_3a_2b,[$__13,$__10]);
          return $__14;});
$Data.$Complex.$Num__NEW223UNQ965DCT54__9__0RDC=
 new _F_(function($__,$Num__,$__3,$__4,$__5,$__6)
         {var $Num__7=
           new _A_($Data.$Complex.$Num__NEW230UNQ1036EVLDCT54__9__0RDC,[$__,$Num__,$__3,$__4,$__5,$__6]);
          return $Num__7;});
$Data.$Complex.$Num__NEW230UNQ1036EVLDCT54__9__0RDC=
 new _F_(function($__,$Num__,$__3,$__4,$__5,$__6)
         {var $Num__7=
           _e_(new _A_($UHC.$Base.$Num__CLS74__11__0,[$Num__]));
          var $__17=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2esignum,[$Num__,$__3,$__4,$__5,$__6]);
          var $__18=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2enegate,[$__4]);
          var $__19=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2efromInteger,[$__4]);
          var $__20=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2eabs,[$__3,$__4]);
          var $__21=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2d,[$__4]);
          var $__22=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2b,[$__4]);
          var $__23=
           new _A_($Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2e_2a,[$__4]);
          var $__24=
           {_tag_:0,_1:$__23,_2:$__22,_3:$__21,_4:$__,_5:$__20,_6:$Num__7._6,_7:$__19,_8:$__18,_9:$__17};
          return $__24;});
$Data.$Complex.$Num__DCT54__9__0DFLUHC_2eBase_2esignum=
 new _F_(function($Num__,$__,$__3,$__4,$__5,$x1)
         {var $__7=
           _e_($x1);
          var $__10=
           new _A_($Data.$Complex.$__56__4090__0NEW241UNQ1097CCN,[$__,$__5,$__7,$__7._1,$__7._2]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__12=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__11]);
          var $__13=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__4,$__12,$__7._1]));
          var $__swJSW47__0;
          switch($__13._tag_)
           {case 0:
             $__swJSW47__0=
              $__10;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__14]);
             var $__16=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__4,$__15,$__7._2]));
             var $__swJSW48__0;
             switch($__16._tag_)
              {case 0:
                $__swJSW48__0=
                 $__10;
                break;
               case 1:
                var $__17=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__18=
                 new _A_($UHC.$Base.$fromInteger,[$Num__,$__17]);
                $__swJSW48__0=
                 $__18;
                break;}
             $__swJSW47__0=
              $__swJSW48__0;
             break;}
          return $__swJSW47__0;});
$Data.$Complex.$__56__4090__0NEW241UNQ1097CCN=
 new _F_(function($__,$__2,$__3,$__4,$__5)
         {var $r=
           new _A_($Data.$Complex.$magnitude,[$__,$__3]);
          var $__7=
           new _A_($UHC.$Base.$_2f,[$__2,$__5,$r]);
          var $__8=
           new _A_($UHC.$Base.$_2f,[$__2,$__4,$r]);
          return new _A_($Data.$Complex.$_3a_2b,[$__8,$__7]);});
$Data.$Complex.$Num__DCT54__9__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__3620__2__1NEW160UNQ991,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__4202__2__0NEW163UNQ970,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__3721__2__0NEW166UNQ1015,[$__3]);
          var $__5=
           new _A_($Data.$Complex.$__56__4104__1__0NEW169UNQ1029,[$__4]);
          var $__6=
           new _A_($Data.$Complex.$__56__3618__2__4NEW172UNQ969,[$__]);
          var $__7=
           new _A_($Data.$Complex.$__56__3618__2__3NEW175UNQ968,[$__6]);
          var $__8=
           new _A_($Data.$Complex.$__56__3618__2__2NEW178UNQ967,[$__7]);
          var $__9=
           new _A_($Data.$Complex.$__56__3618__2__1NEW181UNQ966,[$__8]);
          var $__56__3619__3=
           new _A_($Data.$Complex.$__54__0__0,[$__9]);
          var $Num__=
           _i_();
          _i_set_($Num__,new _A_($Data.$Complex.$Num__NEW223UNQ965DCT54__9__0RDC,[$__56__3619__3,$Num__,$__,$__4,$__5,$__3]));
          return $Num__;});
$Data.$Complex.$__56__3215__2__2NEW259UNQ863=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__3215__2__1NEW262UNQ862=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__3486__2__0NEW265UNQ882=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$Fractional__DCT54__11__0DFLUHC_2eBase_2efromRational=
 new _F_(function($__,$__2,$a)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__4]);
          var $__6=
           new _A_($UHC.$Base.$fromRational,[$__,$a]);
          return new _A_($Data.$Complex.$_3a_2b,[$__6,$__5]);});
$Data.$Complex.$Fractional__DCT54__11__0DFLUHC_2eBase_2e_2f=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__4);
          return new _A_($Data.$Complex.$__58__504__0,[$__,$__2,$__3,$__5._1,$__5._2]);});
$Data.$Complex.$__58__504__0=
 new _F_(function($__,$__2,$__3,$x,$y,$__6)
         {var $__7=
           _e_($__6);
          var $__10=
           new _A_($UHC.$Base.$exponent,[$__,$__7._2]);
          var $__11=
           new _A_($UHC.$Base.$exponent,[$__,$__7._1]);
          var $__12=
           new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,$__11,$__10]);
          var $k=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$__12]);
          var $x_27_27=
           new _A_($UHC.$Base.$scaleFloat,[$__,$k,$__7._1]);
          var $y_27_27=
           new _A_($UHC.$Base.$scaleFloat,[$__,$k,$__7._2]);
          var $__16=
           new _A_($UHC.$Base.$_2a,[$__3,$__7._2,$y_27_27]);
          var $__17=
           new _A_($UHC.$Base.$_2a,[$__3,$__7._1,$x_27_27]);
          var $d=
           new _A_($UHC.$Base.$_2b,[$__3,$__17,$__16]);
          var $__19=
           new _A_($UHC.$Base.$_2a,[$__3,$x,$y_27_27]);
          var $__20=
           new _A_($UHC.$Base.$_2a,[$__3,$y,$x_27_27]);
          var $__21=
           new _A_($UHC.$Base.$_2d,[$__3,$__20,$__19]);
          var $__22=
           new _A_($UHC.$Base.$_2f,[$__2,$__21,$d]);
          var $__23=
           new _A_($UHC.$Base.$_2a,[$__3,$y,$y_27_27]);
          var $__24=
           new _A_($UHC.$Base.$_2a,[$__3,$x,$x_27_27]);
          var $__25=
           new _A_($UHC.$Base.$_2b,[$__3,$__24,$__23]);
          var $__26=
           new _A_($UHC.$Base.$_2f,[$__2,$__25,$d]);
          return new _A_($Data.$Complex.$_3a_2b,[$__26,$__22]);});
$Data.$Complex.$Fractional__NEW294UNQ854DCT54__11__0RDC=
 new _F_(function($__,$Fractional__,$__3,$__4,$__5)
         {var $Fractional__6=
           new _A_($Data.$Complex.$Fractional__NEW300UNQ895EVLDCT54__11__0RDC,[$__,$Fractional__,$__3,$__4,$__5]);
          return $Fractional__6;});
$Data.$Complex.$Fractional__NEW300UNQ895EVLDCT54__11__0RDC=
 new _F_(function($__,$Fractional__,$__3,$__4,$__5)
         {var $Fractional__6=
           _e_(new _A_($UHC.$Base.$Fractional__CLS74__21__0,[$Fractional__]));
          var $__12=
           new _A_($Data.$Complex.$Fractional__DCT54__11__0DFLUHC_2eBase_2efromRational,[$__4,$__5]);
          var $__13=
           new _A_($Data.$Complex.$Fractional__DCT54__11__0DFLUHC_2eBase_2e_2f,[$__3,$__4,$__5]);
          var $__14=
           {_tag_:0,_1:$__13,_2:$__,_3:$Fractional__6._3,_4:$__12,_5:$Fractional__6._5};
          return $__14;});
$Data.$Complex.$Fractional__DCT54__11__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__3215__2__2NEW259UNQ863,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__3215__2__1NEW262UNQ862,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__3486__2__0NEW265UNQ882,[$__3]);
          var $__56__3214__3=
           new _A_($Data.$Complex.$Num__DCT54__9__0,[$__]);
          var $Fractional__=
           _i_();
          _i_set_($Fractional__,new _A_($Data.$Complex.$Fractional__NEW294UNQ854DCT54__11__0RDC,[$__56__3214__3,$Fractional__,$__,$__3,$__4]));
          return $Fractional__;});
$Data.$Complex.$__56__1050__1__0NEW311UNQ487=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__1001__2__59NEW314UNQ426=
 new _F_(function($__)
         {var $RealFrac__=
           _e_($__);
          return $RealFrac__._2;});
$Data.$Complex.$__56__1001__2__58NEW317UNQ425=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$Complex.$__56__2664__2__0NEW320UNQ519=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$Complex.$__56__1001__2__10NEW323UNQ404=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__1058__2__0NEW326UNQ488=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__56__2446__1__0NEW329UNQ591=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2etanh=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__4);
          var $siny=
           new _A_($UHC.$Base.$sin,[$__,$__5._2]);
          var $sinhx=
           new _A_($UHC.$Base.$sinh,[$__,$__5._1]);
          var $cosy=
           new _A_($UHC.$Base.$cos,[$__,$__5._2]);
          var $coshx=
           new _A_($UHC.$Base.$cosh,[$__,$__5._1]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__2,$siny,$sinhx]);
          var $__13=
           new _A_($UHC.$Base.$_2a,[$__2,$cosy,$coshx]);
          var $__14=
           new _A_($Data.$Complex.$_3a_2b,[$__13,$__12]);
          var $__15=
           new _A_($UHC.$Base.$_2a,[$__2,$siny,$coshx]);
          var $__16=
           new _A_($UHC.$Base.$_2a,[$__2,$cosy,$sinhx]);
          var $__17=
           new _A_($Data.$Complex.$_3a_2b,[$__16,$__15]);
          return new _A_($UHC.$Base.$_2f,[$__3,$__17,$__14]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2etan=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__4);
          var $sinx=
           new _A_($UHC.$Base.$sin,[$__,$__5._1]);
          var $sinhy=
           new _A_($UHC.$Base.$sinh,[$__,$__5._2]);
          var $cosx=
           new _A_($UHC.$Base.$cos,[$__,$__5._1]);
          var $coshy=
           new _A_($UHC.$Base.$cosh,[$__,$__5._2]);
          var $__12=
           new _A_($UHC.$Base.$negate,[$__2,$sinx]);
          var $__13=
           new _A_($UHC.$Base.$_2a,[$__2,$__12,$sinhy]);
          var $__14=
           new _A_($UHC.$Base.$_2a,[$__2,$cosx,$coshy]);
          var $__15=
           new _A_($Data.$Complex.$_3a_2b,[$__14,$__13]);
          var $__16=
           new _A_($UHC.$Base.$_2a,[$__2,$cosx,$sinhy]);
          var $__17=
           new _A_($UHC.$Base.$_2a,[$__2,$sinx,$coshy]);
          var $__18=
           new _A_($Data.$Complex.$_3a_2b,[$__17,$__16]);
          return new _A_($UHC.$Base.$_2f,[$__3,$__18,$__15]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esqrt=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7,$x1)
         {var $__9=
           _e_($x1);
          var $__12=
           new _A_($Data.$Complex.$__56__2432__0NEW359UNQ728CCN,[$__,$__2,$__3,$__4,$__7,$__9,$__9._1,$__9._2]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__14=
           new _A_($UHC.$Base.$fromInteger,[$__4,$__13]);
          var $__15=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__6,$__14,$__9._1]));
          var $__swJSW65__0;
          switch($__15._tag_)
           {case 0:
             $__swJSW65__0=
              $__12;
             break;
            case 1:
             var $__16=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__17=
              new _A_($UHC.$Base.$fromInteger,[$__4,$__16]);
             var $__18=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__6,$__17,$__9._2]));
             var $__swJSW66__0;
             switch($__18._tag_)
              {case 0:
                $__swJSW66__0=
                 $__12;
                break;
               case 1:
                var $__19=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__20=
                 new _A_($UHC.$Base.$fromInteger,[$__5,$__19]);
                $__swJSW66__0=
                 $__20;
                break;}
             $__swJSW65__0=
              $__swJSW66__0;
             break;}
          return $__swJSW65__0;});
$Data.$Complex.$__56__2432__0NEW359UNQ728CCN=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7,$__8)
         {var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__4,$__9]);
          var $__11=
           new _A_($UHC.$Base.$abs,[$__4,$__7]);
          var $__12=
           new _A_($Data.$Complex.$magnitude,[$__,$__6]);
          var $__13=
           new _A_($UHC.$Base.$_2b,[$__4,$__12,$__11]);
          var $__14=
           new _A_($UHC.$Base.$_2f,[$__2,$__13,$__10]);
          var $u_27=
           new _A_($UHC.$Base.$sqrt,[$__3,$__14]);
          var $__16=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__17=
           new _A_($UHC.$Base.$fromInteger,[$__4,$__16]);
          var $__18=
           new _A_($UHC.$Base.$_2a,[$__4,$u_27,$__17]);
          var $__19=
           new _A_($UHC.$Base.$abs,[$__4,$__8]);
          var $v_27=
           new _A_($UHC.$Base.$_2f,[$__2,$__19,$__18]);
          var $__21=
           new _A_($Data.$Complex.$__56__2630__0NEW380UNQ733,[$__4,$__5,$__7,$u_27,$v_27]);
          var $u=
           new _A_($Data.$Complex.$uNEW392UNQ734,[$__21]);
          var $v=
           new _A_($Data.$Complex.$vNEW395UNQ735,[$__21]);
          var $__24=
           new _A_($Data.$Complex.$__58__732NEW398,[$__4,$__5,$__8,$v]);
          return new _A_($Data.$Complex.$_3a_2b,[$u,$__24]);});
$Data.$Complex.$__56__2630__0NEW380UNQ733=
 new _F_(function($__,$__2,$__3,$u_27,$v_27)
         {var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3c,[$__2,$__3,$__7]);
          var $__9=
           _e_($__8);
          var $__swJSW67__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              [$u_27,$v_27];
             $__swJSW67__0=
              $__10;
             break;
            case 1:
             var $__11=
              [$v_27,$u_27];
             $__swJSW67__0=
              $__11;
             break;}
          return $__swJSW67__0;});
$Data.$Complex.$uNEW392UNQ734=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$Complex.$vNEW395UNQ735=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$Complex.$__58__732NEW398=
 new _F_(function($__,$__2,$__3,$v)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_3c,[$__2,$__3,$__6]);
          var $__8=
           _e_($__7);
          var $__swJSW70__0;
          switch($__8._tag_)
           {case 0:
             $__swJSW70__0=
              $v;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$negate,[$__,$v]);
             $__swJSW70__0=
              $__9;
             break;}
          return $__swJSW70__0;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esinh=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          var $__7=
           new _A_($UHC.$Base.$cosh,[$__,$__4._1]);
          var $__8=
           new _A_($UHC.$Base.$sin,[$__,$__4._2]);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__2,$__8,$__7]);
          var $__10=
           new _A_($UHC.$Base.$sinh,[$__,$__4._1]);
          var $__11=
           new _A_($UHC.$Base.$cos,[$__,$__4._2]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__2,$__11,$__10]);
          var $__13=
           new _A_($Data.$Complex.$_3a_2b,[$__12,$__9]);
          return $__13;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esin=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          var $__7=
           new _A_($UHC.$Base.$sinh,[$__,$__4._2]);
          var $__8=
           new _A_($UHC.$Base.$cos,[$__,$__4._1]);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__2,$__8,$__7]);
          var $__10=
           new _A_($UHC.$Base.$cosh,[$__,$__4._2]);
          var $__11=
           new _A_($UHC.$Base.$sin,[$__,$__4._1]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__2,$__11,$__10]);
          var $__13=
           new _A_($Data.$Complex.$_3a_2b,[$__12,$__9]);
          return $__13;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2elog=
 new _F_(function($__,$__2,$z)
         {var $__4=
           new _A_($Data.$Complex.$phase,[$__,$z]);
          var $__5=
           new _A_($Data.$Complex.$magnitude,[$__,$z]);
          var $__6=
           new _A_($UHC.$Base.$log,[$__2,$__5]);
          return new _A_($Data.$Complex.$_3a_2b,[$__6,$__4]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eexp=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          var $expx=
           new _A_($UHC.$Base.$exp,[$__,$__4._1]);
          var $__8=
           new _A_($UHC.$Base.$sin,[$__,$__4._2]);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__2,$expx,$__8]);
          var $__10=
           new _A_($UHC.$Base.$cos,[$__,$__4._2]);
          var $__11=
           new _A_($UHC.$Base.$_2a,[$__2,$expx,$__10]);
          return new _A_($Data.$Complex.$_3a_2b,[$__11,$__9]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2ecosh=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          var $__7=
           new _A_($UHC.$Base.$sinh,[$__,$__4._1]);
          var $__8=
           new _A_($UHC.$Base.$sin,[$__,$__4._2]);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__2,$__8,$__7]);
          var $__10=
           new _A_($UHC.$Base.$cosh,[$__,$__4._1]);
          var $__11=
           new _A_($UHC.$Base.$cos,[$__,$__4._2]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__2,$__11,$__10]);
          var $__13=
           new _A_($Data.$Complex.$_3a_2b,[$__12,$__9]);
          return $__13;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2ecos=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          var $__7=
           new _A_($UHC.$Base.$sinh,[$__,$__4._2]);
          var $__8=
           new _A_($UHC.$Base.$sin,[$__,$__4._1]);
          var $__9=
           new _A_($UHC.$Base.$negate,[$__2,$__8]);
          var $__10=
           new _A_($UHC.$Base.$_2a,[$__2,$__9,$__7]);
          var $__11=
           new _A_($UHC.$Base.$cosh,[$__,$__4._2]);
          var $__12=
           new _A_($UHC.$Base.$cos,[$__,$__4._1]);
          var $__13=
           new _A_($UHC.$Base.$_2a,[$__2,$__12,$__11]);
          var $__14=
           new _A_($Data.$Complex.$_3a_2b,[$__13,$__10]);
          return $__14;});
$Data.$Complex.$Floating__NEW469UNQ394DCT54__16__0RDC=
 new _F_(function($__,$Floating__,$Floating__3,$__4,$__5,$__6,$__7,$__8,$__9,$__10,$__11)
         {var $Floating__12=
           new _A_($Data.$Complex.$Floating__NEW481UNQ609EVLDCT54__16__0RDC,[$__,$Floating__,$Floating__3,$__4,$__5,$__6,$__7,$__8,$__9,$__10,$__11]);
          return $Floating__12;});
$Data.$Complex.$Floating__NEW481UNQ609EVLDCT54__16__0RDC=
 new _F_(function($__,$Floating__,$Floating__3,$__4,$__5,$__6,$__7,$__8,$__9,$__10,$__11)
         {var $Floating__12=
           _e_(new _A_($UHC.$Base.$Floating__CLS74__22__0,[$Floating__]));
          var $__32=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2etanh,[$__6,$__7,$__9]);
          var $__33=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2etan,[$__6,$__7,$__9]);
          var $__34=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esqrt,[$__4,$__5,$__6,$__7,$__8,$__10,$__11]);
          var $__35=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esinh,[$__6,$__7]);
          var $__36=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2esin,[$__6,$__7]);
          var $__37=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2elog,[$__4,$__6]);
          var $__38=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eexp,[$__6,$__7]);
          var $__39=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2ecosh,[$__6,$__7]);
          var $__40=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2ecos,[$__6,$__7]);
          var $__41=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eatanh,[$Floating__,$__8,$__9]);
          var $__42=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eatan,[$Floating__,$__7,$__8,$__9]);
          var $__43=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2easinh,[$Floating__,$__8]);
          var $__44=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2easin,[$Floating__,$__7,$__8]);
          var $__45=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eacosh,[$Floating__,$__8,$__9]);
          var $__46=
           new _A_($Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eacos,[$Floating__,$__7,$__8]);
          var $__47=
           {_tag_:0,_1:$Floating__12._1,_2:$__,_3:$__46,_4:$__45,_5:$__44,_6:$__43,_7:$__42,_8:$__41,_9:$__40,_10:$__39,_11:$__38,_12:$__37,_13:$Floating__12._13,_14:$Floating__3,_15:$__36,_16:$__35,_17:$__34,_18:$__33,_19:$__32};
          return $__47;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eatanh=
 new _F_(function($Floating__,$__,$__3,$z)
         {var $__5=
           new _A_($UHC.$Base.$_2a,[$__,$z,$z]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_2d,[$__,$__7,$__5]);
          var $__9=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__8]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_2b,[$__,$__11,$z]);
          var $__13=
           new _A_($UHC.$Base.$_2f,[$__3,$__12,$__9]);
          return new _A_($UHC.$Base.$log,[$Floating__,$__13]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eatan=
 new _F_(function($Floating__,$__,$__3,$__4,$z)
         {var $z6=
           _e_($z);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__3,$z6,$z6]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_2b,[$__3,$__11,$__9]);
          var $__13=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__12]);
          var $__14=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__15=
           new _A_($UHC.$Base.$fromInteger,[$__,$__14]);
          var $__16=
           new _A_($UHC.$Base.$_2d,[$__,$__15,$z6._2]);
          var $__17=
           new _A_($Data.$Complex.$_3a_2b,[$__16,$z6._1]);
          var $__18=
           new _A_($UHC.$Base.$_2f,[$__4,$__17,$__13]);
          var $__19=
           new _A_($UHC.$Base.$log,[$Floating__,$__18]);
          var $x_27=
           new _A_($Data.$Complex.$x_27NEW570UNQ675,[$__19]);
          var $y_27=
           new _A_($Data.$Complex.$y_27NEW573UNQ676,[$__19]);
          var $__22=
           new _A_($UHC.$Base.$negate,[$__,$x_27]);
          return new _A_($Data.$Complex.$_3a_2b,[$y_27,$__22]);});
$Data.$Complex.$x_27NEW570UNQ675=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Data.$Complex.$y_27NEW573UNQ676=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._2;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2easinh=
 new _F_(function($Floating__,$__,$z)
         {var $__4=
           new _A_($UHC.$Base.$_2a,[$__,$z,$z]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_2b,[$__,$__6,$__4]);
          var $__8=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_2b,[$__,$z,$__8]);
          return new _A_($UHC.$Base.$log,[$Floating__,$__9]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2easin=
 new _F_(function($Floating__,$__,$__3,$z)
         {var $z5=
           _e_($z);
          var $__8=
           new _A_($UHC.$Base.$_2a,[$__3,$z5,$z5]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_2d,[$__3,$__10,$__8]);
          var $__12=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__11]);
          var $__13=
           new _A_($UHC.$Base.$negate,[$__,$z5._2]);
          var $__14=
           new _A_($Data.$Complex.$_3a_2b,[$__13,$z5._1]);
          var $__15=
           new _A_($UHC.$Base.$_2b,[$__3,$__14,$__12]);
          var $__16=
           new _A_($UHC.$Base.$log,[$Floating__,$__15]);
          var $x_27=
           new _A_($Data.$Complex.$x_27NEW543UNQ658,[$__16]);
          var $y_27=
           new _A_($Data.$Complex.$y_27NEW546UNQ659,[$__16]);
          var $__19=
           new _A_($UHC.$Base.$negate,[$__,$x_27]);
          return new _A_($Data.$Complex.$_3a_2b,[$y_27,$__19]);});
$Data.$Complex.$x_27NEW543UNQ658=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Data.$Complex.$y_27NEW546UNQ659=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._2;});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eacosh=
 new _F_(function($Floating__,$__,$__3,$z)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_2b,[$__,$z,$__6]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__9=
           new _A_($UHC.$Base.$fromInteger,[$__,$__8]);
          var $__10=
           new _A_($UHC.$Base.$_2d,[$__,$z,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_2f,[$__3,$__10,$__7]);
          var $__12=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__11]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__14=
           new _A_($UHC.$Base.$fromInteger,[$__,$__13]);
          var $__15=
           new _A_($UHC.$Base.$_2b,[$__,$z,$__14]);
          var $__16=
           new _A_($UHC.$Base.$_2a,[$__,$__15,$__12]);
          var $__17=
           new _A_($UHC.$Base.$_2b,[$__,$z,$__16]);
          return new _A_($UHC.$Base.$log,[$Floating__,$__17]);});
$Data.$Complex.$Floating__DCT54__16__0DFLUHC_2eBase_2eacos=
 new _F_(function($Floating__,$__,$__3,$z)
         {var $__5=
           new _A_($UHC.$Base.$_2a,[$__3,$z,$z]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_2d,[$__3,$__7,$__5]);
          var $__9=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__8]);
          var $x_27=
           new _A_($Data.$Complex.$x_27NEW501UNQ632,[$__9]);
          var $y_27=
           new _A_($Data.$Complex.$y_27NEW504UNQ633,[$__9]);
          var $__12=
           new _A_($UHC.$Base.$negate,[$__,$y_27]);
          var $__13=
           new _A_($Data.$Complex.$_3a_2b,[$__12,$x_27]);
          var $__14=
           new _A_($UHC.$Base.$_2b,[$__3,$z,$__13]);
          var $__15=
           new _A_($UHC.$Base.$log,[$Floating__,$__14]);
          var $x_27_27=
           new _A_($Data.$Complex.$x_27_27NEW511UNQ635,[$__15]);
          var $y_27_27=
           new _A_($Data.$Complex.$y_27_27NEW514UNQ636,[$__15]);
          var $__18=
           new _A_($UHC.$Base.$negate,[$__,$x_27_27]);
          return new _A_($Data.$Complex.$_3a_2b,[$y_27_27,$__18]);});
$Data.$Complex.$x_27NEW501UNQ632=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Data.$Complex.$y_27NEW504UNQ633=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._2;});
$Data.$Complex.$x_27_27NEW511UNQ635=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Data.$Complex.$y_27_27NEW514UNQ636=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._2;});
$Data.$Complex.$Floating__DCT54__16__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$Fractional__DCT54__11__0,[$__]);
          var $__3=
           new _A_($Data.$Complex.$Num__DCT54__9__0,[$__]);
          var $__4=
           new _A_($Data.$Complex.$__56__1050__1__0NEW311UNQ487,[$__]);
          var $__5=
           new _A_($Data.$Complex.$__56__1001__2__59NEW314UNQ426,[$__]);
          var $__6=
           new _A_($Data.$Complex.$__56__1001__2__58NEW317UNQ425,[$__5]);
          var $__7=
           new _A_($Data.$Complex.$__56__2664__2__0NEW320UNQ519,[$__6]);
          var $__8=
           new _A_($Data.$Complex.$__56__1001__2__10NEW323UNQ404,[$__4]);
          var $__9=
           new _A_($Data.$Complex.$__56__1058__2__0NEW326UNQ488,[$__8]);
          var $__10=
           new _A_($Data.$Complex.$__56__2446__1__0NEW329UNQ591,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__12=
           new _A_($UHC.$Base.$fromInteger,[$__9,$__11]);
          var $__13=
           new _A_($UHC.$Base.$pi,[$__4]);
          var $Floating__DCT54__16__0DFLUHC_2eBase_2epi=
           new _A_($Data.$Complex.$_3a_2b,[$__13,$__12]);
          var $__56__1000__3=
           new _A_($Data.$Complex.$Fractional__DCT54__11__0,[$__]);
          var $Floating__=
           _i_();
          _i_set_($Floating__,new _A_($Data.$Complex.$Floating__NEW469UNQ394DCT54__16__0RDC,[$__56__1000__3,$Floating__,$Floating__DCT54__16__0DFLUHC_2eBase_2epi,$__,$__8,$__4,$__9,$__3,$__2,$__10,$__7]));
          return $Floating__;});
$Data.$Complex.$__Rep1ComplexDFLUHC_2eBase_2eto1GENRepresentable1=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Data.$Complex.$_3a_2b,[$proj__3._1,$proj__3._2]);
          return $__;});
$Data.$Complex.$__Rep1ComplexDFLUHC_2eBase_2efrom1GENRepresentable1=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$Par1__,[$x2._2]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$Par1__,[$x2._1]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Data.$Complex.$__Rep1ComplexNEW600UNQ50SDCGENRepresentable1=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__Rep1ComplexNEW602UNQ51EVLSDCGENRepresentable1,[$__]);
          return $__2;});
$Data.$Complex.$__Rep1ComplexNEW602UNQ51EVLSDCGENRepresentable1=
 new _F_(function($__)
         {var $Representable1__=
           _e_(new _A_($UHC.$Base.$Representable1__CLS74__370__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Data.$Complex.$__Rep1ComplexDFLUHC_2eBase_2efrom1GENRepresentable1,_2:$Data.$Complex.$__Rep1ComplexDFLUHC_2eBase_2eto1GENRepresentable1};
          return $__5;});
$Data.$Complex.$__Rep1ComplexUNQ50SDCGENRepresentable1=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Complex.$__Rep1ComplexNEW600UNQ50SDCGENRepresentable1,[$Data.$Complex.$__Rep1ComplexUNQ50SDCGENRepresentable1]);}),[]);
$Data.$Complex.$__Rep1ComplexGENRepresentable1=
 new _A_(new _F_(function()
                 {return $Data.$Complex.$__Rep1ComplexUNQ50SDCGENRepresentable1;}),[]);
$Data.$Complex.$__56__124__2__0NEW607UNQ92=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__58__1135__0=
 new _F_(function($__,$theta)
         {var $__3=
           new _A_($UHC.$Base.$sin,[$__,$theta]);
          var $__4=
           new _A_($UHC.$Base.$cos,[$__,$theta]);
          return new _A_($Data.$Complex.$_3a_2b,[$__4,$__3]);});
$Data.$Complex.$cis=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__124__2__0NEW607UNQ92,[$__]);
          return new _A_($Data.$Complex.$__58__1135__0,[$__2]);});
$Data.$Complex.$__56__142__2__2NEW614UNQ114=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__142__2__1NEW617UNQ113=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__225__2__0NEW620UNQ115=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__58__1155__0=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$negate,[$__,$__3._2]);
          var $__7=
           new _A_($Data.$Complex.$_3a_2b,[$__3._1,$__6]);
          return $__7;});
$Data.$Complex.$conjugate=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__142__2__2NEW614UNQ114,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__142__2__1NEW617UNQ113,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__225__2__0NEW620UNQ115,[$__3]);
          return new _A_($Data.$Complex.$__58__1155__0,[$__4]);});
$Data.$Complex.$__56__644__2__0NEW628UNQ249=
 new _F_(function($__)
         {var $Floating__=
           _e_($__);
          return $Floating__._1;});
$Data.$Complex.$__56__548__2__2NEW631UNQ245=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._2;});
$Data.$Complex.$__56__634__2__0NEW634UNQ250=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$Data.$Complex.$__58__1175__0=
 new _F_(function($__,$__2,$r,$theta)
         {var $__5=
           new _A_($UHC.$Base.$sin,[$__2,$theta]);
          var $__6=
           new _A_($UHC.$Base.$_2a,[$__,$r,$__5]);
          var $__7=
           new _A_($UHC.$Base.$cos,[$__2,$theta]);
          var $__8=
           new _A_($UHC.$Base.$_2a,[$__,$r,$__7]);
          return new _A_($Data.$Complex.$_3a_2b,[$__8,$__6]);});
$Data.$Complex.$mkPolar=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Complex.$__56__644__2__0NEW628UNQ249,[$__]);
          var $__3=
           new _A_($Data.$Complex.$__56__548__2__2NEW631UNQ245,[$__2]);
          var $__4=
           new _A_($Data.$Complex.$__56__634__2__0NEW634UNQ250,[$__3]);
          return new _A_($Data.$Complex.$__58__1175__0,[$__4,$__2]);});
$Data.$Complex.$_24D__ComplexDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Data.Complex"]);});
$Data.$Complex.$_24D__ComplexDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Complex"]);});
$Data.$Complex.$_24D__ComplexNEW645UNQ75SDCGENDatatype=
 new _F_(function($_24D__Complex)
         {var $_24D__Complex2=
           new _A_($Data.$Complex.$_24D__ComplexNEW647UNQ76EVLSDCGENDatatype,[$_24D__Complex]);
          return $_24D__Complex2;});
$Data.$Complex.$_24D__ComplexNEW647UNQ76EVLSDCGENDatatype=
 new _F_(function($_24D__Complex)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Complex]));
          var $__5=
           {_tag_:0,_1:$Data.$Complex.$_24D__ComplexDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Data.$Complex.$_24D__ComplexDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Data.$Complex.$_24D__ComplexUNQ75SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Complex.$_24D__ComplexNEW645UNQ75SDCGENDatatype,[$Data.$Complex.$_24D__ComplexUNQ75SDCGENDatatype]);}),[]);
$Data.$Complex.$_24D__ComplexGENDatatype=
 new _A_(new _F_(function()
                 {return $Data.$Complex.$_24D__ComplexUNQ75SDCGENDatatype;}),[]);
$Data.$Complex.$_24C___3a_2bDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,[":+"]);});
$Data.$Complex.$_24C___3a_2bDFLUHC_2eBase_2econFixityGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$Infix__,[$UHC.$Base.$NotAssociative__,6]);});
$Data.$Complex.$_24C___3a_2bNEW654UNQ82SDCGENConstructor=
 new _F_(function($_24C___3a_2b)
         {var $_24C___3a_2b2=
           new _A_($Data.$Complex.$_24C___3a_2bNEW656UNQ83EVLSDCGENConstructor,[$_24C___3a_2b]);
          return $_24C___3a_2b2;});
$Data.$Complex.$_24C___3a_2bNEW656UNQ83EVLSDCGENConstructor=
 new _F_(function($_24C___3a_2b)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C___3a_2b]));
          var $__7=
           {_tag_:0,_1:$Data.$Complex.$_24C___3a_2bDFLUHC_2eBase_2econFixityGENConstructor,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Data.$Complex.$_24C___3a_2bDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Data.$Complex.$_24C___3a_2bUNQ82SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Complex.$_24C___3a_2bNEW654UNQ82SDCGENConstructor,[$Data.$Complex.$_24C___3a_2bUNQ82SDCGENConstructor]);}),[]);
$Data.$Complex.$_24C___3a_2bGENConstructor=
 new _A_(new _F_(function()
                 {return $Data.$Complex.$_24C___3a_2bUNQ82SDCGENConstructor;}),[]);
