// UHC.MVar
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$MVar=
 ($UHC.$MVar ? $UHC.$MVar : {});
$UHC.$MVar.$tryTakeMutVar=
 new _F_(function($v,$s)
         {var $__=
           new _A_($UHC.$MutVar.$readMutVar,[$v,$s]);
          var $__4=
           _e_($__);
          var $x7=
           _e_($__4[1]);
          var $__swJSW1__0;
          switch($x7._tag_)
           {case 0:
             var $s3=
              new _A_($UHC.$MutVar.$writeMutVar,[$v,$UHC.$Base.$Nothing__,$__4[0]]);
             var $s310=
              _e_($s3);
             $__swJSW1__0=
              [$s3,$x7];
             break;
            case 1:
             $__swJSW1__0=
              $__;
             break;}
          return $__swJSW1__0;});
$UHC.$MVar.$__336__24__0=
 new _F_(function($__,$s)
         {var $__3=
           new _A_($UHC.$MutVar.$readMutVar,[$__,$s]);
          var $__4=
           _e_($__3);
          var $x7=
           _e_($__4[1]);
          var $__swJSW3__0;
          switch($x7._tag_)
           {case 0:
             var $s3=
              new _A_($UHC.$MutVar.$writeMutVar,[$__,$UHC.$Base.$Nothing__,$__4[0]]);
             var $s310=
              _e_($s3);
             $__swJSW3__0=
              [$s3,$x7];
             break;
            case 1:
             $__swJSW3__0=
              $__3;
             break;}
          return $__swJSW3__0;});
$UHC.$MVar.$tryTakeMVar=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$MVar.$__336__24__0,[$__]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$__2]);});
$UHC.$MVar.$__334__621__0NEW16UNQ103CCN=
 new _F_(function($v,$a,$s2)
         {var $__=
           new _A_($UHC.$Base.$Just__,[$a]);
          var $s3=
           new _A_($UHC.$MutVar.$writeMutVar,[$v,$__,$s2]);
          var $s36=
           _e_($s3);
          return [$s3,$UHC.$Base.$Nothing__];});
$UHC.$MVar.$tryPutMutVar=
 new _F_(function($v,$a,$s)
         {var $__=
           new _A_($UHC.$MutVar.$readMutVar,[$v,$s]);
          var $__5=
           _e_($__);
          var $__8=
           new _A_($UHC.$MVar.$__334__621__0NEW16UNQ103CCN,[$v,$a,$__5[0]]);
          var $__9=
           _e_($__5[1]);
          var $__swJSW5__0;
          switch($__9._tag_)
           {case 0:
             var $__11=
              new _A_($UHC.$Base.$Just__,[$a]);
             var $__12=
              [$__5[0],$__11];
             $__swJSW5__0=
              $__12;
             break;
            case 1:
             $__swJSW5__0=
              $__8;
             break;}
          return $__swJSW5__0;});
$UHC.$MVar.$__336__73__0=
 new _F_(function($__,$x,$s)
         {var $__4=
           new _A_($UHC.$MVar.$tryPutMutVar,[$__,$x,$s]);
          var $__5=
           _e_($__4);
          var $__8=
           [$__5[0],$UHC.$Base.$False__];
          var $__9=
           _e_($__5[1]);
          var $__swJSW7__0;
          switch($__9._tag_)
           {case 0:
             $__swJSW7__0=
              $__8;
             break;
            case 1:
             var $__11=
              [$__5[0],$UHC.$Base.$True__];
             $__swJSW7__0=
              $__11;
             break;}
          return $__swJSW7__0;});
$UHC.$MVar.$tryPutMVar=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$MVar.$__336__73__0,[$__,$x]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$__3]);});
$UHC.$MVar.$__336__93__0=
 new _F_(function($__,$s)
         {var $__3=
           new _A_($UHC.$MVar.$tryTakeMutVar,[$__,$s]);
          var $__4=
           _e_($__3);
          var $__7=
           _e_($__4[1]);
          var $__swJSW9__0;
          switch($__7._tag_)
           {case 0:
             var $__9=
              [$__4[0],$__7._1];
             $__swJSW9__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$packedStringToString,["UHC.MVar.takeMVar: MVar holds nothing"]);
             var $__11=
              new _A_($UHC.$Base.$error,[$__10]);
             $__swJSW9__0=
              $__11;
             break;}
          return $__swJSW9__0;});
$UHC.$MVar.$takeMVar=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$MVar.$__336__93__0,[$__]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$__2]);});
$UHC.$MVar.$sameMVar=
 new _F_(function($__,$__2)
         {return new _A_($UHC.$MutVar.$sameMutVar,[$__,$__2]);});
$UHC.$MVar.$__336__120__0=
 new _F_(function($__,$x,$s)
         {var $__4=
           new _A_($UHC.$MVar.$tryPutMutVar,[$__,$x,$s]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["UHC.MVar.putMVar: MVar already holds something"]);
          var $__6=
           new _A_($UHC.$Base.$error,[$__5]);
          var $__7=
           _e_($__4);
          var $__10=
           _e_($__7[1]);
          var $__swJSW11__0;
          switch($__10._tag_)
           {case 0:
             $__swJSW11__0=
              $__6;
             break;
            case 1:
             var $__12=
              [$__7[0],[]];
             $__swJSW11__0=
              $__12;
             break;}
          return $__swJSW11__0;});
$UHC.$MVar.$putMVar=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$MVar.$__336__120__0,[$__,$x]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$__3]);});
$UHC.$MVar.$__336__138__0=
 new _F_(function($s1)
         {var $__=
           new _A_($UHC.$MutVar.$newMutVar,[$UHC.$Base.$Nothing__,$s1]);
          var $__3=
           _e_($__);
          var $__6=
           [$__3[0],$__3[1]];
          return $__6;});
$UHC.$MVar.$newEmptyMVar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$UHC.$MVar.$__336__138__0]);}),[]);
$UHC.$MVar.$__336__152__0=
 new _F_(function($__,$s)
         {var $__3=
           new _A_($UHC.$MutVar.$readMutVar,[$__,$s]);
          var $__4=
           _e_($__3);
          var $__7=
           _e_($__4[1]);
          var $__swJSW14__0;
          switch($__7._tag_)
           {case 0:
             var $__9=
              [$__4[0],$UHC.$Base.$False__];
             $__swJSW14__0=
              $__9;
             break;
            case 1:
             var $__10=
              [$__4[0],$UHC.$Base.$True__];
             $__swJSW14__0=
              $__10;
             break;}
          return $__swJSW14__0;});
$UHC.$MVar.$isEmptyMVar=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$MVar.$__336__152__0,[$__]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$IO__,$__2]);});
$UHC.$MVar.$addMVarFinalizer=
 new _F_(function($__,$finalizer)
         {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);});
$UHC.$MVar.$_24okUNQ243=
 new _F_(function($value,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($UHC.$MVar.$putMVar,[$_24x,$value]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$UHC.$MVar.$newMVar=
 new _F_(function($value)
         {var $__=
           new _A_($UHC.$MVar.$_24okUNQ243,[$value]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$MVar.$newEmptyMVar,$__]);});
$UHC.$MVar.$_24okUNQ212=
 new _F_(function($m,$io,$_24x)
         {var $__=
           new _A_($io,[$_24x]);
          var $__5=
           new _A_($UHC.$MVar.$__336__212__0,[$m,$_24x]);
          var $__6=
           new _A_($UHC.$OldException.$catchAny,[$__,$__5]);
          var $__7=
           new _A_($UHC.$MVar.$_24okUNQ219,[$m,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__7]);});
$UHC.$MVar.$_24okUNQ219=
 new _F_(function($m,$_24x,$_24x3)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x3]);
          var $__5=
           new _A_($UHC.$MVar.$putMVar,[$m,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$UHC.$MVar.$__336__212__0=
 new _F_(function($m,$_24x,$e)
         {var $__=
           new _A_($UHC.$Base.$throw,[$e]);
          var $__5=
           new _A_($UHC.$MVar.$putMVar,[$m,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$UHC.$MVar.$withMVar=
 new _F_(function($m,$io)
         {var $__=
           new _A_($UHC.$MVar.$takeMVar,[$m]);
          var $__4=
           new _A_($UHC.$MVar.$_24okUNQ212,[$m,$io]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$UHC.$MVar.$Eq__NEW79UNQ247DCT332__4__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           new _A_($UHC.$MVar.$Eq__NEW81UNQ248EVLDCT332__4__0RDC,[$Eq__]);
          return $Eq__2;});
$UHC.$MVar.$Eq__NEW81UNQ248EVLDCT332__4__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__5=
           {_tag_:0,_1:$Eq__2._1,_2:$UHC.$MVar.$sameMVar};
          return $__5;});
$UHC.$MVar.$Eq__UNQ247DCT332__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$MVar.$Eq__NEW79UNQ247DCT332__4__0RDC,[$UHC.$MVar.$Eq__UNQ247DCT332__4__0RDC]);}),[]);
$UHC.$MVar.$Eq__DCT332__4__0=
 new _A_(new _F_(function()
                 {return $UHC.$MVar.$Eq__UNQ247DCT332__4__0RDC;}),[]);
