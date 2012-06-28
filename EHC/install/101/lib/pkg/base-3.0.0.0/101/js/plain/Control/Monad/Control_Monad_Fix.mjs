// Control.Monad.Fix
var $Control=
 ($Control ? $Control : {});
$Control.$Monad=
 ($Control.$Monad ? $Control.$Monad : {});
$Control.$Monad.$Fix=
 ($Control.$Monad.$Fix ? $Control.$Monad.$Fix : {});
$Control.$Monad.$Fix.$mfix=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Control.$Monad.$Fix.$MonadFix__CLS78__0__0=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined};
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__DCT78__1__0DFLControl_2eMonad_2eFix_2emfix=
 new _F_(function($f)
         {var $__=
           _i_();
          var $a=
           _i_();
          _i_set_($__,new _A_($Control.$Monad.$Fix.$unJustUNQ24,[$a]));
          _i_set_($a,new _A_($f,[$__]));
          return $a;});
$Control.$Monad.$Fix.$unJustUNQ24=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW1__0;
          switch($__._tag_)
           {case 0:
             $__swJSW1__0=
              $__._1;
             break;
            case 1:
             var $__4=
              new _A_($UHC.$Base.$packedStringToString,["mfix Maybe: Nothing"]);
             var $__5=
              new _A_($UHC.$Base.$error,[$__4]);
             $__swJSW1__0=
              $__5;
             break;}
          return $__swJSW1__0;});
$Control.$Monad.$Fix.$MonadFix__NEW12UNQ18DCT78__1__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           new _A_($Control.$Monad.$Fix.$MonadFix__NEW14UNQ19EVLDCT78__1__0RDC,[$MonadFix__]);
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__NEW14UNQ19EVLDCT78__1__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           _e_(new _A_($Control.$Monad.$Fix.$MonadFix__CLS78__0__0,[$MonadFix__]));
          var $__5=
           {_tag_:0,_1:$Control.$Monad.$Fix.$MonadFix__DCT78__1__0DFLControl_2eMonad_2eFix_2emfix,_2:$UHC.$Base.$Monad__DCT74__75__0};
          return $__5;});
$Control.$Monad.$Fix.$MonadFix__UNQ18DCT78__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$Fix.$MonadFix__NEW12UNQ18DCT78__1__0RDC,[$Control.$Monad.$Fix.$MonadFix__UNQ18DCT78__1__0RDC]);}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT78__1__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$Fix.$MonadFix__UNQ18DCT78__1__0RDC;}),[]);
$Control.$Monad.$Fix.$MonadFix__NEW19UNQ31DCT78__3__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           new _A_($Control.$Monad.$Fix.$MonadFix__NEW21UNQ33EVLDCT78__3__0RDC,[$MonadFix__]);
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__NEW21UNQ33EVLDCT78__3__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           _e_(new _A_($Control.$Monad.$Fix.$MonadFix__CLS78__0__0,[$MonadFix__]));
          var $__5=
           new _A_($Control.$Monad.$Fix.$MonadFix__DCT78__3__0DFLControl_2eMonad_2eFix_2emfix,[$MonadFix__]);
          var $__6=
           {_tag_:0,_1:$__5,_2:$UHC.$Base.$Monad__DCT74__85__0};
          return $__6;});
$Control.$Monad.$Fix.$MonadFix__DCT78__3__0DFLControl_2eMonad_2eFix_2emfix=
 new _F_(function($MonadFix__,$f)
         {var $__=
           new _A_($UHC.$Base.$_2e,[$f,$UHC.$Base.$head]);
          var $__4=
           new _A_($Data.$Function.$fix,[$__]);
          var $__5=
           _e_($__4);
          var $__swJSW4__0;
          switch($__5._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$_2e,[$UHC.$Base.$tail,$f]);
             var $__9=
              new _A_($Control.$Monad.$Fix.$mfix,[$MonadFix__,$__8]);
             var $__10=
              new _A_($UHC.$Base.$_3a,[$__5._1,$__9]);
             $__swJSW4__0=
              $__10;
             break;
            case 1:
             $__swJSW4__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW4__0;});
$Control.$Monad.$Fix.$MonadFix__UNQ31DCT78__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$Fix.$MonadFix__NEW19UNQ31DCT78__3__0RDC,[$Control.$Monad.$Fix.$MonadFix__UNQ31DCT78__3__0RDC]);}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT78__3__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$Fix.$MonadFix__UNQ31DCT78__3__0RDC;}),[]);
$Control.$Monad.$Fix.$MonadFix__NEW33UNQ43DCT78__4__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           new _A_($Control.$Monad.$Fix.$MonadFix__NEW35UNQ44EVLDCT78__4__0RDC,[$MonadFix__]);
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__NEW35UNQ44EVLDCT78__4__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           _e_(new _A_($Control.$Monad.$Fix.$MonadFix__CLS78__0__0,[$MonadFix__]));
          var $__5=
           {_tag_:0,_1:$System.$IO.$Fix.$fixIO,_2:$UHC.$Base.$Monad__DCT74__339__0};
          return $__5;});
$Control.$Monad.$Fix.$MonadFix__UNQ43DCT78__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$Fix.$MonadFix__NEW33UNQ43DCT78__4__0RDC,[$Control.$Monad.$Fix.$MonadFix__UNQ43DCT78__4__0RDC]);}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT78__4__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$Fix.$MonadFix__UNQ43DCT78__4__0RDC;}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT78__6__0DFLControl_2eMonad_2eFix_2emfix=
 new _F_(function($f,$r)
         {var $a=
           _i_();
          _i_set_($a,new _A_($f,[$a,$r]));
          return $a;});
$Control.$Monad.$Fix.$MonadFix__NEW42UNQ48DCT78__6__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           new _A_($Control.$Monad.$Fix.$MonadFix__NEW44UNQ49EVLDCT78__6__0RDC,[$MonadFix__]);
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__NEW44UNQ49EVLDCT78__6__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           _e_(new _A_($Control.$Monad.$Fix.$MonadFix__CLS78__0__0,[$MonadFix__]));
          var $__5=
           {_tag_:0,_1:$Control.$Monad.$Fix.$MonadFix__DCT78__6__0DFLControl_2eMonad_2eFix_2emfix,_2:$Control.$Monad.$Instances.$Monad__DCT42__2__0};
          return $__5;});
$Control.$Monad.$Fix.$MonadFix__UNQ48DCT78__6__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$Fix.$MonadFix__NEW42UNQ48DCT78__6__0RDC,[$Control.$Monad.$Fix.$MonadFix__UNQ48DCT78__6__0RDC]);}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT78__6__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$Fix.$MonadFix__UNQ48DCT78__6__0RDC;}),[]);
$Control.$Monad.$Fix.$_24Dict_2dMonadFix=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
