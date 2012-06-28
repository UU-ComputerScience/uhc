// Control.Monad
var $Control=
 ($Control ? $Control : {});
$Control.$Monad=
 ($Control.$Monad ? $Control.$Monad : {});
$Control.$Monad.$zipWithM__=
 new _F_(function($__,$f,$xs,$ys)
         {var $__5=
           new _A_($UHC.$Base.$zipWith,[$f,$xs,$ys]);
          return new _A_($UHC.$Base.$sequence__,[$__,$__5]);});
$Control.$Monad.$zipWithM=
 new _F_(function($__,$f,$xs,$ys)
         {var $__5=
           new _A_($UHC.$Base.$zipWith,[$f,$xs,$ys]);
          return new _A_($UHC.$Base.$sequence,[$__,$__5]);});
$Control.$Monad.$when=
 new _F_(function($__,$p,$s)
         {var $__4=
           _e_($p);
          var $__swJSW0__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,[]]);
             $__swJSW0__0=
              $__5;
             break;
            case 1:
             $__swJSW0__0=
              $s;
             break;}
          return $__swJSW0__0;});
$Control.$Monad.$unless=
 new _F_(function($__,$p,$s)
         {var $__4=
           _e_($p);
          var $__swJSW1__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW1__0=
              $s;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,[]]);
             $__swJSW1__0=
              $__5;
             break;}
          return $__swJSW1__0;});
$Control.$Monad.$replicateM__=
 new _F_(function($__,$n,$x)
         {var $__4=
           new _A_($UHC.$Base.$replicate,[$n,$x]);
          return new _A_($UHC.$Base.$sequence__,[$__,$__4]);});
$Control.$Monad.$replicateM=
 new _F_(function($__,$n,$x)
         {var $__4=
           new _A_($UHC.$Base.$replicate,[$n,$x]);
          return new _A_($UHC.$Base.$sequence,[$__,$__4]);});
$Control.$Monad.$mzero=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$Control.$Monad.$mplus=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Control.$Monad.$msum=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$mzero,[$__]);
          var $__3=
           new _A_($Control.$Monad.$mplus,[$__]);
          return new _A_($UHC.$Base.$foldr,[$__3,$__2]);});
$Control.$Monad.$mapAndUnzipM=
 new _F_(function($__,$f,$xs)
         {var $__4=
           new _A_($UHC.$Base.$return,[$__]);
          var $__5=
           new _A_($UHC.$Base.$_2e,[$__4,$UHC.$Base.$unzip]);
          var $__6=
           new _A_($UHC.$Base.$map,[$f,$xs]);
          var $__7=
           new _A_($UHC.$Base.$sequence,[$__,$__6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__7,$__5]);});
$Control.$Monad.$_24okUNQ85=
 new _F_(function($__,$f,$m2,$m3,$m4,$m5,$_24x)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ89,[$__,$f,$m3,$m4,$m5,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__8]);});
$Control.$Monad.$_24okUNQ89=
 new _F_(function($__,$f,$m3,$m4,$m5,$_24x,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ93,[$__,$f,$m4,$m5,$_24x,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__8]);});
$Control.$Monad.$_24okUNQ93=
 new _F_(function($__,$f,$m4,$m5,$_24x,$_24x6,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ97,[$__,$f,$m5,$_24x,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m4,$__8]);});
$Control.$Monad.$_24okUNQ97=
 new _F_(function($__,$f,$m5,$_24x,$_24x5,$_24x6,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ101,[$__,$f,$_24x,$_24x5,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m5,$__8]);});
$Control.$Monad.$_24okUNQ101=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5,$_24x6,$_24x7)
         {var $__8=
           new _A_($f,[$_24x,$_24x4,$_24x5,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$return,[$__,$__8]);});
$Control.$Monad.$liftM5=
 new _F_(function($__,$f,$m1,$m2,$m3,$m4,$m5)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ85,[$__,$f,$m2,$m3,$m4,$m5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__8]);});
$Control.$Monad.$_24okUNQ120=
 new _F_(function($__,$f,$m2,$m3,$m4,$_24x)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ124,[$__,$f,$m3,$m4,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__7]);});
$Control.$Monad.$_24okUNQ124=
 new _F_(function($__,$f,$m3,$m4,$_24x,$_24x6)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ128,[$__,$f,$m4,$_24x,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__7]);});
$Control.$Monad.$_24okUNQ128=
 new _F_(function($__,$f,$m4,$_24x,$_24x5,$_24x6)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ132,[$__,$f,$_24x,$_24x5,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m4,$__7]);});
$Control.$Monad.$_24okUNQ132=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5,$_24x6)
         {var $__7=
           new _A_($f,[$_24x,$_24x4,$_24x5,$_24x6]);
          return new _A_($UHC.$Base.$return,[$__,$__7]);});
$Control.$Monad.$liftM4=
 new _F_(function($__,$f,$m1,$m2,$m3,$m4)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ120,[$__,$f,$m2,$m3,$m4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__7]);});
$Control.$Monad.$_24okUNQ148=
 new _F_(function($__,$f,$m2,$m3,$_24x)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ152,[$__,$f,$m3,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__6]);});
$Control.$Monad.$_24okUNQ152=
 new _F_(function($__,$f,$m3,$_24x,$_24x5)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ156,[$__,$f,$_24x,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__6]);});
$Control.$Monad.$_24okUNQ156=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5)
         {var $__6=
           new _A_($f,[$_24x,$_24x4,$_24x5]);
          return new _A_($UHC.$Base.$return,[$__,$__6]);});
$Control.$Monad.$liftM3=
 new _F_(function($__,$f,$m1,$m2,$m3)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ148,[$__,$f,$m2,$m3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__6]);});
$Control.$Monad.$_24okUNQ169=
 new _F_(function($__,$f,$m2,$_24x)
         {var $__5=
           new _A_($Control.$Monad.$_24okUNQ173,[$__,$f,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__5]);});
$Control.$Monad.$_24okUNQ173=
 new _F_(function($__,$f,$_24x,$_24x4)
         {var $__5=
           new _A_($f,[$_24x,$_24x4]);
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Control.$Monad.$liftM2=
 new _F_(function($__,$f,$m1,$m2)
         {var $__5=
           new _A_($Control.$Monad.$_24okUNQ169,[$__,$f,$m2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__5]);});
$Control.$Monad.$_24okUNQ183=
 new _F_(function($__,$f,$_24x)
         {var $__4=
           new _A_($f,[$_24x]);
          return new _A_($UHC.$Base.$return,[$__,$__4]);});
$Control.$Monad.$liftM=
 new _F_(function($__,$f,$m1)
         {var $__4=
           new _A_($Control.$Monad.$_24okUNQ183,[$__,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__4]);});
$Control.$Monad.$join=
 new _F_(function($__,$x)
         {return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$x,$UHC.$Base.$id]);});
$Control.$Monad.$__346__4215__2__0NEW53UNQ294=
 new _F_(function($__)
         {var $Monad__=
           _e_($__);
          return $Monad__._3;});
$Control.$Monad.$__348__290__0=
 new _F_(function($__,$__2,$x1)
         {var $__4=
           _e_($x1);
          var $__swJSW5__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($Control.$Monad.$mzero,[$__]);
             $__swJSW5__0=
              $__5;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$return,[$__2,[]]);
             $__swJSW5__0=
              $__6;
             break;}
          return $__swJSW5__0;});
$Control.$Monad.$guard=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$__346__4215__2__0NEW53UNQ294,[$__]);
          return new _A_($Control.$Monad.$__348__290__0,[$__,$__2]);});
$Control.$Monad.$forever=
 new _F_(function($__,$a)
         {var $__3=
           new _A_($Control.$Monad.$forever,[$__,$a]);
          return new _A_($UHC.$Base.$_3e_3e,[$__,$a,$__3]);});
$Control.$Monad.$forM__=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$mapM__,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$forM=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$mapM,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$__348__333__0=
 new _F_(function($__,$x1,$xs,$fax)
         {return new _A_($Control.$Monad.$foldM,[$__,$x1,$fax,$xs]);});
$Control.$Monad.$foldM=
 new _F_(function($__,$x1,$x2,$x3)
         {var $x35=
           _e_($x3);
          var $__swJSW6__0;
          switch($x35._tag_)
           {case 0:
             var $__8=
              new _A_($x1,[$x2,$x35._1]);
             var $__9=
              new _A_($Control.$Monad.$__348__333__0,[$__,$x1,$x35._2]);
             var $__10=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__8,$__9]);
             $__swJSW6__0=
              $__10;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$return,[$__,$x2]);
             $__swJSW6__0=
              $__11;
             break;}
          return $__swJSW6__0;});
$Control.$Monad.$foldM__=
 new _F_(function($__,$f,$a,$xs)
         {var $__5=
           new _A_($UHC.$Base.$return,[$__,[]]);
          var $__6=
           new _A_($Control.$Monad.$foldM,[$__,$f,$a,$xs]);
          return new _A_($UHC.$Base.$_3e_3e,[$__,$__6,$__5]);});
$Control.$Monad.$_24okUNQ257=
 new _F_(function($__,$x1,$x,$xs,$_24x)
         {var $__6=
           new _A_($Control.$Monad.$filterM,[$__,$x1,$xs]);
          var $__7=
           new _A_($Control.$Monad.$_24okUNQ261,[$__,$x,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__6,$__7]);});
$Control.$Monad.$_24okUNQ261=
 new _F_(function($__,$x,$_24x,$_24x4)
         {var $__5=
           new _A_($Control.$Monad.$__348__376NEW79,[$x,$_24x,$_24x4]);
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Control.$Monad.$__348__376NEW79=
 new _F_(function($x,$_24x,$_24x3)
         {var $__=
           _e_($_24x);
          var $__swJSW7__0;
          switch($__._tag_)
           {case 0:
             $__swJSW7__0=
              $_24x3;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$x,$_24x3]);
             $__swJSW7__0=
              $__5;
             break;}
          return $__swJSW7__0;});
$Control.$Monad.$filterM=
 new _F_(function($__,$x1,$x2)
         {var $x24=
           _e_($x2);
          var $__swJSW8__0;
          switch($x24._tag_)
           {case 0:
             var $__7=
              new _A_($x1,[$x24._1]);
             var $__8=
              new _A_($Control.$Monad.$_24okUNQ257,[$__,$x1,$x24._1,$x24._2]);
             $__swJSW8__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__7,$__8]);
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$_5b_5d]);
             $__swJSW8__0=
              $__9;
             break;}
          return $__swJSW8__0;});
$Control.$Monad.$ap=
 new _F_(function($__)
         {return new _A_($Control.$Monad.$liftM2,[$__,$UHC.$Base.$id]);});
$Control.$Monad.$MonadPlus__CLS344__0__0=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined};
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW92UNQ311DCT344__1__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           new _A_($Control.$Monad.$MonadPlus__NEW94UNQ312EVLDCT344__1__0RDC,[$MonadPlus__]);
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW94UNQ312EVLDCT344__1__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           _e_(new _A_($Control.$Monad.$MonadPlus__CLS344__0__0,[$MonadPlus__]));
          var $__6=
           {_tag_:0,_1:$UHC.$Base.$_2b_2b,_2:$UHC.$Base.$_5b_5d,_3:$UHC.$Base.$Monad__DCT74__85__0};
          return $__6;});
$Control.$Monad.$MonadPlus__UNQ311DCT344__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$MonadPlus__NEW92UNQ311DCT344__1__0RDC,[$Control.$Monad.$MonadPlus__UNQ311DCT344__1__0RDC]);}),[]);
$Control.$Monad.$MonadPlus__DCT344__1__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$MonadPlus__UNQ311DCT344__1__0RDC;}),[]);
$Control.$Monad.$MonadPlus__DCT344__4__0DFLControl_2eMonad_2emplus=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW10__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW10__0=
              $x1;
             break;
            case 1:
             $__swJSW10__0=
              $x2;
             break;}
          return $__swJSW10__0;});
$Control.$Monad.$MonadPlus__NEW101UNQ317DCT344__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           new _A_($Control.$Monad.$MonadPlus__NEW103UNQ318EVLDCT344__4__0RDC,[$MonadPlus__]);
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW103UNQ318EVLDCT344__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           _e_(new _A_($Control.$Monad.$MonadPlus__CLS344__0__0,[$MonadPlus__]));
          var $__6=
           {_tag_:0,_1:$Control.$Monad.$MonadPlus__DCT344__4__0DFLControl_2eMonad_2emplus,_2:$UHC.$Base.$Nothing__,_3:$UHC.$Base.$Monad__DCT74__75__0};
          return $__6;});
$Control.$Monad.$MonadPlus__UNQ317DCT344__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$MonadPlus__NEW101UNQ317DCT344__4__0RDC,[$Control.$Monad.$MonadPlus__UNQ317DCT344__4__0RDC]);}),[]);
$Control.$Monad.$MonadPlus__DCT344__4__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$MonadPlus__UNQ317DCT344__4__0RDC;}),[]);
$Control.$Monad.$_3e_3d_3e=
 new _F_(function($__,$f,$g,$x)
         {var $__5=
           new _A_($f,[$x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__5,$g]);});
$Control.$Monad.$_3c_3d_3c=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$_3e_3d_3e,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$_24Dict_2dMonadPlus=
 new _F_(function($x1,$x2,$x3)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3};});
