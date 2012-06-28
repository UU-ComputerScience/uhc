// Data.Foldable
var $Data=
 ($Data ? $Data : {});
$Data.$Foldable=
 ($Data.$Foldable ? $Data.$Foldable : {});
$Data.$Foldable.$foldr1=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._6;});
$Data.$Foldable.$maximum=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$max,[$__2]);
          return new _A_($Data.$Foldable.$foldr1,[$__,$__3]);});
$Data.$Foldable.$max_27UNQ277=
 new _F_(function($cmp,$x,$y)
         {var $__=
           new _A_($cmp,[$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW1__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW1__0=
              $y;
             break;
            case 1:
             $__swJSW1__0=
              $x;
             break;
            case 2:
             $__swJSW1__0=
              $y;
             break;}
          return $__swJSW1__0;});
$Data.$Foldable.$maximumBy=
 new _F_(function($__,$cmp)
         {var $__3=
           new _A_($Data.$Foldable.$max_27UNQ277,[$cmp]);
          return new _A_($Data.$Foldable.$foldr1,[$__,$__3]);});
$Data.$Foldable.$minimum=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$min,[$__2]);
          return new _A_($Data.$Foldable.$foldr1,[$__,$__3]);});
$Data.$Foldable.$min_27UNQ285=
 new _F_(function($cmp,$x,$y)
         {var $__=
           new _A_($cmp,[$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW2__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW2__0=
              $x;
             break;
            case 1:
             $__swJSW2__0=
              $y;
             break;
            case 2:
             $__swJSW2__0=
              $x;
             break;}
          return $__swJSW2__0;});
$Data.$Foldable.$minimumBy=
 new _F_(function($__,$cmp)
         {var $__3=
           new _A_($Data.$Foldable.$min_27UNQ285,[$cmp]);
          return new _A_($Data.$Foldable.$foldr1,[$__,$__3]);});
$Data.$Foldable.$foldr=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$Data.$Foldable.$mapM__=
 new _F_(function($__,$__2,$f)
         {var $__4=
           new _A_($UHC.$Base.$return,[$__2,[]]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$__2]);
          var $__6=
           new _A_($UHC.$Base.$_2e,[$__5,$f]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__6,$__4]);});
$Data.$Foldable.$forM__=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Foldable.$mapM__,[$__,$__2]);
          return new _A_($UHC.$Base.$flip,[$__3]);});
$Data.$Foldable.$msum=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Control.$Monad.$mzero,[$__2]);
          var $__4=
           new _A_($Control.$Monad.$mplus,[$__2]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__4,$__3]);});
$Data.$Foldable.$sequenceA__=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Control.$Applicative.$pure,[$__2,[]]);
          var $__4=
           new _A_($Control.$Applicative.$_2a_3e,[$__2]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__4,$__3]);});
$Data.$Foldable.$sequence__=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$return,[$__2,[]]);
          var $__4=
           new _A_($UHC.$Base.$_3e_3e,[$__2]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__4,$__3]);});
$Data.$Foldable.$toList=
 new _F_(function($__)
         {return new _A_($Data.$Foldable.$foldr,[$__,$UHC.$Base.$_3a,$UHC.$Base.$_5b_5d]);});
$Data.$Foldable.$traverse__=
 new _F_(function($__,$__2,$f)
         {var $__4=
           new _A_($Control.$Applicative.$pure,[$__2,[]]);
          var $__5=
           new _A_($Control.$Applicative.$_2a_3e,[$__2]);
          var $__6=
           new _A_($UHC.$Base.$_2e,[$__5,$f]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__6,$__4]);});
$Data.$Foldable.$for__=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Foldable.$traverse__,[$__,$__2]);
          return new _A_($UHC.$Base.$flip,[$__3]);});
$Data.$Foldable.$f_27UNQ209=
 new _F_(function($__,$f,$x,$k,$z)
         {var $__6=
           new _A_($f,[$z,$x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__6,$k]);});
$Data.$Foldable.$foldlM=
 new _F_(function($__,$__2,$f,$z0,$xs)
         {var $__6=
           new _A_($UHC.$Base.$return,[$__2]);
          var $__7=
           new _A_($Data.$Foldable.$f_27UNQ209,[$__2,$f]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__7,$__6,$xs,$z0]);});
$Data.$Foldable.$foldl1=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$Data.$Foldable.$f_27UNQ197=
 new _F_(function($f,$x,$k,$z)
         {var $__=
           new _A_($f,[$z,$x]);
          return new _A_($UHC.$Base.$_24_21,[$k,$__]);});
$Data.$Foldable.$foldl_27=
 new _F_(function($__,$f,$z0,$xs)
         {var $__5=
           new _A_($Data.$Foldable.$f_27UNQ197,[$f]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__5,$UHC.$Base.$id,$xs,$z0]);});
$Data.$Foldable.$foldl=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$Data.$Foldable.$f_27UNQ218=
 new _F_(function($f,$k,$x,$z)
         {var $__=
           new _A_($f,[$x,$z]);
          return new _A_($UHC.$Base.$_24_21,[$k,$__]);});
$Data.$Foldable.$foldr_27=
 new _F_(function($__,$f,$z0,$xs)
         {var $__5=
           new _A_($Data.$Foldable.$f_27UNQ218,[$f]);
          return new _A_($Data.$Foldable.$foldl,[$__,$__5,$UHC.$Base.$id,$xs,$z0]);});
$Data.$Foldable.$f_27UNQ230=
 new _F_(function($__,$f,$k,$x,$z)
         {var $__6=
           new _A_($f,[$x,$z]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__6,$k]);});
$Data.$Foldable.$foldrM=
 new _F_(function($__,$__2,$f,$z0,$xs)
         {var $__6=
           new _A_($UHC.$Base.$return,[$__2]);
          var $__7=
           new _A_($Data.$Foldable.$f_27UNQ230,[$__2,$f]);
          return new _A_($Data.$Foldable.$foldl,[$__,$__7,$__6,$xs,$z0]);});
$Data.$Foldable.$foldMap=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$Data.$Foldable.$product=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Monoid.$Monoid__DCT108__53__0,[$__2]);
          var $__4=
           new _A_($Data.$Foldable.$foldMap,[$__,$__3,$Data.$Monoid.$Product__]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getProduct,$__4]);});
$Data.$Foldable.$sum=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Data.$Monoid.$Monoid__DCT108__46__0,[$__2]);
          var $__4=
           new _A_($Data.$Foldable.$foldMap,[$__,$__3,$Data.$Monoid.$Sum__]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getSum,$__4]);});
$Data.$Foldable.$fold=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Data.$Foldable.$asum=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Control.$Applicative.$empty,[$__2]);
          var $__4=
           new _A_($Control.$Applicative.$_3c_7c_3e,[$__2]);
          return new _A_($Data.$Foldable.$foldr,[$__,$__4,$__3]);});
$Data.$Foldable.$or=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Foldable.$foldMap,[$__,$Data.$Monoid.$Monoid__DCT108__39__0,$Data.$Monoid.$Any__]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getAny,$__2]);});
$Data.$Foldable.$any=
 new _F_(function($__,$p)
         {var $__3=
           new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$Any__,$p]);
          var $__4=
           new _A_($Data.$Foldable.$foldMap,[$__,$Data.$Monoid.$Monoid__DCT108__39__0,$__3]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getAny,$__4]);});
$Data.$Foldable.$elem=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$__2]);
          var $__4=
           new _A_($Data.$Foldable.$any,[$__]);
          return new _A_($UHC.$Base.$_2e,[$__4,$__3]);});
$Data.$Foldable.$notElem=
 new _F_(function($__,$__2,$x)
         {var $__4=
           new _A_($Data.$Foldable.$elem,[$__,$__2,$x]);
          return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$not,$__4]);});
$Data.$Foldable.$all=
 new _F_(function($__,$p)
         {var $__3=
           new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$All__,$p]);
          var $__4=
           new _A_($Data.$Foldable.$foldMap,[$__,$Data.$Monoid.$Monoid__DCT108__32__0,$__3]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getAll,$__4]);});
$Data.$Foldable.$and=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Foldable.$foldMap,[$__,$Data.$Monoid.$Monoid__DCT108__32__0,$Data.$Monoid.$All__]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$getAll,$__2]);});
$Data.$Foldable.$concatMap=
 new _F_(function($__)
         {return new _A_($Data.$Foldable.$foldMap,[$__,$Data.$Monoid.$Monoid__DCT108__2__0]);});
$Data.$Foldable.$__124__381__0=
 new _F_(function($p,$x)
         {var $__=
           new _A_($p,[$x]);
          var $__4=
           _e_($__);
          var $__swJSW8__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW8__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$x,$UHC.$Base.$_5b_5d]);
             $__swJSW8__0=
              $__5;
             break;}
          return $__swJSW8__0;});
$Data.$Foldable.$find=
 new _F_(function($__,$p)
         {var $__3=
           new _A_($Data.$Foldable.$__124__381__0,[$p]);
          var $__4=
           new _A_($Data.$Foldable.$concatMap,[$__,$__3]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$listToMaybe,$__4]);});
$Data.$Foldable.$concat=
 new _F_(function($__)
         {return new _A_($Data.$Foldable.$fold,[$__,$Data.$Monoid.$Monoid__DCT108__2__0]);});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldr1=
 new _F_(function($Foldable__,$f,$xs)
         {var $__=
           new _A_($Data.$Monoid.$Monoid__DCT108__23__0,[$Data.$Monoid.$Monoid__DCT108__25__0]);
          var $__5=
           new _A_($Data.$Foldable.$mfUNQ156,[$f]);
          var $__6=
           new _A_($Data.$Foldable.$foldr,[$Foldable__,$__5,$UHC.$Base.$Nothing__,$xs]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,["foldr1: empty structure"]);
          var $__8=
           new _A_($UHC.$Base.$error,[$__7]);
          return new _A_($Data.$Maybe.$fromMaybe,[$__8,$__6]);});
$Data.$Foldable.$mfUNQ156=
 new _F_(function($f,$x1,$x2)
         {var $x24=
           _e_($x2);
          var $__swJSW9__0;
          switch($x24._tag_)
           {case 0:
             var $__=
              new _A_($f,[$x1,$x24._1]);
             var $__7=
              new _A_($UHC.$Base.$Just__,[$__]);
             $__swJSW9__0=
              $__7;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$Just__,[$x1]);
             $__swJSW9__0=
              $__;
             break;}
          return $__swJSW9__0;});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldr=
 new _F_(function($Foldable__,$f,$z,$t)
         {var $__=
           new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$Endo__,$f]);
          var $__6=
           new _A_($Data.$Foldable.$foldMap,[$Foldable__,$Data.$Monoid.$Monoid__DCT108__25__0,$__,$t]);
          return new _A_($Data.$Monoid.$appEndo,[$__6,$z]);});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldl1=
 new _F_(function($Foldable__,$f,$xs)
         {var $__=
           new _A_($Data.$Monoid.$Monoid__DCT108__23__0,[$Data.$Monoid.$Monoid__DCT108__25__0]);
          var $__5=
           new _A_($Data.$Foldable.$mfUNQ131,[$f]);
          var $__6=
           new _A_($Data.$Foldable.$foldl,[$Foldable__,$__5,$UHC.$Base.$Nothing__,$xs]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,["foldl1: empty structure"]);
          var $__8=
           new _A_($UHC.$Base.$error,[$__7]);
          return new _A_($Data.$Maybe.$fromMaybe,[$__8,$__6]);});
$Data.$Foldable.$mfUNQ131=
 new _F_(function($f,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW10__0;
          switch($x14._tag_)
           {case 0:
             var $__=
              new _A_($f,[$x14._1,$x2]);
             var $__7=
              new _A_($UHC.$Base.$Just__,[$__]);
             $__swJSW10__0=
              $__7;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$Just__,[$x2]);
             $__swJSW10__0=
              $__;
             break;}
          return $__swJSW10__0;});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldl=
 new _F_(function($Foldable__,$__,$f,$z,$t)
         {var $__6=
           new _A_($UHC.$Base.$flip,[$f]);
          var $__7=
           new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$Endo__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_2e,[$Data.$Monoid.$Dual__,$__7]);
          var $__9=
           new _A_($Data.$Foldable.$foldMap,[$Foldable__,$__,$__8,$t]);
          var $__10=
           new _A_($Data.$Monoid.$getDual,[$__9]);
          return new _A_($Data.$Monoid.$appEndo,[$__10,$z]);});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldMap=
 new _F_(function($Foldable__,$__,$f)
         {var $__4=
           new _A_($Data.$Monoid.$mempty,[$__]);
          var $__5=
           new _A_($Data.$Monoid.$mappend,[$__]);
          var $__6=
           new _A_($UHC.$Base.$_2e,[$__5,$f]);
          return new _A_($Data.$Foldable.$foldr,[$Foldable__,$__6,$__4]);});
$Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efold=
 new _F_(function($Foldable__,$__)
         {return new _A_($Data.$Foldable.$foldMap,[$Foldable__,$__,$UHC.$Base.$id]);});
$Data.$Foldable.$Foldable__CLS120__0__0=
 new _F_(function($Foldable__)
         {var $__=
           new _A_($Data.$Monoid.$Monoid__DCT108__23__0,[$Data.$Monoid.$Monoid__DCT108__25__0]);
          var $__3=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldr1,[$Foldable__]);
          var $__4=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldr,[$Foldable__]);
          var $__5=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldl1,[$Foldable__]);
          var $__6=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldl,[$Foldable__,$__]);
          var $__7=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efoldMap,[$Foldable__]);
          var $__8=
           new _A_($Data.$Foldable.$Foldable__CLS120__0__0DFLData_2eFoldable_2efold,[$Foldable__]);
          var $Foldable__9=
           {_tag_:0,_1:$__8,_2:$__7,_3:$__6,_4:$__5,_5:$__4,_6:$__3};
          return $Foldable__9;});
$Data.$Foldable.$Foldable__DCT120__2__0DFLData_2eFoldable_2efoldr=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW11__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x34._1,$x2]);
             $__swJSW11__0=
              $__;
             break;
            case 1:
             $__swJSW11__0=
              $x2;
             break;}
          return $__swJSW11__0;});
$Data.$Foldable.$Foldable__DCT120__2__0DFLData_2eFoldable_2efoldl=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW12__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x2,$x34._1]);
             $__swJSW12__0=
              $__;
             break;
            case 1:
             $__swJSW12__0=
              $x2;
             break;}
          return $__swJSW12__0;});
$Data.$Foldable.$Foldable__NEW136UNQ310DCT120__2__0RDC=
 new _F_(function($Foldable__)
         {var $Foldable__2=
           new _A_($Data.$Foldable.$Foldable__NEW138UNQ311EVLDCT120__2__0RDC,[$Foldable__]);
          return $Foldable__2;});
$Data.$Foldable.$Foldable__NEW138UNQ311EVLDCT120__2__0RDC=
 new _F_(function($Foldable__)
         {var $Foldable__2=
           _e_(new _A_($Data.$Foldable.$Foldable__CLS120__0__0,[$Foldable__]));
          var $__9=
           {_tag_:0,_1:$Foldable__2._1,_2:$Foldable__2._2,_3:$Data.$Foldable.$Foldable__DCT120__2__0DFLData_2eFoldable_2efoldl,_4:$Foldable__2._4,_5:$Data.$Foldable.$Foldable__DCT120__2__0DFLData_2eFoldable_2efoldr,_6:$Foldable__2._6};
          return $__9;});
$Data.$Foldable.$Foldable__UNQ310DCT120__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Foldable.$Foldable__NEW136UNQ310DCT120__2__0RDC,[$Data.$Foldable.$Foldable__UNQ310DCT120__2__0RDC]);}),[]);
$Data.$Foldable.$Foldable__DCT120__2__0=
 new _A_(new _F_(function()
                 {return $Data.$Foldable.$Foldable__UNQ310DCT120__2__0RDC;}),[]);
$Data.$Foldable.$Foldable__NEW143UNQ367DCT120__3__0RDC=
 new _F_(function($Foldable__)
         {var $Foldable__2=
           new _A_($Data.$Foldable.$Foldable__NEW145UNQ368EVLDCT120__3__0RDC,[$Foldable__]);
          return $Foldable__2;});
$Data.$Foldable.$Foldable__NEW145UNQ368EVLDCT120__3__0RDC=
 new _F_(function($Foldable__)
         {var $Foldable__2=
           _e_(new _A_($Data.$Foldable.$Foldable__CLS120__0__0,[$Foldable__]));
          var $__9=
           {_tag_:0,_1:$Foldable__2._1,_2:$Foldable__2._2,_3:$UHC.$Base.$foldl,_4:$UHC.$Base.$foldl1,_5:$UHC.$Base.$foldr,_6:$UHC.$Base.$foldr1};
          return $__9;});
$Data.$Foldable.$Foldable__UNQ367DCT120__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Foldable.$Foldable__NEW143UNQ367DCT120__3__0RDC,[$Data.$Foldable.$Foldable__UNQ367DCT120__3__0RDC]);}),[]);
$Data.$Foldable.$Foldable__DCT120__3__0=
 new _A_(new _F_(function()
                 {return $Data.$Foldable.$Foldable__UNQ367DCT120__3__0RDC;}),[]);
$Data.$Foldable.$Foldable__DCT120__8__0DFLData_2eFoldable_2efoldr=
 new _F_(function($__,$f,$z)
         {var $__4=
           new _A_($UHC.$Array.$elems,[$__]);
          var $__5=
           new _A_($UHC.$Base.$foldr,[$f,$z]);
          return new _A_($UHC.$Base.$_2e,[$__5,$__4]);});
$Data.$Foldable.$Foldable__NEW153UNQ377DCT120__8__0RDC=
 new _F_(function($Foldable__,$__)
         {var $Foldable__3=
           new _A_($Data.$Foldable.$Foldable__NEW156UNQ379EVLDCT120__8__0RDC,[$Foldable__,$__]);
          return $Foldable__3;});
$Data.$Foldable.$Foldable__NEW156UNQ379EVLDCT120__8__0RDC=
 new _F_(function($Foldable__,$__)
         {var $Foldable__3=
           _e_(new _A_($Data.$Foldable.$Foldable__CLS120__0__0,[$Foldable__]));
          var $__10=
           new _A_($Data.$Foldable.$Foldable__DCT120__8__0DFLData_2eFoldable_2efoldr,[$__]);
          var $__11=
           {_tag_:0,_1:$Foldable__3._1,_2:$Foldable__3._2,_3:$Foldable__3._3,_4:$Foldable__3._4,_5:$__10,_6:$Foldable__3._6};
          return $__11;});
$Data.$Foldable.$Foldable__DCT120__8__0=
 new _F_(function($__)
         {var $Foldable__=
           _i_();
          _i_set_($Foldable__,new _A_($Data.$Foldable.$Foldable__NEW153UNQ377DCT120__8__0RDC,[$Foldable__,$__]));
          return $Foldable__;});
$Data.$Foldable.$_24Dict_2dFoldable=
 new _F_(function($x1,$x2,$x3,$x4,$x5,$x6)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3,_4:$x4,_5:$x5,_6:$x6};});
