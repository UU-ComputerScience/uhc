// Text.Read.Lex
var $Text=
 ($Text ? $Text : {});
$Text.$Read=
 ($Text.$Read ? $Text.$Read : {});
$Text.$Read.$Lex=
 ($Text.$Read.$Lex ? $Text.$Read.$Lex : {});
$Text.$Read.$Lex.$val=
 new _F_(function($__,$x1,$x2,$x3)
         {var $x35=
           _e_($x3);
          var $__swJSW0__0;
          switch($x35._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$__,$x35._1]);
             var $__9=
              new _A_($UHC.$Base.$_2a,[$__,$x2,$x1]);
             var $y_27=
              new _A_($UHC.$Base.$_2b,[$__,$__9,$__8]);
             var $__11=
              new _A_($Text.$Read.$Lex.$val,[$__,$x1,$y_27,$x35._2]);
             $__swJSW0__0=
              new _A_($UHC.$Base.$seq,[$y_27,$__11]);
             break;
            case 1:
             $__swJSW0__0=
              $x2;
             break;}
          return $__swJSW0__0;});
$Text.$Read.$Lex.$__184__31=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$primIntToInteger,[0]);}),[]);
$Text.$Read.$Lex.$__184__30=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$primIntToInteger,[0]);}),[]);
$Text.$Read.$Lex.$notANumber=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a_25,[$Text.$Read.$Lex.$__184__30,$Text.$Read.$Lex.$__184__31]);}),[]);
$Text.$Read.$Lex.$__184__36=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$primIntToInteger,[0]);}),[]);
$Text.$Read.$Lex.$__184__35=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$primIntToInteger,[1]);}),[]);
$Text.$Read.$Lex.$infinity=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a_25,[$Text.$Read.$Lex.$__184__35,$Text.$Read.$Lex.$__184__36]);}),[]);
$Text.$Read.$Lex.$__182__863__2__22NEW13UNQ349=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Text.$Read.$Lex.$__182__1087__2__0NEW16UNQ352=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$Text.$Read.$Lex.$__184__46__0=
 new _F_(function($__,$__2,$x1,$x2,$x3,$x4)
         {var $x47=
           _e_($x4);
          var $__swJSW3__0;
          switch($x47._tag_)
           {case 0:
             var $b_27=
              new _A_($UHC.$Base.$_2a,[$__,$x3,$x1]);
             var $__11=
              new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$__,$x47._1]);
             var $__12=
              new _A_($UHC.$Base.$_2a,[$__,$x2,$x1]);
             var $a_27=
              new _A_($UHC.$Base.$_2b,[$__,$__12,$__11]);
             var $__14=
              new _A_($Text.$Read.$Lex.$frac,[$__2,$x1,$a_27,$b_27,$x47._2]);
             var $__15=
              new _A_($UHC.$Base.$seq,[$b_27,$__14]);
             $__swJSW3__0=
              new _A_($UHC.$Base.$seq,[$a_27,$__15]);
             break;
            case 1:
             var $__16=
              new _A_($UHC.$Base.$_25,[$__2,$x2,$x3]);
             $__swJSW3__0=
              $__16;
             break;}
          return $__swJSW3__0;});
$Text.$Read.$Lex.$frac=
 new _F_(function($__)
         {var $__2=
           new _A_($Text.$Read.$Lex.$__182__863__2__22NEW13UNQ349,[$__]);
          var $__3=
           new _A_($Text.$Read.$Lex.$__182__1087__2__0NEW16UNQ352,[$__2]);
          return new _A_($Text.$Read.$Lex.$__184__46__0,[$__3,$__]);});
$Text.$Read.$Lex.$_24okUNQ681=
 new _F_(function($_24x)
         {var $__=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,79,$_24x]));
          var $__swJSW4__0;
          switch($__._tag_)
           {case 0:
             var $__3=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,88,$_24x]));
             var $__swJSW5__0;
             switch($__3._tag_)
              {case 0:
                var $__4=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,111,$_24x]));
                var $__swJSW6__0;
                switch($__4._tag_)
                 {case 0:
                   var $__5=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,120,$_24x]));
                   var $__swJSW7__0;
                   switch($__5._tag_)
                    {case 0:
                      $__swJSW7__0=
                       $Text.$ParserCombinators.$ReadP.$pfail;
                      break;
                     case 1:
                      var $__6=
                       new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,16]);
                      $__swJSW7__0=
                       $__6;
                      break;}
                   $__swJSW6__0=
                    $__swJSW7__0;
                   break;
                  case 1:
                   var $__7=
                    new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,8]);
                   $__swJSW6__0=
                    $__7;
                   break;}
                $__swJSW5__0=
                 $__swJSW6__0;
                break;
               case 1:
                var $__8=
                 new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,16]);
                $__swJSW5__0=
                 $__8;
                break;}
             $__swJSW4__0=
              $__swJSW5__0;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,8]);
             $__swJSW4__0=
              $__9;
             break;}
          return $__swJSW4__0;});
$Text.$Read.$Lex.$lexBaseChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$Text.$Read.$Lex.$_24okUNQ681]);}),[]);
$Text.$Read.$Lex.$_24okUNQ491=
 new _F_(function($__,$base,$valDigit,$_24x)
         {var $__5=
           new _A_($UHC.$Base.$map,[$valDigit,$_24x]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__6]);
          var $__8=
           new _A_($Text.$Read.$Lex.$val,[$__,$base,$__7,$__5]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__8]);});
$Text.$Read.$Lex.$readIntP=
 new _F_(function($__,$base,$isDigit,$valDigit)
         {var $__5=
           new _A_($Text.$ParserCombinators.$ReadP.$munch1,[$isDigit]);
          var $__6=
           new _A_($Text.$Read.$Lex.$_24okUNQ491,[$__,$base,$valDigit]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5,$__6]);});
$Text.$Read.$Lex.$valDecDig=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,57]);
          var $__3=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,48,$c]);
          var $__4=
           new _A_($UHC.$Base.$_26_26,[$__3,$__]);
          var $__5=
           _e_($__4);
          var $__swJSW8__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW9__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 181_67_0"]);
                var $__8=
                 new _A_($UHC.$Base.$error,[$__7]);
                $__swJSW9__0=
                 $__8;
                break;
               case 1:
                $__swJSW9__0=
                 $UHC.$Base.$Nothing__;
                break;}
             $__swJSW8__0=
              $__swJSW9__0;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$ord,[48]);
             var $__10=
              new _A_($UHC.$Base.$ord,[$c]);
             var $__11=
              new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__10,$__9]);
             var $__12=
              new _A_($UHC.$Base.$Just__,[$__11]);
             $__swJSW8__0=
              $__12;
             break;}
          return $__swJSW8__0;});
$Text.$Read.$Lex.$__182__241__1__0NEW58UNQ134=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Text.$Read.$Lex.$__184__174__0=
 new _F_(function($__,$__2,$x1,$x2)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToString,["valDig: Bad base"]);
          var $__6=
           new _A_($UHC.$Base.$error,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["8"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $x19=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__8,$x1]));
          var $__swJSW11__0;
          switch($x19._tag_)
           {case 0:
             var $__10=
              new _A_($UHC.$Base.$packedStringToInteger,["10"]);
             var $__11=
              new _A_($UHC.$Base.$fromInteger,[$__,$__10]);
             var $x112=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__11,$x1]));
             var $__swJSW12__0;
             switch($x112._tag_)
              {case 0:
                var $__13=
                 new _A_($UHC.$Base.$packedStringToInteger,["16"]);
                var $__14=
                 new _A_($UHC.$Base.$fromInteger,[$__,$__13]);
                var $x115=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__14,$x1]));
                var $__swJSW13__0;
                switch($x115._tag_)
                 {case 0:
                   $__swJSW13__0=
                    $__6;
                   break;
                  case 1:
                   var $__16=
                    new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$x2,57]);
                   var $__17=
                    new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,48,$x2]);
                   var $__18=
                    new _A_($UHC.$Base.$_26_26,[$__17,$__16]);
                   var $__19=
                    _e_($__18);
                   var $__swJSW14__0;
                   switch($__19._tag_)
                    {case 0:
                      var $__20=
                       new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$x2,102]);
                      var $__21=
                       new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,97,$x2]);
                      var $__22=
                       new _A_($UHC.$Base.$_26_26,[$__21,$__20]);
                      var $__23=
                       _e_($__22);
                      var $__swJSW15__0;
                      switch($__23._tag_)
                       {case 0:
                         var $__24=
                          new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$x2,70]);
                         var $__25=
                          new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,65,$x2]);
                         var $__26=
                          new _A_($UHC.$Base.$_26_26,[$__25,$__24]);
                         var $__27=
                          _e_($__26);
                         var $__swJSW16__0;
                         switch($__27._tag_)
                          {case 0:
                            var $__28=
                             _e_($UHC.$Base.$otherwise);
                            var $__swJSW17__0;
                            switch($__28._tag_)
                             {case 0:
                               $__swJSW17__0=
                                $__6;
                               break;
                              case 1:
                               $__swJSW17__0=
                                $UHC.$Base.$Nothing__;
                               break;}
                            $__swJSW16__0=
                             $__swJSW17__0;
                            break;
                           case 1:
                            var $__29=
                             new _A_($UHC.$Base.$ord,[65]);
                            var $__30=
                             new _A_($UHC.$Base.$ord,[$x2]);
                            var $__31=
                             new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__30,$__29]);
                            var $__32=
                             new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__31,10]);
                            var $__33=
                             new _A_($UHC.$Base.$Just__,[$__32]);
                            $__swJSW16__0=
                             $__33;
                            break;}
                         $__swJSW15__0=
                          $__swJSW16__0;
                         break;
                        case 1:
                         var $__34=
                          new _A_($UHC.$Base.$ord,[97]);
                         var $__35=
                          new _A_($UHC.$Base.$ord,[$x2]);
                         var $__36=
                          new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__35,$__34]);
                         var $__37=
                          new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__36,10]);
                         var $__38=
                          new _A_($UHC.$Base.$Just__,[$__37]);
                         $__swJSW15__0=
                          $__38;
                         break;}
                      $__swJSW14__0=
                       $__swJSW15__0;
                      break;
                     case 1:
                      var $__39=
                       new _A_($UHC.$Base.$ord,[48]);
                      var $__40=
                       new _A_($UHC.$Base.$ord,[$x2]);
                      var $__41=
                       new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__40,$__39]);
                      var $__42=
                       new _A_($UHC.$Base.$Just__,[$__41]);
                      $__swJSW14__0=
                       $__42;
                      break;}
                   $__swJSW13__0=
                    $__swJSW14__0;
                   break;}
                $__swJSW12__0=
                 $__swJSW13__0;
                break;
               case 1:
                var $__43=
                 new _A_($Text.$Read.$Lex.$valDecDig,[$x2]);
                $__swJSW12__0=
                 $__43;
                break;}
             $__swJSW11__0=
              $__swJSW12__0;
             break;
            case 1:
             var $__44=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$x2,55]);
             var $__45=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,48,$x2]);
             var $__46=
              new _A_($UHC.$Base.$_26_26,[$__45,$__44]);
             var $__47=
              _e_($__46);
             var $__swJSW18__0;
             switch($__47._tag_)
              {case 0:
                var $__48=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW19__0;
                switch($__48._tag_)
                 {case 0:
                   $__swJSW19__0=
                    $__6;
                   break;
                  case 1:
                   $__swJSW19__0=
                    $UHC.$Base.$Nothing__;
                   break;}
                $__swJSW18__0=
                 $__swJSW19__0;
                break;
               case 1:
                var $__49=
                 new _A_($UHC.$Base.$ord,[48]);
                var $__50=
                 new _A_($UHC.$Base.$ord,[$x2]);
                var $__51=
                 new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__50,$__49]);
                var $__52=
                 new _A_($UHC.$Base.$Just__,[$__51]);
                $__swJSW18__0=
                 $__52;
                break;}
             $__swJSW11__0=
              $__swJSW18__0;
             break;}
          return $__swJSW11__0;});
$Text.$Read.$Lex.$valDig=
 new _F_(function($__)
         {var $__2=
           new _A_($Text.$Read.$Lex.$__182__241__1__0NEW58UNQ134,[$__]);
          return new _A_($Text.$Read.$Lex.$__184__174__0,[$__,$__2]);});
$Text.$Read.$Lex.$scanUNQ415=
 new _F_(function($base,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW20__0;
          switch($x14._tag_)
           {case 0:
             var $__=
              new _A_($Text.$Read.$Lex.$valDig,[$UHC.$Base.$Num__DCT74__101__0,$base,$x14._1]);
             var $__8=
              _e_($__);
             var $__swJSW21__0;
             switch($__8._tag_)
              {case 0:
                var $__10=
                 new _A_($UHC.$Base.$_3a,[$__8._1]);
                var $__11=
                 new _A_($UHC.$Base.$_2e,[$x2,$__10]);
                var $__12=
                 new _A_($Text.$Read.$Lex.$scanUNQ415,[$base,$x14._2,$__11]);
                var $__13=
                 new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$__12]);
                $__swJSW21__0=
                 $__13;
                break;
               case 1:
                var $__14=
                 new _A_($x2,[$UHC.$Base.$_5b_5d]);
                var $__15=
                 new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__14]);
                $__swJSW21__0=
                 $__15;
                break;}
             $__swJSW20__0=
              $__swJSW21__0;
             break;
            case 1:
             var $__=
              new _A_($x2,[$UHC.$Base.$_5b_5d]);
             var $__17=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
             $__swJSW20__0=
              $__17;
             break;}
          return $__swJSW20__0;});
$Text.$Read.$Lex.$_24okUNQ422=
 new _F_(function($base,$_24x)
         {var $__=
           new _A_($Text.$Read.$Lex.$scanUNQ415,[$base,$_24x,$UHC.$Base.$id]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ448]);});
$Text.$Read.$Lex.$_24okUNQ448=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$_24x]);
          var $__3=
           new _A_($UHC.$Base.$null,[$_24x]);
          var $__4=
           new _A_($UHC.$Base.$not,[$__3]);
          var $__5=
           new _A_($Control.$Monad.$guard,[$Text.$ParserCombinators.$ReadP.$MonadPlus__DCT168__6__0,$__4]);
          return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5,$__]);});
$Text.$Read.$Lex.$lexDigits=
 new _F_(function($base)
         {var $__=
           new _A_($Text.$Read.$Lex.$_24okUNQ422,[$base]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$look,$__]);});
$Text.$Read.$Lex.$_24okUNQ566=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$Just__,[$_24x]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);});
$Text.$Read.$Lex.$__184__342=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$Read.$Lex.$lexDigits,[10]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ566]);}),[]);
$Text.$Read.$Lex.$__184__341=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$char,[46]);}),[]);
$Text.$Read.$Lex.$lexFrac=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$__184__341,$Text.$Read.$Lex.$__184__342]);}),[]);
$Text.$Read.$Lex.$_24okUNQ507=
 new _F_(function($base,$_24x)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[0]);
          var $__4=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__134__0,$base]);
          var $__5=
           new _A_($Text.$Read.$Lex.$val,[$UHC.$Base.$Num__DCT74__134__0,$__4,$__,$_24x]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5]);});
$Text.$Read.$Lex.$lexInteger=
 new _F_(function($base)
         {var $__=
           new _A_($Text.$Read.$Lex.$lexDigits,[$base]);
          var $__3=
           new _A_($Text.$Read.$Lex.$_24okUNQ507,[$base]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$__3]);});
$Text.$Read.$Lex.$lexEscCharUNQ783=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$Text.$Read.$Lex.$_24okUNQ933]);}),[]);
$Text.$Read.$Lex.$_24okUNQ933=
 new _F_(function($_24x)
         {var $__=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,34,$_24x]));
          var $__swJSW22__0;
          switch($__._tag_)
           {case 0:
             var $__3=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,39,$_24x]));
             var $__swJSW23__0;
             switch($__3._tag_)
              {case 0:
                var $__4=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,92,$_24x]));
                var $__swJSW24__0;
                switch($__4._tag_)
                 {case 0:
                   var $__5=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,97,$_24x]));
                   var $__swJSW25__0;
                   switch($__5._tag_)
                    {case 0:
                      var $__6=
                       _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,98,$_24x]));
                      var $__swJSW26__0;
                      switch($__6._tag_)
                       {case 0:
                         var $__7=
                          _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,102,$_24x]));
                         var $__swJSW27__0;
                         switch($__7._tag_)
                          {case 0:
                            var $__8=
                             _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,110,$_24x]));
                            var $__swJSW28__0;
                            switch($__8._tag_)
                             {case 0:
                               var $__9=
                                _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,114,$_24x]));
                               var $__swJSW29__0;
                               switch($__9._tag_)
                                {case 0:
                                  var $__10=
                                   _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,116,$_24x]));
                                  var $__swJSW30__0;
                                  switch($__10._tag_)
                                   {case 0:
                                     var $__11=
                                      _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,118,$_24x]));
                                     var $__swJSW31__0;
                                     switch($__11._tag_)
                                      {case 0:
                                        $__swJSW31__0=
                                         $Text.$ParserCombinators.$ReadP.$pfail;
                                        break;
                                       case 1:
                                        var $__12=
                                         new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,11]);
                                        $__swJSW31__0=
                                         $__12;
                                        break;}
                                     $__swJSW30__0=
                                      $__swJSW31__0;
                                     break;
                                    case 1:
                                     var $__13=
                                      new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,9]);
                                     $__swJSW30__0=
                                      $__13;
                                     break;}
                                  $__swJSW29__0=
                                   $__swJSW30__0;
                                  break;
                                 case 1:
                                  var $__14=
                                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,13]);
                                  $__swJSW29__0=
                                   $__14;
                                  break;}
                               $__swJSW28__0=
                                $__swJSW29__0;
                               break;
                              case 1:
                               var $__15=
                                new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,10]);
                               $__swJSW28__0=
                                $__15;
                               break;}
                            $__swJSW27__0=
                             $__swJSW28__0;
                            break;
                           case 1:
                            var $__16=
                             new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,12]);
                            $__swJSW27__0=
                             $__16;
                            break;}
                         $__swJSW26__0=
                          $__swJSW27__0;
                         break;
                        case 1:
                         var $__17=
                          new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,8]);
                         $__swJSW26__0=
                          $__17;
                         break;}
                      $__swJSW25__0=
                       $__swJSW26__0;
                      break;
                     case 1:
                      var $__18=
                       new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,7]);
                      $__swJSW25__0=
                       $__18;
                      break;}
                   $__swJSW24__0=
                    $__swJSW25__0;
                   break;
                  case 1:
                   var $__19=
                    new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,92]);
                   $__swJSW24__0=
                    $__19;
                   break;}
                $__swJSW23__0=
                 $__swJSW24__0;
                break;
               case 1:
                var $__20=
                 new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,39]);
                $__swJSW23__0=
                 $__20;
                break;}
             $__swJSW22__0=
              $__swJSW23__0;
             break;
            case 1:
             var $__21=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,34]);
             $__swJSW22__0=
              $__21;
             break;}
          return $__swJSW22__0;});
$Text.$Read.$Lex.$lexNumericUNQ781=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,10]);
                  var $__2=
                   new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$Text.$Read.$Lex.$lexBaseChar,$__]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__2,$Text.$Read.$Lex.$_24okUNQ900]);}),[]);
$Text.$Read.$Lex.$_24okUNQ900=
 new _F_(function($_24x)
         {var $__=
           new _A_($Text.$Read.$Lex.$lexInteger,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ908]);});
$Text.$Read.$Lex.$_24okUNQ908=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__101__0,$_24x]);
          var $__3=
           new _A_($UHC.$Base.$chr,[$__]);
          var $__4=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__3]);
          var $__5=
           new _A_($UHC.$Base.$maxBound,[$UHC.$Base.$Bounded__DCT74__66__0]);
          var $__6=
           new _A_($UHC.$Base.$ord,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__132__0,$_24x,$__7]);
          var $__9=
           new _A_($Control.$Monad.$guard,[$Text.$ParserCombinators.$ReadP.$MonadPlus__DCT168__6__0,$__8]);
          return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__9,$__4]);});
$Text.$Read.$Lex.$__184__490=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$Text.$Read.$Lex.$_24okUNQ857]);}),[]);
$Text.$Read.$Lex.$_24okUNQ857=
 new _F_(function($_24x)
         {var $__=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,64,$_24x]));
          var $__swJSW32__0;
          switch($__._tag_)
           {case 0:
             var $__3=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,65,$_24x]));
             var $__swJSW33__0;
             switch($__3._tag_)
              {case 0:
                var $__4=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,66,$_24x]));
                var $__swJSW34__0;
                switch($__4._tag_)
                 {case 0:
                   var $__5=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,67,$_24x]));
                   var $__swJSW35__0;
                   switch($__5._tag_)
                    {case 0:
                      var $__6=
                       _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,68,$_24x]));
                      var $__swJSW36__0;
                      switch($__6._tag_)
                       {case 0:
                         var $__7=
                          _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,69,$_24x]));
                         var $__swJSW37__0;
                         switch($__7._tag_)
                          {case 0:
                            var $__8=
                             _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,70,$_24x]));
                            var $__swJSW38__0;
                            switch($__8._tag_)
                             {case 0:
                               var $__9=
                                _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,71,$_24x]));
                               var $__swJSW39__0;
                               switch($__9._tag_)
                                {case 0:
                                  var $__10=
                                   _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,72,$_24x]));
                                  var $__swJSW40__0;
                                  switch($__10._tag_)
                                   {case 0:
                                     var $__11=
                                      _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,73,$_24x]));
                                     var $__swJSW41__0;
                                     switch($__11._tag_)
                                      {case 0:
                                        var $__12=
                                         _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,74,$_24x]));
                                        var $__swJSW42__0;
                                        switch($__12._tag_)
                                         {case 0:
                                           var $__13=
                                            _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,75,$_24x]));
                                           var $__swJSW43__0;
                                           switch($__13._tag_)
                                            {case 0:
                                              var $__14=
                                               _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,76,$_24x]));
                                              var $__swJSW44__0;
                                              switch($__14._tag_)
                                               {case 0:
                                                 var $__15=
                                                  _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,77,$_24x]));
                                                 var $__swJSW45__0;
                                                 switch($__15._tag_)
                                                  {case 0:
                                                    var $__16=
                                                     _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,78,$_24x]));
                                                    var $__swJSW46__0;
                                                    switch($__16._tag_)
                                                     {case 0:
                                                       var $__17=
                                                        _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,79,$_24x]));
                                                       var $__swJSW47__0;
                                                       switch($__17._tag_)
                                                        {case 0:
                                                          var $__18=
                                                           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,80,$_24x]));
                                                          var $__swJSW48__0;
                                                          switch($__18._tag_)
                                                           {case 0:
                                                             var $__19=
                                                              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,81,$_24x]));
                                                             var $__swJSW49__0;
                                                             switch($__19._tag_)
                                                              {case 0:
                                                                var $__20=
                                                                 _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,82,$_24x]));
                                                                var $__swJSW50__0;
                                                                switch($__20._tag_)
                                                                 {case 0:
                                                                   var $__21=
                                                                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,83,$_24x]));
                                                                   var $__swJSW51__0;
                                                                   switch($__21._tag_)
                                                                    {case 0:
                                                                      var $__22=
                                                                       _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,84,$_24x]));
                                                                      var $__swJSW52__0;
                                                                      switch($__22._tag_)
                                                                       {case 0:
                                                                         var $__23=
                                                                          _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,85,$_24x]));
                                                                         var $__swJSW53__0;
                                                                         switch($__23._tag_)
                                                                          {case 0:
                                                                            var $__24=
                                                                             _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,86,$_24x]));
                                                                            var $__swJSW54__0;
                                                                            switch($__24._tag_)
                                                                             {case 0:
                                                                               var $__25=
                                                                                _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,87,$_24x]));
                                                                               var $__swJSW55__0;
                                                                               switch($__25._tag_)
                                                                                {case 0:
                                                                                  var $__26=
                                                                                   _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,88,$_24x]));
                                                                                  var $__swJSW56__0;
                                                                                  switch($__26._tag_)
                                                                                   {case 0:
                                                                                     var $__27=
                                                                                      _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,89,$_24x]));
                                                                                     var $__swJSW57__0;
                                                                                     switch($__27._tag_)
                                                                                      {case 0:
                                                                                        var $__28=
                                                                                         _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,90,$_24x]));
                                                                                        var $__swJSW58__0;
                                                                                        switch($__28._tag_)
                                                                                         {case 0:
                                                                                           var $__29=
                                                                                            _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,91,$_24x]));
                                                                                           var $__swJSW59__0;
                                                                                           switch($__29._tag_)
                                                                                            {case 0:
                                                                                              var $__30=
                                                                                               _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,92,$_24x]));
                                                                                              var $__swJSW60__0;
                                                                                              switch($__30._tag_)
                                                                                               {case 0:
                                                                                                 var $__31=
                                                                                                  _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,93,$_24x]));
                                                                                                 var $__swJSW61__0;
                                                                                                 switch($__31._tag_)
                                                                                                  {case 0:
                                                                                                    var $__32=
                                                                                                     _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,94,$_24x]));
                                                                                                    var $__swJSW62__0;
                                                                                                    switch($__32._tag_)
                                                                                                     {case 0:
                                                                                                       var $__33=
                                                                                                        _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,95,$_24x]));
                                                                                                       var $__swJSW63__0;
                                                                                                       switch($__33._tag_)
                                                                                                        {case 0:
                                                                                                          $__swJSW63__0=
                                                                                                           $Text.$ParserCombinators.$ReadP.$pfail;
                                                                                                          break;
                                                                                                         case 1:
                                                                                                          var $__34=
                                                                                                           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,31]);
                                                                                                          $__swJSW63__0=
                                                                                                           $__34;
                                                                                                          break;}
                                                                                                       $__swJSW62__0=
                                                                                                        $__swJSW63__0;
                                                                                                       break;
                                                                                                      case 1:
                                                                                                       var $__35=
                                                                                                        new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,30]);
                                                                                                       $__swJSW62__0=
                                                                                                        $__35;
                                                                                                       break;}
                                                                                                    $__swJSW61__0=
                                                                                                     $__swJSW62__0;
                                                                                                    break;
                                                                                                   case 1:
                                                                                                    var $__36=
                                                                                                     new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,29]);
                                                                                                    $__swJSW61__0=
                                                                                                     $__36;
                                                                                                    break;}
                                                                                                 $__swJSW60__0=
                                                                                                  $__swJSW61__0;
                                                                                                 break;
                                                                                                case 1:
                                                                                                 var $__37=
                                                                                                  new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,28]);
                                                                                                 $__swJSW60__0=
                                                                                                  $__37;
                                                                                                 break;}
                                                                                              $__swJSW59__0=
                                                                                               $__swJSW60__0;
                                                                                              break;
                                                                                             case 1:
                                                                                              var $__38=
                                                                                               new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,27]);
                                                                                              $__swJSW59__0=
                                                                                               $__38;
                                                                                              break;}
                                                                                           $__swJSW58__0=
                                                                                            $__swJSW59__0;
                                                                                           break;
                                                                                          case 1:
                                                                                           var $__39=
                                                                                            new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,26]);
                                                                                           $__swJSW58__0=
                                                                                            $__39;
                                                                                           break;}
                                                                                        $__swJSW57__0=
                                                                                         $__swJSW58__0;
                                                                                        break;
                                                                                       case 1:
                                                                                        var $__40=
                                                                                         new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,25]);
                                                                                        $__swJSW57__0=
                                                                                         $__40;
                                                                                        break;}
                                                                                     $__swJSW56__0=
                                                                                      $__swJSW57__0;
                                                                                     break;
                                                                                    case 1:
                                                                                     var $__41=
                                                                                      new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,24]);
                                                                                     $__swJSW56__0=
                                                                                      $__41;
                                                                                     break;}
                                                                                  $__swJSW55__0=
                                                                                   $__swJSW56__0;
                                                                                  break;
                                                                                 case 1:
                                                                                  var $__42=
                                                                                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,23]);
                                                                                  $__swJSW55__0=
                                                                                   $__42;
                                                                                  break;}
                                                                               $__swJSW54__0=
                                                                                $__swJSW55__0;
                                                                               break;
                                                                              case 1:
                                                                               var $__43=
                                                                                new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,22]);
                                                                               $__swJSW54__0=
                                                                                $__43;
                                                                               break;}
                                                                            $__swJSW53__0=
                                                                             $__swJSW54__0;
                                                                            break;
                                                                           case 1:
                                                                            var $__44=
                                                                             new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,21]);
                                                                            $__swJSW53__0=
                                                                             $__44;
                                                                            break;}
                                                                         $__swJSW52__0=
                                                                          $__swJSW53__0;
                                                                         break;
                                                                        case 1:
                                                                         var $__45=
                                                                          new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,20]);
                                                                         $__swJSW52__0=
                                                                          $__45;
                                                                         break;}
                                                                      $__swJSW51__0=
                                                                       $__swJSW52__0;
                                                                      break;
                                                                     case 1:
                                                                      var $__46=
                                                                       new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,19]);
                                                                      $__swJSW51__0=
                                                                       $__46;
                                                                      break;}
                                                                   $__swJSW50__0=
                                                                    $__swJSW51__0;
                                                                   break;
                                                                  case 1:
                                                                   var $__47=
                                                                    new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,18]);
                                                                   $__swJSW50__0=
                                                                    $__47;
                                                                   break;}
                                                                $__swJSW49__0=
                                                                 $__swJSW50__0;
                                                                break;
                                                               case 1:
                                                                var $__48=
                                                                 new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,17]);
                                                                $__swJSW49__0=
                                                                 $__48;
                                                                break;}
                                                             $__swJSW48__0=
                                                              $__swJSW49__0;
                                                             break;
                                                            case 1:
                                                             var $__49=
                                                              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,16]);
                                                             $__swJSW48__0=
                                                              $__49;
                                                             break;}
                                                          $__swJSW47__0=
                                                           $__swJSW48__0;
                                                          break;
                                                         case 1:
                                                          var $__50=
                                                           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,15]);
                                                          $__swJSW47__0=
                                                           $__50;
                                                          break;}
                                                       $__swJSW46__0=
                                                        $__swJSW47__0;
                                                       break;
                                                      case 1:
                                                       var $__51=
                                                        new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,14]);
                                                       $__swJSW46__0=
                                                        $__51;
                                                       break;}
                                                    $__swJSW45__0=
                                                     $__swJSW46__0;
                                                    break;
                                                   case 1:
                                                    var $__52=
                                                     new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,13]);
                                                    $__swJSW45__0=
                                                     $__52;
                                                    break;}
                                                 $__swJSW44__0=
                                                  $__swJSW45__0;
                                                 break;
                                                case 1:
                                                 var $__53=
                                                  new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,12]);
                                                 $__swJSW44__0=
                                                  $__53;
                                                 break;}
                                              $__swJSW43__0=
                                               $__swJSW44__0;
                                              break;
                                             case 1:
                                              var $__54=
                                               new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,11]);
                                              $__swJSW43__0=
                                               $__54;
                                              break;}
                                           $__swJSW42__0=
                                            $__swJSW43__0;
                                           break;
                                          case 1:
                                           var $__55=
                                            new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,10]);
                                           $__swJSW42__0=
                                            $__55;
                                           break;}
                                        $__swJSW41__0=
                                         $__swJSW42__0;
                                        break;
                                       case 1:
                                        var $__56=
                                         new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,9]);
                                        $__swJSW41__0=
                                         $__56;
                                        break;}
                                     $__swJSW40__0=
                                      $__swJSW41__0;
                                     break;
                                    case 1:
                                     var $__57=
                                      new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,8]);
                                     $__swJSW40__0=
                                      $__57;
                                     break;}
                                  $__swJSW39__0=
                                   $__swJSW40__0;
                                  break;
                                 case 1:
                                  var $__58=
                                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,7]);
                                  $__swJSW39__0=
                                   $__58;
                                  break;}
                               $__swJSW38__0=
                                $__swJSW39__0;
                               break;
                              case 1:
                               var $__59=
                                new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,6]);
                               $__swJSW38__0=
                                $__59;
                               break;}
                            $__swJSW37__0=
                             $__swJSW38__0;
                            break;
                           case 1:
                            var $__60=
                             new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,5]);
                            $__swJSW37__0=
                             $__60;
                            break;}
                         $__swJSW36__0=
                          $__swJSW37__0;
                         break;
                        case 1:
                         var $__61=
                          new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,4]);
                         $__swJSW36__0=
                          $__61;
                         break;}
                      $__swJSW35__0=
                       $__swJSW36__0;
                      break;
                     case 1:
                      var $__62=
                       new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,3]);
                      $__swJSW35__0=
                       $__62;
                      break;}
                   $__swJSW34__0=
                    $__swJSW35__0;
                   break;
                  case 1:
                   var $__63=
                    new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,2]);
                   $__swJSW34__0=
                    $__63;
                   break;}
                $__swJSW33__0=
                 $__swJSW34__0;
                break;
               case 1:
                var $__64=
                 new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,1]);
                $__swJSW33__0=
                 $__64;
                break;}
             $__swJSW32__0=
              $__swJSW33__0;
             break;
            case 1:
             var $__65=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,0]);
             $__swJSW32__0=
              $__65;
             break;}
          return $__swJSW32__0;});
$Text.$Read.$Lex.$_24okUNQ791=
 new _F_(function($lexEsc,$_24x)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$_24x,92]);
          var $__4=
           _e_($__);
          var $__swJSW64__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              [$_24x,$UHC.$Base.$False__];
             var $__6=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5]);
             $__swJSW64__0=
              $__6;
             break;
            case 1:
             $__swJSW64__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$lexEsc,$Text.$Read.$Lex.$_24okUNQ958]);
             break;}
          return $__swJSW64__0;});
$Text.$Read.$Lex.$_24okUNQ958=
 new _F_(function($_24x)
         {var $__=
           [$_24x,$UHC.$Base.$True__];
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);});
$Text.$Read.$Lex.$lexCharE=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[94]);
                  var $lexCntrlChar=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$__184__490]);
                  var $__3=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,127]);
                  var $__4=
                   new _A_($UHC.$Base.$packedStringToString,["DEL"]);
                  var $__5=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__4]);
                  var $__6=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5,$__3]);
                  var $__7=
                   new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$_5b_5d]);
                  var $__8=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,32]);
                  var $__9=
                   new _A_($UHC.$Base.$packedStringToString,["SP"]);
                  var $__10=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__9]);
                  var $__11=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__10,$__8]);
                  var $__12=
                   new _A_($UHC.$Base.$_3a,[$__11,$__7]);
                  var $__13=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,31]);
                  var $__14=
                   new _A_($UHC.$Base.$packedStringToString,["US"]);
                  var $__15=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__14]);
                  var $__16=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__15,$__13]);
                  var $__17=
                   new _A_($UHC.$Base.$_3a,[$__16,$__12]);
                  var $__18=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,30]);
                  var $__19=
                   new _A_($UHC.$Base.$packedStringToString,["RS"]);
                  var $__20=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__19]);
                  var $__21=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__20,$__18]);
                  var $__22=
                   new _A_($UHC.$Base.$_3a,[$__21,$__17]);
                  var $__23=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,29]);
                  var $__24=
                   new _A_($UHC.$Base.$packedStringToString,["GS"]);
                  var $__25=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__24]);
                  var $__26=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__25,$__23]);
                  var $__27=
                   new _A_($UHC.$Base.$_3a,[$__26,$__22]);
                  var $__28=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,28]);
                  var $__29=
                   new _A_($UHC.$Base.$packedStringToString,["FS"]);
                  var $__30=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__29]);
                  var $__31=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__30,$__28]);
                  var $__32=
                   new _A_($UHC.$Base.$_3a,[$__31,$__27]);
                  var $__33=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,27]);
                  var $__34=
                   new _A_($UHC.$Base.$packedStringToString,["ESC"]);
                  var $__35=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__34]);
                  var $__36=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__35,$__33]);
                  var $__37=
                   new _A_($UHC.$Base.$_3a,[$__36,$__32]);
                  var $__38=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,26]);
                  var $__39=
                   new _A_($UHC.$Base.$packedStringToString,["SUB"]);
                  var $__40=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__39]);
                  var $__41=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__40,$__38]);
                  var $__42=
                   new _A_($UHC.$Base.$_3a,[$__41,$__37]);
                  var $__43=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,25]);
                  var $__44=
                   new _A_($UHC.$Base.$packedStringToString,["EM"]);
                  var $__45=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__44]);
                  var $__46=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__45,$__43]);
                  var $__47=
                   new _A_($UHC.$Base.$_3a,[$__46,$__42]);
                  var $__48=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,24]);
                  var $__49=
                   new _A_($UHC.$Base.$packedStringToString,["CAN"]);
                  var $__50=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__49]);
                  var $__51=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__50,$__48]);
                  var $__52=
                   new _A_($UHC.$Base.$_3a,[$__51,$__47]);
                  var $__53=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,23]);
                  var $__54=
                   new _A_($UHC.$Base.$packedStringToString,["ETB"]);
                  var $__55=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__54]);
                  var $__56=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__55,$__53]);
                  var $__57=
                   new _A_($UHC.$Base.$_3a,[$__56,$__52]);
                  var $__58=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,22]);
                  var $__59=
                   new _A_($UHC.$Base.$packedStringToString,["SYN"]);
                  var $__60=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__59]);
                  var $__61=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__60,$__58]);
                  var $__62=
                   new _A_($UHC.$Base.$_3a,[$__61,$__57]);
                  var $__63=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,21]);
                  var $__64=
                   new _A_($UHC.$Base.$packedStringToString,["NAK"]);
                  var $__65=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__64]);
                  var $__66=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__65,$__63]);
                  var $__67=
                   new _A_($UHC.$Base.$_3a,[$__66,$__62]);
                  var $__68=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,20]);
                  var $__69=
                   new _A_($UHC.$Base.$packedStringToString,["DC4"]);
                  var $__70=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__69]);
                  var $__71=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__70,$__68]);
                  var $__72=
                   new _A_($UHC.$Base.$_3a,[$__71,$__67]);
                  var $__73=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,19]);
                  var $__74=
                   new _A_($UHC.$Base.$packedStringToString,["DC3"]);
                  var $__75=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__74]);
                  var $__76=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__75,$__73]);
                  var $__77=
                   new _A_($UHC.$Base.$_3a,[$__76,$__72]);
                  var $__78=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,18]);
                  var $__79=
                   new _A_($UHC.$Base.$packedStringToString,["DC2"]);
                  var $__80=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__79]);
                  var $__81=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__80,$__78]);
                  var $__82=
                   new _A_($UHC.$Base.$_3a,[$__81,$__77]);
                  var $__83=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,17]);
                  var $__84=
                   new _A_($UHC.$Base.$packedStringToString,["DC1"]);
                  var $__85=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__84]);
                  var $__86=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__85,$__83]);
                  var $__87=
                   new _A_($UHC.$Base.$_3a,[$__86,$__82]);
                  var $__88=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,16]);
                  var $__89=
                   new _A_($UHC.$Base.$packedStringToString,["DLE"]);
                  var $__90=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__89]);
                  var $__91=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__90,$__88]);
                  var $__92=
                   new _A_($UHC.$Base.$_3a,[$__91,$__87]);
                  var $__93=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,15]);
                  var $__94=
                   new _A_($UHC.$Base.$packedStringToString,["SI"]);
                  var $__95=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__94]);
                  var $__96=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__95,$__93]);
                  var $__97=
                   new _A_($UHC.$Base.$_3a,[$__96,$__92]);
                  var $__98=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,13]);
                  var $__99=
                   new _A_($UHC.$Base.$packedStringToString,["CR"]);
                  var $__100=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__99]);
                  var $__101=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__100,$__98]);
                  var $__102=
                   new _A_($UHC.$Base.$_3a,[$__101,$__97]);
                  var $__103=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,12]);
                  var $__104=
                   new _A_($UHC.$Base.$packedStringToString,["FF"]);
                  var $__105=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__104]);
                  var $__106=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__105,$__103]);
                  var $__107=
                   new _A_($UHC.$Base.$_3a,[$__106,$__102]);
                  var $__108=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,11]);
                  var $__109=
                   new _A_($UHC.$Base.$packedStringToString,["VT"]);
                  var $__110=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__109]);
                  var $__111=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__110,$__108]);
                  var $__112=
                   new _A_($UHC.$Base.$_3a,[$__111,$__107]);
                  var $__113=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,10]);
                  var $__114=
                   new _A_($UHC.$Base.$packedStringToString,["LF"]);
                  var $__115=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__114]);
                  var $__116=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__115,$__113]);
                  var $__117=
                   new _A_($UHC.$Base.$_3a,[$__116,$__112]);
                  var $__118=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,9]);
                  var $__119=
                   new _A_($UHC.$Base.$packedStringToString,["HT"]);
                  var $__120=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__119]);
                  var $__121=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__120,$__118]);
                  var $__122=
                   new _A_($UHC.$Base.$_3a,[$__121,$__117]);
                  var $__123=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,8]);
                  var $__124=
                   new _A_($UHC.$Base.$packedStringToString,["BS"]);
                  var $__125=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__124]);
                  var $__126=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__125,$__123]);
                  var $__127=
                   new _A_($UHC.$Base.$_3a,[$__126,$__122]);
                  var $__128=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,7]);
                  var $__129=
                   new _A_($UHC.$Base.$packedStringToString,["BEL"]);
                  var $__130=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__129]);
                  var $__131=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__130,$__128]);
                  var $__132=
                   new _A_($UHC.$Base.$_3a,[$__131,$__127]);
                  var $__133=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,6]);
                  var $__134=
                   new _A_($UHC.$Base.$packedStringToString,["ACK"]);
                  var $__135=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__134]);
                  var $__136=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__135,$__133]);
                  var $__137=
                   new _A_($UHC.$Base.$_3a,[$__136,$__132]);
                  var $__138=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,5]);
                  var $__139=
                   new _A_($UHC.$Base.$packedStringToString,["ENQ"]);
                  var $__140=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__139]);
                  var $__141=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__140,$__138]);
                  var $__142=
                   new _A_($UHC.$Base.$_3a,[$__141,$__137]);
                  var $__143=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,4]);
                  var $__144=
                   new _A_($UHC.$Base.$packedStringToString,["EOT"]);
                  var $__145=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__144]);
                  var $__146=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__145,$__143]);
                  var $__147=
                   new _A_($UHC.$Base.$_3a,[$__146,$__142]);
                  var $__148=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,3]);
                  var $__149=
                   new _A_($UHC.$Base.$packedStringToString,["ETX"]);
                  var $__150=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__149]);
                  var $__151=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__150,$__148]);
                  var $__152=
                   new _A_($UHC.$Base.$_3a,[$__151,$__147]);
                  var $__153=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,2]);
                  var $__154=
                   new _A_($UHC.$Base.$packedStringToString,["STX"]);
                  var $__155=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__154]);
                  var $__156=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__155,$__153]);
                  var $__157=
                   new _A_($UHC.$Base.$_3a,[$__156,$__152]);
                  var $__158=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,0]);
                  var $__159=
                   new _A_($UHC.$Base.$packedStringToString,["NUL"]);
                  var $__160=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__159]);
                  var $__161=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__160,$__158]);
                  var $__162=
                   new _A_($UHC.$Base.$_3a,[$__161,$__157]);
                  var $__163=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,14]);
                  var $__164=
                   new _A_($UHC.$Base.$packedStringToString,["SO"]);
                  var $__165=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__164]);
                  var $__166=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__165,$__163]);
                  var $__167=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,1]);
                  var $__168=
                   new _A_($UHC.$Base.$packedStringToString,["SOH"]);
                  var $__169=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__168]);
                  var $__170=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__169,$__167]);
                  var $__171=
                   new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$__170,$__166]);
                  var $__172=
                   new _A_($UHC.$Base.$_3a,[$__171,$__162]);
                  var $lexAscii=
                   new _A_($Text.$ParserCombinators.$ReadP.$choice,[$__172]);
                  var $__174=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$lexCntrlChar,$lexAscii]);
                  var $__175=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexNumericUNQ781,$__174]);
                  var $lexEsc=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexEscCharUNQ783,$__175]);
                  var $__177=
                   new _A_($Text.$Read.$Lex.$_24okUNQ791,[$lexEsc]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$__177]);}),[]);
$Text.$Read.$Lex.$_24okUNQ1008=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["build/101/ehclib/base/Text/Read/Lex.hs-cpp:148:22: monadic bind"]);
          var $__3=
           new _A_($UHC.$Base.$fail,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
          var $__4=
           _e_($_24x);
          var $__7=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4[0]]);
          return $__7;});
$Text.$Read.$Lex.$lexChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$lexCharE,$Text.$Read.$Lex.$_24okUNQ1008]);}),[]);
$Text.$Read.$Lex.$signedExpUNQ544=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[43]);
                  var $__2=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[45]);
                  var $__3=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$__2,$__]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__3,$Text.$Read.$Lex.$_24okUNQ551]);}),[]);
$Text.$Read.$Lex.$_24okUNQ551=
 new _F_(function($_24x)
         {var $__=
           new _A_($Text.$Read.$Lex.$lexInteger,[10]);
          var $__3=
           new _A_($Text.$Read.$Lex.$_24okUNQ556,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$__3]);});
$Text.$Read.$Lex.$_24okUNQ556=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Text.$Read.$Lex.$__184__1086NEW438,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);});
$Text.$Read.$Lex.$__184__1086NEW438=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$_24x,45]);
          var $__4=
           _e_($__);
          var $__swJSW66__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW66__0=
              $_24x2;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__134__0,$_24x2]);
             $__swJSW66__0=
              $__5;
             break;}
          return $__swJSW66__0;});
$Text.$Read.$Lex.$__184__1112NEW448=
 new _F_(function($signedExp)
         {var $__=
           new _A_($Text.$Read.$Lex.$lexInteger,[10]);
          var $__3=
           new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$signedExp,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__3,$Text.$Read.$Lex.$_24okUNQ562]);});
$Text.$Read.$Lex.$_24okUNQ562=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$Just__,[$_24x]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);});
$Text.$Read.$Lex.$lexExp=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$Read.$Lex.$__184__1112NEW448,[$Text.$Read.$Lex.$signedExpUNQ544]);
                  var $__2=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[69]);
                  var $__3=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[101]);
                  var $__4=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$__3,$__2]);
                  return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4,$__]);}),[]);
$Text.$Read.$Lex.$valDigitUNQ501=
 new _F_(function($__,$base,$c)
         {var $__4=
           new _A_($Text.$Read.$Lex.$valDig,[$__,$base,$c]);
          return new _A_($UHC.$Base.$maybe,[0,$UHC.$Base.$id,$__4]);});
$Text.$Read.$Lex.$isDigitUNQ502=
 new _F_(function($__,$base,$c)
         {var $__4=
           new _A_($Text.$Read.$Lex.$valDig,[$__,$base,$c]);
          var $__5=
           new _A_($UHC.$Base.$const,[$UHC.$Base.$True__]);
          return new _A_($UHC.$Base.$maybe,[$UHC.$Base.$False__,$__5,$__4]);});
$Text.$Read.$Lex.$readIntP_27=
 new _F_(function($__,$base)
         {var $__3=
           new _A_($Text.$Read.$Lex.$valDigitUNQ501,[$__,$base]);
          var $__4=
           new _A_($Text.$Read.$Lex.$isDigitUNQ502,[$__,$base]);
          return new _A_($Text.$Read.$Lex.$readIntP,[$__,$base,$__4,$__3]);});
$Text.$Read.$Lex.$readDecP=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["10"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          return new _A_($Text.$Read.$Lex.$readIntP_27,[$__,$__3]);});
$Text.$Read.$Lex.$readHexP=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["16"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          return new _A_($Text.$Read.$Lex.$readIntP_27,[$__,$__3]);});
$Text.$Read.$Lex.$readOctP=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["8"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          return new _A_($Text.$Read.$Lex.$readIntP_27,[$__,$__3]);});
$Text.$Read.$Lex.$__182__9816__7__1UNQ1127=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Show__DCT74__289__0,[$UHC.$Base.$Show__DCT74__157__0,$UHC.$Base.$Integral__DCT74__143__0]);}),[]);
$Text.$Read.$Lex.$__182__9816__2__1UNQ1132=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Show__DCT74__87__0,[$UHC.$Base.$Show__DCT74__65__0]);}),[]);
$Text.$Read.$Lex.$__180__1__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2,$d,$x__1)
         {var $x__15=
           _e_($x__1);
          var $__swJSW67__0;
          switch($x__15._tag_)
           {case 0:
             var $__7=
              new _A_($UHC.$Base.$showsPrec,[$UHC.$Base.$Show__DCT74__65__0,11,$x__15._1]);
             var $__8=
              new _A_($UHC.$Base.$packedStringToString,["Char "]);
             var $__9=
              new _A_($UHC.$Base.$showString,[$__8]);
             var $__10=
              new _A_($UHC.$Base.$_2e,[$__9,$__7]);
             var $__11=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__12=
              new _A_($UHC.$Base.$showParen,[$__11,$__10]);
             $__swJSW67__0=
              $__12;
             break;
            case 1:
             var $__13=
              new _A_($UHC.$Base.$packedStringToString,["EOF"]);
             var $__14=
              new _A_($UHC.$Base.$showString,[$__13]);
             $__swJSW67__0=
              $__14;
             break;
            case 2:
             var $__16=
              new _A_($UHC.$Base.$showsPrec,[$__,11,$x__15._1]);
             var $__17=
              new _A_($UHC.$Base.$packedStringToString,["Ident "]);
             var $__18=
              new _A_($UHC.$Base.$showString,[$__17]);
             var $__19=
              new _A_($UHC.$Base.$_2e,[$__18,$__16]);
             var $__20=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__21=
              new _A_($UHC.$Base.$showParen,[$__20,$__19]);
             $__swJSW67__0=
              $__21;
             break;
            case 3:
             var $__23=
              new _A_($UHC.$Base.$showsPrec,[$UHC.$Base.$Show__DCT74__157__0,11,$x__15._1]);
             var $__24=
              new _A_($UHC.$Base.$packedStringToString,["Int "]);
             var $__25=
              new _A_($UHC.$Base.$showString,[$__24]);
             var $__26=
              new _A_($UHC.$Base.$_2e,[$__25,$__23]);
             var $__27=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__28=
              new _A_($UHC.$Base.$showParen,[$__27,$__26]);
             $__swJSW67__0=
              $__28;
             break;
            case 4:
             var $__30=
              new _A_($UHC.$Base.$showsPrec,[$__,11,$x__15._1]);
             var $__31=
              new _A_($UHC.$Base.$packedStringToString,["Punc "]);
             var $__32=
              new _A_($UHC.$Base.$showString,[$__31]);
             var $__33=
              new _A_($UHC.$Base.$_2e,[$__32,$__30]);
             var $__34=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__35=
              new _A_($UHC.$Base.$showParen,[$__34,$__33]);
             $__swJSW67__0=
              $__35;
             break;
            case 5:
             var $__37=
              new _A_($UHC.$Base.$showsPrec,[$__2,11,$x__15._1]);
             var $__38=
              new _A_($UHC.$Base.$packedStringToString,["Rat "]);
             var $__39=
              new _A_($UHC.$Base.$showString,[$__38]);
             var $__40=
              new _A_($UHC.$Base.$_2e,[$__39,$__37]);
             var $__41=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__42=
              new _A_($UHC.$Base.$showParen,[$__41,$__40]);
             $__swJSW67__0=
              $__42;
             break;
            case 6:
             var $__44=
              new _A_($UHC.$Base.$showsPrec,[$__,11,$x__15._1]);
             var $__45=
              new _A_($UHC.$Base.$packedStringToString,["String "]);
             var $__46=
              new _A_($UHC.$Base.$showString,[$__45]);
             var $__47=
              new _A_($UHC.$Base.$_2e,[$__46,$__44]);
             var $__48=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__49=
              new _A_($UHC.$Base.$showParen,[$__48,$__47]);
             $__swJSW67__0=
              $__49;
             break;
            case 7:
             var $__51=
              new _A_($UHC.$Base.$showsPrec,[$__,11,$x__15._1]);
             var $__52=
              new _A_($UHC.$Base.$packedStringToString,["Symbol "]);
             var $__53=
              new _A_($UHC.$Base.$showString,[$__52]);
             var $__54=
              new _A_($UHC.$Base.$_2e,[$__53,$__51]);
             var $__55=
              new _A_($UHC.$Base.$primGtInt,[$d,10]);
             var $__56=
              new _A_($UHC.$Base.$showParen,[$__55,$__54]);
             $__swJSW67__0=
              $__56;
             break;}
          return $__swJSW67__0;});
$Text.$Read.$Lex.$__180__1__0NEW521UNQ1126RDC=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           new _A_($Text.$Read.$Lex.$__180__1__0NEW525UNQ1140EVLRDC,[$__,$__2,$__3]);
          return $__4;});
$Text.$Read.$Lex.$__180__1__0NEW525UNQ1140EVLRDC=
 new _F_(function($__,$__2,$__3)
         {var $Show__=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$__]));
          var $__8=
           new _A_($Text.$Read.$Lex.$__180__1__0DFLUHC_2eBase_2eshowsPrec,[$__2,$__3]);
          var $__9=
           {_tag_:0,_1:$Show__._1,_2:$Show__._2,_3:$__8};
          return $__9;});
$Text.$Read.$Lex.$__180__1__0UNQ1126RDC=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$__180__1__0NEW521UNQ1126RDC,[$Text.$Read.$Lex.$__180__1__0UNQ1126RDC,$Text.$Read.$Lex.$__182__9816__2__1UNQ1132,$Text.$Read.$Lex.$__182__9816__7__1UNQ1127]);}),[]);
$Text.$Read.$Lex.$__180__1__0=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$__180__1__0UNQ1126RDC;}),[]);
$Text.$Read.$Lex.$Symbol__=
 new _F_(function($x1)
         {return {_tag_:7,_1:$x1};});
$Text.$Read.$Lex.$String__=
 new _F_(function($x1)
         {return {_tag_:6,_1:$x1};});
$Text.$Read.$Lex.$__184__1290=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$get,$Text.$Read.$Lex.$_24okUNQ978]);}),[]);
$Text.$Read.$Lex.$_24okUNQ978=
 new _F_(function($_24x)
         {var $__=
           new _A_($Text.$Read.$Lex.$__182__8205__0NEW536UNQ983CCN,[$_24x]);
          var $__3=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,38,$_24x]));
          var $__swJSW69__0;
          switch($__3._tag_)
           {case 0:
             $__swJSW69__0=
              $__;
             break;
            case 1:
             var $__4=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,[]]);
             $__swJSW69__0=
              $__4;
             break;}
          return $__swJSW69__0;});
$Text.$Read.$Lex.$__182__8205__0NEW536UNQ983CCN=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$isSpace,[$_24x]);
          var $__3=
           _e_($__);
          var $__swJSW70__0;
          switch($__3._tag_)
           {case 0:
             $__swJSW70__0=
              $Text.$ParserCombinators.$ReadP.$pfail;
             break;
            case 1:
             var $__4=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,[]]);
             var $__5=
              new _A_($Text.$ParserCombinators.$ReadP.$char,[92]);
             var $__6=
              new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__5,$__4]);
             var $__7=
              new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$skipSpaces,$__6]);
             $__swJSW70__0=
              $__7;
             break;}
          return $__swJSW70__0;});
$Text.$Read.$Lex.$bodyUNQ971=
 new _F_(function($lexStrItem,$f)
         {var $__=
           new _A_($Text.$Read.$Lex.$_24okUNQ989,[$lexStrItem,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$lexStrItem,$__]);});
$Text.$Read.$Lex.$_24okUNQ989=
 new _F_(function($lexStrItem,$f,$_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["build/101/ehclib/base/Text/Read/Lex.hs-cpp:273:16: monadic bind"]);
          var $__5=
           new _A_($UHC.$Base.$fail,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
          var $__6=
           _e_($_24x);
          var $__9=
           new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$Eq__DCT74__56__0,$__6[0],34]);
          var $__10=
           new _A_($UHC.$Base.$_7c_7c,[$__9,$__6[1]]);
          var $__11=
           _e_($__10);
          var $__swJSW72__0;
          switch($__11._tag_)
           {case 0:
             var $__12=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             var $s=
              new _A_($f,[$__12]);
             var $__14=
              new _A_($Text.$Read.$Lex.$String__,[$s]);
             $__swJSW72__0=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__14]);
             break;
            case 1:
             var $__15=
              new _A_($UHC.$Base.$_3a,[$__6[0]]);
             var $__16=
              new _A_($UHC.$Base.$_2e,[$f,$__15]);
             var $__17=
              new _A_($Text.$Read.$Lex.$bodyUNQ971,[$lexStrItem,$__16]);
             $__swJSW72__0=
              $__17;
             break;}
          return $__swJSW72__0;});
$Text.$Read.$Lex.$lexString=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[92]);
                  var $lexEmpty=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$__184__1290]);
                  var $__3=
                   _i_();
                  var $lexStrItem=
                   _i_();
                  _i_set_($__3,new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$lexEmpty,$lexStrItem]));
                  _i_set_($lexStrItem,new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$__3,$Text.$Read.$Lex.$lexCharE]));
                  var $__5=
                   new _A_($Text.$Read.$Lex.$bodyUNQ971,[$lexStrItem,$UHC.$Base.$id]);
                  var $__6=
                   new _A_($Text.$ParserCombinators.$ReadP.$char,[34]);
                  return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__6,$__5]);}),[]);
$Text.$Read.$Lex.$Rat__=
 new _F_(function($x1)
         {return {_tag_:5,_1:$x1};});
$Text.$Read.$Lex.$Punc__=
 new _F_(function($x1)
         {return {_tag_:4,_1:$x1};});
$Text.$Read.$Lex.$isPuncCharUNQ533=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[",;()[]{}`"]);
          return new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);});
$Text.$Read.$Lex.$_24okUNQ534=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$_24x,$UHC.$Base.$_5b_5d]);
          var $__3=
           new _A_($Text.$Read.$Lex.$Punc__,[$__]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__3]);});
$Text.$Read.$Lex.$lexPunc=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$satisfy,[$Text.$Read.$Lex.$isPuncCharUNQ533]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ534]);}),[]);
$Text.$Read.$Lex.$isSymbolCharUNQ512=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["!@#$%&*+./<=>?\\^|:-~"]);
          return new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);});
$Text.$Read.$Lex.$_24okUNQ516=
 new _F_(function($__,$reserved__ops,$_24x)
         {var $__4=
           new _A_($UHC.$Base.$elem,[$__,$_24x,$reserved__ops]);
          var $__5=
           _e_($__4);
          var $__swJSW73__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              new _A_($Text.$Read.$Lex.$Symbol__,[$_24x]);
             var $__7=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__6]);
             $__swJSW73__0=
              $__7;
             break;
            case 1:
             var $__8=
              new _A_($Text.$Read.$Lex.$Punc__,[$_24x]);
             var $__9=
              new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__8]);
             $__swJSW73__0=
              $__9;
             break;}
          return $__swJSW73__0;});
$Text.$Read.$Lex.$lexSymbol=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$packedStringToString,["=>"]);
                  var $__2=
                   new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);
                  var $__3=
                   new _A_($UHC.$Base.$packedStringToString,["~"]);
                  var $__4=
                   new _A_($UHC.$Base.$_3a,[$__3,$__2]);
                  var $__5=
                   new _A_($UHC.$Base.$packedStringToString,["@"]);
                  var $__6=
                   new _A_($UHC.$Base.$_3a,[$__5,$__4]);
                  var $__7=
                   new _A_($UHC.$Base.$packedStringToString,["->"]);
                  var $__8=
                   new _A_($UHC.$Base.$_3a,[$__7,$__6]);
                  var $__9=
                   new _A_($UHC.$Base.$packedStringToString,["<-"]);
                  var $__10=
                   new _A_($UHC.$Base.$_3a,[$__9,$__8]);
                  var $__11=
                   new _A_($UHC.$Base.$packedStringToString,["|"]);
                  var $__12=
                   new _A_($UHC.$Base.$_3a,[$__11,$__10]);
                  var $__13=
                   new _A_($UHC.$Base.$packedStringToString,["\\"]);
                  var $__14=
                   new _A_($UHC.$Base.$_3a,[$__13,$__12]);
                  var $__15=
                   new _A_($UHC.$Base.$packedStringToString,["="]);
                  var $__16=
                   new _A_($UHC.$Base.$_3a,[$__15,$__14]);
                  var $__17=
                   new _A_($UHC.$Base.$packedStringToString,["::"]);
                  var $__18=
                   new _A_($UHC.$Base.$_3a,[$__17,$__16]);
                  var $__19=
                   new _A_($UHC.$Base.$packedStringToString,[".."]);
                  var $reserved__ops=
                   new _A_($UHC.$Base.$_3a,[$__19,$__18]);
                  var $__21=
                   new _A_($UHC.$Base.$Eq__DCT74__394__0,[$UHC.$Base.$Eq__DCT74__56__0]);
                  var $__22=
                   new _A_($Text.$ParserCombinators.$ReadP.$munch1,[$Text.$Read.$Lex.$isSymbolCharUNQ512]);
                  var $__23=
                   new _A_($Text.$Read.$Lex.$_24okUNQ516,[$__21,$reserved__ops]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__22,$__23]);}),[]);
$Text.$Read.$Lex.$Int__=
 new _F_(function($x1)
         {return {_tag_:3,_1:$x1};});
$Text.$Read.$Lex.$valExpUNQ577=
 new _F_(function($__,$__2,$rat,$exp)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["10"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_5e_5e,[$__2,$UHC.$Base.$Integral__DCT74__143__0,$__6,$exp]);
          return new _A_($UHC.$Base.$_2a,[$__,$rat,$__7]);});
$Text.$Read.$Lex.$valueFracExpUNQ586=
 new _F_(function($__,$__2,$__3,$x1,$x2,$x3)
         {var $a=
           new _A_($Text.$Read.$Lex.$aNEW617UNQ620CCN,[$__,$__2,$__3,$x1,$x2,$x3]);
          var $x28=
           _e_($x2);
          var $__swJSW74__0;
          switch($x28._tag_)
           {case 0:
             $__swJSW74__0=
              $a;
             break;
            case 1:
             var $x310=
              _e_($x3);
             var $__swJSW75__0;
             switch($x310._tag_)
              {case 0:
                $__swJSW75__0=
                 $a;
                break;
               case 1:
                var $__12=
                 new _A_($Text.$Read.$Lex.$Int__,[$x1]);
                $__swJSW75__0=
                 $__12;
                break;}
             $__swJSW74__0=
              $__swJSW75__0;
             break;}
          return $__swJSW74__0;});
$Text.$Read.$Lex.$aNEW617UNQ620CCN=
 new _F_(function($__,$__2,$__3,$x1,$x2,$x3)
         {var $a=
           new _A_($Text.$Read.$Lex.$aNEW624UNQ621CCN,[$__,$__2,$x1,$x2,$x3]);
          var $x28=
           _e_($x2);
          var $__swJSW76__0;
          switch($x28._tag_)
           {case 0:
             $__swJSW76__0=
              $a;
             break;
            case 1:
             var $x310=
              _e_($x3);
             var $__swJSW77__0;
             switch($x310._tag_)
              {case 0:
                var $__12=
                 new _A_($UHC.$Base.$primIntToInteger,[0]);
                var $__13=
                 new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__132__0,$x310._1,$__12]);
                var $__14=
                 _e_($__13);
                var $__swJSW78__0;
                switch($__14._tag_)
                 {case 0:
                   var $__15=
                    _e_($UHC.$Base.$otherwise);
                   var $__swJSW79__0;
                   switch($__15._tag_)
                    {case 0:
                      $__swJSW79__0=
                       $a;
                      break;
                     case 1:
                      var $__16=
                       new _A_($UHC.$Base.$fromInteger,[$__3,$x1]);
                      var $__17=
                       new _A_($Text.$Read.$Lex.$valExpUNQ577,[$__,$__2,$__16,$x310._1]);
                      var $__18=
                       new _A_($Text.$Read.$Lex.$Rat__,[$__17]);
                      $__swJSW79__0=
                       $__18;
                      break;}
                   $__swJSW78__0=
                    $__swJSW79__0;
                   break;
                  case 1:
                   var $__19=
                    new _A_($UHC.$Base.$primIntToInteger,[10]);
                   var $__20=
                    new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__143__0,$__19,$x310._1]);
                   var $__21=
                    new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__134__0,$x1,$__20]);
                   var $__22=
                    new _A_($Text.$Read.$Lex.$Int__,[$__21]);
                   $__swJSW78__0=
                    $__22;
                   break;}
                $__swJSW77__0=
                 $__swJSW78__0;
                break;
               case 1:
                $__swJSW77__0=
                 $a;
                break;}
             $__swJSW76__0=
              $__swJSW77__0;
             break;}
          return $__swJSW76__0;});
$Text.$Read.$Lex.$aNEW624UNQ621CCN=
 new _F_(function($__,$__2,$x1,$x2,$x3)
         {var $x26=
           _e_($x2);
          var $__swJSW80__0;
          switch($x26._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$Num__DCT74__271__0,[$UHC.$Base.$Integral__DCT74__143__0]);
             var $__9=
              new _A_($UHC.$Base.$primIntToInteger,[1]);
             var $__10=
              new _A_($UHC.$Base.$primIntToInteger,[0]);
             var $__11=
              new _A_($UHC.$Base.$primIntToInteger,[10]);
             var $__12=
              new _A_($Text.$Read.$Lex.$frac,[$UHC.$Base.$Integral__DCT74__143__0,$__11,$__10,$__9,$x26._1]);
             var $__13=
              new _A_($UHC.$Base.$fromInteger,[$__8,$x1]);
             var $rat=
              new _A_($UHC.$Base.$_2b,[$__8,$__13,$__12]);
             var $__15=
              _e_($x3);
             var $__swJSW81__0;
             switch($__15._tag_)
              {case 0:
                var $__17=
                 new _A_($Text.$Read.$Lex.$valExpUNQ577,[$__,$__2,$rat,$__15._1]);
                var $__18=
                 new _A_($Text.$Read.$Lex.$Rat__,[$__17]);
                $__swJSW81__0=
                 $__18;
                break;
               case 1:
                var $__19=
                 new _A_($Text.$Read.$Lex.$Rat__,[$rat]);
                $__swJSW81__0=
                 $__19;
                break;}
             $__swJSW80__0=
              $__swJSW81__0;
             break;
            case 1:
             $__swJSW80__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW80__0;});
$Text.$Read.$Lex.$valueUNQ590=
 new _F_(function($__,$__2,$__3,$xs)
         {var $__5=
           new _A_($UHC.$Base.$primIntToInteger,[0]);
          var $__6=
           new _A_($UHC.$Base.$primIntToInteger,[10]);
          var $__7=
           new _A_($Text.$Read.$Lex.$val,[$UHC.$Base.$Num__DCT74__134__0,$__6,$__5,$xs]);
          return new _A_($Text.$Read.$Lex.$valueFracExpUNQ586,[$__,$__2,$__3,$__7]);});
$Text.$Read.$Lex.$_24okUNQ594=
 new _F_(function($__,$__2,$__3,$_24x)
         {var $__5=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$UHC.$Base.$Nothing__]);
          var $__6=
           new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$Text.$Read.$Lex.$lexFrac,$__5]);
          var $__7=
           new _A_($Text.$Read.$Lex.$_24okUNQ664,[$__,$__2,$__3,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__6,$__7]);});
$Text.$Read.$Lex.$_24okUNQ664=
 new _F_(function($__,$__2,$__3,$_24x,$_24x5)
         {var $__6=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$UHC.$Base.$Nothing__]);
          var $__7=
           new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$Text.$Read.$Lex.$lexExp,$__6]);
          var $__8=
           new _A_($Text.$Read.$Lex.$_24okUNQ668,[$__,$__2,$__3,$_24x,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__7,$__8]);});
$Text.$Read.$Lex.$_24okUNQ668=
 new _F_(function($__,$__2,$__3,$_24x,$_24x5,$_24x6)
         {var $__7=
           new _A_($Text.$Read.$Lex.$valueUNQ590,[$__,$__2,$__3,$_24x,$_24x5,$_24x6]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__7]);});
$Text.$Read.$Lex.$lexDecNumber=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$Num__DCT74__271__0,[$UHC.$Base.$Integral__DCT74__143__0]);
                  var $__2=
                   new _A_($UHC.$Base.$Fractional__DCT74__273__0,[$UHC.$Base.$Integral__DCT74__143__0]);
                  var $__3=
                   new _A_($UHC.$Base.$Num__DCT74__271__0,[$UHC.$Base.$Integral__DCT74__143__0]);
                  var $__4=
                   new _A_($Text.$Read.$Lex.$lexDigits,[10]);
                  var $__5=
                   new _A_($Text.$Read.$Lex.$_24okUNQ594,[$__3,$__2,$__]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4,$__5]);}),[]);
$Text.$Read.$Lex.$_24okUNQ695=
 new _F_(function($_24x)
         {var $__=
           new _A_($Text.$Read.$Lex.$lexDigits,[$_24x]);
          var $__3=
           new _A_($Text.$Read.$Lex.$_24okUNQ702,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$__3]);});
$Text.$Read.$Lex.$_24okUNQ702=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[0]);
          var $__4=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__134__0,$_24x]);
          var $__5=
           new _A_($Text.$Read.$Lex.$val,[$UHC.$Base.$Num__DCT74__134__0,$__4,$__,$_24x2]);
          var $__6=
           new _A_($Text.$Read.$Lex.$Int__,[$__5]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__6]);});
$Text.$Read.$Lex.$__184__1587=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$lexBaseChar,$Text.$Read.$Lex.$_24okUNQ695]);}),[]);
$Text.$Read.$Lex.$__184__1586=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$char,[48]);}),[]);
$Text.$Read.$Lex.$lexHexOct=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$__184__1586,$Text.$Read.$Lex.$__184__1587]);}),[]);
$Text.$Read.$Lex.$lexNumber=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$Text.$Read.$Lex.$lexHexOct,$Text.$Read.$Lex.$lexDecNumber]);}),[]);
$Text.$Read.$Lex.$Ident__=
 new _F_(function($x1)
         {return {_tag_:2,_1:$x1};});
$Text.$Read.$Lex.$isIdfCharUNQ1042=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["_'"]);
          var $__3=
           new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);
          var $__4=
           new _A_($UHC.$Base.$isAlphaNum,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__4,$__3]);});
$Text.$Read.$Lex.$isIdsCharUNQ1040=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,95]);
          var $__3=
           new _A_($UHC.$Base.$isAlpha,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__3,$__]);});
$Text.$Read.$Lex.$lex__idUNQ1044=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$satisfy,[$Text.$Read.$Lex.$isIdsCharUNQ1040]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ1056]);}),[]);
$Text.$Read.$Lex.$_24okUNQ1056=
 new _F_(function($_24x)
         {var $__=
           new _A_($Text.$ParserCombinators.$ReadP.$munch,[$Text.$Read.$Lex.$isIdfCharUNQ1042]);
          var $__3=
           new _A_($Text.$Read.$Lex.$_24okUNQ1060,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$__3]);});
$Text.$Read.$Lex.$_24okUNQ1060=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$_24x,$_24x2]);
          var $__4=
           new _A_($Text.$Read.$Lex.$Ident__,[$__]);
          return new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4]);});
$Text.$Read.$Lex.$lexId=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$Read.$Lex.$Rat__,[$Text.$Read.$Lex.$infinity]);
                  var $__2=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
                  var $__3=
                   new _A_($UHC.$Base.$packedStringToString,["Infinity"]);
                  var $__4=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__3]);
                  var $__5=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4,$__2]);
                  var $__6=
                   new _A_($Text.$Read.$Lex.$Rat__,[$Text.$Read.$Lex.$notANumber]);
                  var $__7=
                   new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__6]);
                  var $__8=
                   new _A_($UHC.$Base.$packedStringToString,["NaN"]);
                  var $__9=
                   new _A_($Text.$ParserCombinators.$ReadP.$string,[$__8]);
                  var $__10=
                   new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__9,$__7]);
                  var $lex__nan=
                   new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$__10,$__5]);
                  return new _A_($Text.$ParserCombinators.$ReadP.$_3c_2b_2b,[$lex__nan,$Text.$Read.$Lex.$lex__idUNQ1044]);}),[]);
$Text.$Read.$Lex.$EOF__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$Text.$Read.$Lex.$_24okUNQ539=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$EOF__]);
          var $__3=
           new _A_($UHC.$Base.$null,[$_24x]);
          var $__4=
           new _A_($Control.$Monad.$guard,[$Text.$ParserCombinators.$ReadP.$MonadPlus__DCT168__6__0,$__3]);
          return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4,$__]);});
$Text.$Read.$Lex.$lexEOF=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$look,$Text.$Read.$Lex.$_24okUNQ539]);}),[]);
$Text.$Read.$Lex.$Char__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$Text.$Read.$Lex.$__Rep0LexemeDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__2=
           _e_($proj__1);
          var $__swJSW82__0;
          switch($proj__2._tag_)
           {case 0:
             var $proj__34=
              _e_($proj__2.unL1);
             var $__swJSW83__0;
             switch($proj__34._tag_)
              {case 0:
                var $proj__46=
                 _e_($proj__34.unL1);
                var $__swJSW84__0;
                switch($proj__46._tag_)
                 {case 0:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Char__,[$proj__46.unL1]);
                   $__swJSW84__0=
                    $__;
                   break;
                  case 1:
                   var $__=
                    new _A_($Text.$Read.$Lex.$String__,[$proj__46.unR1]);
                   $__swJSW84__0=
                    $__;
                   break;}
                $__swJSW83__0=
                 $__swJSW84__0;
                break;
               case 1:
                var $proj__1112=
                 _e_($proj__34.unR1);
                var $__swJSW85__0;
                switch($proj__1112._tag_)
                 {case 0:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Punc__,[$proj__1112.unL1]);
                   $__swJSW85__0=
                    $__;
                   break;
                  case 1:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Ident__,[$proj__1112.unR1]);
                   $__swJSW85__0=
                    $__;
                   break;}
                $__swJSW83__0=
                 $__swJSW85__0;
                break;}
             $__swJSW82__0=
              $__swJSW83__0;
             break;
            case 1:
             var $proj__1818=
              _e_($proj__2.unR1);
             var $__swJSW86__0;
             switch($proj__1818._tag_)
              {case 0:
                var $proj__1920=
                 _e_($proj__1818.unL1);
                var $__swJSW87__0;
                switch($proj__1920._tag_)
                 {case 0:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Symbol__,[$proj__1920.unL1]);
                   $__swJSW87__0=
                    $__;
                   break;
                  case 1:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Int__,[$proj__1920.unR1]);
                   $__swJSW87__0=
                    $__;
                   break;}
                $__swJSW86__0=
                 $__swJSW87__0;
                break;
               case 1:
                var $proj__2626=
                 _e_($proj__1818.unR1);
                var $__swJSW88__0;
                switch($proj__2626._tag_)
                 {case 0:
                   var $__=
                    new _A_($Text.$Read.$Lex.$Rat__,[$proj__2626.unL1]);
                   $__swJSW88__0=
                    $__;
                   break;
                  case 1:
                   var $proj__31=
                    _e_($proj__2626.unR1);
                   $__swJSW88__0=
                    $Text.$Read.$Lex.$EOF__;
                   break;}
                $__swJSW86__0=
                 $__swJSW88__0;
                break;}
             $__swJSW82__0=
              $__swJSW86__0;
             break;}
          return $__swJSW82__0;});
$Text.$Read.$Lex.$__Rep0LexemeDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__swJSW90__0;
          switch($x2._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__5=
              new _A_($UHC.$Base.$M1__,[$__4]);
             var $__6=
              new _A_($UHC.$Base.$M1__,[$__5]);
             var $__7=
              new _A_($UHC.$Base.$L1__,[$__6]);
             var $__8=
              new _A_($UHC.$Base.$L1__,[$__7]);
             var $__9=
              new _A_($UHC.$Base.$L1__,[$__8]);
             var $__10=
              new _A_($UHC.$Base.$M1__,[$__9]);
             $__swJSW90__0=
              $__10;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__12=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__13=
              new _A_($UHC.$Base.$R1__,[$__12]);
             var $__14=
              new _A_($UHC.$Base.$R1__,[$__13]);
             var $__15=
              new _A_($UHC.$Base.$M1__,[$__14]);
             $__swJSW90__0=
              $__15;
             break;
            case 2:
             var $__17=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__18=
              new _A_($UHC.$Base.$M1__,[$__17]);
             var $__19=
              new _A_($UHC.$Base.$M1__,[$__18]);
             var $__20=
              new _A_($UHC.$Base.$R1__,[$__19]);
             var $__21=
              new _A_($UHC.$Base.$R1__,[$__20]);
             var $__22=
              new _A_($UHC.$Base.$L1__,[$__21]);
             var $__23=
              new _A_($UHC.$Base.$M1__,[$__22]);
             $__swJSW90__0=
              $__23;
             break;
            case 3:
             var $__25=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__26=
              new _A_($UHC.$Base.$M1__,[$__25]);
             var $__27=
              new _A_($UHC.$Base.$M1__,[$__26]);
             var $__28=
              new _A_($UHC.$Base.$R1__,[$__27]);
             var $__29=
              new _A_($UHC.$Base.$L1__,[$__28]);
             var $__30=
              new _A_($UHC.$Base.$R1__,[$__29]);
             var $__31=
              new _A_($UHC.$Base.$M1__,[$__30]);
             $__swJSW90__0=
              $__31;
             break;
            case 4:
             var $__33=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__34=
              new _A_($UHC.$Base.$M1__,[$__33]);
             var $__35=
              new _A_($UHC.$Base.$M1__,[$__34]);
             var $__36=
              new _A_($UHC.$Base.$L1__,[$__35]);
             var $__37=
              new _A_($UHC.$Base.$R1__,[$__36]);
             var $__38=
              new _A_($UHC.$Base.$L1__,[$__37]);
             var $__39=
              new _A_($UHC.$Base.$M1__,[$__38]);
             $__swJSW90__0=
              $__39;
             break;
            case 5:
             var $__41=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__42=
              new _A_($UHC.$Base.$M1__,[$__41]);
             var $__43=
              new _A_($UHC.$Base.$M1__,[$__42]);
             var $__44=
              new _A_($UHC.$Base.$L1__,[$__43]);
             var $__45=
              new _A_($UHC.$Base.$R1__,[$__44]);
             var $__46=
              new _A_($UHC.$Base.$R1__,[$__45]);
             var $__47=
              new _A_($UHC.$Base.$M1__,[$__46]);
             $__swJSW90__0=
              $__47;
             break;
            case 6:
             var $__49=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__50=
              new _A_($UHC.$Base.$M1__,[$__49]);
             var $__51=
              new _A_($UHC.$Base.$M1__,[$__50]);
             var $__52=
              new _A_($UHC.$Base.$R1__,[$__51]);
             var $__53=
              new _A_($UHC.$Base.$L1__,[$__52]);
             var $__54=
              new _A_($UHC.$Base.$L1__,[$__53]);
             var $__55=
              new _A_($UHC.$Base.$M1__,[$__54]);
             $__swJSW90__0=
              $__55;
             break;
            case 7:
             var $__57=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__58=
              new _A_($UHC.$Base.$M1__,[$__57]);
             var $__59=
              new _A_($UHC.$Base.$M1__,[$__58]);
             var $__60=
              new _A_($UHC.$Base.$L1__,[$__59]);
             var $__61=
              new _A_($UHC.$Base.$L1__,[$__60]);
             var $__62=
              new _A_($UHC.$Base.$R1__,[$__61]);
             var $__63=
              new _A_($UHC.$Base.$M1__,[$__62]);
             $__swJSW90__0=
              $__63;
             break;}
          return $__swJSW90__0;});
$Text.$Read.$Lex.$__Rep0LexemeNEW789UNQ192SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Text.$Read.$Lex.$__Rep0LexemeNEW791UNQ193EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Text.$Read.$Lex.$__Rep0LexemeNEW791UNQ193EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Text.$Read.$Lex.$__Rep0LexemeDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Text.$Read.$Lex.$__Rep0LexemeDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Text.$Read.$Lex.$__Rep0LexemeUNQ192SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$__Rep0LexemeNEW789UNQ192SDCGENRepresentable0,[$Text.$Read.$Lex.$__Rep0LexemeUNQ192SDCGENRepresentable0]);}),[]);
$Text.$Read.$Lex.$__Rep0LexemeGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$__Rep0LexemeUNQ192SDCGENRepresentable0;}),[]);
$Text.$Read.$Lex.$__182__9795__2__7UNQ1098=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$UHC.$Base.$Eq__DCT74__56__0]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__6UNQ1099=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__7UNQ1098]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__5UNQ1100=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__6UNQ1099]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__42UNQ1105=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$UHC.$Base.$Eq_27__DCT74__389__0]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__40UNQ1107=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__74__268__0,[$UHC.$Base.$Eq__DCT74__130__0]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__39UNQ1108=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$Text.$Read.$Lex.$__182__9795__2__40UNQ1107]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__38UNQ1109=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__39UNQ1108]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__37UNQ1110=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__38UNQ1109]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__36UNQ1111=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__37UNQ1110,$Text.$Read.$Lex.$__182__9795__2__42UNQ1105]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__34UNQ1113=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$UHC.$Base.$Eq__DCT74__130__0]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__33UNQ1114=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__34UNQ1113]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__32UNQ1115=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__33UNQ1114]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__12UNQ1093=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__DCT74__394__0,[$UHC.$Base.$Eq__DCT74__56__0]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__11UNQ1094=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$Text.$Read.$Lex.$__182__9795__2__12UNQ1093]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__10UNQ1095=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__11UNQ1094]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__9UNQ1096=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__10UNQ1095]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__14UNQ1091=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__9UNQ1096,$Text.$Read.$Lex.$__182__9795__2__9UNQ1096]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__26UNQ1121=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__9UNQ1096,$Text.$Read.$Lex.$__182__9795__2__32UNQ1115]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__25UNQ1080=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__26UNQ1121,$Text.$Read.$Lex.$__182__9795__2__36UNQ1111]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__4UNQ1101=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__5UNQ1100,$Text.$Read.$Lex.$__182__9795__2__9UNQ1096]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__3UNQ1102=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__4UNQ1101,$Text.$Read.$Lex.$__182__9795__2__14UNQ1091]);}),[]);
$Text.$Read.$Lex.$__182__9795__2__2UNQ1103=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Text.$Read.$Lex.$__182__9795__2__3UNQ1102,$Text.$Read.$Lex.$__182__9795__2__25UNQ1080]);}),[]);
$Text.$Read.$Lex.$__182__9803__0__7__0UNQ1078=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Text.$Read.$Lex.$__182__9795__2__2UNQ1103]);}),[]);
$Text.$Read.$Lex.$__180__0__0DFLUHC_2eBase_2e_3d_3d=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$geqdefault,[$Text.$Read.$Lex.$__Rep0LexemeGENRepresentable0,$Text.$Read.$Lex.$__182__9803__0__7__0UNQ1078,$UHC.$Base.$undefined]);}),[]);
$Text.$Read.$Lex.$__180__0__0NEW820UNQ1077RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Text.$Read.$Lex.$__180__0__0NEW823UNQ1122EVLRDC,[$__,$__2]);
          return $__3;});
$Text.$Read.$Lex.$__180__0__0NEW823UNQ1122EVLRDC=
 new _F_(function($__,$__2)
         {var $Eq__=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$__]));
          var $__6=
           {_tag_:0,_1:$Eq__._1,_2:$__2};
          return $__6;});
$Text.$Read.$Lex.$__180__0__0UNQ1077RDC=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$__180__0__0NEW820UNQ1077RDC,[$Text.$Read.$Lex.$__180__0__0UNQ1077RDC,$Text.$Read.$Lex.$__180__0__0DFLUHC_2eBase_2e_3d_3d]);}),[]);
$Text.$Read.$Lex.$__180__0__0=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$__180__0__0UNQ1077RDC;}),[]);
$Text.$Read.$Lex.$_24okUNQ1025=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["build/101/ehclib/base/Text/Read/Lex.hs-cpp:142:14: monadic bind"]);
          var $__3=
           new _A_($UHC.$Base.$fail,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
          var $__4=
           _e_($_24x);
          var $__7=
           new _A_($Text.$Read.$Lex.$Char__,[$__4[0]]);
          var $__8=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__7]);
          var $__9=
           new _A_($Text.$ParserCombinators.$ReadP.$char,[39]);
          var $__10=
           new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$Eq__DCT74__56__0,$__4[0],39]);
          var $__12=
           new _A_($UHC.$Base.$_7c_7c,[$__4[1],$__11]);
          var $__13=
           new _A_($Control.$Monad.$guard,[$Text.$ParserCombinators.$ReadP.$MonadPlus__DCT168__6__0,$__12]);
          var $__14=
           new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__13,$__10]);
          return $__14;});
$Text.$Read.$Lex.$__184__1862=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$lexCharE,$Text.$Read.$Lex.$_24okUNQ1025]);}),[]);
$Text.$Read.$Lex.$__184__1861=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$char,[39]);}),[]);
$Text.$Read.$Lex.$lexLitChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$Read.$Lex.$__184__1861,$Text.$Read.$Lex.$__184__1862]);}),[]);
$Text.$Read.$Lex.$__184__1902=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexId,$Text.$Read.$Lex.$lexNumber]);}),[]);
$Text.$Read.$Lex.$__184__1900=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexSymbol,$Text.$Read.$Lex.$__184__1902]);}),[]);
$Text.$Read.$Lex.$__184__1898=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexPunc,$Text.$Read.$Lex.$__184__1900]);}),[]);
$Text.$Read.$Lex.$__184__1896=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexString,$Text.$Read.$Lex.$__184__1898]);}),[]);
$Text.$Read.$Lex.$__184__1894=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexLitChar,$Text.$Read.$Lex.$__184__1896]);}),[]);
$Text.$Read.$Lex.$lexToken=
 new _A_(new _F_(function()
                 {return new _A_($Text.$ParserCombinators.$ReadP.$_2b_2b_2b,[$Text.$Read.$Lex.$lexEOF,$Text.$Read.$Lex.$__184__1894]);}),[]);
$Text.$Read.$Lex.$_24okUNQ1066=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["build/101/ehclib/base/Text/Read/Lex.hs-cpp:69:18: monadic bind"]);
          var $__3=
           new _A_($UHC.$Base.$fail,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__]);
          var $__4=
           _e_($_24x);
          var $__7=
           new _A_($UHC.$Base.$return,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__4[0]]);
          return $__7;});
$Text.$Read.$Lex.$__184__1908=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Text.$ParserCombinators.$ReadP.$gather,[$Text.$Read.$Lex.$lexToken]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$__,$Text.$Read.$Lex.$_24okUNQ1066]);}),[]);
$Text.$Read.$Lex.$hsLex=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$skipSpaces,$Text.$Read.$Lex.$__184__1908]);}),[]);
$Text.$Read.$Lex.$lex=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Text.$ParserCombinators.$ReadP.$Monad__DCT168__4__0,$Text.$ParserCombinators.$ReadP.$skipSpaces,$Text.$Read.$Lex.$lexToken]);}),[]);
$Text.$Read.$Lex.$_24D__LexemeDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Text.Read.Lex"]);});
$Text.$Read.$Lex.$_24D__LexemeDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Lexeme"]);});
$Text.$Read.$Lex.$_24D__LexemeNEW861UNQ275SDCGENDatatype=
 new _F_(function($_24D__Lexeme)
         {var $_24D__Lexeme2=
           new _A_($Text.$Read.$Lex.$_24D__LexemeNEW863UNQ276EVLSDCGENDatatype,[$_24D__Lexeme]);
          return $_24D__Lexeme2;});
$Text.$Read.$Lex.$_24D__LexemeNEW863UNQ276EVLSDCGENDatatype=
 new _F_(function($_24D__Lexeme)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Lexeme]));
          var $__5=
           {_tag_:0,_1:$Text.$Read.$Lex.$_24D__LexemeDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Text.$Read.$Lex.$_24D__LexemeDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Text.$Read.$Lex.$_24D__LexemeUNQ275SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24D__LexemeNEW861UNQ275SDCGENDatatype,[$Text.$Read.$Lex.$_24D__LexemeUNQ275SDCGENDatatype]);}),[]);
$Text.$Read.$Lex.$_24D__LexemeGENDatatype=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24D__LexemeUNQ275SDCGENDatatype;}),[]);
$Text.$Read.$Lex.$_24C__SymbolDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Symbol"]);});
$Text.$Read.$Lex.$_24C__SymbolNEW869UNQ314SDCGENConstructor=
 new _F_(function($_24C__Symbol)
         {var $_24C__Symbol2=
           new _A_($Text.$Read.$Lex.$_24C__SymbolNEW871UNQ315EVLSDCGENConstructor,[$_24C__Symbol]);
          return $_24C__Symbol2;});
$Text.$Read.$Lex.$_24C__SymbolNEW871UNQ315EVLSDCGENConstructor=
 new _F_(function($_24C__Symbol)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Symbol]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__SymbolDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__SymbolUNQ314SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__SymbolNEW869UNQ314SDCGENConstructor,[$Text.$Read.$Lex.$_24C__SymbolUNQ314SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__SymbolGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__SymbolUNQ314SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__StringDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["String"]);});
$Text.$Read.$Lex.$_24C__StringNEW877UNQ290SDCGENConstructor=
 new _F_(function($_24C__String)
         {var $_24C__String2=
           new _A_($Text.$Read.$Lex.$_24C__StringNEW879UNQ291EVLSDCGENConstructor,[$_24C__String]);
          return $_24C__String2;});
$Text.$Read.$Lex.$_24C__StringNEW879UNQ291EVLSDCGENConstructor=
 new _F_(function($_24C__String)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__String]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__StringDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__StringUNQ290SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__StringNEW877UNQ290SDCGENConstructor,[$Text.$Read.$Lex.$_24C__StringUNQ290SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__StringGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__StringUNQ290SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__RatDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Rat"]);});
$Text.$Read.$Lex.$_24C__RatNEW885UNQ330SDCGENConstructor=
 new _F_(function($_24C__Rat)
         {var $_24C__Rat2=
           new _A_($Text.$Read.$Lex.$_24C__RatNEW887UNQ331EVLSDCGENConstructor,[$_24C__Rat]);
          return $_24C__Rat2;});
$Text.$Read.$Lex.$_24C__RatNEW887UNQ331EVLSDCGENConstructor=
 new _F_(function($_24C__Rat)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Rat]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__RatDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__RatUNQ330SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__RatNEW885UNQ330SDCGENConstructor,[$Text.$Read.$Lex.$_24C__RatUNQ330SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__RatGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__RatUNQ330SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__PuncDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Punc"]);});
$Text.$Read.$Lex.$_24C__PuncNEW893UNQ298SDCGENConstructor=
 new _F_(function($_24C__Punc)
         {var $_24C__Punc2=
           new _A_($Text.$Read.$Lex.$_24C__PuncNEW895UNQ299EVLSDCGENConstructor,[$_24C__Punc]);
          return $_24C__Punc2;});
$Text.$Read.$Lex.$_24C__PuncNEW895UNQ299EVLSDCGENConstructor=
 new _F_(function($_24C__Punc)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Punc]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__PuncDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__PuncUNQ298SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__PuncNEW893UNQ298SDCGENConstructor,[$Text.$Read.$Lex.$_24C__PuncUNQ298SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__PuncGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__PuncUNQ298SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__IntDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Int"]);});
$Text.$Read.$Lex.$_24C__IntNEW901UNQ322SDCGENConstructor=
 new _F_(function($_24C__Int)
         {var $_24C__Int2=
           new _A_($Text.$Read.$Lex.$_24C__IntNEW903UNQ323EVLSDCGENConstructor,[$_24C__Int]);
          return $_24C__Int2;});
$Text.$Read.$Lex.$_24C__IntNEW903UNQ323EVLSDCGENConstructor=
 new _F_(function($_24C__Int)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Int]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__IntDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__IntUNQ322SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__IntNEW901UNQ322SDCGENConstructor,[$Text.$Read.$Lex.$_24C__IntUNQ322SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__IntGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__IntUNQ322SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__IdentDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Ident"]);});
$Text.$Read.$Lex.$_24C__IdentNEW909UNQ306SDCGENConstructor=
 new _F_(function($_24C__Ident)
         {var $_24C__Ident2=
           new _A_($Text.$Read.$Lex.$_24C__IdentNEW911UNQ307EVLSDCGENConstructor,[$_24C__Ident]);
          return $_24C__Ident2;});
$Text.$Read.$Lex.$_24C__IdentNEW911UNQ307EVLSDCGENConstructor=
 new _F_(function($_24C__Ident)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Ident]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__IdentDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__IdentUNQ306SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__IdentNEW909UNQ306SDCGENConstructor,[$Text.$Read.$Lex.$_24C__IdentUNQ306SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__IdentGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__IdentUNQ306SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__EOFDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["EOF"]);});
$Text.$Read.$Lex.$_24C__EOFNEW917UNQ338SDCGENConstructor=
 new _F_(function($_24C__EOF)
         {var $_24C__EOF2=
           new _A_($Text.$Read.$Lex.$_24C__EOFNEW919UNQ339EVLSDCGENConstructor,[$_24C__EOF]);
          return $_24C__EOF2;});
$Text.$Read.$Lex.$_24C__EOFNEW919UNQ339EVLSDCGENConstructor=
 new _F_(function($_24C__EOF)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__EOF]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__EOFDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__EOFUNQ338SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__EOFNEW917UNQ338SDCGENConstructor,[$Text.$Read.$Lex.$_24C__EOFUNQ338SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__EOFGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__EOFUNQ338SDCGENConstructor;}),[]);
$Text.$Read.$Lex.$_24C__CharDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Char"]);});
$Text.$Read.$Lex.$_24C__CharNEW925UNQ282SDCGENConstructor=
 new _F_(function($_24C__Char)
         {var $_24C__Char2=
           new _A_($Text.$Read.$Lex.$_24C__CharNEW927UNQ283EVLSDCGENConstructor,[$_24C__Char]);
          return $_24C__Char2;});
$Text.$Read.$Lex.$_24C__CharNEW927UNQ283EVLSDCGENConstructor=
 new _F_(function($_24C__Char)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Char]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Text.$Read.$Lex.$_24C__CharDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Text.$Read.$Lex.$_24C__CharUNQ282SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Text.$Read.$Lex.$_24C__CharNEW925UNQ282SDCGENConstructor,[$Text.$Read.$Lex.$_24C__CharUNQ282SDCGENConstructor]);}),[]);
$Text.$Read.$Lex.$_24C__CharGENConstructor=
 new _A_(new _F_(function()
                 {return $Text.$Read.$Lex.$_24C__CharUNQ282SDCGENConstructor;}),[]);
