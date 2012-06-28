// UHC.OldIO
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$OldIO=
 ($UHC.$OldIO ? $UHC.$OldIO : {});
$UHC.$OldIO.$primOpenFileOrStd=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__3);
          return primOpenFileOrStd($__4,$__5,$__6);});
$UHC.$OldIO.$primHPutChar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primHPutChar($__3,$__4);});
$UHC.$OldIO.$primHPutByteArray=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primHPutByteArray($__3,$__4);});
$UHC.$OldIO.$primHGetContents=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primHGetContents($__2);});
$UHC.$OldIO.$primHGetChar=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primHGetChar($__2);});
$UHC.$OldIO.$primHFlush=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primHFlush($__2);});
$UHC.$OldIO.$primHClose=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primHClose($__2);});
$UHC.$OldIO.$__204__54=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["stdout"]);}),[]);
$UHC.$OldIO.$__204__53=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$JSHandle__,[$UHC.$OldIO.$__204__54]);}),[]);
$UHC.$OldIO.$stdout=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$OldHandle__,[$UHC.$OldIO.$__204__53]);}),[]);
$UHC.$OldIO.$__204__58=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["stderr"]);}),[]);
$UHC.$OldIO.$__204__57=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$JSHandle__,[$UHC.$OldIO.$__204__58]);}),[]);
$UHC.$OldIO.$stderr=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$OldHandle__,[$UHC.$OldIO.$__204__57]);}),[]);
$UHC.$OldIO.$__204__65__0=
 new _F_(function($h,$c,$__)
         {return new _A_($UHC.$OldIO.$primHPutChar,[$h,$c]);});
$UHC.$OldIO.$hPutChar=
 new _F_(function($h,$c)
         {var $__=
           new _A_($UHC.$OldIO.$__204__65__0,[$h,$c]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$putChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$OldIO.$hPutChar,[$UHC.$OldIO.$stdout]);}),[]);
$UHC.$OldIO.$__204__75__0=
 new _F_(function($h,$__)
         {return new _A_($UHC.$OldIO.$primHGetContents,[$h]);});
$UHC.$OldIO.$hGetContents=
 new _F_(function($h)
         {var $__=
           new _A_($UHC.$OldIO.$__204__75__0,[$h]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$__204__82__0=
 new _F_(function($h,$__)
         {return new _A_($UHC.$OldIO.$primHGetChar,[$h]);});
$UHC.$OldIO.$hGetChar=
 new _F_(function($h)
         {var $__=
           new _A_($UHC.$OldIO.$__204__82__0,[$h]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$__204__89__0=
 new _F_(function($h,$__)
         {return new _A_($UHC.$OldIO.$primHFlush,[$h]);});
$UHC.$OldIO.$hFlush=
 new _F_(function($h)
         {var $__=
           new _A_($UHC.$OldIO.$__204__89__0,[$h]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$__204__96__0=
 new _F_(function($h,$__)
         {return new _A_($UHC.$OldIO.$primHClose,[$h]);});
$UHC.$OldIO.$hClose=
 new _F_(function($h)
         {var $__=
           new _A_($UHC.$OldIO.$__204__96__0,[$h]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$hPutStr=
 new _F_(function($h,$s)
         {var $__=
           new _A_($UHC.$Base.$null,[$s]);
          var $__4=
           _e_($__);
          var $__swJSW0__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$tail,[$s]);
             var $__6=
              new _A_($UHC.$OldIO.$hPutStr,[$h,$__5]);
             var $__7=
              new _A_($UHC.$Base.$head,[$s]);
             var $__8=
              new _A_($UHC.$OldIO.$hPutChar,[$h,$__7]);
             var $__9=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__6]);
             $__swJSW0__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             $__swJSW0__0=
              $__10;
             break;}
          return $__swJSW0__0;});
$UHC.$OldIO.$hPutStrLn=
 new _F_(function($h,$s)
         {var $__=
           new _A_($UHC.$OldIO.$hPutChar,[$h,10]);
          var $__4=
           new _A_($UHC.$OldIO.$hPutStr,[$h,$s]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$UHC.$OldIO.$hPrint=
 new _F_(function($__,$h)
         {var $__3=
           new _A_($UHC.$Base.$show,[$__]);
          var $__4=
           new _A_($UHC.$OldIO.$hPutStrLn,[$h]);
          return new _A_($UHC.$Base.$_2e,[$__4,$__3]);});
$UHC.$OldIO.$print=
 new _F_(function($__)
         {return new _A_($UHC.$OldIO.$hPrint,[$__,$UHC.$OldIO.$stdout]);});
$UHC.$OldIO.$putStrLn=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$OldIO.$hPutStrLn,[$UHC.$OldIO.$stdout]);}),[]);
$UHC.$OldIO.$putStr=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$OldIO.$hPutStr,[$UHC.$OldIO.$stdout]);}),[]);
