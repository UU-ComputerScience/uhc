// Data.Maybe
var $Data=
 ($Data ? $Data : {});
$Data.$Maybe=
 ($Data.$Maybe ? $Data.$Maybe : {});
$Data.$Maybe.$maybeToList=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW0__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW0__0=
              $__4;
             break;
            case 1:
             $__swJSW0__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW0__0;});
$Data.$Maybe.$mapMaybe=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW1__0;
          switch($x23._tag_)
           {case 0:
             var $rs=
              new _A_($Data.$Maybe.$mapMaybe,[$x1,$x23._2]);
             var $__=
              new _A_($x1,[$x23._1]);
             var $__8=
              _e_($__);
             var $__swJSW2__0;
             switch($__8._tag_)
              {case 0:
                var $__10=
                 new _A_($UHC.$Base.$_3a,[$__8._1,$rs]);
                $__swJSW2__0=
                 $__10;
                break;
               case 1:
                $__swJSW2__0=
                 $rs;
                break;}
             $__swJSW1__0=
              $__swJSW2__0;
             break;
            case 1:
             $__swJSW1__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW1__0;});
$Data.$Maybe.$listToMaybe=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW3__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$Just__,[$__._1]);
             $__swJSW3__0=
              $__5;
             break;
            case 1:
             $__swJSW3__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW3__0;});
$Data.$Maybe.$isNothing=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW4__0;
          switch($__._tag_)
           {case 0:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW4__0=
              $UHC.$Base.$True__;
             break;}
          return $__swJSW4__0;});
$Data.$Maybe.$isJust=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW5__0;
          switch($__._tag_)
           {case 0:
             $__swJSW5__0=
              $UHC.$Base.$True__;
             break;
            case 1:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW5__0;});
$Data.$Maybe.$fromMaybe=
 new _F_(function($d,$x)
         {var $__=
           _e_($x);
          var $__swJSW6__0;
          switch($__._tag_)
           {case 0:
             $__swJSW6__0=
              $__._1;
             break;
            case 1:
             $__swJSW6__0=
              $d;
             break;}
          return $__swJSW6__0;});
$Data.$Maybe.$fromJust=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW7__0;
          switch($__._tag_)
           {case 0:
             $__swJSW7__0=
              $__._1;
             break;
            case 1:
             var $__4=
              new _A_($UHC.$Base.$packedStringToString,["Maybe.fromJust: Nothing"]);
             var $__5=
              new _A_($UHC.$Base.$error,[$__4]);
             $__swJSW7__0=
              $__5;
             break;}
          return $__swJSW7__0;});
$Data.$Maybe.$_24okUNQ62=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW8__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW8__0=
              $__4;
             break;
            case 1:
             $__swJSW8__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW8__0;});
$Data.$Maybe.$catMaybes=
 new _F_(function($ls)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$Maybe.$_24okUNQ62,$ls]);});
