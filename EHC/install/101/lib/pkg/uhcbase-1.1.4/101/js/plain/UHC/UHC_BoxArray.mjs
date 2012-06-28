// UHC.BoxArray
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$BoxArray=
 ($UHC.$BoxArray ? $UHC.$BoxArray : {});
$UHC.$BoxArray.$indexArray=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          var $__5=
           _e_(primIndexArray($__3,$__4));
          return $__5;});
$UHC.$BoxArray.$primNewArray=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          return primNewArray($__3,$__2);});
$UHC.$BoxArray.$primSameArray=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primSameArray($__3,$__4);});
$UHC.$BoxArray.$primWriteArray=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          return primWriteArray($__4,$__5,$__3);});
$UHC.$BoxArray.$writeArray=
 new _F_(function($__,$i,$x,$s)
         {var $__5=
           new _A_($UHC.$BoxArray.$primWriteArray,[$__,$i,$x]);
          var $__6=
           _e_($__5);
          return $s;});
$UHC.$BoxArray.$unsafeThawArray=
 new _F_(function($a,$s)
         {var $__=
           _e_($a);
          return [$s,$a];});
$UHC.$BoxArray.$unsafeFreezeArray=
 new _F_(function($__,$s)
         {var $__3=
           _e_($__);
          return [$s,$__];});
$UHC.$BoxArray.$sameMutableArray=
 new _F_(function($__,$__2)
         {return new _A_($UHC.$BoxArray.$primSameArray,[$__,$__2]);});
$UHC.$BoxArray.$readArray=
 new _F_(function($__,$i,$s)
         {var $x=
           new _A_($UHC.$BoxArray.$indexArray,[$__,$i]);
          var $x5=
           _e_($x);
          return [$s,$x];});
$UHC.$BoxArray.$newArray=
 new _F_(function($i,$x,$s)
         {var $a=
           new _A_($UHC.$BoxArray.$primNewArray,[$i,$x]);
          var $a5=
           _e_($a);
          return [$s,$a];});
$UHC.$BoxArray.$__Rep1BoxArrayDFLUHC_2eBase_2eto1GENRepresentable1=
 new _F_(function($proj__1)
         {return $UHC.$Base.$undefined;});
$UHC.$BoxArray.$__Rep1BoxArrayDFLUHC_2eBase_2efrom1GENRepresentable1=
 new _F_(function($x)
         {return $UHC.$Base.$undefined;});
$UHC.$BoxArray.$__Rep1BoxArrayNEW29UNQ47SDCGENRepresentable1=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$BoxArray.$__Rep1BoxArrayNEW31UNQ48EVLSDCGENRepresentable1,[$__]);
          return $__2;});
$UHC.$BoxArray.$__Rep1BoxArrayNEW31UNQ48EVLSDCGENRepresentable1=
 new _F_(function($__)
         {var $Representable1__=
           _e_(new _A_($UHC.$Base.$Representable1__CLS74__370__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$__Rep1BoxArrayDFLUHC_2eBase_2efrom1GENRepresentable1,_2:$UHC.$BoxArray.$__Rep1BoxArrayDFLUHC_2eBase_2eto1GENRepresentable1};
          return $__5;});
$UHC.$BoxArray.$__Rep1BoxArrayUNQ47SDCGENRepresentable1=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$__Rep1BoxArrayNEW29UNQ47SDCGENRepresentable1,[$UHC.$BoxArray.$__Rep1BoxArrayUNQ47SDCGENRepresentable1]);}),[]);
$UHC.$BoxArray.$__Rep1BoxArrayGENRepresentable1=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$__Rep1BoxArrayUNQ47SDCGENRepresentable1;}),[]);
$UHC.$BoxArray.$__Rep0BoxArrayDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {return $UHC.$Base.$undefined;});
$UHC.$BoxArray.$__Rep0BoxArrayDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {return $UHC.$Base.$undefined;});
$UHC.$BoxArray.$__Rep0BoxArrayNEW38UNQ38SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$BoxArray.$__Rep0BoxArrayNEW40UNQ39EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$BoxArray.$__Rep0BoxArrayNEW40UNQ39EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$__Rep0BoxArrayDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$BoxArray.$__Rep0BoxArrayDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$BoxArray.$__Rep0BoxArrayUNQ38SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$__Rep0BoxArrayNEW38UNQ38SDCGENRepresentable0,[$UHC.$BoxArray.$__Rep0BoxArrayUNQ38SDCGENRepresentable0]);}),[]);
$UHC.$BoxArray.$__Rep0BoxArrayGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$__Rep0BoxArrayUNQ38SDCGENRepresentable0;}),[]);
$UHC.$BoxArray.$MutableBoxArray__=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$BoxArray.$__Rep0MutableBoxArrayDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {return new _A_($UHC.$BoxArray.$MutableBoxArray__,[$proj__1]);});
$UHC.$BoxArray.$__Rep0MutableBoxArrayDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$K1__,[$x]);
          var $__3=
           new _A_($UHC.$Base.$M1__,[$__]);
          var $__4=
           new _A_($UHC.$Base.$M1__,[$__3]);
          return new _A_($UHC.$Base.$M1__,[$__4]);});
$UHC.$BoxArray.$__Rep0MutableBoxArrayNEW51UNQ63SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$BoxArray.$__Rep0MutableBoxArrayNEW53UNQ64EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$BoxArray.$__Rep0MutableBoxArrayNEW53UNQ64EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$__Rep0MutableBoxArrayDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$BoxArray.$__Rep0MutableBoxArrayDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$BoxArray.$__Rep0MutableBoxArrayUNQ63SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$__Rep0MutableBoxArrayNEW51UNQ63SDCGENRepresentable0,[$UHC.$BoxArray.$__Rep0MutableBoxArrayUNQ63SDCGENRepresentable0]);}),[]);
$UHC.$BoxArray.$__Rep0MutableBoxArrayGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$__Rep0MutableBoxArrayUNQ63SDCGENRepresentable0;}),[]);
$UHC.$BoxArray.$__Rep1MutableBoxArrayDFLUHC_2eBase_2eto1GENRepresentable1=
 new _F_(function($proj__1)
         {return new _A_($UHC.$BoxArray.$MutableBoxArray__,[$proj__1]);});
$UHC.$BoxArray.$__Rep1MutableBoxArrayDFLUHC_2eBase_2efrom1GENRepresentable1=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$Rec1__,[$x]);
          var $__3=
           new _A_($UHC.$Base.$M1__,[$__]);
          var $__4=
           new _A_($UHC.$Base.$M1__,[$__3]);
          return new _A_($UHC.$Base.$M1__,[$__4]);});
$UHC.$BoxArray.$__Rep1MutableBoxArrayNEW63UNQ80SDCGENRepresentable1=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$BoxArray.$__Rep1MutableBoxArrayNEW65UNQ81EVLSDCGENRepresentable1,[$__]);
          return $__2;});
$UHC.$BoxArray.$__Rep1MutableBoxArrayNEW65UNQ81EVLSDCGENRepresentable1=
 new _F_(function($__)
         {var $Representable1__=
           _e_(new _A_($UHC.$Base.$Representable1__CLS74__370__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$__Rep1MutableBoxArrayDFLUHC_2eBase_2efrom1GENRepresentable1,_2:$UHC.$BoxArray.$__Rep1MutableBoxArrayDFLUHC_2eBase_2eto1GENRepresentable1};
          return $__5;});
$UHC.$BoxArray.$__Rep1MutableBoxArrayUNQ80SDCGENRepresentable1=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$__Rep1MutableBoxArrayNEW63UNQ80SDCGENRepresentable1,[$UHC.$BoxArray.$__Rep1MutableBoxArrayUNQ80SDCGENRepresentable1]);}),[]);
$UHC.$BoxArray.$__Rep1MutableBoxArrayGENRepresentable1=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$__Rep1MutableBoxArrayUNQ80SDCGENRepresentable1;}),[]);
$UHC.$BoxArray.$_24D__MutableBoxArrayDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["UHC.BoxArray"]);});
$UHC.$BoxArray.$_24D__MutableBoxArrayDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["MutableBoxArray"]);});
$UHC.$BoxArray.$_24D__MutableBoxArrayNEW72UNQ97SDCGENDatatype=
 new _F_(function($_24D__MutableBoxArray)
         {var $_24D__MutableBoxArray2=
           new _A_($UHC.$BoxArray.$_24D__MutableBoxArrayNEW74UNQ98EVLSDCGENDatatype,[$_24D__MutableBoxArray]);
          return $_24D__MutableBoxArray2;});
$UHC.$BoxArray.$_24D__MutableBoxArrayNEW74UNQ98EVLSDCGENDatatype=
 new _F_(function($_24D__MutableBoxArray)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__MutableBoxArray]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$_24D__MutableBoxArrayDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$UHC.$BoxArray.$_24D__MutableBoxArrayDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$UHC.$BoxArray.$_24D__MutableBoxArrayUNQ97SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$_24D__MutableBoxArrayNEW72UNQ97SDCGENDatatype,[$UHC.$BoxArray.$_24D__MutableBoxArrayUNQ97SDCGENDatatype]);}),[]);
$UHC.$BoxArray.$_24D__MutableBoxArrayGENDatatype=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$_24D__MutableBoxArrayUNQ97SDCGENDatatype;}),[]);
$UHC.$BoxArray.$_24D__BoxArrayDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["UHC.BoxArray"]);});
$UHC.$BoxArray.$_24D__BoxArrayDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["BoxArray"]);});
$UHC.$BoxArray.$_24D__BoxArrayNEW81UNQ56SDCGENDatatype=
 new _F_(function($_24D__BoxArray)
         {var $_24D__BoxArray2=
           new _A_($UHC.$BoxArray.$_24D__BoxArrayNEW83UNQ57EVLSDCGENDatatype,[$_24D__BoxArray]);
          return $_24D__BoxArray2;});
$UHC.$BoxArray.$_24D__BoxArrayNEW83UNQ57EVLSDCGENDatatype=
 new _F_(function($_24D__BoxArray)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__BoxArray]));
          var $__5=
           {_tag_:0,_1:$UHC.$BoxArray.$_24D__BoxArrayDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$UHC.$BoxArray.$_24D__BoxArrayDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$UHC.$BoxArray.$_24D__BoxArrayUNQ56SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$_24D__BoxArrayNEW81UNQ56SDCGENDatatype,[$UHC.$BoxArray.$_24D__BoxArrayUNQ56SDCGENDatatype]);}),[]);
$UHC.$BoxArray.$_24D__BoxArrayGENDatatype=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$_24D__BoxArrayUNQ56SDCGENDatatype;}),[]);
$UHC.$BoxArray.$_24C__MutableBoxArrayDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["MutableBoxArray"]);});
$UHC.$BoxArray.$_24C__MutableBoxArrayNEW89UNQ104SDCGENConstructor=
 new _F_(function($_24C__MutableBoxArray)
         {var $_24C__MutableBoxArray2=
           new _A_($UHC.$BoxArray.$_24C__MutableBoxArrayNEW91UNQ105EVLSDCGENConstructor,[$_24C__MutableBoxArray]);
          return $_24C__MutableBoxArray2;});
$UHC.$BoxArray.$_24C__MutableBoxArrayNEW91UNQ105EVLSDCGENConstructor=
 new _F_(function($_24C__MutableBoxArray)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__MutableBoxArray]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$UHC.$BoxArray.$_24C__MutableBoxArrayDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$UHC.$BoxArray.$_24C__MutableBoxArrayUNQ104SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$BoxArray.$_24C__MutableBoxArrayNEW89UNQ104SDCGENConstructor,[$UHC.$BoxArray.$_24C__MutableBoxArrayUNQ104SDCGENConstructor]);}),[]);
$UHC.$BoxArray.$_24C__MutableBoxArrayGENConstructor=
 new _A_(new _F_(function()
                 {return $UHC.$BoxArray.$_24C__MutableBoxArrayUNQ104SDCGENConstructor;}),[]);
