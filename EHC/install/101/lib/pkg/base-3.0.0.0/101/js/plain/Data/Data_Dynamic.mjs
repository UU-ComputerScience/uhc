// Data.Dynamic
var $Data=
 ($Data ? $Data : {});
$Data.$Dynamic=
 ($Data.$Dynamic ? $Data.$Dynamic : {});
$Data.$Dynamic.$__64__2=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Dynamic"]);}),[]);
$Data.$Dynamic.$dynamicTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Data.$Dynamic.$__64__2]);}),[]);
$Data.$Dynamic.$dynTypeRep=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Data.$Dynamic.$fromDynamic=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$unsafeCoerce,[$__3._2]);
          var $__7=
           new _A_($Data.$Typeable.$typeOf,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3d_3d,[$Data.$Typeable.$Eq__DCT320__0__0,$__3._1,$__7]);
          var $__9=
           _e_($__8);
          var $__swJSW2__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW3__0;
             switch($__10._tag_)
              {case 0:
                $__swJSW3__0=
                 $UHC.$Base.$undefined;
                break;
               case 1:
                $__swJSW3__0=
                 $UHC.$Base.$Nothing__;
                break;}
             $__swJSW2__0=
              $__swJSW3__0;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$Just__,[$__6]);
             $__swJSW2__0=
              $__11;
             break;}
          return $__swJSW2__0;});
$Data.$Dynamic.$__64__35__0=
 new _F_(function($__,$t,$v,$def)
         {var $__5=
           new _A_($Data.$Typeable.$typeOf,[$__,$def]);
          var $__6=
           new _A_($UHC.$Base.$_3d_3d,[$Data.$Typeable.$Eq__DCT320__0__0,$__5,$t]);
          var $__7=
           _e_($__6);
          var $__swJSW4__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW5__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 61_6_0"]);
                var $__10=
                 new _A_($UHC.$Base.$error,[$__9]);
                $__swJSW5__0=
                 $__10;
                break;
               case 1:
                $__swJSW5__0=
                 $def;
                break;}
             $__swJSW4__0=
              $__swJSW5__0;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$unsafeCoerce,[$v]);
             $__swJSW4__0=
              $__11;
             break;}
          return $__swJSW4__0;});
$Data.$Dynamic.$fromDyn=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Dynamic.$__64__35__0,[$__,$__3._1,$__3._2]);});
$Data.$Dynamic.$Typeable__DCT60__1__0DFLData_2eTypeable_2etypeOf=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Data.$Dynamic.$dynamicTc,$UHC.$Base.$_5b_5d]);});
$Data.$Dynamic.$Typeable__NEW24UNQ163DCT60__1__0RDC=
 new _F_(function($Typeable__)
         {var $Typeable__2=
           new _A_($Data.$Dynamic.$Typeable__NEW26UNQ164EVLDCT60__1__0RDC,[$Typeable__]);
          return $Typeable__2;});
$Data.$Dynamic.$Typeable__NEW26UNQ164EVLDCT60__1__0RDC=
 new _F_(function($Typeable__)
         {var $Typeable__2=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__4=
           {_tag_:0,_1:$Data.$Dynamic.$Typeable__DCT60__1__0DFLData_2eTypeable_2etypeOf};
          return $__4;});
$Data.$Dynamic.$Typeable__UNQ163DCT60__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$Typeable__NEW24UNQ163DCT60__1__0RDC,[$Data.$Dynamic.$Typeable__UNQ163DCT60__1__0RDC]);}),[]);
$Data.$Dynamic.$Typeable__DCT60__1__0=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$Typeable__UNQ163DCT60__1__0RDC;}),[]);
$Data.$Dynamic.$Show__DCT60__2__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,[">>"]);
          var $__7=
           new _A_($UHC.$Base.$showString,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$showsPrec,[$Data.$Typeable.$Show__DCT320__4__0,0,$__3._1]);
          var $__9=
           new _A_($UHC.$Base.$_2e,[$__8,$__7]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToString,["<<"]);
          var $__11=
           new _A_($UHC.$Base.$showString,[$__10]);
          var $__12=
           new _A_($UHC.$Base.$_2e,[$__11,$__9]);
          return $__12;});
$Data.$Dynamic.$Show__NEW40UNQ149DCT60__2__0RDC=
 new _F_(function($Show__)
         {var $Show__2=
           new _A_($Data.$Dynamic.$Show__NEW42UNQ152EVLDCT60__2__0RDC,[$Show__]);
          return $Show__2;});
$Data.$Dynamic.$Show__NEW42UNQ152EVLDCT60__2__0RDC=
 new _F_(function($Show__)
         {var $Show__2=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__6=
           {_tag_:0,_1:$Show__2._1,_2:$Show__2._2,_3:$Data.$Dynamic.$Show__DCT60__2__0DFLUHC_2eBase_2eshowsPrec};
          return $__6;});
$Data.$Dynamic.$Show__UNQ149DCT60__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$Show__NEW40UNQ149DCT60__2__0RDC,[$Data.$Dynamic.$Show__UNQ149DCT60__2__0RDC]);}),[]);
$Data.$Dynamic.$Show__DCT60__2__0=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$Show__UNQ149DCT60__2__0RDC;}),[]);
$Data.$Dynamic.$Obj__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$Data.$Dynamic.$__Rep0ObjDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          return $Data.$Dynamic.$Obj__;});
$Data.$Dynamic.$__Rep0ObjDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__=
           new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
          var $__4=
           new _A_($UHC.$Base.$M1__,[$__]);
          return $__4;});
$Data.$Dynamic.$__Rep0ObjNEW54UNQ22SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Dynamic.$__Rep0ObjNEW56UNQ23EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Data.$Dynamic.$__Rep0ObjNEW56UNQ23EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Data.$Dynamic.$__Rep0ObjDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Data.$Dynamic.$__Rep0ObjDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Data.$Dynamic.$__Rep0ObjUNQ22SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$__Rep0ObjNEW54UNQ22SDCGENRepresentable0,[$Data.$Dynamic.$__Rep0ObjUNQ22SDCGENRepresentable0]);}),[]);
$Data.$Dynamic.$__Rep0ObjGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$__Rep0ObjUNQ22SDCGENRepresentable0;}),[]);
$Data.$Dynamic.$Dynamic__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$Data.$Dynamic.$__Rep0DynamicDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Data.$Dynamic.$Dynamic__,[$proj__3._1,$proj__3._2]);
          return $__;});
$Data.$Dynamic.$__Rep0DynamicDFLUHC_2eBase_2efrom0GENRepresentable0=
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
$Data.$Dynamic.$__Rep0DynamicNEW74UNQ52SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Dynamic.$__Rep0DynamicNEW76UNQ53EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Data.$Dynamic.$__Rep0DynamicNEW76UNQ53EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Data.$Dynamic.$__Rep0DynamicDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Data.$Dynamic.$__Rep0DynamicDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Data.$Dynamic.$__Rep0DynamicUNQ52SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$__Rep0DynamicNEW74UNQ52SDCGENRepresentable0,[$Data.$Dynamic.$__Rep0DynamicUNQ52SDCGENRepresentable0]);}),[]);
$Data.$Dynamic.$__Rep0DynamicGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$__Rep0DynamicUNQ52SDCGENRepresentable0;}),[]);
$Data.$Dynamic.$__64__150__0=
 new _F_(function($f,$t1,$__)
         {var $__4=
           _e_($__);
          var $__7=
           new _A_($Data.$Typeable.$funResultTy,[$t1,$__4._1]);
          var $__8=
           _e_($__7);
          var $__swJSW17__0;
          switch($__8._tag_)
           {case 0:
             var $__10=
              new _A_($UHC.$Base.$unsafeCoerce,[$f,$__4._2]);
             var $__11=
              new _A_($Data.$Dynamic.$Dynamic__,[$__8._1,$__10]);
             var $__12=
              new _A_($UHC.$Base.$Just__,[$__11]);
             $__swJSW17__0=
              $__12;
             break;
            case 1:
             $__swJSW17__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW17__0;});
$Data.$Dynamic.$dynApply=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$Dynamic.$__64__150__0,[$__2._2,$__2._1]);});
$Data.$Dynamic.$dynApp=
 new _F_(function($f,$x)
         {var $__=
           new _A_($Data.$Dynamic.$dynApply,[$f,$x]);
          var $__4=
           _e_($__);
          var $__swJSW19__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW19__0=
              $__4._1;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$show,[$Data.$Dynamic.$Show__DCT60__2__0,$x]);
             var $__7=
              new _A_($UHC.$Base.$packedStringToString,[" to argument "]);
             var $__8=
              new _A_($UHC.$Base.$_2b_2b,[$__7,$__6]);
             var $__9=
              new _A_($UHC.$Base.$show,[$Data.$Dynamic.$Show__DCT60__2__0,$f]);
             var $__10=
              new _A_($UHC.$Base.$_2b_2b,[$__9,$__8]);
             var $__11=
              new _A_($UHC.$Base.$packedStringToString,["Can't apply function "]);
             var $__12=
              new _A_($UHC.$Base.$_2b_2b,[$__11,$__10]);
             var $__13=
              new _A_($UHC.$Base.$packedStringToString,["Type error in dynamic application.\n"]);
             var $__14=
              new _A_($UHC.$Base.$_2b_2b,[$__13,$__12]);
             var $__15=
              new _A_($UHC.$Base.$error,[$__14]);
             $__swJSW19__0=
              $__15;
             break;}
          return $__swJSW19__0;});
$Data.$Dynamic.$toDyn=
 new _F_(function($__,$v)
         {var $__3=
           new _A_($UHC.$Base.$unsafeCoerce,[$v]);
          var $__4=
           new _A_($Data.$Typeable.$typeOf,[$__,$v]);
          return new _A_($Data.$Dynamic.$Dynamic__,[$__4,$__3]);});
$Data.$Dynamic.$_24D__ObjDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Data.Dynamic"]);});
$Data.$Dynamic.$_24D__ObjDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Obj"]);});
$Data.$Dynamic.$_24D__ObjNEW108UNQ35SDCGENDatatype=
 new _F_(function($_24D__Obj)
         {var $_24D__Obj2=
           new _A_($Data.$Dynamic.$_24D__ObjNEW110UNQ36EVLSDCGENDatatype,[$_24D__Obj]);
          return $_24D__Obj2;});
$Data.$Dynamic.$_24D__ObjNEW110UNQ36EVLSDCGENDatatype=
 new _F_(function($_24D__Obj)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Obj]));
          var $__5=
           {_tag_:0,_1:$Data.$Dynamic.$_24D__ObjDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Data.$Dynamic.$_24D__ObjDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Data.$Dynamic.$_24D__ObjUNQ35SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$_24D__ObjNEW108UNQ35SDCGENDatatype,[$Data.$Dynamic.$_24D__ObjUNQ35SDCGENDatatype]);}),[]);
$Data.$Dynamic.$_24D__ObjGENDatatype=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$_24D__ObjUNQ35SDCGENDatatype;}),[]);
$Data.$Dynamic.$_24D__DynamicDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Data.Dynamic"]);});
$Data.$Dynamic.$_24D__DynamicDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Dynamic"]);});
$Data.$Dynamic.$_24D__DynamicNEW117UNQ77SDCGENDatatype=
 new _F_(function($_24D__Dynamic)
         {var $_24D__Dynamic2=
           new _A_($Data.$Dynamic.$_24D__DynamicNEW119UNQ78EVLSDCGENDatatype,[$_24D__Dynamic]);
          return $_24D__Dynamic2;});
$Data.$Dynamic.$_24D__DynamicNEW119UNQ78EVLSDCGENDatatype=
 new _F_(function($_24D__Dynamic)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Dynamic]));
          var $__5=
           {_tag_:0,_1:$Data.$Dynamic.$_24D__DynamicDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Data.$Dynamic.$_24D__DynamicDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Data.$Dynamic.$_24D__DynamicUNQ77SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$_24D__DynamicNEW117UNQ77SDCGENDatatype,[$Data.$Dynamic.$_24D__DynamicUNQ77SDCGENDatatype]);}),[]);
$Data.$Dynamic.$_24D__DynamicGENDatatype=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$_24D__DynamicUNQ77SDCGENDatatype;}),[]);
$Data.$Dynamic.$_24C__ObjDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Obj"]);});
$Data.$Dynamic.$_24C__ObjNEW125UNQ42SDCGENConstructor=
 new _F_(function($_24C__Obj)
         {var $_24C__Obj2=
           new _A_($Data.$Dynamic.$_24C__ObjNEW127UNQ43EVLSDCGENConstructor,[$_24C__Obj]);
          return $_24C__Obj2;});
$Data.$Dynamic.$_24C__ObjNEW127UNQ43EVLSDCGENConstructor=
 new _F_(function($_24C__Obj)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Obj]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Data.$Dynamic.$_24C__ObjDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Data.$Dynamic.$_24C__ObjUNQ42SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$_24C__ObjNEW125UNQ42SDCGENConstructor,[$Data.$Dynamic.$_24C__ObjUNQ42SDCGENConstructor]);}),[]);
$Data.$Dynamic.$_24C__ObjGENConstructor=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$_24C__ObjUNQ42SDCGENConstructor;}),[]);
$Data.$Dynamic.$_24C__DynamicDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Dynamic"]);});
$Data.$Dynamic.$_24C__DynamicNEW133UNQ84SDCGENConstructor=
 new _F_(function($_24C__Dynamic)
         {var $_24C__Dynamic2=
           new _A_($Data.$Dynamic.$_24C__DynamicNEW135UNQ85EVLSDCGENConstructor,[$_24C__Dynamic]);
          return $_24C__Dynamic2;});
$Data.$Dynamic.$_24C__DynamicNEW135UNQ85EVLSDCGENConstructor=
 new _F_(function($_24C__Dynamic)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Dynamic]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Constructor__._2,_3:$Constructor__._3,_4:$Data.$Dynamic.$_24C__DynamicDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Data.$Dynamic.$_24C__DynamicUNQ84SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Dynamic.$_24C__DynamicNEW133UNQ84SDCGENConstructor,[$Data.$Dynamic.$_24C__DynamicUNQ84SDCGENConstructor]);}),[]);
$Data.$Dynamic.$_24C__DynamicGENConstructor=
 new _A_(new _F_(function()
                 {return $Data.$Dynamic.$_24C__DynamicUNQ84SDCGENConstructor;}),[]);
