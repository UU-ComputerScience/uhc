// System.IO.Error
var $System=
 ($System ? $System : {});
$System.$IO=
 ($System.$IO ? $System.$IO : {});
$System.$IO.$Error=
 ($System.$IO.$Error ? $System.$IO.$Error : {});
$System.$IO.$Error.$userErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$UserError__;}),[]);
$System.$IO.$Error.$permissionErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$PermissionDenied__;}),[]);
$System.$IO.$Error.$__306__9__0=
 new _F_(function($f,$e)
         {var $__=
           new _A_($f,[$e]);
          return new _A_($UHC.$IOBase.$ioError,[$__]);});
$System.$IO.$Error.$modifyIOError=
 new _F_(function($f,$io)
         {var $__=
           new _A_($System.$IO.$Error.$__306__9__0,[$f]);
          return new _A_($UHC.$IOBase.$catch,[$io,$__]);});
$System.$IO.$Error.$mkIOError=
 new _F_(function($t,$location,$maybe__hdl,$maybe__filename)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          return {_tag_:0,ioe__handle:$maybe__hdl,ioe__type:$t,ioe__location:$location,ioe__description:$__,ioe__filename:$maybe__filename};});
$System.$IO.$Error.$isUserErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW0__0;
          switch($__._tag_)
           {case 0:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW0__0=
              $UHC.$Base.$True__;
             break;}
          return $__swJSW0__0;});
$System.$IO.$Error.$isPermissionErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW1__0;
          switch($__._tag_)
           {case 0:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW1__0=
              $UHC.$Base.$True__;
             break;
            case 12:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW1__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW1__0;});
$System.$IO.$Error.$isIllegalOperationErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW2__0;
          switch($__._tag_)
           {case 0:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW2__0=
              $UHC.$Base.$True__;
             break;
            case 6:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW2__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW2__0;});
$System.$IO.$Error.$isFullErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW3__0;
          switch($__._tag_)
           {case 0:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW3__0=
              $UHC.$Base.$True__;
             break;
            case 14:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW3__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW3__0;});
$System.$IO.$Error.$isEOFErrorType=
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
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW4__0=
              $UHC.$Base.$True__;
             break;
            case 4:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW4__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW4__0;});
$System.$IO.$Error.$isDoesNotExistErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW5__0;
          switch($__._tag_)
           {case 0:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW5__0=
              $UHC.$Base.$True__;
             break;
            case 10:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW5__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW5__0;});
$System.$IO.$Error.$isAlreadyInUseErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW6__0;
          switch($__._tag_)
           {case 0:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW6__0=
              $UHC.$Base.$True__;
             break;
            case 13:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW6__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW6__0;});
$System.$IO.$Error.$isAlreadyExistsErrorType=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW7__0;
          switch($__._tag_)
           {case 0:
             $__swJSW7__0=
              $UHC.$Base.$True__;
             break;
            case 1:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 2:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 3:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 4:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 5:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 6:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 7:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 8:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 9:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 10:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 11:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 12:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 13:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 14:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;
            case 15:
             $__swJSW7__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW7__0;});
$System.$IO.$Error.$ioeSetLocation=
 new _F_(function($ioe,$str)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[UHC.IOBase.ioe_location]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($ioe);
          var $__11=
           {_tag_:0,ioe__handle:$__5.ioe__handle,ioe__type:$__5.ioe__type,ioe__location:$str,ioe__description:$__5.ioe__description,ioe__filename:$__5.ioe__filename};
          return $__11;});
$System.$IO.$Error.$ioeSetHandle=
 new _F_(function($ioe,$hdl)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[UHC.IOBase.ioe_handle]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           new _A_($UHC.$Base.$Just__,[$hdl]);
          var $__6=
           _e_($ioe);
          var $__12=
           {_tag_:0,ioe__handle:$__5,ioe__type:$__6.ioe__type,ioe__location:$__6.ioe__location,ioe__description:$__6.ioe__description,ioe__filename:$__6.ioe__filename};
          return $__12;});
$System.$IO.$Error.$ioeSetFileName=
 new _F_(function($ioe,$filename)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[UHC.IOBase.ioe_filename]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           new _A_($UHC.$Base.$Just__,[$filename]);
          var $__6=
           _e_($ioe);
          var $__12=
           {_tag_:0,ioe__handle:$__6.ioe__handle,ioe__type:$__6.ioe__type,ioe__location:$__6.ioe__location,ioe__description:$__6.ioe__description,ioe__filename:$__5};
          return $__12;});
$System.$IO.$Error.$ioeSetErrorType=
 new _F_(function($ioe,$errtype)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[UHC.IOBase.ioe_type]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($ioe);
          var $__11=
           {_tag_:0,ioe__handle:$__5.ioe__handle,ioe__type:$errtype,ioe__location:$__5.ioe__location,ioe__description:$__5.ioe__description,ioe__filename:$__5.ioe__filename};
          return $__11;});
$System.$IO.$Error.$ioeSetErrorString=
 new _F_(function($ioe,$str)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[UHC.IOBase.ioe_description]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($ioe);
          var $__11=
           {_tag_:0,ioe__handle:$__5.ioe__handle,ioe__type:$__5.ioe__type,ioe__location:$__5.ioe__location,ioe__description:$str,ioe__filename:$__5.ioe__filename};
          return $__11;});
$System.$IO.$Error.$ioeGetLocation=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ioe__location;}),[]);
$System.$IO.$Error.$ioeGetHandle=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ioe__handle;}),[]);
$System.$IO.$Error.$ioeGetFileName=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ioe__filename;}),[]);
$System.$IO.$Error.$ioeGetErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ioe__type;}),[]);
$System.$IO.$Error.$isAlreadyExistsError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isAlreadyExistsErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isAlreadyInUseError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isAlreadyInUseErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isDoesNotExistError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isDoesNotExistErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isEOFError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isEOFErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isFullError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isFullErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isIllegalOperation=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isIllegalOperationErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isPermissionError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isPermissionErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$isUserError=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$System.$IO.$Error.$isUserErrorType,$System.$IO.$Error.$ioeGetErrorType]);}),[]);
$System.$IO.$Error.$illegalOperationErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$IllegalOperation__;}),[]);
$System.$IO.$Error.$fullErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ResourceExhausted__;}),[]);
$System.$IO.$Error.$eofErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$EOF__;}),[]);
$System.$IO.$Error.$doesNotExistErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$NoSuchThing__;}),[]);
$System.$IO.$Error.$__306__185__0=
 new _F_(function($errTy,$ohdl,$opath,$str,$loc,$hdl,$path)
         {var $__=
           new _A_($System.$IO.$Error.$mplusUNQ144,[$path,$opath]);
          var $__9=
           new _A_($System.$IO.$Error.$mplusUNQ144,[$hdl,$ohdl]);
          return new _A_($UHC.$IOBase.$IOError__,[$__9,$errTy,$loc,$str,$__]);});
$System.$IO.$Error.$mplusUNQ144=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW13__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW13__0=
              $x1;
             break;
            case 1:
             $__swJSW13__0=
              $x2;
             break;}
          return $__swJSW13__0;});
$System.$IO.$Error.$annotateIOError=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($System.$IO.$Error.$__306__185__0,[$__2.ioe__type,$__2.ioe__handle,$__2.ioe__filename,$__2.ioe__description]);});
$System.$IO.$Error.$alreadyInUseErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$ResourceBusy__;}),[]);
$System.$IO.$Error.$alreadyExistsErrorType=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$AlreadyExists__;}),[]);
$System.$IO.$Error.$ioeGetErrorString=
 new _F_(function($ioe)
         {var $__=
           new _A_($UHC.$IOBase.$ioe__type,[$ioe]);
          var $__3=
           new _A_($System.$IO.$Error.$isUserErrorType,[$__]);
          var $__4=
           _e_($__3);
          var $__swJSW15__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW16__0;
             switch($__5._tag_)
              {case 0:
                var $__6=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 303_34_0"]);
                var $__7=
                 new _A_($UHC.$Base.$error,[$__6]);
                $__swJSW16__0=
                 $__7;
                break;
               case 1:
                var $__8=
                 new _A_($UHC.$IOBase.$ioe__type,[$ioe]);
                var $__9=
                 new _A_($UHC.$Base.$show,[$UHC.$IOBase.$Show__DCT188__19__0,$__8]);
                $__swJSW16__0=
                 $__9;
                break;}
             $__swJSW15__0=
              $__swJSW16__0;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$IOBase.$ioe__description,[$ioe]);
             $__swJSW15__0=
              $__10;
             break;}
          return $__swJSW15__0;});
