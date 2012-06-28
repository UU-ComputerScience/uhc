// System.Exit
var $System=
 ($System ? $System : {});
$System.$Exit=
 ($System.$Exit ? $System.$Exit : {});
$System.$Exit.$exitWith=
 new _F_(function($code)
         {var $__=
           new _A_($UHC.$Base.$ExitException__,[$code]);
          return new _A_($UHC.$OldException.$throwIO,[$__]);});
$System.$Exit.$exitSuccess=
 new _A_(new _F_(function()
                 {return new _A_($System.$Exit.$exitWith,[$UHC.$Base.$ExitSuccess__]);}),[]);
$System.$Exit.$__154__9=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$ExitFailure__,[1]);}),[]);
$System.$Exit.$exitFailure=
 new _A_(new _F_(function()
                 {return new _A_($System.$Exit.$exitWith,[$System.$Exit.$__154__9]);}),[]);
