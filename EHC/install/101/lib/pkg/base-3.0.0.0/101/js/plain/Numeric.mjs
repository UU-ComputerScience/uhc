// Numeric
var $Numeric=
 ($Numeric ? $Numeric : {});
$Numeric.$showGFloat=
 new _F_(function($__,$d,$x)
         {var $__4=
           new _A_($UHC.$Show.$formatRealFloat,[$__,$UHC.$Show.$FFGeneric__,$d,$x]);
          return new _A_($UHC.$Base.$showString,[$__4]);});
$Numeric.$showFFloat=
 new _F_(function($__,$d,$x)
         {var $__4=
           new _A_($UHC.$Show.$formatRealFloat,[$__,$UHC.$Show.$FFFixed__,$d,$x]);
          return new _A_($UHC.$Base.$showString,[$__4]);});
$Numeric.$showEFloat=
 new _F_(function($__,$d,$x)
         {var $__4=
           new _A_($UHC.$Show.$formatRealFloat,[$__,$UHC.$Show.$FFExponent__,$d,$x]);
          return new _A_($UHC.$Base.$showString,[$__4]);});
