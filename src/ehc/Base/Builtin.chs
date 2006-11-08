%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin: names, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Builtin} import({%{EH}Base.HsName})
%%]

%%[1 export(hsnWild, hsnArrow, strProd, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[3 import(Data.List) export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[5 export(hsnIsList)
%%]

%%[6 export(hsnStar)
%%]

%%[7 export(hsnRow,hsnRec,hsnSum,hsnRowEmpty,hsnIsRec,hsnIsSum)
%%]

%%[7 export(hsnORow,hsnCRow,hsnORec,hsnCRec,hsnOSum,hsnCSum)
%%]

%%[7 export(hsnIsRow)
%%]

%%[7 export(positionalFldNames)
%%]

%%[7 export(hsnFldUpd)
%%]

%%[8 export(hsnFloat)
%%]

%%[8 export(hsnPrimAddInt,hsnMain)
%%]

%%[8 import(Char(isUpper)) export(hsnIsConstructorName)
%%]

%%[9 export(hsnOImpl,hsnCImpl,hsnPrArrow,hsnIsPrArrow,hsnIsUnknown)
%%]

%%[99 export(hsnOrdering, hsnEQ, hsnLT, hsnGT)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
strProd :: Int -> String
%%]

%%[1.HsName.Base.itf
hsnArrow, hsnUnknown, hsnInt, hsnChar, hsnWild
                                    ::  HsName
hsnProd                             ::  Int -> HsName
hsnProdArity                        ::  HsName -> Int
%%]

%%[1.HsName.Base.impl
hsnArrow                            =   HNm "->"
hsnUnknown                          =   HNm "??"
hsnInt                              =   HNm "Int"
hsnChar                             =   HNm "Char"
hsnWild                             =   HNm "_"
strProd         i                   =   ',' : show i
hsnProd                             =   HNm . strProd

hsnIsArrow, hsnIsProd               ::  HsName -> Bool
hsnIsArrow      hsn                 =   hsn == hsnArrow

hsnIsProd       (HNm (',':_))       =   True
hsnIsProd       _                   =   False

hsnProdArity    (HNm (_:ar))        =   read ar
%%]

%%[3.strUn
strUn                               =   "un"
%%]

%%[99.strUn -3.strUn
strUn                               =   "-"
%%]

%%[7.strFldUpd
strFldUpd                           =   "upd_"
%%]

%%[99 -7.strFldUpd
strFldUpd                           =   strUn
%%]

%%[3.hsnUn
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   HNm (strUn ++ show nm)
%%]

%%[12 -3.hsnUn
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   strUn `hsnPrefix` nm
%%]

%%[3.hsnIsUn
hsnIsUn                             ::  HsName -> Bool
hsnIsUn         (HNm s)             =   isPrefixOf strUn s
%%]

%%[12 -3.hsnIsUn
hsnIsUn                             ::  HsName -> Bool
hsnIsUn         hsn
  = case hsnInitLast hsn of
      (_,HNm s) -> isPrefixOf strUn s
%%]

%%[3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         (HNm s)             =   HNm (drop (length strUn) s)
%%]

%%[12 -3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         hsn
  = case hsnInitLast hsn of
      (ns,HNm s) -> mkHNm (ns,HNm (drop (length strUn) s))
%%]

%%[7.hsnFldUpd
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   HNm (strFldUpd ++ show nm)
%%]

%%[12 -7.hsnFldUpd
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   strFldUpd `hsnPrefix` nm
%%]

%%[5
hsnIsList       hsn                 =   hsn == hsnList
%%]

%%[6
hsnStar                             =   HNm "*"
%%]

hsnORow                             =   HNm "(|"
hsnCRow                             =   HNm "|)"
hsnOSum                             =   HNm "(<"
hsnCSum                             =   HNm ">)"
%%[7
hsnORow                             =   HNm "{|"
hsnCRow                             =   HNm "|}"
hsnOSum                             =   HNm "{<"
hsnCSum                             =   HNm ">}"
hsnORec                             =   HNm "("
hsnCRec                             =   HNm ")"

hsnRow                              =   HNm "Row"
hsnRec                              =   HNm "Rec"
hsnSum                              =   HNm "Var"
hsnRowEmpty                         =   HNm (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow

positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map HNPos [1..]
%%]

%%[8
hsnFloat                            =   HNm "Float"
%%]

%%[8
hsnMain                             =   HNm "main"
hsnPrimAddInt                       =   HNm "primAddInt"
%%]

%%[8
constructorInitial :: Char -> Bool
constructorInitial ':' = True
constructorInitial '[' = True
constructorInitial ',' = True
constructorInitial '(' = True
constructorInitial c   = isUpper c

hsnIsConstructorName :: HsName -> Bool
hsnIsConstructorName (HNm (x:xs)) = constructorInitial x
hsnIsConstructorName (HNPos n) = False
%%]
%%[12
hsnIsConstructorName (HNmQ hs) = hsnIsConstructorName (last hs)
%%]








hsnOImpl                            =   HNm "(!"
hsnCImpl                            =   HNm "!)"
%%[9
hsnOImpl                            =   HNm "{!"
hsnCImpl                            =   HNm "!}"
hsnPrArrow                          =   HNm "=>"

hsnIsPrArrow                        ::  HsName -> Bool
hsnIsPrArrow    hsn                 =   hsn == hsnPrArrow
hsnIsUnknown                        =   (==hsnUnknown)
%%]

%%[10 export(hsnDynVar)
hsnDynVar                           =   HNm "?"
%%]

%%[99 export(hsnInteger,hsnDouble)
hsnInteger                          =   HNm "Integer"
hsnDouble                           =   HNm "Double"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.mkRV
mkRV :: String -> HsName
mkRV = HNm
%%]

%%[99 -1.mkRV
mkRV :: HsName -> String -> HsName
mkRV m = hsnSetQual m . HNm
%%]

%%[1 export(hsnNegate)
[hsnNegate]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModNum)
%%]]
      [ "negate" ]
%%]

%%[1 export(hsnError)
[hsnError]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "error" ]
%%]

%%[5 export(hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom)
[hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom]
  = map
%%[[5
      mkRV
%%][99
      (mkRV hsnModEnum)
%%]]
      [ "enumFromThenTo", "enumFromThen", "enumFromTo", "enumFrom" ]
%%]

%%[5 export(hsnBool,hsnTrue,hsnFalse,hsnString,hsnList,hsnListCons,hsnListNil,hsnEq,hsnConcatMap)
[hsnList,hsnListCons,hsnListNil,hsnConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnString
 , hsnEq
%%[[99
 , hsnOrdering, hsnEQ, hsnLT, hsnGT
%%]]
 ]
  = map
%%[[5
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "[]", ":", "[]", "concatMap"
      , "Bool", "True", "False"
      , "String"
      , "=="
%%[[99
	  , "Ordering", "EQ", "LT", "GT"
%%]]
      ]
%%]

%%[8 export(hsnUndefined,hsnFromPackedString,hsnPackedString,hsnId)
[hsnUndefined
 , hsnPackedString, hsnFromPackedString
 , hsnId
 ]
  = map
%%[[8
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "undefined"
      , "PackedString", "fromPackedString"
      , "id"
      ]
%%]

%%[9 export(hsnMonadSeq,hsnMonadBind,hsnMonadFail,hsnClassEq)
[hsnMonadSeq,hsnMonadBind,hsnMonadFail
 , hsnClassEq
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ ">>", ">>=", "fail"
      , "Eq"
      ]
%%]

%%[99 export(hsnFromInteger)
[hsnFromInteger]
  = map
      (mkRV hsnModNum)
      [ "fromInteger" ]
%%]

%%[99 export(hsnFromRational,hsnMkRatio)
[hsnFromRational,hsnMkRatio]
  = map
      (mkRV hsnModReal)
      [ "fromRational", "%" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules + names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 export(hsnModBuiltin)
hsnModBuiltin                       =   mkHNm "#Builtin"
%%]

%%[99 export(hsnModPrelude)
hsnModPrelude                       =   mkHNm "Prelude"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules behind Prelude
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(hsnIsInPrelude)
hsnEHC                              =   HNm "EHC"

hsnIsInPrelude :: HsName -> Bool
hsnIsInPrelude n
  = case hsnInitLast n of
      ((m:_),_) -> m == hsnEHC
      _         -> False
%%]

%%[99 export(hsnModBase,hsnModEnum,hsnModNum,hsnModReal)
hsnModBase                          =   hsnPrefixQual hsnEHC (HNm "Base")
hsnModEnum                          =   hsnPrefixQual hsnEHC (HNm "Enum")
hsnModNum                           =   hsnPrefixQual hsnEHC (HNm "Num")
hsnModReal                          =   hsnPrefixQual hsnEHC (HNm "Real")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Naming conventions for class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(hsnClass2Dict)
-- Dict datatype name for class name, only used when `not ehcCfgClassViaRec'
hsnClass2Dict :: HsName -> HsName
hsnClass2Dict = hsnPrefix "Dict-"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names used without direct access from source code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHBuiltinNames(..),mkEHBuiltinNames)
data EHBuiltinNames
  = EHBuiltinNames
  	  { ehbnId					:: HsName
  	  , ehbnUndefined			:: HsName
  	  , ehbnError				:: HsName
  	  , ehbnFromPackedString	:: HsName
%%[[11
  	  , ehbnString				:: HsName
%%]]
  	  }

mkEHBuiltinNames :: (IdOccKind -> HsName -> HsName) -> EHBuiltinNames
mkEHBuiltinNames f
  = EHBuiltinNames
  	  { ehbnId					= f IdOcc_Val  hsnId
  	  , ehbnUndefined			= f IdOcc_Val  hsnUndefined
  	  , ehbnError				= f IdOcc_Val  hsnError
  	  , ehbnFromPackedString	= f IdOcc_Val  hsnFromPackedString
%%[[11
  	  , ehbnString				= f IdOcc_Type hsnString
%%]]
  	  }
%%]

