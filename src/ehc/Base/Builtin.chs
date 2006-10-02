%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin: names, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Builtin} import({%{EH}Base.HsName})
%%]

%%[1 export(hsnWild, hsnArrow, strProd, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[1 export(hsnNegate,hsnError)
%%]

%%[3 import(Data.List) export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[5 export(hsnBool,hsnTrue,hsnFalse,hsnString,hsnList,hsnListCons,hsnListNil)
%%]

%%[5 export(hsnIsList)
%%]

%%[5 export(hsnEq)
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

%%[8 export(hsnFloat)
%%]

%%[8 export(hsnUndefined,hsnPrimAddInt,hsnMain)
%%]

%%[9 export(hsnOImpl,hsnCImpl,hsnPrArrow,hsnIsPrArrow,hsnIsUnknown)
%%]

%%[9 export(hsnClassEq)
%%]

%%[10 export(hsnDynVar,hsnConcat)
%%]

%%[12 export(hsnModBuiltin)
%%]

%%[99 export(hsnInteger,hsnDouble,hsnModPrelude)
%%]

%%[5 export(hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom,hsnConcatMap)
%%]

%%[9 export(hsnMonadSeq,hsnMonadBind,hsnMonadFail)
%%]

%%[99 export(hsnFromInteger,hsnFromRational,hsnMkRatio)
%%]

%%[99 export(hsnModBase,hsnModEnum,hsnModNum,hsnModReal)
%%]

%%[99 export(hsnIsInPrelude)
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

%%[10
hsnDynVar                           =   HNm "?"
%%]

%%[99
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

%%[1
[hsnNegate]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModNum)
%%]]
      [ "negate" ]
%%]

%%[1
[hsnError]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "error" ]
%%]

%%[5
[hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom]
  = map
%%[[5
      mkRV
%%][99
      (mkRV hsnModEnum)
%%]]
      [ "enumFromThenTo", "enumFromThen", "enumFromTo", "enumFrom" ]
%%]

%%[5
[hsnList,hsnListCons,hsnListNil,hsnConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnString
 , hsnEq
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
      ]
%%]

%%[8
[hsnUndefined
 ]
  = map
%%[[8
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "undefined"
      ]
%%]

%%[9
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

%%[99
[hsnFromInteger]
  = map
      (mkRV hsnModNum)
      [ "fromInteger" ]
%%]

%%[99
[hsnFromRational,hsnMkRatio]
  = map
      (mkRV hsnModReal)
      [ "fromRational", "%" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules + names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
hsnModBuiltin                       =   mkHNm "#Builtin"
%%]

%%[99
hsnModPrelude                       =   mkHNm "Prelude"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules behind Prelude
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
hsnEHC                              =   HNm "EHC"

hsnIsInPrelude :: HsName -> Bool
hsnIsInPrelude n
  = case hsnInitLast n of
      ((m:_),_) -> m == hsnEHC
      _         -> False
%%]

%%[99
hsnModBase                          =   hsnPrefixQual hsnEHC (HNm "Base")
hsnModEnum                          =   hsnPrefixQual hsnEHC (HNm "Enum")
hsnModNum                           =   hsnPrefixQual hsnEHC (HNm "Num")
hsnModReal                          =   hsnPrefixQual hsnEHC (HNm "Real")
%%]
