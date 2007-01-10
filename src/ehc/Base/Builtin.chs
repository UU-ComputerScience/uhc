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

%%[95 export(hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT)
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

%%[3.strHiddenPrefix export(hsnStrHiddenPrefix)
hsnStrHiddenPrefix                  =   "$"
%%]

%%[7
hsnStrSpecialPrefix                 =   "_"
%%]

%%[5 export(mkHNmHidden)
mkHNmHidden :: HSNM x => x -> HsName
mkHNmHidden = mkHNmPrefix hsnStrHiddenPrefix
%%]

%%[3.strUn
strUn                               =   hsnStrHiddenPrefix ++ "un"
%%]

%%[7.strFldUpd
strFldUpd                           =   hsnStrHiddenPrefix ++ "upd_"
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
hsnIsList       hsn                 =   hsn == hsnDataList
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

hsnRow                              =   HNm (hsnStrSpecialPrefix ++ "Row")
hsnRec                              =   HNm (hsnStrSpecialPrefix ++ "Rec")
hsnSum                              =   HNm (hsnStrSpecialPrefix ++ "Var")
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

%%[5 export(hsnBool,hsnTrue,hsnFalse,hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnClassEqFldEq,hsnPrelConcatMap)
[hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnPrelConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnPrelString
 , hsnClassEqFldEq
%%[[95
 , hsnDataOrdering, hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT
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
%%[[95
      , "Ordering", "EQ", "LT", "GT"
%%]]
      ]
%%]

%%[8 export(hsnUndefined,hsnPackedString2String,hsnPackedString,hsnPrelId)
[hsnUndefined
 , hsnPackedString, hsnPackedString2String
 , hsnPrelId
%%[[99
 , hsnPackedString2Integer
%%]]
 ]
  = map
%%[[8
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ "undefined"
      , "PackedString", "packedString2String"
      , "id"
%%[[99
      , "packedString2Integer"
%%]]
      ]
%%]

%%[9 export(hsnMonadSeq,hsnMonadBind,hsnMonadFail,hsnClassEq)
[hsnMonadSeq,hsnMonadBind,hsnMonadFail
 , hsnClassEq
%%[[95
 , hsnBoolAnd
 , hsnClassOrd, hsnClassOrdFldCompare
 , hsnPrelConcat2, hsnPrelConcat
 , hsnPrelCompose
%%]]
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModBase)
%%]]
      [ ">>", ">>=", "fail"
      , "Eq"
%%[[95
      , "&&"
      , "Ord", "compare"
      , "concat", "++"
      , "."
%%]]
      ]
%%]

%%[95
[hsnClassShow
 , hsnClassShowFldShow, hsnClassShowFldShowsPrec
 , hsnPrelShowString, hsnPrelShowParen
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModShow)
%%]]
      [ "Show"
      , "show", "showsPrec"
      , "showString", "showParen"
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
hsnModShow                          =   hsnPrefixQual hsnEHC (HNm "Show")
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
      { ehbnId                      :: HsName
      , ehbnUndefined               :: HsName
      , ehbnError                   :: HsName
      , ehbnPackedString            :: HsName
      , ehbnPackedString2String     :: HsName
%%[[11
      , ehbnPrelString              :: HsName
%%]]
%%[[95
      , ehbnBoolTrue                :: HsName
      , ehbnBoolFalse               :: HsName
      , ehbnBoolAnd                 :: HsName
      , ehbnClassEq                 :: HsName
      , ehbnClassEqFldEq            :: HsName
      , ehbnClassOrd                :: HsName
      , ehbnClassOrdFldCompare      :: HsName
      , ehbnDataOrdering            :: HsName
      , ehbnDataOrderingAltLT       :: HsName
      , ehbnDataOrderingAltEQ       :: HsName
      , ehbnDataOrderingAltGT       :: HsName
      , ehbnClassShow               :: HsName
      , ehbnClassShowFldShow        :: HsName
      , ehbnClassShowFldShowsPrec   :: HsName
      , ehbnPrelShowString          :: HsName
      , ehbnPrelShowParen           :: HsName
      , ehbnPrelConcat              :: HsName
      , ehbnPrelConcat2             :: HsName
      , ehbnPrelCompose             :: HsName
%%]]
%%[[99
      , ehbnPackedString2Integer    :: HsName
%%]]
      }

mkEHBuiltinNames :: (IdOccKind -> HsName -> HsName) -> EHBuiltinNames
mkEHBuiltinNames f
  = EHBuiltinNames
      { ehbnId                      = f IdOcc_Val  		hsnPrelId
      , ehbnUndefined               = f IdOcc_Val  		hsnUndefined
      , ehbnError                   = f IdOcc_Val  		hsnError
      , ehbnPackedString            = f IdOcc_Type 		hsnPackedString
      , ehbnPackedString2String     = f IdOcc_Val  		hsnPackedString2String
%%[[11
      , ehbnPrelString              = f IdOcc_Type 		hsnPrelString
%%]]
%%[[95
      , ehbnBoolTrue                = f IdOcc_Val  		hsnTrue
      , ehbnBoolFalse               = f IdOcc_Val  		hsnFalse
      , ehbnBoolAnd                 = f IdOcc_Val  		hsnBoolAnd
      , ehbnClassEq                 = f IdOcc_Class 	hsnClassEq
      , ehbnClassEqFldEq            = f IdOcc_Val   	hsnClassEqFldEq
      , ehbnClassOrd                = f IdOcc_Class 	hsnClassOrd
      , ehbnClassOrdFldCompare      = f IdOcc_Val   	hsnClassOrdFldCompare
      , ehbnDataOrdering            = f IdOcc_Type   	hsnDataOrdering
      , ehbnDataOrderingAltLT       = f IdOcc_Val   	hsnDataOrderingAltLT
      , ehbnDataOrderingAltEQ       = f IdOcc_Val   	hsnDataOrderingAltEQ
      , ehbnDataOrderingAltGT       = f IdOcc_Val   	hsnDataOrderingAltGT
      , ehbnClassShow               = f IdOcc_Class 	hsnClassShow
      , ehbnClassShowFldShow        = f IdOcc_Val   	hsnClassShowFldShow
      , ehbnClassShowFldShowsPrec   = f IdOcc_Val   	hsnClassShowFldShowsPrec
      , ehbnPrelShowString          = f IdOcc_Val   	hsnPrelShowString
      , ehbnPrelShowParen           = f IdOcc_Val   	hsnPrelShowParen
      , ehbnPrelConcat              = f IdOcc_Val   	hsnPrelConcat
      , ehbnPrelConcat2             = f IdOcc_Val   	hsnPrelConcat2
      , ehbnPrelCompose             = f IdOcc_Val   	hsnPrelCompose
%%]]
%%[[99
      , ehbnPackedString2Integer    = f IdOcc_Val  		hsnPackedString2Integer
%%]]
      }
%%]

