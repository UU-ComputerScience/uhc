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

%%[8 export(hsnMain)
%%]

%%[8 import(Char(isUpper)) export(hsnIsConstructorName)
%%]

%%[9 export(hsnOImpl,hsnCImpl,hsnPrArrow,hsnIsPrArrow,hsnIsUnknown)
%%]

%%[95 export(hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin Haskell names: basics
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
hsnArrow                            =   hsnFromString "->"
hsnUnknown                          =   hsnFromString "??"
hsnInt                              =   hsnFromString "Int"
hsnChar                             =   hsnFromString "Char"
hsnWild                             =   hsnFromString "_"
strProd         i                   =   ',' : show i
hsnProd                             =   hsnFromString . strProd

hsnIsArrow, hsnIsProd               ::  HsName -> Bool
hsnIsArrow      hsn                 =   hsn == hsnArrow

hsnIsProd       (HNm s)             =   case hsnHNmFldToString s of
                                          (',':_) -> True
                                          _       -> False
%%[[7
hsnIsProd       _                   =   False
%%]]

hsnProdArity    (HNm s)             =   case hsnHNmFldToString s of
                                          (_:ar) -> read ar
                                          _      -> 0
%%]

%%[1.strHiddenPrefix export(hsnStrHiddenPrefix)
hsnStrHiddenPrefix                  =   "$"
%%]

%%[7
hsnStrSpecialPrefix                 =   "_"
%%]

%%[1 export(mkHNmHidden)
mkHNmHidden :: HSNM x => x -> HsName
mkHNmHidden = mkHNmPrefix hsnStrHiddenPrefix
%%]

%%[94 export(mkHNmExport)
mkHNmExport :: HSNM x => x -> HsName
mkHNmExport = mkHNmPrefix (hsnStrHiddenPrefix ++ "export_")
%%]

%%[3.strUn
strUn                               =   hsnStrHiddenPrefix ++ "un"
%%]

%%[7.strFldUpd
strFldUpd                           =   hsnStrHiddenPrefix ++ "upd_"
%%]

%%[3.hsnUn
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   hsnFromString (strUn ++ show nm)
%%]

%%[20 -3.hsnUn
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   strUn `hsnPrefix` nm
%%]

%%[3.hsnIsUn
hsnIsUn                             ::  HsName -> Bool
hsnIsUn         (HNm s)             =   isPrefixOf strUn $ hsnHNmFldToString s
%%]

%%[20 -3.hsnIsUn
hsnIsUn                             ::  HsName -> Bool
hsnIsUn         hsn
  = case hsnInitLast hsn of
      (_,HNm s) -> isPrefixOf strUn $ hsnHNmFldToString s
%%]

%%[3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         (HNm s)             =   hsnFromString $ drop (length strUn) $ hsnHNmFldToString s
%%]

%%[20 -3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         hsn
  = case hsnInitLast hsn of
      (ns,HNm s) -> mkHNm (ns,hsnFromString $ drop (length strUn) $ hsnHNmFldToString s)
%%]

%%[7.hsnFldUpd
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   hsnFromString (strFldUpd ++ show nm)
%%]

%%[20 -7.hsnFldUpd
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   strFldUpd `hsnPrefix` nm
%%]

%%[5
hsnIsList       hsn                 =   hsn == hsnDataList
%%]

%%[6
hsnStar                             =   hsnFromString "*"
%%]

hsnORow                             =   hsnFromString "(|"
hsnCRow                             =   hsnFromString "|)"
hsnOSum                             =   hsnFromString "(<"
hsnCSum                             =   hsnFromString ">)"
%%[7
hsnORow                             =   hsnFromString "{|"
hsnCRow                             =   hsnFromString "|}"
hsnOSum                             =   hsnFromString "{<"
hsnCSum                             =   hsnFromString ">}"
hsnORec                             =   hsnFromString "("
hsnCRec                             =   hsnFromString ")"

hsnRow                              =   hsnFromString (hsnStrSpecialPrefix ++ "Row")
hsnRec                              =   hsnFromString (hsnStrSpecialPrefix ++ "Rec")
hsnSum                              =   hsnFromString (hsnStrSpecialPrefix ++ "Var")
hsnRowEmpty                         =   hsnFromString (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow

positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map HNPos [1..]
%%]

%%[8
hsnMain                             =   hsnFromString "main"
%%]

%%[8
constructorInitial :: Char -> Bool
constructorInitial ':' = True
constructorInitial '[' = True
constructorInitial ',' = True
constructorInitial '(' = True
constructorInitial c   = isUpper c

hsnIsConstructorName :: HsName -> Bool
hsnIsConstructorName (HNm s  ) = case hsnHNmFldToString s of
                                   (x:xs) -> constructorInitial x
hsnIsConstructorName (HNPos n) = False
%%]
%%[20
hsnIsConstructorName (HNmQ hs) = hsnIsConstructorName (last hs)
%%]








hsnOImpl                            =   hsnFromString "(!"
hsnCImpl                            =   hsnFromString "!)"
%%[9
hsnOImpl                            =   hsnFromString "{!"
hsnCImpl                            =   hsnFromString "!}"
hsnPrArrow                          =   hsnFromString "=>"

hsnIsPrArrow                        ::  HsName -> Bool
hsnIsPrArrow    hsn                 =   hsn == hsnPrArrow
hsnIsUnknown                        =   (==hsnUnknown)
%%]

%%[10 export(hsnDynVar)
hsnDynVar                           =   hsnFromString "?"
%%]

%%[4 export(hsnCovariant, hsnContravariant, hsnInvariant)
hsnCovariant, hsnContravariant, hsnInvariant :: HsName
hsnCovariant      = mkHNm "+Covariant"
hsnContravariant  = mkHNm "-Contravariant"
hsnInvariant      = mkHNm "*Invariant"
%%]

%%[17 export(hsnPolNegation)
hsnPolNegation :: HsName
hsnPolNegation    = mkHNm "^Negate"
%%]

%%[97 export(hsnInteger)
hsnInteger                          =   hsnFromString "Integer"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: constructor utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.mkRV
mkRV :: String -> HsName
mkRV = hsnFromString
%%]

%%[99 -1.mkRV
mkRV :: HsName -> String -> HsName
mkRV m = hsnSetQual m . hsnFromString
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: basics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(hsnNegate)
[hsnNegate]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlNum)
%%]]
      [ "negate" ]
%%]

%%[1 export(hsnError)
[hsnError]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlBase)
%%]]
      [ "error" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: datatypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TBD: Needs cleaning up, correct partitioning in variants

%%[5 export(hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom)
[hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom]
  = map
%%[[5
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlEnum)
%%]]
      [ "enumFromThenTo", "enumFromThen", "enumFromTo", "enumFrom" ]
%%]

%%[5 export(hsnBool,hsnTrue,hsnFalse,hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnClassEqFldEq,hsnPrelConcatMap)
[hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnPrelConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnDataOrdering, hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT
 , hsnPrelString
 , hsnClassEqFldEq
%%[[95
 , hsnMap
 , hsnClassBounded, hsnClassBoundedFldMinBound, hsnClassBoundedFldMaxBound
 , hsnClassEnum, hsnClassEnumFldFromEnum, hsnClassEnumFldToEnum, hsnClassEnumFldSucc, hsnClassEnumFldPred
%%]]
 ]
  = map
%%[[5
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlBase)
%%]]
      [ "[]", ":", "[]", "concatMap"
      , "Bool", "True", "False"
      , "Ordering", "EQ", "LT", "GT"
      , "String"
      , "=="
%%[[95
      , "map"
      , "Bounded", "minBound", "maxBound"
      , "Enum", "fromEnum", "toEnum", "succ", "pred"
%%]]
      ]
%%]

%%[97 export(hsnPackedStringToInteger, hsnPrimIntegerToInt)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: codegen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TBD: Needs cleaning up, correct partitioning in variants

%%[8 export(hsnUndefined,hsnPackedString,hsnPackedStringToString,hsnPrelId,hsnPrimAddInt)
[ hsnUndefined
 , hsnPackedString
 , hsnPackedStringToString
 , hsnPrelId
 , hsnPrimAddInt
 , hsnPrimGtInt
%%[[95
 , hsnPrimLtInt
%%]]
%%[[97
 , hsnPackedStringToInteger
 , hsnPrimIntegerToInt
%%]]
%%[[97
 , hsnPrimEqChar
%%]]
 ]
  = map
%%[[8
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlBase)
%%]]
      [ "undefined"
      , "PackedString"
      , "packedStringToString"
      , "id"
      , "primAddInt"
      , "primGtInt"
%%[[95
      , "primLtInt"
%%]]
%%[[97
      , "packedStringToInteger"
      , "primIntegerToInt"
%%]]
%%[[97
      , "primEqChar"
%%]]
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TBD: Needs cleaning up, correct partitioning in variants

%%[9 export(hsnMonadSeq,hsnMonadBind,hsnMonadFail,hsnClassEq)
[hsnMonadSeq,hsnMonadBind,hsnMonadFail
 , hsnClassEq
%%[[95
 , hsnBoolAnd
 , hsnBoolOr
 , hsnClassOrd, hsnClassOrdFldCompare
 , hsnPrelConcat2, hsnPrelConcat
 , hsnPrelCompose
%%]]
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlBase)
%%]]
      [ ">>", ">>=", "fail"
      , "Eq"
%%[[95
      , "&&"
      , "||"
      , "Ord", "compare"
      , "++", "concat"
      , "."
%%]]
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FFI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
[hsnFunPtr]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModIntlPtr)
%%]]
      [ "FunPtr"
      ]
%%]

%%[99
[hsnAddr]
  = map
      (mkRV hsnModIntlPtr)
      [ "Addr"
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: deriving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95
[hsnClassShow
 , hsnClassShowFldShow, hsnClassShowFldShowsPrec
 , hsnPrelShowString, hsnPrelShowParen
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlShow)
%%]]
      [ "Show"
      , "show", "showsPrec"
      , "showString", "showParen"
      ]
%%]

%%[95
[hsnClassRead
 , hsnClassReadFldRead, hsnClassReadFldReadsPrec
 , hsnPrelLex, hsnPrelReadParen
 ]
  = map
%%[[9
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlRead)
%%]]
      [ "Read"
      , "read", "readsPrec"
      , "lex", "readParen"
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: numbers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97 export(hsnFromInteger)
[hsnFromInteger]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlNum)
%%]]
      [ "fromInteger" ]
%%]

%%[97
[hsnFloat,hsnDouble
 ]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlPrelude)
%%]]
      [ "Float", "Double"
      ]
%%]

%%[97
[hsnWord,hsnWord8,hsnWord16,hsnWord32,hsnWord64
 ]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlWord)
%%]]
      [ "Word", "Word8", "Word16", "Word32", "Word64"
      ]
%%]

%%[97
[hsnInt8, hsnInt16, hsnInt32, hsnInt64
 ]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlInt)
%%]]
      [ "Int8" , "Int16" , "Int32" , "Int64"
      ]
%%]

%%[97 export(hsnFromRational)
[hsnFromRational]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlFractional)
%%]]
      [ "fromRational" ]
%%]

%%[97 export(hsnMkRatio)
[hsnMkRatio]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlPrelude) -- (mkRV hsnModIntlRatio)
%%]]
      [ ":%" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: IO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98 export(hsnIO)
[hsnIO,hsnHandle,hsnByteArray,hsnRealWorld]
  = map
%%[[98
      mkRV
%%][99
      (mkRV hsnModIntlPrelude)
%%]]
      [ "IO", "Handle", "ByteArray", "RealWorld" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(hsnEhcRunMain)
[hsnEhcRunMain]
  = map
      (mkRV hsnModIntlPrelude)
      [ "ehcRunMain" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules + names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(hsnModBuiltin)
hsnModBuiltin                       =   mkHNm "#Builtin"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixed modules behind Prelude
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(hsnIsInPrelude)
hsnEHC                              =   hsnFromString "EHC"

hsnIsInPrelude :: HsName -> Bool
hsnIsInPrelude n
  = case hsnInitLast n of
      ((m:_),_) -> m == hsnEHC
      _         -> False
%%]

%%[99 export(hsnModPrelude,hsnModIntlPrelude)
-- hsnModIntlRatio                         =   hsnPrefixQual hsnEHC (hsnFromString "Ratio")
-- hsnModIntlReal                          =   hsnPrefixQual hsnEHC (hsnFromString "Real")
-- hsnModIntlFractional                    =   hsnPrefixQual hsnEHC (hsnFromString "Fractional")
hsnModIntlBase                          =   hsnPrefixQual hsnEHC (hsnFromString "Base")
hsnModIntlEnum                          =   hsnPrefixQual hsnEHC (hsnFromString "Enum")
hsnModIntlNum                           =   hsnPrefixQual hsnEHC (hsnFromString "Num")
hsnModIntlRead                          =   hsnPrefixQual hsnEHC (hsnFromString "Read")
hsnModIntlShow                          =   hsnPrefixQual hsnEHC (hsnFromString "Show")
hsnModIntlPrelude                       =   hsnPrefixQual hsnEHC (hsnFromString "Prelude")
hsnModPrelude                           =                         hsnFromString "Prelude"
%%]

%%[99 export(hsnModIntlInt,hsnModIntlWord)
hsnModIntlInt                           =   hsnPrefixQual hsnEHC (hsnFromString "Int")
hsnModIntlWord                          =   hsnPrefixQual hsnEHC (hsnFromString "Word")
hsnModIntlPtr                           =   hsnPrefixQual hsnEHC (hsnFromString "Ptr")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Naming conventions for class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(hsnClass2Dict)
-- Dict datatype name for class name, only used when `not ehcCfgClassViaRec'
hsnClass2Dict :: HsName -> HsName
hsnClass2Dict = mkHNmHidden . hsnPrefix "Dict-"
%%]

%%[9 export(hsnClass2Kind)
hsnClass2Kind :: HsName -> HsName
hsnClass2Kind = mkHNmHidden . hsnPrefix "ClassKind-"
%%]

%%[9 export(hsnClass2Polarity)
hsnClass2Polarity :: HsName -> HsName
hsnClass2Polarity = mkHNmHidden . hsnPrefix "ClassPolarity-"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names used without direct access from source code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHBuiltinNames(..),mkEHBuiltinNames)
data EHBuiltinNames
  = EHBuiltinNames
      { ehbnId                          :: HsName
      , ehbnUndefined                   :: HsName
      , ehbnError                       :: HsName
      , ehbnPackedString                :: HsName
      , ehbnPackedStringToString        :: HsName
      , ehbnPrimAddInt                  :: HsName
      , ehbnPrimGtInt                   :: HsName
      , ehbnBoolTrue                    :: HsName
      , ehbnBoolFalse                   :: HsName
      , ehbnDataOrderingAltLT           :: HsName
      , ehbnDataOrderingAltEQ           :: HsName
      , ehbnDataOrderingAltGT           :: HsName
%%[[11
      , ehbnPrelString                  :: HsName
%%]]
%%[[20
%%]]
%%[[94
      , ehbnFunPtr                      :: HsName
%%]]
%%[[95
      , ehbnMap                         :: HsName
      , ehbnDataBool                    :: HsName
      , ehbnBoolAnd                     :: HsName
      , ehbnBoolOr                      :: HsName
      , ehbnClassEq                     :: HsName
      , ehbnClassEqFldEq                :: HsName
      , ehbnClassOrd                    :: HsName
      , ehbnClassOrdFldCompare          :: HsName
      , ehbnDataOrdering                :: HsName
      , ehbnClassShow                   :: HsName
      , ehbnClassShowFldShow            :: HsName
      , ehbnClassShowFldShowsPrec       :: HsName
      , ehbnPrelShowString              :: HsName
      , ehbnPrelShowParen               :: HsName
      , ehbnPrelConcat                  :: HsName
      , ehbnPrelConcat2                 :: HsName
      , ehbnPrelConcatMap               :: HsName
      , ehbnPrelCompose                 :: HsName
      , ehbnClassEnum                   :: HsName
      , ehbnClassEnumFldFromEnum        :: HsName
      , ehbnClassEnumFldToEnum          :: HsName
      , ehbnClassEnumFldSucc            :: HsName
      , ehbnClassEnumFldPred            :: HsName
      , ehbnClassEnumFldEnumFrom        :: HsName
      , ehbnClassEnumFldEnumFromTo      :: HsName
      , ehbnClassEnumFldEnumFromThen    :: HsName
      , ehbnClassEnumFldEnumFromThenTo  :: HsName
      , ehbnClassBounded                :: HsName
      , ehbnClassBoundedFldMinBound     :: HsName
      , ehbnClassBoundedFldMaxBound     :: HsName
      , ehbnPrimLtInt                   :: HsName
%%]]
%%[[97
      , ehbnInt8                        :: HsName
      , ehbnInt16                       :: HsName
      , ehbnInt32                       :: HsName
      , ehbnInt64                       :: HsName
      , ehbnWord                        :: HsName
      , ehbnWord8                       :: HsName
      , ehbnWord16                      :: HsName
      , ehbnWord32                      :: HsName
      , ehbnWord64                      :: HsName
      , ehbnFloat                       :: HsName
      , ehbnDouble                      :: HsName
      , ehbnPackedStringToInteger       :: HsName
      , ehbnDataList                    :: HsName
      , ehbnDataListAltCons             :: HsName
      , ehbnDataListAltNil              :: HsName
%%]]
%%[[98
      , ehbnIO                          :: HsName
      , ehbnHandle                      :: HsName
      , ehbnByteArray                   :: HsName
      , ehbnRealWorld                   :: HsName
%%]]
%%[[99
      -- , ehbnEhcRunMain                  :: HsName
      , ehbnClassRead                   :: HsName
      , ehbnClassReadFldRead            :: HsName
      , ehbnClassReadFldReadsPrec       :: HsName
      , ehbnPrelLex                     :: HsName
      , ehbnPrelReadParen               :: HsName
      , ehbnPrimEqChar                  :: HsName
      , ehbnAddr                        :: HsName
%%]]
      }

mkEHBuiltinNames :: (IdOccKind -> HsName -> HsName) -> EHBuiltinNames
mkEHBuiltinNames f
  = EHBuiltinNames
      { ehbnId                          = f IdOcc_Val       hsnPrelId
      , ehbnUndefined                   = f IdOcc_Val       hsnUndefined
      , ehbnError                       = f IdOcc_Val       hsnError
      , ehbnPackedString                = f IdOcc_Type      hsnPackedString
      , ehbnPackedStringToString        = f IdOcc_Val       hsnPackedStringToString
      , ehbnPrimAddInt                  = f IdOcc_Val       hsnPrimAddInt
      , ehbnPrimGtInt                   = f IdOcc_Val       hsnPrimGtInt
      , ehbnBoolTrue                    = f IdOcc_Val       hsnTrue
      , ehbnBoolFalse                   = f IdOcc_Val       hsnFalse
      , ehbnDataOrderingAltLT           = f IdOcc_Val       hsnDataOrderingAltLT
      , ehbnDataOrderingAltEQ           = f IdOcc_Val       hsnDataOrderingAltEQ
      , ehbnDataOrderingAltGT           = f IdOcc_Val       hsnDataOrderingAltGT
%%[[11
      , ehbnPrelString                  = f IdOcc_Type      hsnPrelString
%%]]
%%[[20
%%]]
%%[[94
      , ehbnFunPtr                      = f IdOcc_Type      hsnFunPtr
%%]]
%%[[95
      , ehbnMap                         = f IdOcc_Type      hsnMap
      , ehbnDataBool                    = f IdOcc_Type      hsnBool
      , ehbnBoolAnd                     = f IdOcc_Val       hsnBoolAnd
      , ehbnBoolOr                      = f IdOcc_Val       hsnBoolOr
      , ehbnClassEq                     = f IdOcc_Class     hsnClassEq
      , ehbnClassEqFldEq                = f IdOcc_Val       hsnClassEqFldEq
      , ehbnClassOrd                    = f IdOcc_Class     hsnClassOrd
      , ehbnClassOrdFldCompare          = f IdOcc_Val       hsnClassOrdFldCompare
      , ehbnDataOrdering                = f IdOcc_Type      hsnDataOrdering
      , ehbnClassShow                   = f IdOcc_Class     hsnClassShow
      , ehbnClassShowFldShow            = f IdOcc_Val       hsnClassShowFldShow
      , ehbnClassShowFldShowsPrec       = f IdOcc_Val       hsnClassShowFldShowsPrec
      , ehbnPrelShowString              = f IdOcc_Val       hsnPrelShowString
      , ehbnPrelShowParen               = f IdOcc_Val       hsnPrelShowParen
      , ehbnPrelConcat                  = f IdOcc_Val       hsnPrelConcat
      , ehbnPrelConcat2                 = f IdOcc_Val       hsnPrelConcat2
      , ehbnPrelConcatMap               = f IdOcc_Val       hsnPrelConcatMap
      , ehbnPrelCompose                 = f IdOcc_Val       hsnPrelCompose
      , ehbnClassEnum                   = f IdOcc_Class     hsnClassEnum
      , ehbnClassEnumFldFromEnum        = f IdOcc_Val       hsnClassEnumFldFromEnum
      , ehbnClassEnumFldToEnum          = f IdOcc_Val       hsnClassEnumFldToEnum
      , ehbnClassEnumFldSucc            = f IdOcc_Val       hsnClassEnumFldSucc
      , ehbnClassEnumFldPred            = f IdOcc_Val       hsnClassEnumFldPred
      , ehbnClassEnumFldEnumFrom        = f IdOcc_Val       hsnEnumFrom
      , ehbnClassEnumFldEnumFromTo      = f IdOcc_Val       hsnEnumFromTo
      , ehbnClassEnumFldEnumFromThen    = f IdOcc_Val       hsnEnumFromThen
      , ehbnClassEnumFldEnumFromThenTo  = f IdOcc_Val       hsnEnumFromThenTo
      , ehbnClassBounded                = f IdOcc_Class     hsnClassBounded
      , ehbnClassBoundedFldMinBound     = f IdOcc_Val       hsnClassBoundedFldMinBound
      , ehbnClassBoundedFldMaxBound     = f IdOcc_Val       hsnClassBoundedFldMaxBound
      , ehbnPrimLtInt                   = f IdOcc_Val       hsnPrimLtInt
%%]]
%%[[97
      , ehbnInt8                        = f IdOcc_Type      hsnInt8
      , ehbnInt16                       = f IdOcc_Type      hsnInt16
      , ehbnInt32                       = f IdOcc_Type      hsnInt32
      , ehbnInt64                       = f IdOcc_Type      hsnInt64
      , ehbnWord                        = f IdOcc_Type      hsnWord
      , ehbnWord8                       = f IdOcc_Type      hsnWord8
      , ehbnWord16                      = f IdOcc_Type      hsnWord16
      , ehbnWord32                      = f IdOcc_Type      hsnWord32
      , ehbnWord64                      = f IdOcc_Type      hsnWord64
      , ehbnFloat                       = f IdOcc_Type      hsnFloat
      , ehbnDouble                      = f IdOcc_Type      hsnDouble
      , ehbnPackedStringToInteger       = f IdOcc_Val       hsnPackedStringToInteger
      , ehbnDataList                    = f IdOcc_Type      hsnDataList
      , ehbnDataListAltCons             = f IdOcc_Type      hsnDataListAltCons
      , ehbnDataListAltNil              = f IdOcc_Type      hsnDataListAltNil
%%]]
%%[[98
      , ehbnIO                          = f IdOcc_Type      hsnIO
      , ehbnHandle                      = f IdOcc_Type      hsnHandle
      , ehbnByteArray                   = f IdOcc_Type      hsnByteArray
      , ehbnRealWorld                   = f IdOcc_Type      hsnRealWorld
%%]]
%%[[99
      -- , ehbnEhcRunMain                  = f IdOcc_Val       hsnEhcRunMain
      , ehbnClassRead                   = f IdOcc_Class     hsnClassRead
      , ehbnClassReadFldRead            = f IdOcc_Val       hsnClassReadFldRead
      , ehbnClassReadFldReadsPrec       = f IdOcc_Val       hsnClassReadFldReadsPrec
      , ehbnPrelLex                     = f IdOcc_Val       hsnPrelLex
      , ehbnPrelReadParen               = f IdOcc_Val       hsnPrelReadParen
      , ehbnPrimEqChar                  = f IdOcc_Val       hsnPrimEqChar
      , ehbnAddr                        = f IdOcc_Type      hsnAddr
%%]]
      }
%%]

