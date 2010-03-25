%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin: names, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Builtin} import({%{EH}Base.HsName}, Data.Maybe)
%%]

%%[1 export(hsnWild, hsnArrow, strProd, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[3 import(Data.List) export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[8 import(Char(isUpper))
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

hsnIsProd       n | isJust ms       = case fromJust ms of
                                        (',':_) -> True
                                        _       -> False
                                    where ms = mbHNm n
%%[[7
hsnIsProd       _                   =   False
%%]]

hsnProdArity    n | isJust ms       = case fromJust ms of
                                        (_:ar) -> read ar
                                        _      -> 0
                                    where ms = mbHNm n
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
hsnIsUn                             =   maybe False (isPrefixOf strUn) . hsnMbBaseString
-- hsnIsUn         (HsName_Base s)     =   isPrefixOf strUn $ hsnHNmFldToString s
%%]

%%[2020 -3.hsnIsUn
hsnIsUn                             ::  HsName -> Bool
hsnIsUn         hsn
  = case hsnInitLast hsn of
      (_,n) | isJust ms -> isPrefixOf strUn $ fromJust ms
                        where ms = mbHNm n
%%]

%%[3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         n                   =   maybe n (\(s,mk) -> mk $ drop (length strUn) s) $ hsnBaseUnpack n
-- hsnUnUn         (HsName_Base s)     =   hsnFromString $ drop (length strUn) $ hsnHNmFldToString s
%%]

%%[2020 -3.hsnUnUn
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         hsn
  = case hsnInitLast hsn of
      (ns,n) | isJust ms -> mkHNm (ns,hsnFromString $ drop (length strUn) $ fromJust ms)
                         where ms = mbHNm n
%%]

%%[7.hsnFldUpd export(hsnFldUpd)
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   hsnFromString (strFldUpd ++ show nm)
%%]

%%[20 -7.hsnFldUpd export(hsnFldUpd)
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   strFldUpd `hsnPrefix` nm
%%]

%%[5 export(hsnIsList)
hsnIsList       hsn                 =   hsn == hsnDataList
%%]

%%[7 export(hsnORow,hsnCRow,hsnORec,hsnCRec,hsnOSum,hsnCSum)
hsnORow                             =   hsnFromString "{|"
hsnCRow                             =   hsnFromString "|}"
hsnOSum                             =   hsnFromString "{<"
hsnCSum                             =   hsnFromString ">}"
hsnORec                             =   hsnFromString "("
hsnCRec                             =   hsnFromString ")"
%%]

%%[7 export(hsnRow,hsnRec,hsnSum,hsnRowEmpty,hsnIsRec,hsnIsSum,hsnIsRow)
hsnRow                              =   hsnFromString (hsnStrSpecialPrefix ++ "Row")
hsnRec                              =   hsnFromString (hsnStrSpecialPrefix ++ "Rec")
hsnSum                              =   hsnFromString (hsnStrSpecialPrefix ++ "Var")
hsnRowEmpty                         =   hsnFromString (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow
%%]

%%[7 export(positionalFldNames)
positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map mkHNmPos [1..]
%%]

%%[8 export(hsnMain)
hsnMain                             =   hsnFromString "main"
%%]

%%[8 export(hsnIsConstructorName)
constructorInitial :: Char -> Bool
constructorInitial ':' = True
constructorInitial '[' = True
constructorInitial ',' = True
constructorInitial '(' = True
constructorInitial c   = isUpper c

hsnIsConstructorName :: HsName -> Bool
hsnIsConstructorName n | isJust ms = case fromJust ms of
                                       (x:xs) -> constructorInitial x
                                   where ms = mbHNm n
hsnIsConstructorName (HsName_Pos n)= False
%%]
%%[20
hsnIsConstructorName n             = hsnIsConstructorName (snd $ hsnInitLast n)
%%]

%%[9 export(hsnOImpl,hsnCImpl,hsnPrArrow,hsnIsPrArrow,hsnIsUnknown)
hsnOImpl          =   hsnFromString "{!"
hsnCImpl          =   hsnFromString "!}"
hsnPrArrow        =   hsnFromString "=>"

hsnIsPrArrow                        ::  HsName -> Bool
hsnIsPrArrow    hsn                 =   hsn == hsnPrArrow
hsnIsUnknown                        =   (==hsnUnknown)
%%]

%%[10 export(hsnDynVar)
hsnDynVar         =   hsnFromString "?"
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

%%[18 export(hsnOParensUnboxed,hsnCParensUnboxed)
hsnOParensUnboxed =   hsnFromString "(#"
hsnCParensUnboxed =   hsnFromString "#)"
%%]

%%[18 export(hsnRecUnboxed)
hsnRecUnboxed     =   hsnFromString (hsnStrSpecialPrefix ++ "Rec#")
%%]

%%[97 export(hsnInteger)
hsnInteger        =   hsnFromString "Integer"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin: kinds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(charKindStar,hsnKindStar)
charKindStar                        =   '*'
hsnKindStar                         =   hsnFromString [charKindStar]
%%]

%%[7 export(hsnKindRow)
hsnKindRow                          =   hsnRow
%%]

%%[18 export(hsnKindUnboxed)
hsnKindUnboxed                      =   hsnFromString "#"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin: unboxed types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These are *really* builtin in that they do are not related to Prelude defined equivalents.
In fact, no unboxed types can be user defined.

%%[18 export(hsnIntUnboxed)
hsnIntUnboxed      =   hsnFromString "Int#"
%%]

%%[97 export(hsnInt8Unboxed, hsnInt16Unboxed, hsnInt32Unboxed, hsnInt64Unboxed, hsnWordUnboxed, hsnWord8Unboxed, hsnWord16Unboxed, hsnWord32Unboxed, hsnWord64Unboxed)
hsnInt8Unboxed     =   hsnFromString "Int8#"
hsnInt16Unboxed    =   hsnFromString "Int16#"
hsnInt32Unboxed    =   hsnFromString "Int32#"
hsnInt64Unboxed    =   hsnFromString "Int64#"
hsnWordUnboxed     =   hsnFromString "Word#"
hsnWord8Unboxed    =   hsnFromString "Word8#"
hsnWord16Unboxed   =   hsnFromString "Word16#"
hsnWord32Unboxed   =   hsnFromString "Word32#"
hsnWord64Unboxed   =   hsnFromString "Word64#"
%%]

%%[99 export(hsnAddrUnboxed)
hsnAddrUnboxed     =   hsnFromString "Addr#"
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlNum)
%%]]
      [ "negate" ]
%%]

%%[1 export(hsnError)
[hsnError]
  = map
%%[[1
      mkRV
%%][99
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlEnum)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
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

%%[97 export(hsnPackedStringToInteger, hsnPrimIntegerToInt, hsnPrimIntToInteger)
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
 , hsnPrimIntToInteger
%%]]
%%[[97
 , hsnPrimEqChar
%%]]
 ]
  = map
%%[[8
      mkRV
%%][99
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
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
      , "primIntToInteger"
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
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
      (mkRV hsnModIntlTypes)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlShow)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlRead)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlNum)
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
      (mkRV hsnModIntlBase)
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
      (mkRV hsnModIntlTypes)
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
      (mkRV hsnModIntlTypes)
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
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlFractional)
%%]]
      [ "fromRational" ]
%%]

%%[97 export(hsnMkRatio)
[hsnMkRatio]
  = map
%%[[97
      mkRV
%%][99
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlRatio)
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
      (mkRV hsnModIntlBase)
%%]]
      [ "IO", "Handle", "ByteArray", "RealWorld" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known/available runtime values: main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(hsnEhcRunMain)
[hsnEhcRunMain]
  = map
      (mkRV hsnModIntlRun)
      [ "ehcRunMain" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Explicit stack trace construction entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(hsnStackTracePush)
[hsnStackTracePush]
  = map
      (mkRV hsnModIntlBase)
      [ "pushExplicitStackTrace" ]
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
hsnUHC                              =   hsnFromString "UHC"

hsnIsInPrelude :: HsName -> Bool
hsnIsInPrelude n
  = case hsnInitLast n of
      ((m:_),_) -> m == hsnUHC
      _         -> False
%%]

%%[99 export(hsnModPrelude,hsnModIntlBase)
-- hsnModIntlRatio                         =   hsnPrefixQual hsnUHC (hsnFromString "Ratio")
-- hsnModIntlReal                          =   hsnPrefixQual hsnUHC (hsnFromString "Real")
-- hsnModIntlFractional                    =   hsnPrefixQual hsnUHC (hsnFromString "Fractional")
hsnModIntlBase                          =   hsnPrefixQual hsnUHC (hsnFromString "Base")
hsnModIntlEnum                          =   hsnPrefixQual hsnUHC (hsnFromString "Enum")
hsnModIntlNum                           =   hsnPrefixQual hsnUHC (hsnFromString "Num")
hsnModIntlRead                          =   hsnPrefixQual hsnUHC (hsnFromString "Read")
hsnModIntlShow                          =   hsnPrefixQual hsnUHC (hsnFromString "Show")
hsnModPrelude                           =                         hsnFromString "Prelude"
%%]

%%[99
hsnModIntlTypes                         =   hsnPrefixQual hsnUHC (hsnFromString "Types")
hsnModIntlPtr                           =   hsnPrefixQual hsnUHC (hsnFromString "Ptr")
hsnModIntlRun                           =   hsnPrefixQual hsnUHC (hsnFromString "Run")
hsnModIntlIOBase                        =   hsnPrefixQual hsnUHC (hsnFromString "IOBase")
hsnModIntlStackTrace                    =   hsnPrefixQual hsnUHC (hsnFromString "StackTrace")
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
      , ehbnDataListAltNil              :: HsName
      , ehbnDataListAltCons             :: HsName
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
      , ehbnPrimIntToInteger            :: HsName
      , ehbnFromInteger                 :: HsName
      , ehbnDataList                    :: HsName
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
      -- , ehbnStackTracePush              :: HsName
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
      , ehbnDataListAltNil              = f IdOcc_Val       hsnDataListAltNil
      , ehbnDataListAltCons             = f IdOcc_Val       hsnDataListAltCons
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
      , ehbnPrimIntToInteger            = f IdOcc_Val       hsnPrimIntToInteger
      , ehbnFromInteger                 = f IdOcc_Val       hsnFromInteger
      , ehbnDataList                    = f IdOcc_Type      hsnDataList
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
      -- , ehbnStackTracePush              = f IdOcc_Val       hsnStackTracePush
%%]]
      }
%%]

