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

%%[91 export(hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT)
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

%%[1 export(hsnIsWild)
hsnIsWild :: HsName -> Bool
%%[[1
hsnIsWild x = x == hsnWild
%%][20
hsnIsWild x = hsnQualified x == hsnWild
%%]]
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

%%[7 export(mkHNmSpecial)
mkHNmSpecial :: HSNM x => x -> HsName
mkHNmSpecial = mkHNmPrefix hsnStrSpecialPrefix
%%]

%%[90 export(mkHNmExport)
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
mkRV' :: HsName -> HsName -> HsName
mkRV' _ = id

mkRV :: String -> HsName
mkRV = mkRV' undefined . hsnFromString

%%[[92
mkGenerRV :: String -> HsName
mkGenerRV = mkRV
%%]]
%%]

%%[99 -1.mkRV
mkRV' :: HsName -> HsName -> HsName
mkRV' m = hsnSetQual m

mkRV :: HsName -> String -> HsName
mkRV m = mkRV' m . hsnFromString

mkGenerRV :: String -> HsName
mkGenerRV = mkRV hsnModIntlBase
%%]

%%[92
mkGenerRVN' :: Int -> String -> String -> HsName
mkGenerRVN' n s suff = mkGenerRV (s ++ show n ++ suff)

mkGenerRVN :: Int -> String -> HsName
mkGenerRVN  n s = mkGenerRVN' n s ""

mkGenerRVN2 :: Int -> String -> HsName
mkGenerRVN2 n s = mkGenerRVN' n s "_"
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

%%[99
[hsnClassIx, hsnClassIxFldRange, hsnClassIxFldIndex, hsnClassIxFldInRange]
  = map
      (mkRV hsnModIntlIx) -- (mkRV hsnModIntlEnum)
      [ "Ix", "range", "index", "inRange" ]
%%]

%%[5 export(hsnBool,hsnTrue,hsnFalse,hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnClassEqFldEq,hsnPrelConcatMap)
[hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnPrelConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnDataOrdering, hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT
 , hsnPrelString
 , hsnClassEqFldEq
%%[[91
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
%%[[91
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
%%[[91
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
%%[[91
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
%%[[91
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
%%[[91
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

%%[90
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

%%[91
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

%%[91
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

%%[92
hsnModIntlGenericsTuple                 =   hsnPrefixQual hsnUHC (mkHNm         "Generics.Tuple")
%%]

%%[99 export(hsnModPrelude,hsnModIntlBase)
-- hsnModIntlRatio                         =   hsnPrefixQual hsnUHC (hsnFromString "Ratio")
-- hsnModIntlReal                          =   hsnPrefixQual hsnUHC (hsnFromString "Real")
-- hsnModIntlFractional                    =   hsnPrefixQual hsnUHC (hsnFromString "Fractional")
hsnModIntlBase                          =   hsnPrefixQual hsnUHC (hsnFromString "Base")
hsnModIntlEnum                          =   hsnPrefixQual hsnUHC (hsnFromString "Enum")
hsnModIntlIx                            =   hsnPrefixQual hsnUHC (hsnFromString "Ix")
hsnModIntlNum                           =   hsnPrefixQual hsnUHC (hsnFromString "Num")
hsnModIntlGenerics                      =   hsnPrefixQual hsnUHC (hsnFromString "Generics")
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
%%% Naming conventions for generic deriving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[92 export(hsnNm2Gener,hsnNm2GenerReprSyn,hsnNm2GenerDatatype,hsnNm2GenerConstructor,hsnNm2GenerSelector,hsnNm2GenerReprTuple)
-- | a hidden name corresponding to visible names
hsnNm2Gener :: HsName -> HsName
hsnNm2Gener = mkHNmHidden -- . hsnPrefix "Dict-"

-- | a hidden, but programmer accessible, name for representation type synonym, for datatypes
hsnNm2GenerReprSyn :: Int -> HsName -> HsName
hsnNm2GenerReprSyn i = mkHNmSpecial . hsnPrefix ("Rep" ++ show i)

-- | a hidden, but programmer accessible, name for representation type synonym, for tuples
hsnNm2GenerReprTuple :: Int -> Int -> HsName
hsnNm2GenerReprTuple arity i = hsnNm2GenerReprSyn i (mkHNm $ "Tuple" ++ show arity)

-- | a hidden name for representation datatype for a datatype
hsnNm2GenerDatatype :: HsName -> HsName
hsnNm2GenerDatatype = hsnNm2Gener . hsnPrefix ("D_")

-- | a hidden name for representation datatype for a datatype constructor
hsnNm2GenerConstructor :: HsName -> HsName
hsnNm2GenerConstructor = hsnNm2Gener . hsnPrefix ("C_")

-- | a hidden name for representation datatype for a datatype field selector
hsnNm2GenerSelector :: HsName -> HsName
hsnNm2GenerSelector = hsnNm2Gener . hsnPrefix ("S_")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Namings upon which generic deriving depends, taken into account by name & dependency analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[92 export(builtinGenerClassNmL)
builtinGenerClassNmL :: [HsName]
builtinGenerClassNmL
  = [ bi i
    | bi <- [ ehbnGenerClassConstructor
            , ehbnGenerClassDatatype
            , ehbnGenerClassSelector
            ]
    ]
  where i = mkEHBuiltinNames (\_ n -> hsnQualified n)
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
      , ehbnDataList                    :: HsName
      , ehbnDataBool                    :: HsName
%%[[11
      , ehbnPrelString                  :: HsName
%%]]
%%[[20
%%]]
%%[[90
      , ehbnFunPtr                      :: HsName
%%]]
%%[[91
      , ehbnMap                         :: HsName
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
%%[[92
		-- for generic deriving
      , ehbnGenerClassRepresentableN    	:: Int -> HsName
      , ehbnGenerClassRepresentableNFldFrom	:: Int -> HsName
      , ehbnGenerClassRepresentableNFldTo	:: Int -> HsName
      , ehbnGenerClassDatatype    			:: HsName
      , ehbnGenerClassDatatypeFldName    	:: HsName
      , ehbnGenerClassDatatypeFldModule    	:: HsName
      , ehbnGenerClassSelector    			:: HsName
      , ehbnGenerClassSelectorFldName		:: HsName
      , ehbnGenerDataNoSelector				:: HsName
      , ehbnGenerClassConstructor    		:: HsName
      , ehbnGenerClassConstructorFldName	:: HsName
      , ehbnGenerClassConstructorFldFixity	:: HsName
      , ehbnGenerClassConstructorFldIsRec	:: HsName
      , ehbnGenerDataVoid1              	:: HsName
      , ehbnGenerDataUnit1              	:: HsName
      , ehbnGenerDataUnit1AltU1          	:: HsName
      , ehbnGenerDataKonst1             	:: HsName
      , ehbnGenerDataKonst1AltK1         	:: HsName
      , ehbnGenerDataMeta1              	:: HsName
      , ehbnGenerDataMeta1AltM1          	:: HsName
      , ehbnGenerDataFixity    				:: HsName
      , ehbnGenerDataFixityAltPrefix		:: HsName
      , ehbnGenerDataFixityAltInfix			:: HsName
      , ehbnGenerDataAssociativity    		:: HsName
      , ehbnGenerDataAssociativityAltLeft	:: HsName
      , ehbnGenerDataAssociativityAltRight	:: HsName
      , ehbnGenerDataAssociativityAltNot	:: HsName
      , ehbnGenerDataSum    				:: HsName
      , ehbnGenerDataSumAltLeft				:: HsName
      , ehbnGenerDataSumAltRight			:: HsName
      , ehbnGenerDataProd    				:: HsName
      , ehbnGenerDataProdAltProd			:: HsName
      , ehbnGenerDataPar0    				:: HsName
      , ehbnGenerDataPar1    				:: HsName
      , ehbnGenerDataPar1AltPar1			:: HsName
      , ehbnGenerDataRec0    				:: HsName
      , ehbnGenerDataRec1    				:: HsName
      , ehbnGenerDataRec1AltRec1			:: HsName
      , ehbnGenerDataComp1    				:: HsName
      , ehbnGenerDataComp1AltComp1			:: HsName
      , ehbnGenerDataMetaB    				:: HsName
      , ehbnGenerDataMetaR    				:: HsName
      , ehbnGenerDataMetaP    				:: HsName
      , ehbnGenerDataMetaD    				:: HsName
      , ehbnGenerDataMetaC    				:: HsName
      , ehbnGenerDataMetaS    				:: HsName
      , ehbnGenerDataMetaDN    				:: Int -> HsName
      , ehbnGenerDataMetaCN    				:: Int -> HsName
      , ehbnGenerDataMetaS1    				:: HsName
      , ehbnGenerTupleRepresentableN		:: Int -> Int -> HsName
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
%%]]
%%[[98
      , ehbnIO                          :: HsName
      , ehbnHandle                      :: HsName
      , ehbnByteArray                   :: HsName
      , ehbnRealWorld                   :: HsName
%%]]
%%[[99
      , ehbnClassIx                     :: HsName
      , ehbnClassIxFldRange             :: HsName
      , ehbnClassIxFldIndex             :: HsName
      , ehbnClassIxFldInRange           :: HsName
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
      , ehbnDataBool                    = f IdOcc_Type      hsnBool
      , ehbnDataList                    = f IdOcc_Type      hsnDataList
%%[[11
      , ehbnPrelString                  = f IdOcc_Type      hsnPrelString
%%]]
%%[[20
%%]]
%%[[90
      , ehbnFunPtr                      = f IdOcc_Type      hsnFunPtr
%%]]
%%[[91
      , ehbnMap                         = f IdOcc_Type      hsnMap
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
%%[[92
		-- for generic deriving
      , ehbnGenerClassRepresentableN    	= \n -> f IdOcc_Class (mkGenerRVN  n "Representable"	)
      , ehbnGenerClassRepresentableNFldFrom	= \n -> f IdOcc_Val   (mkGenerRVN  n "from"				)
      , ehbnGenerClassRepresentableNFldTo	= \n -> f IdOcc_Val   (mkGenerRVN  n "to"				)
      , ehbnGenerClassDatatype    			=       f IdOcc_Class (mkGenerRV     "Datatype"			)
      , ehbnGenerClassDatatypeFldName    	=       f IdOcc_Val   (mkGenerRV     "datatypeName"		)
      , ehbnGenerClassDatatypeFldModule    	=       f IdOcc_Val   (mkGenerRV     "moduleName"		)
      , ehbnGenerClassSelector    			=       f IdOcc_Class (mkGenerRV     "Selector"			)
      , ehbnGenerClassSelectorFldName		=       f IdOcc_Val   (mkGenerRV     "selName"			)
      , ehbnGenerDataNoSelector				=       f IdOcc_Type  (mkGenerRV     "NoSelector"		)
      , ehbnGenerClassConstructor    		=       f IdOcc_Class (mkGenerRV     "Constructor"		)
      , ehbnGenerClassConstructorFldName	=       f IdOcc_Val   (mkGenerRV     "conName"			)
      , ehbnGenerClassConstructorFldFixity	=       f IdOcc_Val   (mkGenerRV     "conFixity"		)
      , ehbnGenerClassConstructorFldIsRec	=       f IdOcc_Val   (mkGenerRV     "conIsRecord"		)
      , ehbnGenerDataVoid1              	=       f IdOcc_Type  (mkGenerRV     "V1"				)
      , ehbnGenerDataUnit1              	=       f IdOcc_Type  (mkGenerRV     "U1"				)
      , ehbnGenerDataUnit1AltU1          	=       f IdOcc_Val   (mkGenerRV     "U1"				)
      , ehbnGenerDataKonst1             	=       f IdOcc_Type  (mkGenerRV     "K1"				)
      , ehbnGenerDataKonst1AltK1         	=       f IdOcc_Val   (mkGenerRV     "K1"				)
      , ehbnGenerDataMeta1              	=       f IdOcc_Type  (mkGenerRV     "M1"				)
      , ehbnGenerDataMeta1AltM1          	=       f IdOcc_Val   (mkGenerRV     "M1"				)
      , ehbnGenerDataFixity    				=       f IdOcc_Type  (mkGenerRV     "Fixity"			)
      , ehbnGenerDataFixityAltPrefix		=       f IdOcc_Val   (mkGenerRV     "Prefix"			)
      , ehbnGenerDataFixityAltInfix			=       f IdOcc_Val   (mkGenerRV     "Infix"			)
      , ehbnGenerDataAssociativity    		=       f IdOcc_Type  (mkGenerRV     "Associativity"	)
      , ehbnGenerDataAssociativityAltLeft	=       f IdOcc_Val   (mkGenerRV     "LeftAssociative"	)
      , ehbnGenerDataAssociativityAltRight	=       f IdOcc_Val   (mkGenerRV     "RightAssociative"	)
      , ehbnGenerDataAssociativityAltNot	=       f IdOcc_Val   (mkGenerRV     "NotAssociative"	)
      , ehbnGenerDataSum    				=       f IdOcc_Type  (mkGenerRV     ":+:"				)
      , ehbnGenerDataSumAltLeft				=       f IdOcc_Val   (mkGenerRV     "L1"				)
      , ehbnGenerDataSumAltRight			=       f IdOcc_Val   (mkGenerRV     "R1"				)
      , ehbnGenerDataProd    				=       f IdOcc_Type  (mkGenerRV     ":*:"				)
      , ehbnGenerDataProdAltProd			=       f IdOcc_Val   (mkGenerRV     ":*:"				)
      , ehbnGenerDataPar0    				=       f IdOcc_Type  (mkGenerRV     "Par0"				)
      , ehbnGenerDataPar1    				=       f IdOcc_Type  (mkGenerRV     "Par1"				)
      , ehbnGenerDataPar1AltPar1			=       f IdOcc_Val   (mkGenerRV     "Par1"				)
      , ehbnGenerDataRec0    				=       f IdOcc_Type  (mkGenerRV     "Rec0"				)
      , ehbnGenerDataRec1    				=       f IdOcc_Type  (mkGenerRV     "Rec1"				)
      , ehbnGenerDataRec1AltRec1			=       f IdOcc_Val   (mkGenerRV     "Rec1"				)
      , ehbnGenerDataComp1    				=       f IdOcc_Type  (mkGenerRV     ":.:"				)
      , ehbnGenerDataComp1AltComp1			=       f IdOcc_Val   (mkGenerRV     "Comp1"			)
      , ehbnGenerDataMetaB    				=       f IdOcc_Type  (mkGenerRV     "B"				)
      , ehbnGenerDataMetaR    				=       f IdOcc_Type  (mkGenerRV     "R"				)
      , ehbnGenerDataMetaP    				=       f IdOcc_Type  (mkGenerRV     "P"				)
      , ehbnGenerDataMetaD    				=       f IdOcc_Type  (mkGenerRV     "D"				)
      , ehbnGenerDataMetaC    				=       f IdOcc_Type  (mkGenerRV     "C"				)
      , ehbnGenerDataMetaS    				=       f IdOcc_Type  (mkGenerRV     "S"				)
      , ehbnGenerDataMetaDN    				= \n -> f IdOcc_Type  (mkGenerRVN  n "D"				)
      , ehbnGenerDataMetaCN    				= \n -> f IdOcc_Type  (mkGenerRVN  n "C"				)
      , ehbnGenerDataMetaS1    				=       f IdOcc_Type  (mkGenerRV     "S1"				)
      , ehbnGenerTupleRepresentableN    	= \n a -> f IdOcc_Type (mkRV' hsnModIntlGenericsTuple $ hsnNm2GenerReprTuple a n)
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
%%]]
%%[[98
      , ehbnIO                          = f IdOcc_Type      hsnIO
      , ehbnHandle                      = f IdOcc_Type      hsnHandle
      , ehbnByteArray                   = f IdOcc_Type      hsnByteArray
      , ehbnRealWorld                   = f IdOcc_Type      hsnRealWorld
%%]]
%%[[99
      , ehbnClassIx                     = f IdOcc_Class     hsnClassIx
      , ehbnClassIxFldRange             = f IdOcc_Val       hsnClassIxFldRange  
      , ehbnClassIxFldIndex             = f IdOcc_Val       hsnClassIxFldIndex  
      , ehbnClassIxFldInRange           = f IdOcc_Val       hsnClassIxFldInRange
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

