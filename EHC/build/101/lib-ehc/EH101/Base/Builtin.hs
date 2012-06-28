module EH101.Base.Builtin
( hsnWild, hsnArrow, strProd, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar
, hsnIsWild
, hsnStrHiddenPrefix
, mkHNmHidden
, hsnNegate
, hsnError
, hsnUn, hsnIsUn, hsnUnUn
, hsnEqTilde, hsnIsEqTilde
, hsnCovariant, hsnContravariant, hsnInvariant
, hsnIsList
, hsnEnumFromThenTo, hsnEnumFromThen, hsnEnumFromTo, hsnEnumFrom
, hsnBool, hsnTrue, hsnFalse, hsnDataList, hsnDataListAltCons, hsnDataListAltNil, hsnClassEqFldEq, hsnPrelConcatMap
, charKindStar, hsnKindStar
, mkHNmSpecial
, hsnORow, hsnCRow, hsnORec, hsnCRec, hsnOSum, hsnCSum
, hsnRow, hsnRec, hsnSum, hsnRowEmpty, hsnIsRec, hsnIsSum, hsnIsRow
, positionalFldNames
, hsnKindRow
, hsnMain
, hsnIsConstructorName
, hsnUndefined, hsnPackedString, hsnPackedStringToString, hsnPrelId, hsnPrimAddInt
, EHBuiltinNames (..), mkEHBuiltinNames
, hsnOImpl, hsnCImpl, hsnPrArrow, hsnIsPrArrow, hsnIsUnknown
, hsnMonadSeq, hsnMonadBind, hsnMonadFail, hsnClassEq
, hsnClass2Dict
, hsnClass2Kind
, hsnClass2Polarity
, hsnDynVar
, hsnPolNegation
, hsnOParensUnboxed, hsnCParensUnboxed
, hsnRecUnboxed
, hsnKindUnboxed
, hsnIntUnboxed
, hsnFldUpd
, hsnModBuiltin
, hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT
, hsnNm2Gener, hsnNm2GenerReprSyn, hsnNm2GenerDatatype, hsnNm2GenerConstructor, hsnNm2GenerSelector, hsnNm2GenerReprTuple
, builtinGenerClassNmL
, hsnInteger
, hsnInt8Unboxed, hsnInt16Unboxed, hsnInt32Unboxed, hsnInt64Unboxed, hsnWordUnboxed, hsnWord8Unboxed, hsnWord16Unboxed, hsnWord32Unboxed, hsnWord64Unboxed
, hsnPackedStringToInteger, hsnPrimIntegerToInt, hsnPrimIntToInteger
, hsnFromInteger
, hsnFromRational
, hsnMkRatio
, hsnIO
, hsnAddrUnboxed
, hsnEhcRunMain
, hsnStackTracePush
, hsnIsInPrelude
, hsnModPrelude, hsnModIntlBase )
where
import EH101.Base.HsName
import Data.Maybe
import EH.Util.Utils
import Data.List
import Data.Char (isUpper)





{-# LINE 28 "src/ehc/Base/Builtin.chs" #-}
strProd :: Int -> String

{-# LINE 32 "src/ehc/Base/Builtin.chs" #-}
hsnArrow, hsnUnknown, hsnInt, hsnChar, hsnWild
                                    ::  HsName
hsnProd                             ::  Int -> HsName
hsnProdArity                        ::  HsName -> Int

{-# LINE 39 "src/ehc/Base/Builtin.chs" #-}
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
hsnIsProd       _                   =   False

hsnProdArity    n | isJust ms       = case fromJust ms of
                                        (_:ar) -> read ar
                                        _      -> 0
                                    where ms = mbHNm n

{-# LINE 65 "src/ehc/Base/Builtin.chs" #-}
hsnIsWild :: HsName -> Bool
hsnIsWild x = hsnQualified x == hsnWild

{-# LINE 74 "src/ehc/Base/Builtin.chs" #-}
hsnStrHiddenPrefix                  =   "$"

{-# LINE 78 "src/ehc/Base/Builtin.chs" #-}
hsnStrSpecialPrefix                 =   "_"

{-# LINE 82 "src/ehc/Base/Builtin.chs" #-}
mkHNmHidden :: HSNM x => x -> HsName
mkHNmHidden = mkHNmPrefix hsnStrHiddenPrefix

{-# LINE 87 "src/ehc/Base/Builtin.chs" #-}
mkHNmSpecial :: HSNM x => x -> HsName
mkHNmSpecial = mkHNmPrefix hsnStrSpecialPrefix

{-# LINE 97 "src/ehc/Base/Builtin.chs" #-}
strUn                               =   hsnStrHiddenPrefix ++ "un"

{-# LINE 101 "src/ehc/Base/Builtin.chs" #-}
strFldUpd                           =   hsnStrHiddenPrefix ++ "upd_"

{-# LINE 110 "src/ehc/Base/Builtin.chs" #-}
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   strUn `hsnPrefix` nm

{-# LINE 115 "src/ehc/Base/Builtin.chs" #-}
hsnIsUn                             ::  HsName -> Bool
hsnIsUn                             =   maybe False (isPrefixOf strUn) . hsnMbBaseString
-- hsnIsUn         (HsName_Base s)     =   isPrefixOf strUn $ hsnHNmFldToString s

{-# LINE 129 "src/ehc/Base/Builtin.chs" #-}
hsnUnUn                             ::  HsName -> HsName
hsnUnUn         n                   =   maybe n (\(s,mk) -> mk $ drop (length strUn) s) $ hsnBaseUnpack n
-- hsnUnUn         (HsName_Base s)     =   hsnFromString $ drop (length strUn) $ hsnHNmFldToString s

{-# LINE 148 "src/ehc/Base/Builtin.chs" #-}
hsnFldUpd                           ::  HsName -> HsName
hsnFldUpd       nm                  =   strFldUpd `hsnPrefix` nm

{-# LINE 153 "src/ehc/Base/Builtin.chs" #-}
hsnIsList       hsn                 =   hsn == hsnDataList

{-# LINE 157 "src/ehc/Base/Builtin.chs" #-}
hsnORow                             =   hsnFromString "{|"
hsnCRow                             =   hsnFromString "|}"
hsnOSum                             =   hsnFromString "{<"
hsnCSum                             =   hsnFromString ">}"
hsnORec                             =   hsnFromString "("
hsnCRec                             =   hsnFromString ")"

{-# LINE 166 "src/ehc/Base/Builtin.chs" #-}
hsnRow                              =   hsnFromString (hsnStrSpecialPrefix ++ "Row")
hsnRec                              =   hsnFromString (hsnStrSpecialPrefix ++ "Rec")
hsnSum                              =   hsnFromString (hsnStrSpecialPrefix ++ "Var")
hsnRowEmpty                         =   hsnFromString (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow

{-# LINE 178 "src/ehc/Base/Builtin.chs" #-}
positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map mkHNmPos [1..]

{-# LINE 183 "src/ehc/Base/Builtin.chs" #-}
hsnMain                             =   hsnFromString "main"

{-# LINE 187 "src/ehc/Base/Builtin.chs" #-}
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
{-# LINE 201 "src/ehc/Base/Builtin.chs" #-}
hsnIsConstructorName n             = hsnIsConstructorName (snd $ hsnInitLast n)

{-# LINE 205 "src/ehc/Base/Builtin.chs" #-}
hsnOImpl          =   hsnFromString "{!"
hsnCImpl          =   hsnFromString "!}"
hsnPrArrow        =   hsnFromString "=>"

hsnIsPrArrow                        ::  HsName -> Bool
hsnIsPrArrow    hsn                 =   hsn == hsnPrArrow
hsnIsUnknown                        =   (==hsnUnknown)

{-# LINE 215 "src/ehc/Base/Builtin.chs" #-}
hsnDynVar         =   hsnFromString "?"

{-# LINE 219 "src/ehc/Base/Builtin.chs" #-}
hsnEqTilde        =   hsnFromString "~"

hsnIsEqTilde = (==hsnEqTilde)

{-# LINE 225 "src/ehc/Base/Builtin.chs" #-}
hsnCovariant, hsnContravariant, hsnInvariant :: HsName
hsnCovariant      = mkHNm "+Covariant"
hsnContravariant  = mkHNm "-Contravariant"
hsnInvariant      = mkHNm "*Invariant"

{-# LINE 232 "src/ehc/Base/Builtin.chs" #-}
hsnPolNegation :: HsName
hsnPolNegation    = mkHNm "^Negate"

{-# LINE 237 "src/ehc/Base/Builtin.chs" #-}
hsnOParensUnboxed =   hsnFromString "(#"
hsnCParensUnboxed =   hsnFromString "#)"

{-# LINE 242 "src/ehc/Base/Builtin.chs" #-}
hsnRecUnboxed     =   hsnFromString (hsnStrSpecialPrefix ++ "Rec#")

{-# LINE 246 "src/ehc/Base/Builtin.chs" #-}
hsnInteger        =   hsnFromString "Integer"

{-# LINE 254 "src/ehc/Base/Builtin.chs" #-}
charKindStar                        =   '*'
hsnKindStar                         =   hsnFromString [charKindStar]

{-# LINE 259 "src/ehc/Base/Builtin.chs" #-}
hsnKindRow                          =   hsnRow

{-# LINE 263 "src/ehc/Base/Builtin.chs" #-}
hsnKindUnboxed                      =   hsnFromString "#"

{-# LINE 274 "src/ehc/Base/Builtin.chs" #-}
hsnIntUnboxed      =   hsnFromString "Int#"

{-# LINE 278 "src/ehc/Base/Builtin.chs" #-}
hsnInt8Unboxed     =   hsnFromString "Int8#"
hsnInt16Unboxed    =   hsnFromString "Int16#"
hsnInt32Unboxed    =   hsnFromString "Int32#"
hsnInt64Unboxed    =   hsnFromString "Int64#"
hsnWordUnboxed     =   hsnFromString "Word#"
hsnWord8Unboxed    =   hsnFromString "Word8#"
hsnWord16Unboxed   =   hsnFromString "Word16#"
hsnWord32Unboxed   =   hsnFromString "Word32#"
hsnWord64Unboxed   =   hsnFromString "Word64#"

{-# LINE 290 "src/ehc/Base/Builtin.chs" #-}
hsnAddrUnboxed     =   hsnFromString "Addr#"

{-# LINE 311 "src/ehc/Base/Builtin.chs" #-}
mkRV' :: HsName -> HsName -> HsName
mkRV' m = hsnSetQual m

mkRV :: HsName -> String -> HsName
mkRV m = mkRV' m . hsnFromString

mkGenerRV :: String -> HsName
mkGenerRV = mkRV hsnModIntlBase

{-# LINE 322 "src/ehc/Base/Builtin.chs" #-}
mkGenerRVN' :: Int -> String -> String -> HsName
mkGenerRVN' n s suff = mkGenerRV (s ++ show n ++ suff)

mkGenerRVN :: Int -> String -> HsName
mkGenerRVN  n s = mkGenerRVN' n s ""

mkGenerRVN2 :: Int -> String -> HsName
mkGenerRVN2 n s = mkGenerRVN' n s "_"

{-# LINE 337 "src/ehc/Base/Builtin.chs" #-}
[hsnNegate]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlNum)
      [ "negate" ]

{-# LINE 348 "src/ehc/Base/Builtin.chs" #-}
[hsnError]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
      [ "error" ]

{-# LINE 365 "src/ehc/Base/Builtin.chs" #-}
[hsnEnumFromThenTo,hsnEnumFromThen,hsnEnumFromTo,hsnEnumFrom]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlEnum)
      [ "enumFromThenTo", "enumFromThen", "enumFromTo", "enumFrom" ]

{-# LINE 376 "src/ehc/Base/Builtin.chs" #-}
[hsnClassIx, hsnClassIxFldRange, hsnClassIxFldIndex, hsnClassIxFldInRange]
  = map
      (mkRV hsnModIntlIx) -- (mkRV hsnModIntlEnum)
      [ "Ix", "range", "index", "inRange" ]

{-# LINE 383 "src/ehc/Base/Builtin.chs" #-}
[hsnDataList,hsnDataListAltCons,hsnDataListAltNil,hsnPrelConcatMap
 , hsnBool,hsnTrue,hsnFalse
 , hsnDataOrdering, hsnDataOrderingAltEQ, hsnDataOrderingAltLT, hsnDataOrderingAltGT
 , hsnPrelString
 , hsnClassEqFldEq
 , hsnMap
 , hsnClassBounded, hsnClassBoundedFldMinBound, hsnClassBoundedFldMaxBound
 , hsnClassEnum, hsnClassEnumFldFromEnum, hsnClassEnumFldToEnum, hsnClassEnumFldSucc, hsnClassEnumFldPred
 ]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
      [ "[]", ":", "[]", "concatMap"
      , "Bool", "True", "False"
      , "Ordering", "EQ", "LT", "GT"
      , "String"
      , "=="
      , "map"
      , "Bounded", "minBound", "maxBound"
      , "Enum", "fromEnum", "toEnum", "succ", "pred"
      ]


{-# LINE 423 "src/ehc/Base/Builtin.chs" #-}
[ hsnUndefined
 , hsnPackedString
 , hsnPackedStringToString
 , hsnPrelId
 , hsnPrimAddInt
 , hsnPrimGtInt
 , hsnPrimLtInt
 , hsnPackedStringToInteger
 , hsnPrimIntegerToInt
 , hsnPrimIntToInteger
 , hsnPrimEqChar
 ]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
      [ "undefined"
      , "PackedString"
      , "packedStringToString"
      , "id"
      , "primAddInt"
      , "primGtInt"
      , "primLtInt"
      , "packedStringToInteger"
      , "primIntegerToInt"
      , "primIntToInteger"
      , "primEqChar"
      ]

{-# LINE 474 "src/ehc/Base/Builtin.chs" #-}
[hsnMonadSeq,hsnMonadBind,hsnMonadFail
 , hsnClassEq
 , hsnBoolAnd
 , hsnBoolOr
 , hsnClassOrd, hsnClassOrdFldCompare
 , hsnPrelConcat2, hsnPrelConcat
 , hsnPrelCompose
 ]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlBase)
      [ ">>", ">>=", "fail"
      , "Eq"
      , "&&"
      , "||"
      , "Ord", "compare"
      , "++", "concat"
      , "."
      ]

{-# LINE 507 "src/ehc/Base/Builtin.chs" #-}
[hsnFunPtr]
  = map
      (mkRV hsnModIntlPtr)
      [ "FunPtr"
      ]

{-# LINE 519 "src/ehc/Base/Builtin.chs" #-}
[hsnAddr]
  = map
      (mkRV hsnModIntlTypes)
      [ "Addr"
      ]

{-# LINE 531 "src/ehc/Base/Builtin.chs" #-}
[hsnClassShow
 , hsnClassShowFldShow, hsnClassShowFldShowsPrec
 , hsnPrelShowString, hsnPrelShowParen
 ]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlShow)
      [ "Show"
      , "show", "showsPrec"
      , "showString", "showParen"
      ]

{-# LINE 548 "src/ehc/Base/Builtin.chs" #-}
[hsnClassRead
 , hsnClassReadFldRead, hsnClassReadFldReadsPrec
 , hsnPrelLex, hsnPrelReadParen
 ]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlRead)
      [ "Read"
      , "read", "readsPrec"
      , "lex", "readParen"
      ]

{-# LINE 569 "src/ehc/Base/Builtin.chs" #-}
[hsnFromInteger]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlNum)
      [ "fromInteger" ]

{-# LINE 580 "src/ehc/Base/Builtin.chs" #-}
[hsnFloat,hsnDouble
 ]
  = map
      (mkRV hsnModIntlBase)
      [ "Float", "Double"
      ]

{-# LINE 593 "src/ehc/Base/Builtin.chs" #-}
[hsnWord,hsnWord8,hsnWord16,hsnWord32,hsnWord64
 ]
  = map
      (mkRV hsnModIntlTypes)
      [ "Word", "Word8", "Word16", "Word32", "Word64"
      ]

{-# LINE 606 "src/ehc/Base/Builtin.chs" #-}
[hsnInt8, hsnInt16, hsnInt32, hsnInt64
 ]
  = map
      (mkRV hsnModIntlTypes)
      [ "Int8" , "Int16" , "Int32" , "Int64"
      ]

{-# LINE 619 "src/ehc/Base/Builtin.chs" #-}
[hsnFromRational]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlFractional)
      [ "fromRational" ]

{-# LINE 630 "src/ehc/Base/Builtin.chs" #-}
[hsnMkRatio]
  = map
      (mkRV hsnModIntlBase) -- (mkRV hsnModIntlRatio)
      [ ":%" ]

{-# LINE 645 "src/ehc/Base/Builtin.chs" #-}
[hsnIO,hsnHandle,hsnByteArray,hsnRealWorld]
  = map
      (mkRV hsnModIntlBase)
      [ "IO", "Handle", "ByteArray", "RealWorld" ]

{-# LINE 660 "src/ehc/Base/Builtin.chs" #-}
[hsnEhcRunMain]
  = map
      (mkRV hsnModIntlRun)
      [ "ehcRunMain" ]

{-# LINE 671 "src/ehc/Base/Builtin.chs" #-}
[hsnStackTracePush]
  = map
      (mkRV hsnModIntlBase)
      [ "pushExplicitStackTrace" ]

{-# LINE 682 "src/ehc/Base/Builtin.chs" #-}
hsnModBuiltin                       =   mkHNm "#Builtin"

{-# LINE 690 "src/ehc/Base/Builtin.chs" #-}
hsnUHC                              =   hsnFromString "UHC"

{-# LINE 694 "src/ehc/Base/Builtin.chs" #-}
hsnIsInPrelude :: HsName -> Bool
hsnIsInPrelude n
  = case hsnInitLast n of
      ((m:_),_) -> m == hsnUHC
      _         -> False

{-# LINE 702 "src/ehc/Base/Builtin.chs" #-}
hsnModIntlGenericsTuple                 =   hsnPrefixQual hsnUHC (mkHNm         "Generics.Tuple")

{-# LINE 706 "src/ehc/Base/Builtin.chs" #-}
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

{-# LINE 720 "src/ehc/Base/Builtin.chs" #-}
hsnModIntlTypes                         =   hsnPrefixQual hsnUHC (hsnFromString "Types")
hsnModIntlPtr                           =   hsnPrefixQual hsnUHC (hsnFromString "Ptr")
hsnModIntlRun                           =   hsnPrefixQual hsnUHC (hsnFromString "Run")
hsnModIntlIOBase                        =   hsnPrefixQual hsnUHC (hsnFromString "IOBase")
hsnModIntlStackTrace                    =   hsnPrefixQual hsnUHC (hsnFromString "StackTrace")

{-# LINE 732 "src/ehc/Base/Builtin.chs" #-}
-- Dict datatype name for class name, only used when `not ehcCfgClassViaRec'
hsnClass2Dict :: HsName -> HsName
hsnClass2Dict = mkHNmHidden . hsnPrefix "Dict-"

{-# LINE 738 "src/ehc/Base/Builtin.chs" #-}
hsnClass2Kind :: HsName -> HsName
hsnClass2Kind = mkHNmHidden . hsnPrefix "ClassKind-"

{-# LINE 743 "src/ehc/Base/Builtin.chs" #-}
hsnClass2Polarity :: HsName -> HsName
hsnClass2Polarity = mkHNmHidden . hsnPrefix "ClassPolarity-"

{-# LINE 752 "src/ehc/Base/Builtin.chs" #-}
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

{-# LINE 782 "src/ehc/Base/Builtin.chs" #-}
builtinGenerClassNmL :: [HsName]
builtinGenerClassNmL
  = [ bi i
    | bi <- [ ehbnGenerClassConstructor
            , ehbnGenerClassDatatype
            , ehbnGenerClassSelector
            ]
    ]
  where i = mkEHBuiltinNames (\_ n -> hsnQualified n)

{-# LINE 798 "src/ehc/Base/Builtin.chs" #-}
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
      , ehbnPrelString                  :: HsName
      , ehbnFunPtr                      :: HsName
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
      , ehbnIO                          :: HsName
      , ehbnHandle                      :: HsName
      , ehbnByteArray                   :: HsName
      , ehbnRealWorld                   :: HsName
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
      , ehbnPrelString                  = f IdOcc_Type      hsnPrelString
      , ehbnFunPtr                      = f IdOcc_Type      hsnFunPtr
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
      , ehbnIO                          = f IdOcc_Type      hsnIO
      , ehbnHandle                      = f IdOcc_Type      hsnHandle
      , ehbnByteArray                   = f IdOcc_Type      hsnByteArray
      , ehbnRealWorld                   = f IdOcc_Type      hsnRealWorld
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
      }

