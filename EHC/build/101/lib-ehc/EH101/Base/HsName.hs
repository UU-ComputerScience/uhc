module EH101.Base.HsName
( HSNM (..)
, HsName (..)
, mkHNmBase
, hsnBaseUnpack', hsnBaseUnpack
, hsnMbBaseString, hsnBaseString
, mbHNm
, hsnFromString
, hsnInitLast
, hsnPrefix, hsnSuffix, mkHNmPrefix
, IdOccKind (..)
, IdOcc (..)
, HsNameS
, HsNameUniqifier (..)
, HsNameUnique (..)
, hsnUniqify, hsnUniqifyUID, hsnUniqifyStr, hsnUniqifyInt, hsnUniqifyEval
, mkHNmPos
, cmpHsNameOnNm
, hsnShow
, rowLabCmp
, OrigName (..)
, hsnShowAlphanumeric, hsnShowAlphanumericShort
, hsnSplitQualify, hsnQualified, hsnPrefixQual, hsnMapQualified
, hsnFixUniqifiers
, hsnStripUniqifiers
, hsnSafeJavaLike
, hsnJavaLikeVar
, hsnJavaLikeVarCls
, hsnJavaLikeVarToFld
, hsnJavaLikeDataTy, hsnJavaLikeDataCon, hsnJavaLikeDataFldAt, hsnJavaLikeDataFlds
, FvS, FvSMp
, HsNameMp, hsnRepl
, RPatNm (..)
, rpatNmIsOrig
, Track (..)
, hsnConcat
, hsnQualifier, hsnSetQual, hsnIsQual, hsnMapQual, hsnSetLevQual
, hsnQualUniqify )
where
import EH.Util.Utils
import EH.Util.Pretty
import Data.List
import EH101.Base.UID
import UU.Scanner.Position
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Char
import Numeric
import EH.Util.FPath
import Data.Char
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize








{-# LINE 54 "src/ehc/Base/HsName.chs" #-}
-- | A HsNameUniqifier represents the 'type' of unification
data HsNameUniqifier
  = HsNameUniqifier_Blank				-- just a new identifier, with an empty show
  | HsNameUniqifier_New					-- just a new identifier
  | HsNameUniqifier_GloballyUnique		-- globally unique
  | HsNameUniqifier_Evaluated			-- evaluated
  | HsNameUniqifier_Field				-- extracted field
  | HsNameUniqifier_Class				-- class
  | HsNameUniqifier_ClassDict			-- dictionary
  | HsNameUniqifier_SelfDict			-- dictionary under construction itself, passed as arg in tying the knot recursion
  | HsNameUniqifier_ResultDict			-- dictionary under construction result
  | HsNameUniqifier_SuperClass			-- super class field
  | HsNameUniqifier_DictField			-- dictionary field
  | HsNameUniqifier_Inline				-- new identifier because of inlining
  | HsNameUniqifier_GloballyUniqueDict	-- globally unique dictionary
  | HsNameUniqifier_FieldOffset			-- offset for a field
  | HsNameUniqifier_CaseContinuation	-- continuation of a case expression
  | HsNameUniqifier_GrinUpdated			-- Grin: updated value
  | HsNameUniqifier_FFIArg				-- arg evaluated for FFI
  | HsNameUniqifier_LacksLabel			-- label used in lacking predicates
  | HsNameUniqifier_BindAspect			-- binding aspect
  | HsNameUniqifier_Strict				-- strict variant of binding
  | HsNameUniqifier_GenericClass		-- a name introduced by generics
  | HsNameUniqifier_JSSwitchResult		-- var for result of switch
  | HsNameUniqifier_FFE					-- name of value to be ff exported
  | HsNameUniqifier_FFECoerced			-- name of possibly coerced value to be ff exported
  deriving (Eq,Ord,Enum)

-- | The show of a HsNameUniqifier is found back in the pretty printed code, current convention is 3 uppercase letters, as a balance between size and clarity of meaning
instance Show HsNameUniqifier where
  show HsNameUniqifier_Blank			 	= ""
  show HsNameUniqifier_New			 		= "NEW"
  show HsNameUniqifier_GloballyUnique 		= "UNQ"
  show HsNameUniqifier_Evaluated 			= "EVL"
  show HsNameUniqifier_Field	 			= "FLD"
  show HsNameUniqifier_Class	 			= "CLS"
  show HsNameUniqifier_ClassDict	 		= "DCT"
  show HsNameUniqifier_SelfDict	 			= "SDC"
  show HsNameUniqifier_ResultDict	 		= "RDC"
  show HsNameUniqifier_SuperClass	 		= "SUP"
  show HsNameUniqifier_DictField	 		= "DFL"
  show HsNameUniqifier_Inline		 		= "INL"
  show HsNameUniqifier_GloballyUniqueDict	= "UND"
  show HsNameUniqifier_FieldOffset			= "OFF"
  show HsNameUniqifier_CaseContinuation		= "CCN"
  show HsNameUniqifier_GrinUpdated			= "UPD"
  show HsNameUniqifier_FFIArg				= "FFI"
  show HsNameUniqifier_LacksLabel			= "LBL"
  show HsNameUniqifier_BindAspect			= "ASP"
  show HsNameUniqifier_Strict			    = "STR"
  show HsNameUniqifier_GenericClass			= "GEN"
  show HsNameUniqifier_JSSwitchResult		= "JSW"
  show HsNameUniqifier_FFE					= "FFE"
  show HsNameUniqifier_FFECoerced			= "FFC"

{-# LINE 123 "src/ehc/Base/HsName.chs" #-}
-- | A HsNameUnique represents the optional additional info to make the uniqification even more unique
data HsNameUnique
  = HsNameUnique_None
  | HsNameUnique_String     !String
  | HsNameUnique_Int        !Int
  | HsNameUnique_UID        !UID
  deriving (Eq,Ord)

instance Show HsNameUnique where
  show (HsNameUnique_None    ) = ""
  show (HsNameUnique_String s) = s
  show (HsNameUnique_Int    i) = show i
  show (HsNameUnique_UID    u) = show u

{-# LINE 139 "src/ehc/Base/HsName.chs" #-}
type HsNameUniqifierMp = Map.Map HsNameUniqifier [HsNameUnique]

emptyHsNameUniqifierMp :: HsNameUniqifierMp
emptyHsNameUniqifierMp = Map.empty

showHsNameUniqifierMp' :: Bool -> String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp' showLen usep us
  = [ slen u ++ show uqf ++ concat [ usep ++ show uu | uu <- u, uu /= HsNameUnique_None ] | (uqf,u) <- Map.toList us ]
  where slen u | showLen   = usep ++ show (length u)
               | otherwise = ""

showHsNameUniqifierMp :: String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp = showHsNameUniqifierMp' True

{-# LINE 169 "src/ehc/Base/HsName.chs" #-}
uniqifierMpAdd :: HsNameUniqifier -> HsNameUnique -> HsNameUniqifierMp -> HsNameUniqifierMp
uniqifierMpAdd ufier u m = Map.unionWith (++) (Map.singleton ufier [u]) m

{-# LINE 178 "src/ehc/Base/HsName.chs" #-}
hsnUniqify' :: HsNameUniqifier -> HsNameUnique -> HsName -> HsName
hsnUniqify' ufier u
  = mk
  where mk n@(HsName_Modf {hsnUniqifiers=us}) = n {hsnUniqifiers = uniqifierMpAdd ufier u us}
        mk n                                 = mk (HsName_Modf [] n Map.empty)

-- | Uniqify with just a name suffix
hsnUniqify :: HsNameUniqifier -> HsName -> HsName
hsnUniqify ufier = hsnUniqify' ufier HsNameUnique_None

-- | Uniqify with a name suffix + extra Int uniq info
hsnUniqifyInt :: HsNameUniqifier -> Int -> HsName -> HsName
hsnUniqifyInt ufier u = hsnUniqify' ufier (HsNameUnique_Int u)

-- | Uniqify with a name suffix + extra UID uniq info
hsnUniqifyUID :: HsNameUniqifier -> UID -> HsName -> HsName
hsnUniqifyUID ufier u = hsnUniqify' ufier (HsNameUnique_UID u)

-- | Uniqify with a name suffix + extra String uniq info
hsnUniqifyStr :: HsNameUniqifier -> String -> HsName -> HsName
hsnUniqifyStr ufier u = hsnUniqify' ufier (HsNameUnique_String u)

-- | Uniqify for use as evaluated name
hsnUniqifyEval :: HsName -> HsName
hsnUniqifyEval = hsnUniqify HsNameUniqifier_Evaluated

{-# LINE 210 "src/ehc/Base/HsName.chs" #-}
data HsName
  =   HsName_Base                   !String
  |   HsName_Modf
        { hsnQualifiers         ::  ![String]
        , hsnBase               ::  !HsName
        , hsnUniqifiers         ::  !HsNameUniqifierMp
        }
  |   HsName_Pos                    !Int
  -- |   HNmQ                          ![HsName]
  |   HNmNr                         !Int !OrigName
  deriving (Eq,Ord)

{-# LINE 236 "src/ehc/Base/HsName.chs" #-}
-- | Just lift a string to the base HsName variant
mkHNmBase :: String -> HsName
mkHNmBase s = HsName_Modf [] (HsName_Base s) Map.empty

{-# LINE 246 "src/ehc/Base/HsName.chs" #-}
-- | unpack a HsName into qualifiers + base string + repack function
hsnBaseUnpack' :: HsName -> Maybe ([String],String,[String] -> String -> HsName)
hsnBaseUnpack' (HsName_Base s    ) = Just ([],s,\_ s -> HsName_Base s)
hsnBaseUnpack' (HsName_Modf q b u) = fmap (\(bs,mk) -> (q, bs, \q s -> HsName_Modf q (mk s) u)) (hsnBaseUnpack b)
hsnBaseUnpack' _                   = Nothing

-- | unpack a HsName into base string + repack function
hsnBaseUnpack :: HsName -> Maybe (String,String -> HsName)
hsnBaseUnpack (HsName_Base s    ) = Just (s,HsName_Base)
hsnBaseUnpack (HsName_Modf q b u) = fmap (\(bs,mk) -> (bs, \s -> HsName_Modf q (mk s) u)) (hsnBaseUnpack b)
hsnBaseUnpack _                   = Nothing

{-# LINE 264 "src/ehc/Base/HsName.chs" #-}
-- | If name is a HsName_Base after some unpacking, return the base string, without qualifiers, without uniqifiers
hsnMbBaseString :: HsName -> Maybe String
hsnMbBaseString = fmap fst . hsnBaseUnpack

hsnBaseString :: HsName -> String
hsnBaseString = maybe "??" id . hsnMbBaseString


{-# LINE 274 "src/ehc/Base/HsName.chs" #-}
-- | Just lift a int to the int HsName variant
mkHNmPos :: Int -> HsName
mkHNmPos s = HsName_Modf [] (HsName_Pos s) Map.empty

{-# LINE 284 "src/ehc/Base/HsName.chs" #-}
cmpHsNameOnNm n1           n2           = compare n1 n2

{-# LINE 288 "src/ehc/Base/HsName.chs" #-}
mbHNm :: HsName -> Maybe String
mbHNm = hsnMbBaseString

{-# LINE 293 "src/ehc/Base/HsName.chs" #-}
hsnFromString :: String -> HsName
hsnFromString = mkHNmBase

{-# LINE 303 "src/ehc/Base/HsName.chs" #-}
data OrigName
  = OrigNone
  | OrigLocal  HsName
  | OrigGlobal HsName
  | OrigFunc   HsName
  deriving (Eq,Ord)

{-# LINE 312 "src/ehc/Base/HsName.chs" #-}
instance PP HsName where
  pp h = pp (show h)

{-# LINE 318 "src/ehc/Base/HsName.chs" #-}
hsnShow :: Bool -> String -> String -> HsName -> String
hsnShow _ _    _    (HsName_Base   s         )  = {- hsnHNmFldToString -} s
hsnShow l qsep usep (HsName_Modf qs b us     )  = concat $ (intersperse qsep $ qs ++ [hsnShow l qsep usep b]) ++ showHsNameUniqifierMp' l usep us
hsnShow _ _    _    (HsName_Pos  p           )  = show p
hsnShow _ _    _    (HNmNr n OrigNone        )  = "x_"        ++ show n
hsnShow l _    usep (HNmNr n (OrigLocal  hsn))  = "x_"        ++ show n ++ "_" ++ hsnShow l "." usep hsn
hsnShow l _    usep (HNmNr n (OrigGlobal hsn))  = "global_x_" ++ show n ++ "_" ++ hsnShow l "." usep hsn
hsnShow l _    usep (HNmNr n (OrigFunc   hsn))  = "fun_x_"    ++ show n ++ "_" ++ hsnShow l "." usep hsn

{-# LINE 331 "src/ehc/Base/HsName.chs" #-}
instance Show HsName where
  show = hsnShow True "." "_@"

{-# LINE 340 "src/ehc/Base/HsName.chs" #-}
-- | A HsName is either a complex/aggregrate name or a base case
hsnCanonicSplit :: HsName -> Either ([String],HsName) HsName
-- hsnCanonicSplit   (HNmQ        ns    ) = Left $ maybe ([],mkHNmBase "??") (\(i,l) -> (catMaybes $ map hsnMbBaseString i,l) ) (initlast ns)
hsnCanonicSplit n@(HsName_Modf qs _ _) = Left $ (qs, n {hsnQualifiers = []})
hsnCanonicSplit n                      = Right n

{-# LINE 350 "src/ehc/Base/HsName.chs" #-}
hsnToList :: HsName -> [HsName]
hsnToList n = either (\(qs,b) -> map mkHNmBase qs ++ [b]) (:[]) (hsnCanonicSplit n)

{-# LINE 355 "src/ehc/Base/HsName.chs" #-}
hsnInitLast :: HsName -> ([HsName],HsName)
hsnInitLast n = either (\(qs,b) -> (map mkHNmBase qs, b)) (\x -> ([],x)) (hsnCanonicSplit n)

{-# LINE 360 "src/ehc/Base/HsName.chs" #-}
hsnPrefix                           ::  String -> HsName -> HsName
hsnPrefix   p   hsn
  = maybe (mkHNmBase $ p ++ show hsn) (\(s,mk) -> mk $ p ++ s) $ hsnBaseUnpack hsn
{-
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,hsnFromString (p ++ show n))
-}

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p
  = maybe (mkHNmBase $ show hsn ++ p) (\(s,mk) -> mk $ s ++ p) $ hsnBaseUnpack hsn
{-
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,hsnFromString (show n ++ p))
-}

mkHNmPrefix :: HSNM x => String -> x -> HsName
mkHNmPrefix p = hsnPrefix p . mkHNm

{-# LINE 381 "src/ehc/Base/HsName.chs" #-}
stringAlphanumeric :: String -> String
stringAlphanumeric s
  = concat (map (charAlphanumeric) s)

{-# LINE 394 "src/ehc/Base/HsName.chs" #-}
charAlphanumeric :: Char -> String
charAlphanumeric '\'' = "prime"
charAlphanumeric ':' = "colon"
charAlphanumeric '!' = "exclam"
charAlphanumeric '@' = "at"
charAlphanumeric '#' = "number"
charAlphanumeric '$' = "dollar"
charAlphanumeric '%' = "percent"
charAlphanumeric '^' = "circon"
charAlphanumeric '&' = "amp"
charAlphanumeric '*' = "star"
charAlphanumeric '+' = "plus"
charAlphanumeric '-' = "minus"
charAlphanumeric '.' = "dot"
charAlphanumeric '/' = "slash"
charAlphanumeric '\\' = "backsl"
charAlphanumeric '|' = "bar"
charAlphanumeric '<' = "lt"
charAlphanumeric '=' = "eq"
charAlphanumeric '>' = "gt"
charAlphanumeric '?' = "quest"
charAlphanumeric '~' = "tilde"
charAlphanumeric '[' = "sub"    -- although this is not a legal Haskell operator symbol, it can be part of the Nil constructor
charAlphanumeric ']' = "bus"
charAlphanumeric '(' = "open"    -- although this is not a legal Haskell operator symbol, it can be part of the tuple constructor
charAlphanumeric ',' = "comma"
charAlphanumeric ')' = "close"
charAlphanumeric  c  = [c]

{-# LINE 427 "src/ehc/Base/HsName.chs" #-}
dontStartWithDigit :: String -> String
dontStartWithDigit xs@(a:_) | isDigit a || a=='_' = "y"++xs
                            | otherwise           = xs

hsnShowAlphanumericShort :: HsName -> String
hsnShowAlphanumericShort (HNmNr n (OrigFunc   orig)) = hsnShowAlphanumeric orig
hsnShowAlphanumericShort x = hsnShowAlphanumeric x

hsnShowAlphanumeric :: HsName -> String
hsnShowAlphanumeric (HsName_Base s  )           = dontStartWithDigit(stringAlphanumeric s)
hsnShowAlphanumeric (HsName_Pos p)              = "y" ++ show p
hsnShowAlphanumeric (HNmNr n OrigNone)          = "x" ++ show n
hsnShowAlphanumeric (HNmNr n (OrigLocal orig))  = "x" ++ show n   -- hsnShowAlphanumeric orig
hsnShowAlphanumeric (HNmNr n (OrigGlobal orig)) = "global_" ++ hsnShowAlphanumeric orig
hsnShowAlphanumeric (HNmNr n (OrigFunc   orig)) = "fun_"    ++ hsnShowAlphanumeric orig
hsnShowAlphanumeric (HsName_Modf q b u)         = concat $ intersperse "_" $ q ++ [hsnShowAlphanumeric b] ++ map stringAlphanumeric (showHsNameUniqifierMp "_" u)
-- hsnShowAlphanumeric n                           = concat $ intersperse "_" $ map hsnShowAlphanumeric $ hsnToList n

{-# LINE 450 "src/ehc/Base/HsName.chs" #-}
hsnToFPath :: HsName -> FPath
hsnToFPath n
  = mkFPathFromDirsFile qs b
  where (qs,b) = hsnInitLast n

instance FPATH HsName where
  mkFPath = hsnToFPath

{-# LINE 461 "src/ehc/Base/HsName.chs" #-}
hsnConcat                           ::  HsName -> HsName -> HsName
hsnConcat       h1    h2            =   hsnFromString (show h1 ++ show h2)

{-# LINE 470 "src/ehc/Base/HsName.chs" #-}
-- compare for row labels, lexicographic ordering (currently)
rowLabCmp :: HsName -> HsName -> Ordering
rowLabCmp = cmpHsNameOnNm

{-# LINE 480 "src/ehc/Base/HsName.chs" #-}
-- qualifier (i.e. module name) and qualified part of name
hsnSplitQualify :: HsName -> (Maybe HsName,HsName)
hsnSplitQualify n
  = case hsnInitLast n of
      ([],n') -> (Nothing,n')
      (ns,n') -> (Just (mkHNm ns),n')

-- qualified part of a name
hsnQualified :: HsName -> HsName
hsnQualified = snd . hsnSplitQualify

-- prefix/qualify with module name, on top of possible previous qualifier
hsnPrefixQual :: HsName -> HsName -> HsName
hsnPrefixQual m n = mkHNm (hsnToList m ++ hsnToList n)

-- map qualified part
hsnMapQualified :: (String -> String) -> HsName -> HsName
hsnMapQualified f qn
  = maybe qn (\(s,mk) -> mk $ f s) $ hsnBaseUnpack qn
{-
  = case hsnSplitQualify qn of
      (Nothing,n) -> f n
      (Just q ,n) -> hsnPrefixQual q (f n)
-}

{-# LINE 507 "src/ehc/Base/HsName.chs" #-}
-- qualifier (i.e. module name) of name
hsnQualifier :: HsName -> Maybe HsName
hsnQualifier = fst . hsnSplitQualify

-- replace/set qualifier
hsnSetQual :: HsName -> HsName -> HsName
hsnSetQual m = hsnPrefixQual m . hsnQualified

hsnMapQual :: (HsName -> HsName) -> HsName -> HsName
hsnMapQual f qn
  = case hsnSplitQualify qn of
      (Nothing,n) -> qn
      (Just q ,n) -> hsnSetQual (f q) n

-- is qualified?
hsnIsQual :: HsName -> Bool
hsnIsQual = isJust . hsnQualifier

hsnSetLevQual :: Int -> HsName -> HsName -> HsName
hsnSetLevQual 0 m n = hsnSetQual m n
hsnSetLevQual _ _ n = n

{-# LINE 535 "src/ehc/Base/HsName.chs" #-}
hsnFixUniqifiers' :: Bool -> String -> HsName -> HsName
hsnFixUniqifiers' showlen sep (HsName_Modf qs n us) = HsName_Modf qs (hsnSuffix n (concat $ showHsNameUniqifierMp' showlen sep us)) Map.empty
hsnFixUniqifiers' _       _   n                     = n

hsnFixUniqifiers :: HsName -> HsName
hsnFixUniqifiers = hsnFixUniqifiers' True "_@"

hsnJavalikeFixUniqifiers :: HsName -> HsName
hsnJavalikeFixUniqifiers = hsnFixUniqifiers' False ""

{-# LINE 551 "src/ehc/Base/HsName.chs" #-}
hsnStripUniqifiers :: HsName -> HsName
hsnStripUniqifiers (HsName_Modf qs n us) = HsName_Modf qs n emptyHsNameUniqifierMp
hsnStripUniqifiers n                     = n

{-# LINE 561 "src/ehc/Base/HsName.chs" #-}
hsnQualUniqify :: HsName -> HsName -> HsName
hsnQualUniqify modNm n
  = if hsnIsQual n
    then n
    else hsnSetQual modNm n

{-# LINE 573 "src/ehc/Base/HsName.chs" #-}
class HSNM a where
  mkHNm :: a -> HsName

instance HSNM HsName where
  mkHNm = id

instance HSNM Int where
  mkHNm = mkHNm . show


{-# LINE 590 "src/ehc/Base/HsName.chs" #-}
instance HSNM String where
  mkHNm s
    = mkHNm $ map hsnFromString $ splitForQualified s

{-# LINE 596 "src/ehc/Base/HsName.chs" #-}
instance HSNM ([HsName],HsName) where
  mkHNm (l,n) = mkHNm (l ++ [n])

instance HSNM [HsName] where
  mkHNm [n] = n
  mkHNm []  = hsnFromString "" -- ????, or empty alternative of HsName
  mkHNm ns  = case initlast ns of
                Just (i,l) -> case l of
                                n@(HsName_Modf _ _ _) -> n {hsnQualifiers = qs}
                                n                     -> HsName_Modf qs n Map.empty
                           where qs = catMaybes $ map hsnMbBaseString i

{-# LINE 619 "src/ehc/Base/HsName.chs" #-}
instance Position HsName where
  line   _ = (-1)
  column _ = (-1)
  file   _ = ""

{-# LINE 630 "src/ehc/Base/HsName.chs" #-}
deriving instance Typeable HsNameUniqifier
deriving instance Data HsNameUniqifier

deriving instance Typeable HsNameUnique
deriving instance Data HsNameUnique

deriving instance Typeable HsName
deriving instance Data HsName

deriving instance Typeable OrigName
deriving instance Data OrigName

deriving instance Typeable IdOccKind
deriving instance Data IdOccKind

deriving instance Typeable IdOcc
deriving instance Data IdOcc

{-# LINE 654 "src/ehc/Base/HsName.chs" #-}
instance Binary HsNameUniqifier where
  put = putEnum8
  get = getEnum8

instance Binary HsNameUnique where
  put (HsNameUnique_String  a    ) = putWord8 0 >> put a
  put (HsNameUnique_Int     a    ) = putWord8 1 >> put a
  put (HsNameUnique_UID     a    ) = putWord8 2 >> put a
  put (HsNameUnique_None         ) = putWord8 3
  get = do t <- getWord8
           case t of
             0 -> liftM  HsNameUnique_String    get
             1 -> liftM  HsNameUnique_Int       get
             2 -> liftM  HsNameUnique_UID       get
             3 -> return HsNameUnique_None

instance Binary HsName where
  put (HsName_Base  a    ) = putWord8 0 >> put a
--  put (HNmQ         a    ) = putWord8 1 >> put a
  put (HsName_Pos   a    ) = putWord8 2 >> put a
  put (HNmNr        a b  ) = putWord8 3 >> put a >> put b
  put (HsName_Modf  a b c) = putWord8 4 >> put a >> put b >> put c
  get = do t <- getWord8
           case t of
             0 -> liftM  HsName_Base    get
             -- 1 -> liftM  HNmQ           get
             2 -> liftM  HsName_Pos     get
             3 -> liftM2 HNmNr          get get
             4 -> liftM3 HsName_Modf    get get get

instance Serialize HsName where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

instance Binary OrigName where
  put (OrigNone    ) = putWord8 0
  put (OrigLocal  a) = putWord8 1 >> put a
  put (OrigGlobal a) = putWord8 2 >> put a
  put (OrigFunc   a) = putWord8 3 >> put a
  get = do t <- getWord8
           case t of
             0 -> return OrigNone
             1 -> liftM  OrigLocal  get
             2 -> liftM  OrigGlobal get
             3 -> liftM  OrigFunc   get

instance Binary IdOccKind where
  put = putEnum8
  get = getEnum8

instance Serialize IdOccKind where
  sput = sputPlain
  sget = sgetPlain

instance Binary IdOcc where
  put (IdOcc a b) = put a >> put b
  get = liftM2 IdOcc get get

instance Serialize IdOcc where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

{-# LINE 726 "src/ehc/Base/HsName.chs" #-}
data IdOccKind
  = IdOcc_Val
  | IdOcc_Pat
  | IdOcc_Type
  | IdOcc_Kind
  | IdOcc_Fld
  | IdOcc_Class
  | IdOcc_Inst
  | IdOcc_Dflt
  | IdOcc_Any
  | IdOcc_Data
  | IdOcc_Fusion
  deriving (Eq,Ord,Enum)

{-# LINE 752 "src/ehc/Base/HsName.chs" #-}
-- intended for parsing
instance Show IdOccKind where
  show IdOcc_Val      = "Value"
  show IdOcc_Pat      = "Pat"
  show IdOcc_Type     = "Type"
  show IdOcc_Kind     = "Kind"
  show IdOcc_Fld      = "Field"
  show IdOcc_Class    = "Class"
  show IdOcc_Inst     = "Instance"
  show IdOcc_Dflt     = "Default"
  show IdOcc_Any      = "Any"
  show IdOcc_Data     = "Data"
  show IdOcc_Fusion   = "Fusion"

{-# LINE 778 "src/ehc/Base/HsName.chs" #-}
-- intended for parsing
instance PP IdOccKind where
  pp = text . show

{-# LINE 784 "src/ehc/Base/HsName.chs" #-}
data IdOcc
  = IdOcc { ioccNm :: !HsName, ioccKind :: !IdOccKind }
  deriving (Show,Eq,Ord)


{-# LINE 795 "src/ehc/Base/HsName.chs" #-}
type HsNameS = Set.Set HsName

{-# LINE 803 "src/ehc/Base/HsName.chs" #-}
-- ensure a name valid for JVM like backends
hsnSafeJavaLike :: HsName -> HsName
hsnSafeJavaLike
  = hsnMapQualified (concatMap safe) . hsnJavalikeFixUniqifiers
  where safe '_'                                      = "__"
        safe c | isDigit c || isLetter c || c == '_'  = [c]
               | otherwise                            = "_" ++ showHex (ord c) ""

{-# LINE 823 "src/ehc/Base/HsName.chs" #-}
-- safe name of a variable
hsnJavaLikeVar
  :: ( HsName -> HsName				-- adapt for particular platform, before mangling here
     , HsName -> HsName				-- post prefix
     , String -> String				-- adapt module qualifiers
     )
     -> HsName -> HsName -> HsName -> HsName
hsnJavaLikeVar (preadapt, postprefix, updqual) pkg mod v
  = postprefix $ hsnSafeJavaLike $ handleUpper $ qual $ preadapt v
  where handleUpper v
          = case hsnBaseUnpack v of
               Just (s@(c:vs), mk) | isUpper c -> mk (s ++ "_")
               _ -> v
        qual v
          = case hsnBaseUnpack' v of
               Just (q, s, mk) -> mk (map updqual q) s
               _ -> v

{-# LINE 847 "src/ehc/Base/HsName.chs" #-}
-- name of the class of a variable
hsnJavaLikeVarCls :: HsName -> HsName -> HsName -> HsName
hsnJavaLikeVarCls pkg mod v
  = hsnSetQual pkg v

{-# LINE 858 "src/ehc/Base/HsName.chs" #-}
-- field name of var name
hsnJavaLikeVarToFld :: HsName -> HsName
hsnJavaLikeVarToFld v
  = hsnQualified v

{-# LINE 869 "src/ehc/Base/HsName.chs" #-}
-- name of class of data type
hsnJavaLikeDataTy :: HsName -> HsName -> HsName -> HsName
hsnJavaLikeDataTy pkg mod d = hsnSafeJavaLike d `hsnSuffix` "_Ty"

-- name of class of data constructor
hsnJavaLikeDataCon :: HsName -> HsName -> HsName -> HsName
hsnJavaLikeDataCon pkg mod d = hsnSafeJavaLike d `hsnSuffix` "_Con"

-- name of field of data
hsnJavaLikeDataFldAt :: Int -> String
hsnJavaLikeDataFldAt i = show i

-- all names of fields of data
hsnJavaLikeDataFlds :: Int -> [String]
hsnJavaLikeDataFlds arity = map hsnJavaLikeDataFldAt [0..arity-1]

{-# LINE 892 "src/ehc/Base/HsName.chs" #-}
type FvS = HsNameS
type FvSMp = Map.Map HsName FvS

{-# LINE 901 "src/ehc/Base/HsName.chs" #-}
type HsNameMp = Map.Map HsName HsName

hsnRepl :: HsNameMp -> HsName -> HsName
hsnRepl m n = Map.findWithDefault n n m

{-# LINE 912 "src/ehc/Base/HsName.chs" #-}
data RPatNm
  = RPatNmOrig {rpatNmNm :: !HsName}
  | RPatNmUniq {rpatNmNm :: !HsName}
  deriving Eq

instance Ord RPatNm where
  x `compare` y = rpatNmNm x `cmpHsNameOnNm` rpatNmNm y

instance Show RPatNm where
  show pnm = show (rpatNmNm pnm)

instance PP RPatNm where
  pp (RPatNmOrig n) = n >|< "(O)"
  pp (RPatNmUniq n) = n >|< "(U)"

{-# LINE 929 "src/ehc/Base/HsName.chs" #-}
rpatNmIsOrig :: RPatNm -> Bool
rpatNmIsOrig (RPatNmOrig _) = True
rpatNmIsOrig _              = False

{-# LINE 940 "src/ehc/Base/HsName.chs" #-}

data Track
  = TrackNone
  | TrackSelf
  | TrackCtx Int
  | TrackSelect Int Track
  | TrackVarApply HsName [Track]
  deriving (Eq, Ord, Show)


{-# LINE 952 "src/ehc/Base/HsName.chs" #-}

instance Serialize Track where
  sput (TrackNone             ) = sputWord8 0
  sput (TrackSelf             ) = sputWord8 1
  sput (TrackCtx        a     ) = sputWord8 2 >> sput a
  sput (TrackSelect     a  b  ) = sputWord8 3 >> sput a >> sput b
  sput (TrackVarApply   a  b  ) = sputWord8 4 >> sput a >> sput b

  sget
    = do t <- sgetWord8
         case t of
           0 -> return TrackNone
           1 -> return TrackSelf
           2 -> liftM  TrackCtx      sget
           3 -> liftM2 TrackSelect   sget sget
           4 -> liftM2 TrackVarApply sget sget

deriving instance Data Track
deriving instance Typeable Track



