%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
HsName represents names as used inside the compiler. A HsName can be a
base name, like a string or an int (for offset names), or it can be a
modified base name. Modification is done by qualification and
unification, the first related to hierarchical modules, the second used
internally whenever a name gives rise to a newly introduced identifier.
In that case the unification also carries a HsNameUniqifier which indicates
what the purpose of the uniqification is. The unification is carried
with a name forever, so it can also be used to detect properties of a
name. For example, a name can represent an evaluated value. However, it
is the responsibility of the compiler part using a particular
HsNameUniqifier to guarantee such an invariant.
%%]

%%[1 module {%{EH}Base.HsName} import(EH.Util.Utils,EH.Util.Pretty, Data.List)
%%]

%%[1 import({%{EH}Base.UID})
%%]

%%[1 import(UU.Scanner.Position) export(HSNM(..))
%%]

%%[3 import(qualified Data.Set as Set,Data.Maybe)
%%]

%%[8 export(hsnShowAlphanumeric, hsnShowAlphanumericShort)
%%]

%%[8 import(EH.Util.FPath,Char,qualified Data.Map as Map)
%%]

%%[8 export(OrigName(..))
%%]

%%[10 export(hsnConcat)
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, uniqification info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(HsNameUniqifier(..))
-- | A HsNameUniqifier represents the 'type' of unification
data HsNameUniqifier
  = HsNameUniqifier_New					-- just a new identifier
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
  deriving (Eq,Ord,Enum)

-- | The show of a HsNameUniqifier is found back in the pretty printed code, current convention is 3 uppercase letters, as a balance between size and clarity of meaning
instance Show HsNameUniqifier where
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
%%]

%%[7 export(HsNameUnique(..))
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
%%]

%%[7
type HsNameUniqifierMp = Map.Map HsNameUniqifier [HsNameUnique]

showHsNameUniqifierMp :: String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp usep us = [ usep ++ show (length u) ++ show uqf ++ concat [ usep ++ show uu | uu <- u, uu /= HsNameUnique_None ] | (uqf,u) <- Map.toList us ]
%%]

%%[7
%%]
class HsNameUniqueable x where
  hsnuniqeable :: x -> HsNameUnique

instance HsNameUniqueable String where
  hsnuniqeable = HsNameUnique_String

instance HsNameUniqueable Int where
  hsnuniqeable = HsNameUnique_Int

instance HsNameUniqueable UID where
  hsnuniqeable = HsNameUnique_UID

%%[7
uniqifierMpAdd :: HsNameUniqifier -> HsNameUnique -> HsNameUniqifierMp -> HsNameUniqifierMp
uniqifierMpAdd ufier u m = Map.unionWith (++) (Map.singleton ufier [u]) m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, datatype
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.HsName.type export(HsName(..))
data HsName
  =   HsName_Base                   !String
%%[[7
  |   HsName_Modf
        { hsnQualifiers         ::  ![String]
        , hsnBase               ::  !HsName
        , hsnUniqifiers         ::  !HsNameUniqifierMp
        }
  |   HsName_Pos                    !Int
  -- |   HNmQ                          ![HsName]
%%]]
%%[[8
  |   HNmNr                         !Int !OrigName
%%]]
  deriving (Eq,Ord)
%%]

instance Eq HsName where
  n1 == n2
    = hsnCanonicSplit n1 == hsnCanonicSplit n2
    where base (HsName_Base s1) (HsName_Base s2) = s1 == s2

instance Ord HsName where
  n1 `compare` n2 = hsnCanonicSplit n1 `compare` hsnCanonicSplit n2

%%[1 export(mkHNmBase,hsnMbBaseString,hsnBaseUnpack)
-- | Just lift a string to the base HsName variant
mkHNmBase :: String -> HsName
%%[[1
mkHNmBase = HsName_Base
%%][7
mkHNmBase s = HsName_Modf [] (HsName_Base s) Map.empty
%%]]

-- | unpack a HsName into base string + repack function
hsnBaseUnpack :: HsName -> Maybe (String,String -> HsName)
hsnBaseUnpack (HsName_Base s    ) = Just (s,HsName_Base)
%%[[7
hsnBaseUnpack (HsName_Modf q b u) = fmap (\(bs,mk) -> (bs, \s -> HsName_Modf q (mk s) u)) (hsnBaseUnpack b)
-- hsnBaseUnpack (HNmQ        ns   ) = do { (i,l) <- initlast ns ; (bs,mk) <- hsnBaseUnpack l ; return (bs, \s -> (HNmQ $ i ++ [mk s])) }
%%]]
hsnBaseUnpack _                   = Nothing

-- | If name is a HsName_Base after some unpacking, return the base string, without qualifiers, without uniqifiers
hsnMbBaseString :: HsName -> Maybe String
hsnMbBaseString = fmap fst . hsnBaseUnpack

%%]

%%[7 export(mkHNmPos)
-- | Just lift a int to the int HsName variant
mkHNmPos :: Int -> HsName
%%[[1
mkHNmPos = HsName_Pos
%%][7
mkHNmPos s = HsName_Modf [] (HsName_Pos s) Map.empty
%%]]
%%]

%%[7 export(cmpHsNameOnNm)
cmpHsNameOnNm n1           n2           = compare n1 n2
%%]

%%[1 export(mbHNm)
mbHNm :: HsName -> Maybe String
mbHNm = hsnMbBaseString
%%]

%%[1.hsnFromString export(hsnFromString)
hsnFromString :: String -> HsName
hsnFromString = mkHNmBase
%%]

%%[1111.hsnHNmFldToString export(hsnHNmFldToString)
hsnHNmFldToString :: String -> String
hsnHNmFldToString = id
%%]

%%[8
data OrigName
  = OrigNone
  | OrigLocal  HsName
  | OrigGlobal HsName
  | OrigFunc   HsName
  deriving (Eq,Ord)
%%]

%%[1
instance PP HsName where
  pp h = pp (show h)
%%]


%%[7 export(hsnShow)
hsnShow :: String -> String -> HsName -> String
hsnShow _    _    (HsName_Base   s         )  = {- hsnHNmFldToString -} s
hsnShow qsep usep (HsName_Modf qs b us     )  = concat $ (intersperse qsep $ qs ++ [hsnShow qsep usep b]) ++ showHsNameUniqifierMp usep us
hsnShow _    _    (HsName_Pos  p           )  = show p
-- hsnShow qsep usep (HNmQ   ns               )  = concat $ intersperse qsep $ map (hsnShow qsep usep) ns
%%[[8
hsnShow _    _    (HNmNr n OrigNone        )  = "x_"        ++ show n
hsnShow _    usep (HNmNr n (OrigLocal  hsn))  = "x_"        ++ show n ++ "_" ++ hsnShow "." usep hsn
hsnShow _    usep (HNmNr n (OrigGlobal hsn))  = "global_x_" ++ show n ++ "_" ++ hsnShow "." usep hsn
hsnShow _    usep (HNmNr n (OrigFunc   hsn))  = "fun_x_"    ++ show n ++ "_" ++ hsnShow "." usep hsn
%%]]
%%]

%%[1
instance Show HsName where
%%[[1
  show (HsName_Base s) = s
%%][7
  show = hsnShow "." "_@"
%%]]
%%]

%%[1
-- | A HsName is either a complex/aggregrate name or a base case
hsnCanonicSplit :: HsName -> Either ([String],HsName) HsName
%%[[7
-- hsnCanonicSplit   (HNmQ        ns    ) = Left $ maybe ([],mkHNmBase "??") (\(i,l) -> (catMaybes $ map hsnMbBaseString i,l) ) (initlast ns)
hsnCanonicSplit n@(HsName_Modf qs _ _) = Left $ (qs, n {hsnQualifiers = []})
%%]]
hsnCanonicSplit n                      = Right n
%%]

%%[1
hsnToList :: HsName -> [HsName]
hsnToList n = either (\(qs,b) -> map mkHNmBase qs ++ [b]) (:[]) (hsnCanonicSplit n)
%%]

%%[1 export(hsnInitLast)
hsnInitLast :: HsName -> ([HsName],HsName)
hsnInitLast n = either (\(qs,b) -> (map mkHNmBase qs, b)) (\x -> ([],x)) (hsnCanonicSplit n)
%%]

%%[1 export(hsnPrefix,hsnSuffix,mkHNmPrefix)
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
%%]

%%[8
stringAlphanumeric :: String -> String
stringAlphanumeric s
  = concat (map (charAlphanumeric) s)
%%]
stringAlphanumeric :: String -> String
stringAlphanumeric s
 | isAlphaNum c || c=='_'  = s
 | otherwise               = concat (map (('_':).charAlphanumeric) s)
  where c = head s   -- we assume that the whole string is alphanumeric if the first character is.
                     -- this is more efficient than testing each character of the string separately,
                     -- and can safely be assumed in Haskell

%%[8
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
%%]
charAlphanumeric  c  | isDigit c = [c]
                     | otherwise = error ("no alphanumeric representation for " ++ show c)

%%[8
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
%%]




%%[8
hsnToFPath :: HsName -> FPath
hsnToFPath n
  = mkFPathFromDirsFile qs b
  where (qs,b) = hsnInitLast n

instance FPATH HsName where
  mkFPath = hsnToFPath
%%]

-- replace this by something better, taking into account qualifiers
%%[10
hsnConcat                           ::  HsName -> HsName -> HsName
hsnConcat       h1    h2            =   hsnFromString (show h1 ++ show h2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Uniqification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(hsnUniqify,hsnUniqifyUID,hsnUniqifyStr,hsnUniqifyInt,hsnUniqifyEval)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Row specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 hs export(rowLabCmp)
-- compare for row labels, lexicographic ordering (currently)
rowLabCmp :: HsName -> HsName -> Ordering
rowLabCmp = cmpHsNameOnNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HsName & module related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(hsnSplitQualify,hsnQualified,hsnPrefixQual,hsnMapQualified)
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
%%]

%%[20 export(hsnQualifier,hsnSetQual,hsnIsQual,hsnMapQual,hsnSetLevQual)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fix the uniqifier part by making a string of it, suffixing it to the base part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(hsnFixUniqifiers)
hsnFixUniqifiers :: HsName -> HsName
hsnFixUniqifiers (HsName_Modf qs n us) = HsName_Modf qs (hsnSuffix n (concat $ showHsNameUniqifierMp "_@" us)) Map.empty
hsnFixUniqifiers n                     = n
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make globally unique by prefixing with module name (if not already done so)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(hsnQualUniqify)
hsnQualUniqify :: HsName -> HsName -> HsName
hsnQualUniqify modNm n
  = if hsnIsQual n
    then n
    else hsnSetQual modNm n
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make HsName of something
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
class HSNM a where
  mkHNm :: a -> HsName

instance HSNM HsName where
  mkHNm = id

instance HSNM Int where
  mkHNm = mkHNm . show

%%]

%%[1.HSNM.String
instance HSNM String where
  mkHNm s = hsnFromString s
%%]

%%[8.HSNM.String -1.HSNM.String
instance HSNM String where
  mkHNm s
    = mkHNm $ map hsnFromString $ splitForQualified s
%%]

%%[1
instance HSNM ([HsName],HsName) where
  mkHNm (l,n) = mkHNm (l ++ [n])

instance HSNM [HsName] where
  mkHNm [n] = n
  mkHNm []  = hsnFromString "" -- ????, or empty alternative of HsName
%%[[7777
  mkHNm ns  = HNmQ ns
%%]]
%%[[7
  mkHNm ns  = case initlast ns of
                Just (i,l) -> case l of
                                n@(HsName_Modf _ _ _) -> n {hsnQualifiers = qs}
                                n                     -> HsName_Modf qs n Map.empty
                           where qs = catMaybes $ map hsnMbBaseString i
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HsName class instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
instance Position HsName where
  line   _ = (-1)
  column _ = (-1)
  file   _ = ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier occurrences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(IdOccKind(..))
data IdOccKind
  = IdOcc_Val
  | IdOcc_Pat
  | IdOcc_Type
%%[[6
  | IdOcc_Kind
%%]]
%%[[9
  | IdOcc_Class
  | IdOcc_Inst
  | IdOcc_Dflt
%%]]
  | IdOcc_Any
%%[[20
  | IdOcc_Data
%%]]
  deriving (Eq,Ord,Enum)
%%]

%%[1
-- intended for parsing
instance Show IdOccKind where
  show IdOcc_Val      = "Value"
  show IdOcc_Pat      = "Pat"
  show IdOcc_Type     = "Type"
%%[[6
  show IdOcc_Kind     = "Kind"
%%]]
%%[[9
  show IdOcc_Class    = "Class"
  show IdOcc_Inst     = "Instance"
  show IdOcc_Dflt     = "Default"
%%]]
  show IdOcc_Any      = "Any"
%%[[20
  show IdOcc_Data     = "Data"
%%]]
%%]

%%[1
-- intended for parsing
instance PP IdOccKind where
  pp = text . show
%%]

%%[1 export(IdOcc(..))
data IdOcc
  = IdOcc { ioccNm :: !HsName, ioccKind :: !IdOccKind }
  deriving (Show,Eq,Ord)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set of names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3 export(HsNameS)
type HsNameS = Set.Set HsName
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(FvS,FvSMp)
type FvS = HsNameS
type FvSMp = Map.Map HsName FvS
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Map of  names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(HsNameMp,hsnRepl)
type HsNameMp = Map.Map HsName HsName

hsnRepl :: HsNameMp -> HsName -> HsName
hsnRepl m n = Map.findWithDefault n n m
%%]

