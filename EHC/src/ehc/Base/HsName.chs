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

%%[1 module {%{EH}Base.HsName} import(UHC.Util.Utils,UHC.Util.Pretty, Data.List)
%%]

%%[1 import({%{EH}Base.UID})
%%]

%%[1 import(UU.Scanner.Position) export(HSNM(..))
%%]

%%[3 import(qualified Data.Set as Set,Data.Maybe)
%%]

%%[8 import(qualified Data.Set as Set,Data.Maybe, Data.Char, Numeric)
%%]

%%[7 import(qualified Data.Map as Map)
%%]

%%[8 import(UHC.Util.FPath,Data.Char)
%%]

%%[8 export(OrigName(..))
%%]

%%[10 export(hsnConcat)
%%]

%%[50 import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[99 import(Data.Hashable)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, uniqification info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(HsNameUniqifier(..))
-- | A HsNameUniqifier represents the 'type' of unification
data HsNameUniqifier
  = HsNameUniqifier_Blank               -- just a new identifier, with an empty show
  | HsNameUniqifier_New                 -- just a new identifier
  | HsNameUniqifier_Error      			-- error
  | HsNameUniqifier_GloballyUnique      -- globally unique
  | HsNameUniqifier_Evaluated           -- evaluated
  | HsNameUniqifier_Field               -- extracted field
  | HsNameUniqifier_Class               -- class
  | HsNameUniqifier_ClassDict           -- dictionary
  | HsNameUniqifier_SelfDict            -- dictionary under construction itself, passed as arg in tying the knot recursion
  | HsNameUniqifier_ResultDict          -- dictionary under construction result
  | HsNameUniqifier_SuperClass          -- super class field
  | HsNameUniqifier_DictField           -- dictionary field
  | HsNameUniqifier_Inline              -- new identifier because of inlining
  | HsNameUniqifier_GloballyUniqueDict  -- globally unique dictionary
  | HsNameUniqifier_FieldOffset         -- offset for a field
  | HsNameUniqifier_CaseContinuation    -- continuation of a case expression
  | HsNameUniqifier_GrinUpdated         -- Grin: updated value
  | HsNameUniqifier_FFIArg              -- arg evaluated for FFI
  | HsNameUniqifier_LacksLabel          -- label used in lacking predicates
  | HsNameUniqifier_BindAspect          -- binding aspect
  | HsNameUniqifier_Strict              -- strict variant of binding
%%[[92
  | HsNameUniqifier_GenericClass        -- a name introduced by generics
%%]]
%%[[(8 javascript)
  | HsNameUniqifier_JSSwitchResult      -- var for result of switch
%%]]
%%[[(8 grin)
  | HsNameUniqifier_GRINTmpVar      	-- tmp var at GRIN level
%%]]
%%[[(8 cmm)
  | HsNameUniqifier_CMMTmpVar      		-- tmp var at CMM level
%%]]
%%[[90
  | HsNameUniqifier_FFE                 -- name of value to be ff exported
  | HsNameUniqifier_FFECoerced          -- name of possibly coerced value to be ff exported
%%]]
  deriving (Eq,Ord,Enum)

-- | The show of a HsNameUniqifier is found back in the pretty printed code, current convention is 3 uppercase letters, as a balance between size and clarity of meaning
instance Show HsNameUniqifier where
  show HsNameUniqifier_Blank                = ""
  show HsNameUniqifier_New                  = "NEW"
  show HsNameUniqifier_Error       			= "ERR"
  show HsNameUniqifier_GloballyUnique       = "UNQ"
  show HsNameUniqifier_Evaluated            = "EVL"
  show HsNameUniqifier_Field                = "FLD"
  show HsNameUniqifier_Class                = "CLS"
  show HsNameUniqifier_ClassDict            = "DCT"
  show HsNameUniqifier_SelfDict             = "SDC"
  show HsNameUniqifier_ResultDict           = "RDC"
  show HsNameUniqifier_SuperClass           = "SUP"
  show HsNameUniqifier_DictField            = "DFL"
  show HsNameUniqifier_Inline               = "INL"
  show HsNameUniqifier_GloballyUniqueDict   = "UND"
  show HsNameUniqifier_FieldOffset          = "OFF"
  show HsNameUniqifier_CaseContinuation     = "CCN"
  show HsNameUniqifier_GrinUpdated          = "UPD"
  show HsNameUniqifier_FFIArg               = "FFI"
  show HsNameUniqifier_LacksLabel           = "LBL"
  show HsNameUniqifier_BindAspect           = "ASP"
  show HsNameUniqifier_Strict               = "STR"
%%[[91
  show HsNameUniqifier_GenericClass         = "GEN"
%%]]
%%[[(8 javascript)
  show HsNameUniqifier_JSSwitchResult       = "JSW"
%%]]
%%[[(8 grin)
  show HsNameUniqifier_GRINTmpVar       	= "GRN"
%%]]
%%[[(8 cmm)
  show HsNameUniqifier_CMMTmpVar       		= "CMM"
%%]]
%%[[90
  show HsNameUniqifier_FFE                  = "FFE"
  show HsNameUniqifier_FFECoerced           = "FFC"
%%]]
%%]

%%[7 export(HsNameUnique(..))
-- | A HsNameUnique represents the optional additional info to make the uniqification even more unique
data HsNameUnique
  = HsNameUnique_None
  | HsNameUnique_String     !String
  | HsNameUnique_Int        !Int
  | HsNameUnique_UID        !UID
  deriving (Eq,Ord)

showHsNameUnique :: (UID -> String) -> (String -> String) -> HsNameUnique -> String
showHsNameUnique _    _    (HsNameUnique_None    ) = ""
showHsNameUnique _    shws (HsNameUnique_String s) = shws s
showHsNameUnique _    _    (HsNameUnique_Int    i) = show i
showHsNameUnique shwu _    (HsNameUnique_UID    u) = shwu u

instance Show HsNameUnique where
  show = showHsNameUnique hsnShowUID id
%%]

%%[7 export(HsNameUniqifierMp)
type HsNameUniqifierMp = Map.Map HsNameUniqifier [HsNameUnique]

emptyHsNameUniqifierMp :: HsNameUniqifierMp
emptyHsNameUniqifierMp = Map.empty

-- | Show uniqifier map, parseable back again when properly parameterized.
showHsNameUniqifierMp'' :: (UID -> String) -> (String -> String) -> (String -> String) -> Bool -> String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp'' shwu shws brk showLen usep us
  = [ usep ++ slen u ++ show uqf ++ (brk $ concat [ usep ++ showHsNameUnique shwu shws uu | uu <- u, uu /= HsNameUnique_None ]) | (uqf,u) <- Map.toList us ]
  where slen u | showLen && l /= 1  = usep ++ show l
               | otherwise          = ""
               where l = length u

showHsNameUniqifierMp' :: Bool -> String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp' = showHsNameUniqifierMp'' hsnShowUID id id

showHsNameUniqifierMp :: String -> HsNameUniqifierMp -> [String]
showHsNameUniqifierMp = showHsNameUniqifierMp' True
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

uniqifierMpUnion :: HsNameUniqifierMp -> HsNameUniqifierMp -> HsNameUniqifierMp
uniqifierMpUnion = Map.unionWith (++)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Uniqification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(hsnUniqify,hsnUniqifyUID,hsnUniqifyStr,hsnUniqifyInt,hsnUniqifyEval)
hsnUniqify' :: HsNameUniqifier -> HsNameUnique -> HsName -> HsName
hsnUniqify' ufier u
  = mk
  where mk n@(HsName_Modf {hsnUniqifiers=us}) = hsnFixateHash (n {hsnUniqifiers = uniqifierMpAdd ufier u us})
        mk n                                  = mk (hsnMkModf [] n Map.empty)

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

%%[8 export(hsnStripUniqify)
-- | Remove uniqification, if present
hsnStripUniqify :: HsName -> Maybe HsName
hsnStripUniqify n@(HsName_Modf {hsnUniqifiers=us})
  | Map.null us   = Nothing
  | otherwise     = Just $ n {hsnUniqifiers = Map.empty}
hsnStripUniqify _ = Nothing
%%]

%%[8 export(hsnSimplifications)
-- | Simplify name into list of simplifications of increasing complexity, all strictly simpler than the one given. [] therefore means no simplifications exist
hsnSimplifications :: HsName -> [HsName]
hsnSimplifications n@(HsName_Modf {}) = case hsnStripUniqify n of
    Just n' -> hsnSimplifications n' ++ [n']
    _       -> hsnSimplifications $ hsnBase n
hsnSimplifications   (HsName_UID  {hsnUID = u}) = map mkHNm $ uidSimplifications u
-- hsnSimplifications n@(HsName_Base {}          ) = [] -- [n]
hsnSimplifications _                            = []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names: hashing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
hsnHashWithSalt :: Int -> HsName -> Int
hsnHashWithSalt salt (HsName_Base s      ) = hashWithSalt salt s
hsnHashWithSalt salt (HsName_UID  i      ) = hashWithSalt salt i
hsnHashWithSalt salt (HsName_Pos  p      ) = hashWithSalt salt p
hsnHashWithSalt salt (HsName_Modf _ q b u) = hashWithSalt salt q `hashWithSalt` hashWithSalt salt b `hashWithSalt` hashWithSalt salt (Map.toList u)
hsnHashWithSalt salt (HsName_Nr i n      ) = i `hashWithSalt` hashWithSalt salt n

instance Hashable HsName where
  hashWithSalt salt n@(HsName_Modf h _ _ _) | h /= 0 = h
  hashWithSalt salt n                                = hsnHashWithSalt salt n

instance Hashable OrigName where
  hashWithSalt salt (OrigNone    ) = salt
  hashWithSalt salt (OrigLocal  n) = 23 `hashWithSalt` hashWithSalt salt n
  hashWithSalt salt (OrigGlobal n) = 19 `hashWithSalt` hashWithSalt salt n
  hashWithSalt salt (OrigFunc   n) = 17 `hashWithSalt` hashWithSalt salt n

instance Hashable HsNameUnique where
  hashWithSalt salt (HsNameUnique_None    ) = salt
  hashWithSalt salt (HsNameUnique_String s) = hashWithSalt salt s
  hashWithSalt salt (HsNameUnique_Int    i) = hashWithSalt salt i
  hashWithSalt salt (HsNameUnique_UID    u) = hashWithSalt salt u

instance Hashable HsNameUniqifier where
  hashWithSalt salt u = hashWithSalt salt (fromEnum u)
%%]

%%[1
-- | Fixate hash
hsnFixateHash :: HsName -> HsName
%%[[99
hsnFixateHash n@(HsName_Modf _ _ _ _) = n {hsnHash = hsnHashWithSalt 17 n}
%%]]
hsnFixateHash n                       = n
{-# INLINE hsnFixateHash #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, datatype
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.HsName.type export(HsName)
-- | Haskell name representation, exports of constructors only intented for internal use
data HsName
  =   HsName_Base
        { hsnBaseStr            ::  !String
        }
  |   HsName_UID
        { hsnUID                ::  !UID
        }
%%[[7
  |   HsName_Modf
        { 
          -- a secret hash, prefixing other fields as to enforce comparison on the hash first; only used at variant 99 and onwards to avoid clutter
          hsnHash               ::  !Int
        , hsnQualifiers         ::  ![String]
        , hsnBase               ::  !HsName
        , hsnUniqifiers         ::  !HsNameUniqifierMp
        }
  |   HsName_Pos                    !Int
%%]]
%%[[8
  |   HsName_Nr                     !Int !OrigName
%%]]
  deriving (Eq,Ord)
%%]

%%[1 export(hsnEmpty)
hsnEmpty :: HsName
hsnEmpty = mkHNm ""
%%]

%%[1111 export(hsnIsEmpty)
hsnIsEmpty (HsName_Base s) = null s
%%[[7
hsnIsEmpty (HsName_Modf {hsnBase=b})
                           = hsnIsEmpty b
%%]]
hsnIsEmpty _               = False
%%]

%%[7 export(hsnMbPos, hsnIsPos)
-- | Is HsName a HsName_Pos?
hsnMbPos :: HsName -> Maybe Int
hsnMbPos (HsName_Pos p) = Just p
hsnMbPos _              = Nothing

hsnIsPos :: HsName -> Bool
hsnIsPos = isJust . hsnMbPos
{-# INLINE hsnIsPos #-}
%%]

%%[8 export(hsnMbNr, hsnIsNr)
-- | Is HsName a HsName_Pos?
hsnMbNr :: HsName -> Maybe (Int,OrigName)
hsnMbNr (HsName_Nr i o) = Just (i,o)
hsnMbNr _               = Nothing

hsnIsNr :: HsName -> Bool
hsnIsNr = isJust . hsnMbNr
{-# INLINE hsnIsNr #-}
%%]

%%[7 export(hsnMkModf)
-- | Smart constructor for HsName_Modf
hsnMkModf :: [String] -> HsName -> HsNameUniqifierMp -> HsName
%%[[1
hsnMkModf = HsName_Modf 0
%%][99
-- hsnMkModf q b u = hsnFixateHash $ HsName_Modf 0 q b u
hsnMkModf q b u = hsnFixateHash $ either (\(_,n) -> n {hsnQualifiers = q, hsnUniqifiers = hsnUniqifiers n `uniqifierMpUnion` u}) (\b -> HsName_Modf 0 q b u) $ hsnCanonicSplit b
%%]]
{-# INLINE hsnMkModf #-}
%%]

%%[8 export(hsnMkNr)
-- | Smart constructor for HsName_Nr
hsnMkNr :: Int -> OrigName -> HsName
hsnMkNr = HsName_Nr
{-# INLINE hsnMkNr #-}
%%]

instance Eq HsName where
  n1 == n2
    = hsnCanonicSplit n1 == hsnCanonicSplit n2
    -- where base (HsName_Base s1) (HsName_Base s2) = s1 == s2

instance Ord HsName where
  n1 `compare` n2 = hsnCanonicSplit n1 `compare` hsnCanonicSplit n2

%%[1 export(mkHNmBase)
-- | Just lift a string to the base HsName variant
mkHNmBase :: String -> HsName
%%[[1
mkHNmBase = HsName_Base
%%][7
mkHNmBase s = hsnMkModf [] (HsName_Base s) Map.empty
%%]]
%%]

%%[1 export(hsnEnsureIsBase)
-- | Eliminate alternative internal representations
hsnEnsureIsBase :: HsName -> HsName
hsnEnsureIsBase n@(HsName_UID _) = mkHNm $ show n
%%[[7
hsnEnsureIsBase   (HsName_Pos i) = mkHNm $ show i
%%]]
hsnEnsureIsBase n                = n
%%]

%%[1 export(hsnBaseUnpack',hsnBaseUnpack)
-- | unpack a HsName into qualifiers + base string + repack function
hsnBaseUnpack' :: HsName -> Maybe ([String],String,[String] -> String -> HsName)
hsnBaseUnpack' (HsName_Base s    ) = Just ([],s,\_ s -> HsName_Base s)
%%[[7
hsnBaseUnpack' (HsName_Modf _ q b u) = fmap (\(bs,mk) -> (q, bs, \q s -> hsnMkModf q (mk s) u)) (hsnBaseUnpack b)
hsnBaseUnpack' _                     = Nothing
%%]]

-- | unpack a HsName into base string + repack function
hsnBaseUnpack :: HsName -> Maybe (String,String -> HsName)
hsnBaseUnpack (HsName_Base s    ) = Just (s,HsName_Base)
%%[[7
hsnBaseUnpack (HsName_Modf _ q b u) = fmap (\(bs,mk) -> (bs, \s -> hsnMkModf q (mk s) u)) (hsnBaseUnpack b)
hsnBaseUnpack _                     = Nothing
%%]]
%%]

%%[1 export(hsnMbBaseString,hsnIsBaseString,hsnBaseString)
-- | If name is a HsName_Base after some unpacking, return the base string, without qualifiers, without uniqifiers
hsnMbBaseString :: HsName -> Maybe String
hsnMbBaseString = fmap fst . hsnBaseUnpack
{-# INLINE hsnMbBaseString #-}

-- | Is name is a HsName_Base after some unpacking?
hsnIsBaseString :: HsName -> Bool
hsnIsBaseString = isJust . hsnMbBaseString
{-# INLINE hsnIsBaseString #-}

hsnBaseString :: HsName -> String
hsnBaseString = maybe "??" id . hsnMbBaseString

%%]

%%[7 export(mkHNmPos)
-- | Just lift a int to the int HsName variant
mkHNmPos :: Int -> HsName
%%[[1
mkHNmPos = HsName_Pos
%%][7
mkHNmPos s = hsnMkModf [] (HsName_Pos s) Map.empty
%%]]
%%]

%%[7 export(cmpHsNameOnNm)
-- | Compare, ignoring hash
cmpHsNameOnNm :: HsName -> HsName -> Ordering
%%[[99
cmpHsNameOnNm (HsName_Modf _ q1 b1 u1) (HsName_Modf _ q2 b2 u2) = compare (HsName_Modf 0 q1 b1 u1)  (HsName_Modf 0 q2 b2 u2)
%%]]
cmpHsNameOnNm n1                       n2                       = compare n1                        n2
%%]

%%[1111 export(mbHNm)
mbHNm :: HsName -> Maybe String
mbHNm = hsnMbBaseString
{-# INLINE mbHNm #-}
%%]

%%[1.hsnFromString export(hsnFromString)
hsnFromString :: String -> HsName
hsnFromString = mkHNmBase
{-# INLINE hsnFromString #-}
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


%%[7 export(hsnShow, hsnShow')
-- | Parameterizable show of HsName when used from within the Show instance for HsName, or for a parseable representation used by (e.g.) Core pretty printing
hsnShow' :: (UID -> String) -> (String -> String) -> (String -> String) -> String -> String -> HsName -> String
hsnShow' shwu shws brk qsep usep n
    = shw n
  where shw n = case n of
          HsName_Base   s                 -> s
          HsName_UID    i                 -> shwu i
          HsName_Modf _ qs b us           -> concat $ (intersperse qsep $ qs ++ [shw b]) ++ showHsNameUniqifierMp'' shwu shws brk False usep us
          HsName_Pos    p                 -> show p
%%[[8
          HsName_Nr n OrigNone            -> "x_"        ++ show n
          HsName_Nr n (OrigLocal  hsn)    -> "x_"        ++ show n ++ "_" ++ shw hsn
          HsName_Nr n (OrigGlobal hsn)    -> "global_x_" ++ show n ++ "_" ++ shw hsn
          HsName_Nr n (OrigFunc   hsn)    -> "fun_x_"    ++ show n ++ "_" ++ shw hsn
%%]]

-- | Parseable show of HsName when used from within the Show instance for HsName
hsnShow :: String -> String -> HsName -> String
hsnShow q u n = hsnShow' hsnShowUID id id q u n
{-# INLINE hsnShow #-}

hsnShowUID i = 'u' : show i
%%]

%%[1
instance Show HsName where
%%[[1
  show (HsName_Base s) = s
  show (HsName_UID  i) = hsnShowUID i
%%][7
  show = hsnShow "." "_@"
%%]]
%%]

%%[1
-- | A HsName is either a complex/aggregrate name or a base case
hsnCanonicSplit :: HsName -> Either ([String],HsName) HsName
%%[[7
hsnCanonicSplit n@(HsName_Modf _ qs _ _) = Left $ (qs, hsnFixateHash (n {hsnQualifiers = []}))
%%]]
hsnCanonicSplit n                        = Right n
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

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p
  = maybe (mkHNmBase $ show hsn ++ p) (\(s,mk) -> mk $ s ++ p) $ hsnBaseUnpack hsn

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

%%[8 export(hsnShowAlphanumeric, hsnShowAlphanumericShort)
dontStartWithDigit :: String -> String
dontStartWithDigit xs@(a:_) | isDigit a || a=='_' = "y"++xs
                            | otherwise           = xs

hsnShowAlphanumericShort :: HsName -> String
hsnShowAlphanumericShort (HsName_Nr n (OrigFunc   orig)) = hsnShowAlphanumeric orig
hsnShowAlphanumericShort x = hsnShowAlphanumeric x

hsnShowAlphanumeric :: HsName -> String
hsnShowAlphanumeric (HsName_Base s  )           = dontStartWithDigit(stringAlphanumeric s)
hsnShowAlphanumeric (HsName_UID  i  )           = "u" ++ show i
hsnShowAlphanumeric (HsName_Pos p)              = "y" ++ show p
hsnShowAlphanumeric (HsName_Nr n OrigNone)          = "x" ++ show n
hsnShowAlphanumeric (HsName_Nr n (OrigLocal orig))  = "x" ++ show n   -- hsnShowAlphanumeric orig
hsnShowAlphanumeric (HsName_Nr n (OrigGlobal orig)) = "global_" ++ hsnShowAlphanumeric orig
hsnShowAlphanumeric (HsName_Nr n (OrigFunc   orig)) = "fun_"    ++ hsnShowAlphanumeric orig
hsnShowAlphanumeric (HsName_Modf _ q b u)         = concat $ intersperse "_" $ q ++ [hsnShowAlphanumeric b] ++ map stringAlphanumeric (showHsNameUniqifierMp "_" u)
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

%%[8 export(hsnQualifier,hsnSetQual,hsnIsQual)
-- qualifier (i.e. module name) of name
hsnQualifier :: HsName -> Maybe HsName
%%[[8
hsnQualifier = \_ -> Nothing
%%][50
hsnQualifier = fst . hsnSplitQualify
%%]]

-- replace/set qualifier
hsnSetQual :: HsName -> HsName -> HsName
%%[[8
hsnSetQual _ = id
%%][50
hsnSetQual m = hsnPrefixQual m . hsnQualified
%%]]

-- is qualified?
hsnIsQual :: HsName -> Bool
%%[[8
hsnIsQual = \_ -> False
%%][50
hsnIsQual = isJust . hsnQualifier
%%]]
%%]

%%[50 export(hsnMapQual,hsnSetLevQual)
hsnMapQual :: (HsName -> HsName) -> HsName -> HsName
hsnMapQual f qn
  = case hsnSplitQualify qn of
      (Nothing,n) -> qn
      (Just q ,n) -> hsnSetQual (f q) n

hsnSetLevQual :: Int -> HsName -> HsName -> HsName
hsnSetLevQual 0 m n = hsnSetQual m n
hsnSetLevQual _ _ n = n
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fix the uniqifier part by making a string of it, suffixing it to the base part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(hsnFixUniqifiers)
hsnFixUniqifiers' :: Bool -> String -> HsName -> HsName
hsnFixUniqifiers' showlen sep (HsName_Modf _ qs n us) = hsnMkModf qs (hsnSuffix n (concat $ showHsNameUniqifierMp' showlen sep us)) Map.empty
hsnFixUniqifiers' _       _   n                       = n

hsnFixUniqifiers :: HsName -> HsName
hsnFixUniqifiers = hsnFixUniqifiers' True "_@"

hsnJavalikeFixUniqifiers :: HsName -> HsName
hsnJavalikeFixUniqifiers = hsnFixUniqifiers' False ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Strip the uniqifier part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(hsnStripUniqifiers)
hsnStripUniqifiers :: HsName -> HsName
hsnStripUniqifiers (HsName_Modf _ qs n us) = hsnMkModf qs n emptyHsNameUniqifierMp
hsnStripUniqifiers n                       = n
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make globally unique by prefixing with module name (if not already done so)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(hsnQualUniqify)
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

%%[1
instance HSNM UID where
  mkHNm = HsName_UID
  -- mkHNm x = hsnFromString ('_' : show x)
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
                                n@(HsName_Modf _ _ _ _) -> hsnFixateHash (n {hsnQualifiers = qs})
                                n                       -> hsnMkModf qs n Map.empty
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

%%[50
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

%%[50
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
  put (HsName_UID   a    ) = putWord8 1 >> put a
  put (HsName_Pos   a    ) = putWord8 2 >> put a
  put (HsName_Nr        a b  ) = putWord8 3 >> put a >> put b
  put (HsName_Modf  a b c d) = putWord8 4 >> put a >> put b >> put c >> put d
  get = do t <- getWord8
           case t of
             0 -> liftM  HsName_Base    get
             1 -> liftM  HsName_UID     get
             2 -> liftM  HsName_Pos     get
             3 -> liftM2 HsName_Nr      get get
             4 -> liftM4 HsName_Modf    get get get get

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
%%[[7
  | IdOcc_Fld
%%]]
%%[[9
  | IdOcc_Class
  | IdOcc_Inst
  | IdOcc_Dflt
%%]]
  | IdOcc_Any
%%[[50
  | IdOcc_Data
%%]]
%%[[93
  | IdOcc_Fusion
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
%%[[7
  show IdOcc_Fld      = "Field"
%%]]
%%[[9
  show IdOcc_Class    = "Class"
  show IdOcc_Inst     = "Instance"
  show IdOcc_Dflt     = "Default"
%%]]
  show IdOcc_Any      = "Any"
%%[[50
  show IdOcc_Data     = "Data"
%%]]
%%[[93
  show IdOcc_Fusion   = "Fusion"
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
%%% Safe names for Java like backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(hsnSafeJavaLike)
-- ensure a name valid for backends which are more restrictive in their allowed identifier character set
hsnSafeJavaLike :: HsName -> HsName
hsnSafeJavaLike
  = hsnMapQualified (concatMap safe . first) . hsnJavalikeFixUniqifiers . hsnEnsureIsBase
  where safe '_'                                      = "__"
        safe c | isDigit c || isLetter c || c == '_'  = [c]
               | otherwise                            = "_" ++ showHex (ord c) ""
        first s@(c:_) | isDigit c = '_' : s
        first s                   =       s
%%]
        safe '.'  = "_dot"
        safe ':'  = "_colon"
        safe '/'  = "_fslash"
        -- safe '<'  = "_lt"
        -- safe '>'  = "_gt"
        safe '\\' = "_bslash"
        safe '['  = "_lbrack"
        safe ']'  = "_rbrack"
        safe '@'  = "_at"
        safe  c   = [c]


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name of a pattern var/con
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RPatNm(..))
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
%%]

%%[(8 codegen) hs export(rpatNmIsOrig)
rpatNmIsOrig :: RPatNm -> Bool
rpatNmIsOrig (RPatNmOrig _) = True
rpatNmIsOrig _              = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin, remainder in {%{EH}Base.HsName.Builtin}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(hsnUnknown)
hsnUnknown ::  HsName
hsnUnknown =   hsnFromString "??"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Track dictionary intentions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) hs export(Track(..))

data Track
  = TrackNone
  | TrackSelf
  | TrackCtx Int
  | TrackSelect Int Track
  | TrackVarApply HsName [Track]
  deriving (Eq, Ord, Show)

%%]

%%[(50 codegen) hs

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


%%]


