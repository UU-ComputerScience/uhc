%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.HsName} import(EH.Util.Utils,EH.Util.Pretty, Data.List)
%%]

%%[1 import(UU.Scanner.Position) export(HSNM(..),HsName(..))
%%]

%%[3 import(qualified Data.Set as Set)
%%]

%%[8 export(hsnShowAlphanumeric, hsnShowAlphanumericShort)
%%]

%%[8 import(EH.Util.FPath,Char,Data.Maybe,qualified Data.Map as Map)
%%]

%%[8 export(OrigName(..))
%%]

%%[10 export(hsnConcat)
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[9999 import({%{EH}Base.Hashable})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, datatype
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Alternative impl via 

--------
qualified Data.ByteString.Char8 as BS

  =   HNm   !BS.ByteString

hsnFromString :: String -> HsName
hsnFromString = HNm . BS.pack

hsnHNmFldToString :: BS.ByteString -> String
hsnHNmFldToString = BS.unpack
--------
is not faster

%%[1.HsName.type
data HsName
  =   HNm String
  deriving (Eq,Ord)

instance Show HsName where
  show (HNm s) = s
%%]

%%[9999
instance Hashable HsName where
  hash (HNm  h _ ) = h
  hash (HNmQ h _ ) = h
  hash (HNPos p  ) = hash p
  hash (HNmNr i _) = hash i
%%]

-- for speed, comparison will be done on the basis of a hash
%%[7.HsName.type -1.HsName.type
data HsName
%%[[7
  =   HNm   { hsnHNmFld :: !String }
%%][9999
  =   HNm   { hsnHash :: !Hash, hsnHNmFld :: !String }
%%]]
  |   HNPos !Int
%%[[8
  |   HNmNr !Int !OrigName
%%[[7
  |   HNmQ  ![HsName]
%%][9999
  |   HNmQ  !Hash ![HsName]
%%]]
%%]]
  deriving (Eq,Ord)
%%]

We also need comparison based on name only, not on hash.

%%[7 export(cmpHsNameOnNm)
%%[[9999
cmpHsNameOnNm (HNm  h1 s1) (HNm  h2 s2) = compare s1 s2
cmpHsNameOnNm (HNmQ h1 s1) (HNmQ h2 s2) = compare s1 s2
%%]]
cmpHsNameOnNm n1           n2           = compare n1 n2
%%]

%%[1 export(mbHNm)
mbHNm :: HsName -> Maybe String
%%[[1
mbHNm (HNm s  )
%%][9999
mbHNm (HNm _ s)
%%]]
                = Just (hsnHNmFldToString s)
%%[[7
mbHNm _         = Nothing
%%]]
%%]

%%[1.hsnFromString export(hsnFromString)
hsnFromString :: String -> HsName
hsnFromString s
%%[[1
  = HNm s
%%][9999
  = HNm (hash s) s
%%]]
%%]

%%[1.hsnHNmFldToString export(hsnHNmFldToString)
hsnHNmFldToString :: String -> String
hsnHNmFldToString = id
%%]

%%[8
data OrigName = OrigNone
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
hsnShow :: String -> HsName -> String
%%[[7
hsnShow _   (HNm   s  )
%%][9999
hsnShow _   (HNm _ s  )
%%]]
                         = hsnHNmFldToString s
hsnShow _   (HNPos p  )  = show p
%%[[8
hsnShow _   (HNmNr n OrigNone        )  = "x_"        ++ show n
hsnShow _   (HNmNr n (OrigLocal  hsn))  = "x_"        ++ show n ++ "_" ++ hsnShow "." hsn
hsnShow _   (HNmNr n (OrigGlobal hsn))  = "global_x_" ++ show n ++ "_" ++ hsnShow "." hsn
hsnShow _   (HNmNr n (OrigFunc   hsn))  = "fun_x_"    ++ show n ++ "_" ++ hsnShow "." hsn
%%[[7
hsnShow sep (HNmQ   ns)  
%%][9999
hsnShow sep (HNmQ _ ns)  
%%]]
                         = concat $ intersperse sep $ map show ns
%%]]
%%]

%%[7
instance Show HsName where
  show = hsnShow "."
%%]

%%[1
hsnToList :: HsName -> [HsName]
%%[[8
hsnToList (HNmQ   ns) = ns
%%][9999
hsnToList (HNmQ _ ns) = ns
%%]]
hsnToList n           = [n]
%%]

%%[1 export(hsnInitLast)
hsnInitLast :: HsName -> ([HsName],HsName)
hsnInitLast = maybe (panic "hsnInitLast") id . initlast . hsnToList
%%]

%%[1 export(hsnPrefix,hsnSuffix,mkHNmPrefix)
hsnPrefix                           ::  String -> HsName -> HsName
hsnPrefix   p   hsn
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,hsnFromString (p ++ show n))

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,hsnFromString (show n ++ p))

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

hsnShowAlphanumeric :: HsName -> String
hsnShowAlphanumericShort :: HsName -> String

hsnShowAlphanumericShort (HNmNr n (OrigFunc   orig)) = hsnShowAlphanumeric orig
hsnShowAlphanumericShort x = hsnShowAlphanumeric x

%%[[8
hsnShowAlphanumeric (HNm s  ) = dontStartWithDigit(stringAlphanumeric s)
%%][9999
hsnShowAlphanumeric (HNm _ s) = dontStartWithDigit(stringAlphanumeric $ hsnHNmFldToString s)
%%]]
hsnShowAlphanumeric (HNPos p)                   = "y" ++ show p
hsnShowAlphanumeric (HNmNr n OrigNone)          = "x" ++ show n
hsnShowAlphanumeric (HNmNr n (OrigLocal orig))  = "x" ++ show n   -- hsnShowAlphanumeric orig
hsnShowAlphanumeric (HNmNr n (OrigGlobal orig)) = "global_" ++ hsnShowAlphanumeric orig
hsnShowAlphanumeric (HNmNr n (OrigFunc   orig)) = "fun_"    ++ hsnShowAlphanumeric orig
%%[[8
%%[[8
hsnShowAlphanumeric (HNmQ   ns)
%%][9999
hsnShowAlphanumeric (HNmQ _ ns)
%%]]
                                = concat $ intersperse "_" $ map hsnShowAlphanumeric ns
%%]]
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
hsnMapQualified :: (HsName -> HsName) -> HsName -> HsName
hsnMapQualified f qn
  = case hsnSplitQualify qn of
      (Nothing,n) -> f n
      (Just q ,n) -> hsnPrefixQual q (f n)
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
%%[[8
  mkHNm ns  = HNmQ ns
%%][9999
  mkHNm ns  = HNmQ (hashList ns) ns
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
instance Binary HsName where
  put (HNm   a  ) = putWord8 0 >> put a
  put (HNmQ  a  ) = putWord8 1 >> put a
  put (HNPos a  ) = putWord8 2 >> put a
  put (HNmNr a b) = putWord8 3 >> put a >> put b
  get = do t <- getWord8
           case t of
             0 -> liftM  HNm   get
             1 -> liftM  HNmQ  get
             2 -> liftM  HNPos get
             3 -> liftM2 HNmNr get get

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

This saves space but trades in some 10% (or even more) execution time.
No internal sharing thus.

%%[20
%%]
instance Serialize HsName where
  sput = sputShared
  sget = sgetShared
  sputNested (HNm   a  ) = sputWord8 0 >> sput a
  sputNested (HNmQ  a  ) = sputWord8 1 >> sput a
  sputNested (HNPos a  ) = sputWord8 2 >> sput a
  sputNested (HNmNr a b) = sputWord8 3 >> sput a >> sput b
  sgetNested
    = do t <- sgetWord8
         case t of
           0 -> liftM  HNm   sget
           1 -> liftM  HNmQ  sget
           2 -> liftM  HNPos sget
           3 -> liftM2 HNmNr sget sget

instance Serialize OrigName where
  sput = sputShared
  sget = sgetShared
  sputNested (OrigNone    ) = sputWord8 0
  sputNested (OrigLocal  a) = sputWord8 1 >> sput a
  sputNested (OrigGlobal a) = sputWord8 2 >> sput a
  sputNested (OrigFunc   a) = sputWord8 3 >> sput a
  sgetNested
    = do t <- sgetWord8
         case t of
           0 -> return OrigNone
           1 -> liftM  OrigLocal  sget
           2 -> liftM  OrigGlobal sget
           3 -> liftM  OrigFunc   sget

instance Binary IdOccKind where
  put = putEnum8
  get = getEnum8

instance Serialize IdOccKind where
  sput = sputPlain
  sget = sgetPlain

instance Serialize IdOcc where
  sput = sputShared
  sget = sgetShared
  sputNested (IdOcc a b) = sput a >> sput b
  sgetNested = liftM2 IdOcc sget sget



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

