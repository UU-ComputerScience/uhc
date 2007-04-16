%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.HsName} import(EH.Util.Utils,EH.Util.Pretty, Data.List)
%%]

%%[1 import(UU.Scanner.Position) export(HSNM(..),HsName(..))
%%]

%%[3 import(qualified Data.Set as Set)
%%]

%%[8 export(hsnShowAlphanumeric)
%%]

%%[8 import(EH.Util.FPath,Char,Data.Maybe,qualified Data.Map as Map)
%%]

%%[8 export(stringAlphanumeric,  hsnAlphanumeric)
%%]

%%[10 export(hsnConcat)
%%]

%%[20 export(hsnQualified,hsnQualifier,hsnPrefixQual,hsnSetQual,hsnIsQual,hsnMapQual,hsnSetLevQual)
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

%%[1.hsnFromString export(hsnFromString)
hsnFromString :: String -> HsName
hsnFromString = HNm
%%]

%%[1.hsnHNmFldToString export(hsnHNmFldToString)
hsnHNmFldToString :: String -> String
hsnHNmFldToString = id
%%]

%%[1
instance PP HsName where
  pp h = pp (show h)
%%]

%%[7.HsName.type -1.HsName.type
data HsName
%%[[7
  =   HNm   !String
%%]]
  |   HNPos !Int
%%]
%%[8
  |   HNmNr !Int !(Maybe HsName)
%%]
%%[20
  |   HNmQ  ![HsName]
%%]
%%[7
  deriving (Eq,Ord)
%%]

%%[7
hsnShow :: String -> HsName -> String
hsnShow _   (HNm s    )  = hsnHNmFldToString s
hsnShow _   (HNPos p  )  = show p
%%[[8
hsnShow _   (HNmNr n _)  = "x_" ++ show n
%%]]
%%[[20
hsnShow sep (HNmQ ns  )  = concat $ intersperse sep $ map show ns
%%]]
%%]

%%[8
hsnShowAlphanumeric :: HsName -> String
hsnShowAlphanumeric = show . hsnAlphanumeric
%%]

%%[7
instance Show HsName where
  show = hsnShow "."
%%]

%%[5 export(hsnInitLast)
hsnToList :: HsName -> [HsName]
%%[[20
hsnToList (HNmQ ns) = ns
%%]
hsnToList n         = [n]
%%]

%%[5
hsnInitLast :: HsName -> ([HsName],HsName)
hsnInitLast = maybe (panic "hsnInitLast") id . initlast . hsnToList
%%]

%%[5 export(hsnPrefix,hsnSuffix,mkHNmPrefix)
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
dontStartWithDigit xs | isDigit(head xs) = "y"++xs
                      | otherwise        = xs

hsnAlphanumeric :: HsName -> HsName
%%[[8
hsnAlphanumeric (HNm s) = hsnFromString (dontStartWithDigit(stringAlphanumeric s))
%%][99
hsnAlphanumeric (HNm s) = hsnFromString (dontStartWithDigit(stringAlphanumeric $ hsnHNmFldToString s))
%%]]
hsnAlphanumeric (HNPos p) = hsnFromString ("y"++show p)
--hsnAlphanumeric (HNmNr n mbOrig) = hsnFromString ("x"++show n)
hsnAlphanumeric (HNmNr n Nothing) = hsnFromString ("x"++show n)
hsnAlphanumeric (HNmNr n (Just orig)) = hsnAlphanumeric orig
%%]
%%[20
hsnAlphanumeric (HNmQ ns) = hsnFromString $ hsnShow "_" $ HNmQ (map hsnAlphanumeric ns)
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
%%% HsName & module related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
-- qualified part of a name
hsnQualified :: HsName -> HsName
hsnQualified = snd . hsnInitLast

-- qualifier (i.e. module name) of name
hsnQualifier :: HsName -> Maybe HsName
hsnQualifier n
  = case hsnInitLast n of
      ([],_) -> Nothing
      (ns,_) -> Just (mkHNm ns)

-- prefix/qualify with module name, on top of possible previous qualifier
hsnPrefixQual :: HsName -> HsName -> HsName
hsnPrefixQual m n = mkHNm (hsnToList m ++ hsnToList n)

-- replace/set qualifier
hsnSetQual :: HsName -> HsName -> HsName
hsnSetQual m = hsnPrefixQual m . hsnQualified

hsnMapQual :: (HsName -> HsName) -> HsName -> HsName
hsnMapQual f qn
  = case hsnInitLast qn of
      ([],n) -> n
      (ns,n) -> hsnSetQual (f (mkHNm ns)) n

-- is qualified?
hsnIsQual :: HsName -> Bool
hsnIsQual = isJust . hsnQualifier

hsnSetLevQual :: Int -> HsName -> HsName -> HsName
hsnSetLevQual 0 m n = hsnSetQual m n
hsnSetLevQual _ _ n = n
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
    = mkHNm $ map hsnFromString $ ws'
    where ws  = wordsBy (=='.') s
          ws' = case initlast2 ws of
                  Just (ns,"","") -> ns ++ ["."]
                  _               -> ws
%%]

%%[5
instance HSNM ([HsName],HsName) where
  mkHNm (l,n) = mkHNm (l ++ [n])

instance HSNM [HsName] where
  mkHNm [n] = n
  mkHNm []  = hsnFromString "" -- ????, or empty alternative of HsName
%%[[20
  mkHNm ns  = HNmQ ns
%%]
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
  deriving (Eq,Ord)
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
%%% Map of  names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(HsNameMp,hsnRepl)
type HsNameMp = Map.Map HsName HsName

hsnRepl :: HsNameMp -> HsName -> HsName
hsnRepl m n = Map.findWithDefault n n m
%%]


