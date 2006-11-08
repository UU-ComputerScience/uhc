%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.HsName} import(EH.Util.Utils,UU.Pretty, EH.Util.PPUtils,Data.List)
%%]

%%[1 import(UU.Scanner.Position) export(HSNM(..),HsName(..))
%%]

%%[8 export(hsnShowAlphanumeric)
%%]

%%[8 import(EH.Util.FPath,Char,Data.Maybe)
%%]

%%[8 export(hsnInitLast)
%%]

%%[8 export(hsnPrefix,hsnSuffix,stringAlphanumeric,  hsnAlphanumeric)
%%]

%%[10 export(hsnConcat)
%%]

%%[12 export(hsnQualified,hsnQualifier,hsnPrefixQual,hsnSetQual,hsnIsQual,hsnMapQual,hsnSetLevQual)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names, datatype
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.HsName.type
data HsName
  =   HNm String
  deriving (Eq,Ord)

instance Show HsName where
  show (HNm s) = s
%%]

%%[1
instance PP HsName where
  pp h = pp (show h)
%%]

%%[7.HsName.type -1.HsName.type
data HsName
  =   HNm String
  |   HNPos Int
%%]
%%[12
  |   HNmQ [HsName]
%%]
%%[7
  deriving (Eq,Ord)
%%]

%%[7
hsnShow :: String -> HsName -> String
hsnShow _   (HNm s    )  = s
hsnShow _   (HNPos p  )  = show p
%%[[12
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

%%[8
hsnToList :: HsName -> [HsName]
%%[[12
hsnToList (HNmQ ns) = ns
%%]
hsnToList n         = [n]

hsnInitLast :: HsName -> ([HsName],HsName)
hsnInitLast = maybe (panic "hsnInitLast") id . initlast . hsnToList
%%]

%%[8
hsnPrefix                           ::  String -> HsName -> HsName
hsnPrefix   p   hsn
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,HNm (p ++ show n))

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p
  = case hsnInitLast hsn of
      (ns,n) -> mkHNm (ns,HNm (show n ++ p))
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
hsnAlphanumeric :: HsName -> HsName
hsnAlphanumeric (HNm s) = HNm (stringAlphanumeric s)
hsnAlphanumeric n@(HNPos p) = HNm ("x"++show p)
-- hsnAlphanumeric n@(HNPos p) = n
%%]
%%[12
hsnAlphanumeric (HNmQ ns) = HNm $ hsnShow "_" $ HNmQ (map hsnAlphanumeric ns)
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
hsnConcat       h1    h2            =   HNm (show h1 ++ show h2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HsName & module related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
hsnQualified :: HsName -> HsName
hsnQualified = snd . hsnInitLast

hsnQualifier :: HsName -> Maybe HsName
hsnQualifier n
  = case hsnInitLast n of
      ([],_) -> Nothing
      (ns,_) -> Just (mkHNm ns)

hsnPrefixQual :: HsName -> HsName -> HsName
hsnPrefixQual m n = mkHNm (hsnToList m ++ hsnToList n)

hsnSetQual :: HsName -> HsName -> HsName
hsnSetQual m = hsnPrefixQual m . hsnQualified

hsnMapQual :: (HsName -> HsName) -> HsName -> HsName
hsnMapQual f qn
  = case hsnInitLast qn of
      ([],n) -> n
      (ns,n) -> hsnSetQual (f (mkHNm ns)) n

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
  mkHNm s = HNm s
%%]

%%[8.HSNM.String -1.HSNM.String
instance HSNM String where
  mkHNm s
    = mkHNm $ map HNm $ ws'
    where ws  = wordsBy (=='.') s
          ws' = case initlast2 ws of
                  Just (ns,"","") -> ns ++ ["."]
                  _               -> ws
%%]

%%[8
instance HSNM ([HsName],HsName) where
  mkHNm (l,n) = mkHNm (l ++ [n])

instance HSNM [HsName] where
  mkHNm [n] = n
  mkHNm []  = HNm "" -- ????, or empty alternative of HsName
%%[[12
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
%%[[12
  | IdOcc_Data
%%]]
  deriving (Show,Eq,Ord)
%%]

%%[1
-- intended for parsing
instance PP IdOccKind where
  pp IdOcc_Val      = pp "Value"
  pp IdOcc_Pat      = pp "Pat"
  pp IdOcc_Type     = pp "Type"
%%[[6
  pp IdOcc_Kind     = pp "Kind"
%%]]
%%[[9
  pp IdOcc_Class    = pp "Class"
  pp IdOcc_Inst     = pp "Instance"
  pp IdOcc_Dflt     = pp "Default"
%%]]
  pp IdOcc_Any      = pp "Any"
%%[[12
  pp IdOcc_Data     = pp "Data"
%%]]
%%]

%%[1 export(IdOcc(..),ppIdOcc)
data IdOcc
  = IdOcc { ioccNm :: HsName, ioccKind :: IdOccKind }
  deriving (Show,Eq,Ord)

-- intended for parsing
ppIdOcc :: (HsName -> PP_Doc) -> IdOcc -> PP_Doc
ppIdOcc pn o = ppCurlysCommas [pn (ioccNm o),pp (ioccKind o)]

instance PP IdOcc where
  pp = ppIdOcc pp
%%]

