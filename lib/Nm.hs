module Nm
where

import Data.Maybe
import Data.Char
import Data.List
import UU.Pretty
import Utils
import PPUtils

-------------------------------------------------------------------------
-- Names (for use in Shuffle, Ruler)
-------------------------------------------------------------------------

data Nm' s
  = NmEmp
  | Nm     { nmStr      :: s }
  | NmSel  { nmNm       :: Nm' s
           , nmMbSel    :: Maybe s
           }
  deriving (Eq,Ord)

type Nm = Nm' String

nmBase' :: Nm -> String
nmBase' (NmSel n _) = nmBase' n
nmBase' (Nm s)      = s
nmBase' NmEmp       = ""

nmBase :: Nm -> Nm
nmBase = Nm . nmBase'

nmSetSuff :: Nm -> String -> Nm
nmSetSuff n s = NmSel (nmBase n) (Just s)

nmSetBase :: Nm -> String -> Nm
nmSetBase n s
  = nmFromMbL (Just s:nL)
  where (_:nL) = nmToMbL n

nmSetSel :: Nm' s -> s -> Nm' s
nmSetSel n s = NmSel n (Just s)

nmSel :: Nm -> String
nmSel = maybe "" id . nmMbSel

nmInit :: Nm -> Nm
nmInit (NmSel n _) = n
nmInit n           = n

nmToMbL :: Nm' s -> [Maybe s]
nmToMbL 
  = reverse . ns
  where ns (NmSel n s) = s : ns n
        ns (Nm s) = [Just s]
        ns NmEmp  = []

nmToL :: Nm -> [String]
nmToL = map (maybe "" id) . nmToMbL

nmFromMbL :: [Maybe s] -> Nm' s
nmFromMbL
  = n . reverse
  where n [Just s] = Nm s
        n (s:ss)   = NmSel (n ss) s
        n []       = NmEmp

nmFromL :: [s] -> Nm' s
nmFromL = nmFromMbL . map Just

nmApd :: Nm' s -> Nm' s -> Nm' s
nmApd n1 n2
  = nmFromMbL (l1 ++ l2)
  where l1 = nmToMbL n1
        l2 = nmToMbL n2

nmStrApd :: Nm -> Nm -> Nm
nmStrApd n1 n2
  = Nm (s1 ++ s2)
  where s1 = show n1
        s2 = show n2

nmCapitalize :: Nm -> Nm
nmCapitalize n
  = case nmToMbL n of
      (Just s:ns) -> nmFromMbL (Just (strCapitalize s) : ns)
      _           -> n

nmDashed :: Nm -> Nm
nmDashed = Nm . map (\c -> if isAlphaNum c then c else '-') . show

nmFlatten :: Nm -> Nm
nmFlatten = Nm . show

nmShow' :: String -> Nm -> String
nmShow' sep = concat . intersperse sep . nmToL

nmShowAG :: Nm -> String
nmShowAG = nmShow' "_"

instance Show Nm where
  show = nmShow' "."

instance PP Nm where
  pp = ppListSep "" "" "." . nmToL

instance Functor Nm' where
  fmap f NmEmp  = NmEmp
  fmap f (Nm s) = Nm (f s)
  fmap f (NmSel n ms) = NmSel (fmap f n) (fmap f ms)

-------------------------------------------------------------------------
-- Make name of something
-------------------------------------------------------------------------

class NM a where
  mkNm :: a -> Nm

instance NM Nm where
  mkNm = id

instance NM String where
  mkNm s = nmFromL [s]

instance NM Int where
  mkNm = mkNm . show


