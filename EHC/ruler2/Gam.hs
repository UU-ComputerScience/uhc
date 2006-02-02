-------------------------------------------------------------------------
-- Gamma
-------------------------------------------------------------------------

module Gam
  ( Gam, emptyGam
  , ppGam, ppGam'
  , dblGamLookup
  , gamTryLookups, gamLookupWithDefault
  )
  where

import Data.Maybe
import qualified Data.Map as Map
import PPUtils
import UU.Pretty
import Common

-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

type Gam k v = Map.Map k v

emptyGam = Map.empty

ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam = ppListSepV "[" "]" "," . map (\(k,v) -> pp k >#< ":->" >#< pp v) . Map.toList

ppGam' :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam' = vlist . map (\(k,v) -> pp k >#< ":->" >#< pp v) . Map.toList

dblGamLookup :: Ord k => (i1 -> Gam k i2) -> k -> k -> Gam k i1 -> Maybe (i1,i2)
dblGamLookup gOf sn vn g
  = case Map.lookup sn g of
      Just si
        -> fmap ((,) si) . Map.lookup vn . gOf $ si
      _ -> Nothing

instance (PP k,PP v) => PP (Gam k v) where
  pp g = ppGam' g

-------------------------------------------------------------------------
-- General purpose lookup with a default key
-------------------------------------------------------------------------

gamTryLookups :: Ord k => v -> (e -> v) -> [k] -> Gam k e -> v
gamTryLookups dflt extr keys g
  = case keys of
      (k:ks) -> case Map.lookup k g of
                  Just i  -> extr i
                  Nothing -> gamTryLookups dflt extr ks g
      _      -> dflt

gamLookupWithDefault :: Ord k => k -> v -> (e -> v) -> [k] -> Gam k e -> v
gamLookupWithDefault dfltKey dflt extr keys g
  = gamTryLookups dflt extr (keys ++ [dfltKey]) g

