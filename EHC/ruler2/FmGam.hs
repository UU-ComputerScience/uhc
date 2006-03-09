-------------------------------------------------------------------------
-- Format Gamma
-------------------------------------------------------------------------

module FmGam
  ( module Gam
  
  , FmInfo(..), FmGam
  , fmSingleton, fmNull
  , fmGamFromList, fmGamFromList'
  , fmGamToList'
  , fmGamUnion, fmGamUnions
  , fmGamLookup, fmGamMap
  
  , FmKdGam, fkGamLookup
  
  , FmDrGam, fdGamLookup
  
  , RwGam, rwGamLookup, rwSingleton, rwGamUnion
  , ppRwGam
  )
  where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import PPUtils
import UU.Pretty
import Common
import Gam

-------------------------------------------------------------------------
-- Formats
-------------------------------------------------------------------------

data FmInfo e
  = FmInfo
      { fmNm    :: Nm
      , fmKdGam :: FmKdGam e
      }

instance Show (FmInfo e) where
  show _ = "FmInfo"

instance PP e => PP (FmInfo e) where
  pp i = "FM" >#< pp (fmNm i) >#< (ppGam . fmKdGam $ i)

type FmGam e = Gam Nm (FmInfo e)

fmSingleton :: Nm -> FmKind -> e -> FmGam e
fmSingleton n k e = gamSingleton n (FmInfo n (gamSingleton k e))

fmNull :: FmGam e -> Bool
fmNull = all (gamIsEmpty . fmKdGam) . gamElemsShadow

fmGamFromList' :: FmKind -> [(Nm,e)] -> FmGam e
fmGamFromList' fk = gamUnionsShadow . map (\(n,e) -> fmSingleton n fk e)

fmGamToList' :: FmKind -> FmGam e -> [(Nm,e)]
fmGamToList' fk g = [ (n,e) | (n,i) <- gamAssocsShadow g, e <- fkGamLookup [] (:[]) [fk] (fmKdGam i) ]

fmGamFromList :: [(Nm,e)] -> FmGam e
fmGamFromList = fmGamFromList' FmAll

fmGamUnion :: FmGam e -> FmGam e -> FmGam e
fmGamUnion = gamUnionWith (\i1 i2 -> i1 {fmKdGam = fmKdGam i1 `gamUnionShadow` fmKdGam i2})

fmGamUnions :: [FmGam e] -> FmGam e
fmGamUnions = foldr fmGamUnion emptyGam

{-
fmLGamUnion :: FmGam [e] -> FmGam [e] -> FmGam [e]
fmLGamUnion = gamUnionWith (\i1 i2 -> i1 {fmKdGam = gamUnionWith (++) (fmKdGam i1) (fmKdGam i2)})
-}

fmGamLookup :: Nm -> FmKind -> FmGam e -> Maybe e
fmGamLookup n k g
  = case gamLookup n g of
      Just i
        -> fkGamLookup Nothing Just [k] (fmKdGam i)
      _ -> Nothing

fmGamMap :: (Nm -> a -> b) -> FmGam a -> FmGam b
fmGamMap f = gamMapWithKey (\n i -> i {fmKdGam = gamMap (\e -> f n e) (fmKdGam i)})

-------------------------------------------------------------------------
-- FmGam for FmKind
-------------------------------------------------------------------------

type FmKdGam e = Gam FmKind e

fkGamLookup :: v -> (e -> v) -> [FmKind] -> FmKdGam e -> v
fkGamLookup = gamTryLookupsWithDefault FmAll

-------------------------------------------------------------------------
-- FmGam for AtDir
-------------------------------------------------------------------------

type FmDrGam e = Gam AtDir e

fdGamLookup :: v -> (e -> v) -> [AtDir] -> FmDrGam e -> v
fdGamLookup = gamTryLookupsWithDefault AtInOut

-------------------------------------------------------------------------
-- Rewrite rules
-------------------------------------------------------------------------

type RwGam e = FmGam (FmDrGam [e])

rwGamLookup :: Nm -> FmKind -> AtDir -> RwGam e -> Maybe [e]
rwGamLookup n k d g
  = case fmGamLookup n k g of
      Just g'
        -> fdGamLookup Nothing Just [d] g'
      _ -> Nothing

rwSingleton :: Nm -> FmKind -> AtDir -> e -> RwGam e
rwSingleton n k d e = gamSingleton n (FmInfo n (gamSingleton k (gamSingleton d [e])))

rwGamUnion :: RwGam e -> RwGam e -> RwGam e
rwGamUnion = gamUnionWith (\i1 i2 -> i1 {fmKdGam = gamUnionWith (gamUnionWith (++)) (fmKdGam i1) (fmKdGam i2)})

ppRwGam :: PP e => RwGam e -> PP_Doc
ppRwGam = ppGam' . gamMap (\i -> fmNm i >#< ppGam (fmKdGam i))

