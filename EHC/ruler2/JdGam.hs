-------------------------------------------------------------------------
-- Judgment Gamma
-------------------------------------------------------------------------

module JdGam
  ( JdInfo(..), JdGam
  , jdgUnion
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import UU.Pretty
import PPUtils
import Common
import Gam
import FmGam

-------------------------------------------------------------------------
-- Judgement formats
-------------------------------------------------------------------------

data JdInfo e
  = JdInfo
      { jdExpr  :: e
      }
  | JdDel

instance Show (JdInfo e) where
  show _ = "JdInfo"

instance PP e => PP (JdInfo e) where
  pp (JdInfo e) = "Jd" >#< pp e
  pp (JdDel   ) = pp "JdDel"

type JdGam e = FmKdGam (JdInfo e)

jdgUnion :: JdGam e -> JdGam e -> JdGam e
jdgUnion gn g
  = Map.foldWithKey
      (\fk i g
        -> case i of
             JdDel -> Map.delete fk g
             _     -> Map.insert fk i g
      )
      g gn

