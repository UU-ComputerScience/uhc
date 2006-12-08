-------------------------------------------------------------------------
-- Judgment Gamma
-------------------------------------------------------------------------

module JdShpGam
  ( JdShpInfo(..), JdShpGam
  , jdshpgUnionShadow
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import UU.Pretty
import EH.Util.PPUtils
import Common
import Gam
import FmGam

-------------------------------------------------------------------------
-- Judgement formats
-------------------------------------------------------------------------

data JdShpInfo e
  = JdShpInfo
      { jdshExpr  :: e
      }
  | JdShpDel

instance Show (JdShpInfo e) where
  show _ = "JdShpInfo"

instance PP e => PP (JdShpInfo e) where
  pp (JdShpInfo e) = "Jd" >#< pp e
  pp (JdShpDel   ) = pp "JdShpDel"

type JdShpGam e = FmKdGam (JdShpInfo e)

jdshpgUnionShadow :: JdShpGam e -> JdShpGam e -> JdShpGam e
jdshpgUnionShadow gn g
  = gamFoldWithKey
      (\fk i g
        -> case i of
             JdShpDel -> gamDelete fk g
             _        -> gamInsertShadow fk i g
      )
      g gn

