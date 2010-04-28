-------------------------------------------------------------------------
-- Judgment Gamma
-------------------------------------------------------------------------

%%[1 hs module (JdShpGam)
%%]

%%[1 hs export (JdShpInfo(..), JdShpGam, jdshpgUnionShadow)
%%]

%%[1 hs import (qualified Data.Set as Set, qualified Data.Map as Map, EH.Util.Pretty, Common, Gam, FmGam)
%%]

-------------------------------------------------------------------------
-- Judgement formats
-------------------------------------------------------------------------

%%[1 hs
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

%%]
