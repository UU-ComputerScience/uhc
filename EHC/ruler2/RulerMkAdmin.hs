-- $Id: Ruler.ag 231 2005-06-07 14:39:41Z atze $

-------------------------------------------------------------------------
-- Supporting functions for admin building
-------------------------------------------------------------------------

module RulerMkAdmin where

import Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Nm
import Utils

import RulerUtils
import RulerAdmin

-------------------------------------------------------------------------
-- Rule sets
-------------------------------------------------------------------------

-- attr directions for names in gam
gamAtDirMp :: VwScInfo e -> Gam Nm v -> Map.Map Nm [AtDir]
gamAtDirMp vi g = Map.mapWithKey (\n _ -> maybe [] atDirs . Map.lookup n . vwscFullAtGam $ vi) g

-- split attr dir map into sets of syn/inh attrs
atDirMpSynInh :: Map.Map Nm [AtDir] -> (Set.Set Nm,Set.Set Nm)
atDirMpSynInh m
  = Map.foldWithKey (\n d (s,i) -> (if AtSyn `elem` d then Set.insert n s else s
                                   ,if AtInh `elem` d then Set.insert n i else i))
                    (Set.empty,Set.empty) m

-- union of all judge attr defs in a set (of names with a specific direction)
jaGamUseInS :: JAGam e -> Set.Set Nm -> Set.Set Nm
jaGamUseInS g s = Set.unions [ jaNmS i | (n,i) <- Map.toList g, n `Set.member` s ]

-- default attr gam of judgement, based on scheme
jaGamDflt :: (Nm -> e) -> Nm -> Nm -> ScGam e -> JAGam e
jaGamDflt mkE sn nVw scGam
  = case scVwGamLookup sn nVw scGam of
      Just (_,vi) -> Map.mapWithKey (\n _ -> JAInfo n (mkE n) (Set.singleton n)) . vwscFullAtGam $ vi
      Nothing     -> emptyGam

-- determine sets if inh/syn var's
reGamUpdInOut :: Nm -> ScGam e -> REGam e -> REGam e
reGamUpdInOut nVw scGam pg
  = Map.map
       (\i ->
           case i of
               REInfoJudge _ sn _ _ jg | isJust mvi
                 -> i  {reInNmS = jaGamUseInS jg aInhS, reOutNmS = jaGamUseInS jg aSynS}
                 where mvi = scVwGamLookup sn nVw scGam
                       aDirMp = gamAtDirMp (snd . maybe (panic "reGamUpdInOut") id $ mvi) jg
                       (aSynS,aInhS) = atDirMpSynInh aDirMp
               _ -> i
       )
       pg

-- extend rule expr's gam with defaults, additionaly return increment
reGamExtDflt' :: (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> (REGam e,REGam e)
reGamExtDflt' dfltScJaGam nVw scGam g
  = Map.mapAccumWithKey
      (\gIncr n i
        -> case i of
             REInfoJudge _ sn _ _ jg
               -> (Map.insert n (i {reJAGam = d `Map.difference` jg}) gIncr,i {reJAGam = jg `Map.union` d})
               where d = dfltScJaGam sn n
      )
      emptyGam g

reGamExtDflt :: (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> REGam e
reGamExtDflt dfltScJaGam nVw scGam
  = snd . reGamExtDflt' dfltScJaGam nVw scGam

-- named expressions for each judgement
reGamExprFmGam :: (e -> FmGam e) -> REGam e -> Gam Nm (FmGam e)
reGamExprFmGam exprNmGam
  = Map.map (Map.fold (\i g -> exprNmGam (jaExpr i) `Map.union` g) emptyGam . reJAGam)

-- extend rule expr's gam with new one
reGamExtWithNew :: (e -> FmGam e) -> (FmGam e -> e -> e) -> (e -> Set.Set Nm) -> (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> REGam e -> REGam e
reGamExtWithNew exprNmGam exprSubst exprNmS dfltJaGam nVw scGam gamPrev g
  = Map.foldWithKey
      (\n i gamPrev
        -> case i of
             REInfoJudge _ sn _ _ jg
               -> Map.insert n (pi {reJAGam = jg'}) gamPrev
               where (base,pi)
                       = case Map.lookup n gamPrev of
                           Just pi | reScNm pi == sn
                             -> (reJAGam pi,pi)
                           _ -> (dfltJG,i)
                     dfltJG = dfltJaGam sn n
                     jg' = Map.map (\i -> let e = exprSubst nmfg (jaExpr i)
                                          in  i {jaExpr = e,jaNmS = exprNmS e})
                                   (jg `Map.union` base)
                     nmfg = Map.findWithDefault emptyGam n nmFmGams
             REInfoDel ns
               -> foldr Map.delete gamPrev ns
      )
      gamPrev g
  where nmFmGams = reGamExprFmGam exprNmGam gamPrev


