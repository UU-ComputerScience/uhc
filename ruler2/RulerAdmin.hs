-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerAdmin
  ( module FmGam
  , module JdGam
  
  , AtInfo(..), AtGam
  , emptyAtInfo
  , atGamNode, atMbSynInh
  
  , ExplInfo(..), ExplGam
  
  , VwScInfo(..), VwScGam
  , emptyVwScInfo
  
  , ScInfo(..), ScGam
  , emptyScInfo
  , scVwGamLookup, scVwGamNodeAt
  
  , JAInfo(..), JAGam
  , jaGamToFmGam, fmGamToJaGam
  
  , REInfo(..), REGam
  , reMbJAGam
  
  , RlChInfo(..), RlChGam
  , rcGamUnion
  
  , VwRlInfo(..), VwRlGam
  , emptyVwRlInfo
  , vwrlDelEmptyJd, vrwlIsEmpty, vwrlScc, vwrlUndefs
  
  , RlInfo(..), RlGam
  , emptyRlInfo
  , rlVwGamLookup
  
  , RsInfo(..), RsGam
  , emptyRsInfo
  , rsInfoIsPlain, rsInfoIsGroup
  , rsRlOrder, rsInfoMbRlGam
  )
  where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Utils
import PPUtils
import UU.Pretty
import qualified UU.DData.Scc as Scc
import Common
import Gam
import FmGam
import JdGam

-------------------------------------------------------------------------
-- Attr
-------------------------------------------------------------------------

data AtInfo
  = AtInfo
      { atNm    :: Nm
      , atDirs  :: [AtDir]
      , atProps :: [AtProp]
      , atTy    :: Nm
      }

emptyAtInfo :: AtInfo
emptyAtInfo = AtInfo nmUnk [] []  nmUnk

instance Show AtInfo where
  show _ = "AtInfo"

instance PP AtInfo where
  pp i = "AT" >#< pp (atTy i) >#< pp (show (atDirs i)) >#< pp (show (atProps i))

type AtGam = Gam Nm AtInfo

atGamNode :: AtGam -> Maybe Nm
atGamNode g
  = do let aNdGm = Map.filter (\ai -> AtNode `elem` atProps ai) g
       case Map.toList aNdGm of
         ((na,ai):_) -> return na
         _           -> Nothing

atMbSynInh :: AtInfo -> Maybe Nm
atMbSynInh i
  = if      AtThread `elem` atProps i then Just (nmInit n)
    else if AtUpdown `elem` atProps i then Just (nmInit n)
    else if AtInh    `elem` atDirs  i
         && AtSyn    `elem` atDirs  i then Just n
                                      else Nothing
  where n = atNm i

-------------------------------------------------------------------------
-- Explanations
-------------------------------------------------------------------------

data ExplInfo e
  = ExplInfo
      { explExpr  :: e
      }

instance Show (ExplInfo e) where
  show _ = "ExplInfo"

instance PP e => PP (ExplInfo e) where
  pp (ExplInfo e) = "Expl" >#< pp e

type ExplGam e = Gam Nm (ExplInfo e)

-------------------------------------------------------------------------
-- View (related to scheme)
-------------------------------------------------------------------------

data VwScInfo e
  = VwScInfo
      { vwscNm          :: Nm
      , vwscJdGam       :: JdGam e
      , vwscAtGam       :: AtGam
      , vwscFullAtGam   :: AtGam
      , vwscExplGam     :: ExplGam e
      }

emptyVwScInfo = VwScInfo nmNone emptyGam emptyGam emptyGam emptyGam

instance Show (VwScInfo e) where
  show _ = "VwScInfo"

instance PP e => PP (VwScInfo e) where
  pp i = "VWSc" >#< pp (vwscNm i) >#< (ppGam (vwscAtGam i) >-< ppGam (vwscFullAtGam i) >-< ppGam (vwscJdGam i) >-< ppGam (vwscExplGam i))

type VwScGam e = Gam Nm (VwScInfo e)

-------------------------------------------------------------------------
-- Scheme
-------------------------------------------------------------------------

data ScInfo e
  = ScInfo
      { scNm        :: Nm
      , scMbAGStr   :: Maybe String
      , scKind      :: ScKind
      , scVwGam     :: VwScGam e
      }

emptyScInfo = ScInfo nmNone Nothing ScJudge emptyGam

instance Show (ScInfo e) where
  show _ = "ScInfo"

instance PP e => PP (ScInfo e) where
  pp i = "SC" >#< pp (scNm i) >#< ppGam (scVwGam i)

type ScGam e = Gam Nm (ScInfo e)

scVwGamLookup :: Nm -> Nm -> ScGam e -> Maybe (ScInfo e,VwScInfo e)
scVwGamLookup = dblGamLookup scVwGam

scVwGamNodeAt :: Nm -> Nm -> ScGam e -> Maybe Nm
scVwGamNodeAt nSc nVw g
  = do (si,vi) <- scVwGamLookup nSc nVw g
       atGamNode (vwscAtGam vi)

-------------------------------------------------------------------------
-- RExpr's judgement attr equations
-------------------------------------------------------------------------

data JAInfo e
  = JAInfo
      { jaNm    :: Nm
      , jaExpr  :: e
      , jaNmS   :: Set.Set Nm
      }
  | JAInfoDel
      { jaNm    :: Nm
      }

instance Show (JAInfo e) where
  show _ = "JAInfo"

instance PP e => PP (JAInfo e) where
  pp i@(JAInfo _ _ _) = "JA" >#< (pp (jaExpr i) >-< pp (show (jaNmS i)))
  pp i@(JAInfoDel _)  = "JADel" >#< jaNm i

type JAGam e = Gam Nm (JAInfo e)

jaGamToFmGam :: (e -> e) -> JAGam e -> FmGam e
jaGamToFmGam f = fmGamFromList . map (\(n,i) -> (n,f (jaExpr i))) . Map.toList

fmGamToJaGam :: FmKind -> FmGam e -> JAGam e
fmGamToJaGam fm = Map.fromList . map (\(n,e) -> (n,JAInfo n e Set.empty)) . Map.toList . Map.map (fkGamLookup (panic "fmGamToJaGam") id [fm] . fmKdGam)

-------------------------------------------------------------------------
-- RExpr
-------------------------------------------------------------------------

data REInfo e
  = REInfoJudge
      { reNm                :: Nm
      , reScNm              :: Nm
      , reInNmS, reOutNmS   :: Set.Set Nm
      , reJAGam             :: JAGam e
      }
  | REInfoDel
      { reNms               :: [Nm]
      }

reMbJAGam :: REInfo e -> Maybe (JAGam e)
reMbJAGam (REInfoJudge _ _ _ _ g) = Just g
reMbJAGam _                       = Nothing

instance Show (REInfo e) where
  show _ = "REInfo"

instance PP e => PP (REInfo e) where
  pp (REInfoJudge n sn i o g) = "REJdg" >#< pp n >#< pp sn >#< (pp (show i) >#< pp (show o) >-< ppGam g)
  pp (REInfoDel   ns        ) = "REDel" >#< ppCommas ns

type REGam e = Gam Nm (REInfo e)

-------------------------------------------------------------------------
-- Changed attr's w.r.t. previous view
-------------------------------------------------------------------------

data RlChInfo
  = RlChInfo
      { rcJdNm      :: Nm
      , rcAtNm      :: Nm
      }

instance Show RlChInfo where
  show _ = "RlChInfo"

instance PP RlChInfo where
  pp i = "RC" >#< pp (rcJdNm i) >#< pp (rcAtNm i)

type RlChGam = Gam Nm (Gam Nm RlChInfo)

rcGamUnion :: RlChGam -> RlChGam -> RlChGam
rcGamUnion = Map.unionWith Map.union

-------------------------------------------------------------------------
-- View (related to rule)
-------------------------------------------------------------------------

data VwRlInfo e
  = VwRlInfo
      { vwrlNm                              :: Nm
      , vwrlPreGam, vwrlPostGam             :: REGam e
      , vwrlFullPreGam, vwrlFullPostGam     :: REGam e
      , vwrlPreScc                          :: [[Nm]]
      , vwrlMbChGam                         :: Maybe RlChGam
      }

emptyVwRlInfo = VwRlInfo nmNone emptyGam emptyGam emptyGam emptyGam [] Nothing

instance Show (VwRlInfo e) where
  show _ = "VwRlInfo"

instance PP e => PP (VwRlInfo e) where
  pp i = "VWRl" >#< pp (vwrlNm i) >#< (ppGam (vwrlPreGam i)
                                       >-< ppGam (vwrlPostGam i)
                                       >-< ppGam (vwrlFullPreGam i)
                                       >-< ppGam (vwrlFullPostGam i)
                                       >-< pp (show (vwrlPreScc i))
                                       >-< maybe empty (ppGam . Map.map ppGam) (vwrlMbChGam i)
                                      )

type VwRlGam e = Gam Nm (VwRlInfo e)

vwrlDelEmptyJd :: VwRlInfo e -> VwRlInfo e
vwrlDelEmptyJd i
  = i { vwrlFullPreGam = rgDel (vwrlFullPreGam i), vwrlFullPostGam = rgDel (vwrlFullPostGam i) }
  where jgIsEmp = Map.null
        rgDel = Map.filter (not . jgIsEmp . reJAGam)
        
vrwlIsEmpty :: VwRlInfo e -> Bool
vrwlIsEmpty i
  = Map.null (vwrlFullPreGam i) && Map.null (vwrlFullPostGam i)

vwrlScc :: VwRlInfo e -> [[Nm]]
vwrlScc 
  = unNm . Scc.scc . concat . dpd . vwrlFullPreGam
  where dpd g
          = d
          where d = [ (jd n,map nm . Set.toList $ is) : zip (map nm . Set.toList $ os) (repeat [jd n])
                    | (REInfoJudge n _ is os _) <- Map.elems g
                    ]
        nm n = nmSetSel n "n"
        jd n = nmSetSel n "j"
        unNm scc = [ l' | l <- scc, let l' = [ nmInit n | n <- l, nmSel n == "j" ], not (null l') ]

vwrlUndefs :: VwRlInfo e -> Set.Set Nm
vwrlUndefs i
  = (prei `Set.union` posto) `Set.difference` (preo `Set.union` posti)
  where nms g
          = (Set.unions iss,Set.unions oss)
          where (iss,oss) = unzip [ (reInNmS i,reOutNmS i) | i <- Map.elems g ]
        (prei,preo) = nms (vwrlFullPreGam i)
        (posti,posto) = nms (vwrlFullPostGam i)

-------------------------------------------------------------------------
-- Rule
-------------------------------------------------------------------------

data RlInfo e
  = RlInfo
      { rlNm        :: Nm
      , rlPos       :: SPos
      , rlMbOnNm    :: Maybe Nm
      , rlMbAGStr   :: Maybe String
      , rlSeqNr     :: Int
      , rlInclVwS   :: Set.Set Nm
      , rlVwGam     :: VwRlGam e
      }

emptyRlInfo = RlInfo nmUnk emptySPos Nothing Nothing 0 Set.empty emptyGam

instance Show (RlInfo e) where
  show _ = "RlInfo"

instance PP e => PP (RlInfo e) where
  pp i = "RL" >#< pp (rlNm i) >#< ppGam (rlVwGam i)

type RlGam e = Gam Nm (RlInfo e)

rlVwGamLookup :: Nm -> Nm -> RlGam e -> Maybe (RlInfo e,VwRlInfo e)
rlVwGamLookup = dblGamLookup rlVwGam

-------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------

data RsInfo e
  = RsInfo
      { rsNm        :: Nm
      , rsScNm      :: Nm
      , rsInclVwS   :: Set.Set Nm
      , rsDescr     :: String
      , rsRlGam     :: RlGam e
      }
  | RsInfoGroup
      { rsNm        :: Nm
      , rsScNm      :: Nm
      , rsInclVwS   :: Set.Set Nm
      , rsDescr     :: String
      , rsRlNms     :: [(Nm,Nm)]
      }

emptyRsInfo = RsInfo nmUnk nmUnk Set.empty "" emptyGam

rsInfoIsPlain :: RsInfo e -> Bool
rsInfoIsPlain (RsInfo _ _ _ _ _) = True
rsInfoIsPlain _                  = False

rsInfoIsGroup :: RsInfo e -> Bool
rsInfoIsGroup (RsInfoGroup _ _ _ _ _) = True
rsInfoIsGroup _                       = False

instance Show (RsInfo e) where
  show _ = "RsInfo"

instance PP e => PP (RsInfo e) where
  pp (RsInfo      n _ _ _ g) = "RS" >#< pp n >#< ppGam g
  pp (RsInfoGroup n _ _ _ _) = "RSGrp" >#< pp n

type RsGam e = Gam Nm (RsInfo e)

rsRlOrder :: RsInfo e -> [Nm]
rsRlOrder i
  = case i of
      RsInfo      _ _ _ _ g  -> map snd . sort $ [ (rlSeqNr i,rlNm i) | i <- Map.elems g ]
      RsInfoGroup _ _ _ _ ns -> map snd ns

rsInfoMbRlGam :: RsInfo e -> Maybe (RlGam e)
rsInfoMbRlGam (RsInfo _ _ _ _ g) = Just g
rsInfoMbRlGam _                  = Nothing

