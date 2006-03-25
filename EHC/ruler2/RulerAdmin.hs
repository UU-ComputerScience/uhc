module RulerAdmin
  ( module FmGam
  , module JdShpGam
  
  , AtInfo(..), AtGam
  , emptyAtInfo
  , atGamNode, atMbSynInh
  
  , ScAtBld(..)
  , ScAtBldRename(..)
  , emptyScAtBld
  , sabrGamRename
  
  , ExplInfo(..), ExplGam
  
  , VwScInfo(..), VwScGam
  , emptyVwScInfo
  , vwscAtGam
  
  , ScInfo(..), ScGam
  , emptyScInfo
  , scVwGamLookup, scVwGamNodeAt
  
  , JAInfo(..), JAGam
  , mkJAInfo
  , jaGamToFmGam, fmGamToJaGam
  
  , REInfo(..), REGam
  , reMbJAGam
  , reGamUnionShadow
  , reGamFilterOutDel
  , reGamJAGamDifference, reGamJAGamDifferenceOnExpr
  
  , RlChInfo(..), RlChGam
  , rcGamUnionShadow
  
  , RlJdBld(..)
  
  , VwRlInfo(..), VwRlGam
  , vwrlPreGam, vwrlPostGam
  , emptyVwRlInfo
  , vwrlDelEmptyJd, vrwlIsEmpty, vwrlScc, vwrlUndefs
  
  , RlInfo(..), RlGam
  , emptyRlInfo
  , rlVwGamLookup
  
  , RsInfo(..), RsGam
  , emptyRsInfo
  , rsInfoIsPlain, rsInfoIsGroup
  , rsRlVwGamLookup
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
import JdShpGam
import ExprUtils

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
  = do let aNdGm = gamFilter (\ai -> AtNode `elem` atProps ai) g
       case gamAssocsShadow aNdGm of
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
-- Attr build description based on scheme
-------------------------------------------------------------------------

data ScAtBldRename
  = ScAtBldRename
      { sabrNm      :: Nm
      , sabrNmOther :: Nm
      }
  | ScAtBldEqualTo
      { sabrNm      :: Nm
      , sabrNmOther :: Nm
      }

instance Show ScAtBldRename where
  show _ = "ScAtBldRename"

instance PP ScAtBldRename where
  pp (ScAtBldRename  n o) = "SABR" >#< n >#< ":=" >#< o
  pp (ScAtBldEqualTo n o) = "SABR" >#< n >#<  "=" >#< o

sabrGamRename :: [ScAtBldRename] -> Gam Nm v -> (Gam Nm v,[Nm])
sabrGamRename rnL g
  = foldl (\(g,nL) r
            -> case r of
                 ScAtBldRename n o
                   | isJust mbO
                     -> (gamInsert n (fromJust mbO) $ gamDelete o $ g,nL)
                   | otherwise
                     -> (g,o:nL)
                   where mbO = gamLookup o g
                 _   -> (g,nL)
          )
          (g,[]) rnL

-------------------------------------------------------------------------
-- Attr build description for scheme
-------------------------------------------------------------------------

data ScAtBld
  = ScAtBldDirect
      { sabAtGam    :: AtGam
      }
  | ScAtBldScheme
      { sabNm       :: Nm
      , sabPos      :: SPos
      , sabRenameL  :: [ScAtBldRename]
      }

emptyScAtBld :: ScAtBld
emptyScAtBld = ScAtBldScheme nmUnk emptySPos []

instance Show ScAtBld where
  show _ = "ScAtBld"

instance PP ScAtBld where
  pp   (ScAtBldDirect i    ) = "SAB-D" >#< pp i
  pp i@(ScAtBldScheme _ _ _) = "SAB-S" >#< sabNm i >#< ppCommaList (sabRenameL i)

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

type AtEqlToMp = Map.Map Nm (Set.Set Nm)

data VwScInfo e
  = VwScInfo
      { vwscNm              :: Nm
      , vwscJdShpGam        :: JdShpGam e
      , vwscAtBldL          :: [ScAtBld]
      , vwscFullAtBldL      :: [ScAtBld]
      , vwscFullAtBldGam    :: AtGam
      , vwscFullAtGam       :: AtGam
      , vwscExplGam         :: ExplGam e
      }

emptyVwScInfo :: VwScInfo e
emptyVwScInfo = VwScInfo nmNone emptyGam [] [] emptyGam emptyGam emptyGam

vwscAtGam :: VwScInfo e -> AtGam
vwscAtGam i = gamUnions [ g | (ScAtBldDirect g) <- vwscAtBldL i ]

instance Show (VwScInfo e) where
  show _ = "VwScInfo"

instance PP e => PP (VwScInfo e) where
  pp i = "VWSc" >#< pp (vwscNm i) >#< (ppCommaList (vwscAtBldL i) >-< ppGam (vwscFullAtGam i) >-< ppGam (vwscJdShpGam i) >-< ppGam (vwscExplGam i))

type VwScGam e = Gam Nm (VwScInfo e)

-------------------------------------------------------------------------
-- Scheme
-------------------------------------------------------------------------

data ScInfo e
  = ScInfo
      { scPos       :: SPos
      , scNm        :: Nm
      , scMbAGStr   :: Maybe String
      , scKind      :: ScKind
      , scVwGam     :: VwScGam e
      }

emptyScInfo :: ScInfo e
emptyScInfo = ScInfo emptySPos nmNone Nothing ScJudge emptyGam

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

mkJAInfo :: Nm -> Expr -> JAInfo Expr
mkJAInfo n e = JAInfo n e (exprNmS e)

instance Show (JAInfo e) where
  show _ = "JAInfo"

instance PP e => PP (JAInfo e) where
  pp i@(JAInfo _ _ _) = "JA" >#< (pp (jaNm i) >|< ":" >#< pp (jaExpr i) >-< pp (show (jaNmS i)))
  pp i@(JAInfoDel _)  = "JADel" >#< jaNm i

type JAGam e = Gam Nm (JAInfo e)

jaGamToFmGam :: (e -> e) -> JAGam e -> FmGam e
jaGamToFmGam f = fmGamFromList . map (\(n,i) -> (n,f (jaExpr i))) . gamAssocsShadow

fmGamToJaGam :: FmKind -> FmGam Expr -> JAGam Expr
fmGamToJaGam fm = gamFromAssocs . map (\(n,e) -> (n,mkJAInfo n e)) . gamAssocsShadow . gamMap (fkGamLookup (panic "fmGamToJaGam") id [fm] . fmKdGam)

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

reGamUnionShadow :: REGam e -> REGam e -> REGam e
reGamUnionShadow g gamPrev
  = gamFoldWithKey
      (\n i gamPrev
        -> case i of
             REInfoJudge _ sn _ _ jg
               -> gamInsertShadow n (iPrev {reJAGam = jg `gamUnionShadow` jaGamPrev}) gamPrev
               where (jaGamPrev,iPrev)
                       = case gamLookup n gamPrev of
                           Just iPrev@(REInfoJudge _ snPrev _ _ _) | snPrev == sn
                             -> (reJAGam iPrev,iPrev)
                           _ -> (emptyGam,i)
             REInfoDel ns
               -> gamInsert n i $ foldr gamDelete gamPrev ns
      )
      gamPrev g

reGamFilterOutDel :: REGam e -> REGam e
reGamFilterOutDel = gamFilter (\i -> case i of {REInfoDel _ -> False ; _ -> True})

reGamFilterOutEmpty :: REGam e -> REGam e
reGamFilterOutEmpty = gamFilter (not . gamIsEmpty . reJAGam)

{-
reGamJAGamDifference :: REGam e -> REGam e -> REGam e
reGamJAGamDifference g gamDiff
  = gamMap (\i -> gamLookupMaybe i (\j -> i {reJAGam = reJAGam i `gamDifference` reJAGam j}) (reNm i) gamDiff) g
-}
reGamJAGamDifference' :: (JAGam e -> JAGam e -> JAGam e) -> REGam e -> REGam e -> REGam e
reGamJAGamDifference' jgFilterOut g gamDiff
  = reGamFilterOutEmpty
  $ gamMap (\i -> gamLookupMaybe i
                                 (\j -> i {reJAGam = reJAGam i `jgFilterOut` reJAGam j})
                                 (reNm i) gamDiff
           )
           g

reGamJAGamDifferenceOnExpr :: Eq e => REGam e -> REGam e -> REGam e
reGamJAGamDifferenceOnExpr
  = reGamJAGamDifference' jgFilterOut
  where jgFilterOut g gOut = gamFilterWithKey (\n i -> gamLookupMaybe True (\j -> jaExpr i /= jaExpr j) n gOut) g

reGamJAGamDifference :: REGam e -> REGam e -> REGam e
reGamJAGamDifference
  = reGamJAGamDifference' gamDifference

infixr 5 `reGamUnionShadow`

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

rcGamUnionShadow :: RlChGam -> RlChGam -> RlChGam
rcGamUnionShadow = gamUnionWith gamUnionShadow

-------------------------------------------------------------------------
-- Attr build description for rule
-------------------------------------------------------------------------

data RlJdBld e
  = RlJdBldDirect
      { rjbPreGam   :: REGam e
      , rjbPostGam  :: REGam e
      }
  | RlJdBldFromRuleset
      { rjbPos      :: SPos
      , rjbRsNm     :: Nm
      , rjbRlNm     :: Nm
      }

instance Show (RlJdBld e) where
  show _ = "RlJdBld"

instance PP e => PP (RlJdBld e) where
  pp   (RlJdBldDirect      g1 g2) = "RJB-D"  >#< (ppGam g1 >-< ppGam g2)
  pp i@(RlJdBldFromRuleset _ _ _) = "RJB-RS" >#< rjbRsNm i >#< rjbRlNm i

-------------------------------------------------------------------------
-- View (related to rule)
-------------------------------------------------------------------------

data VwRlInfo e
  = VwRlInfo
      { vwrlNm                              :: Nm
      , vwrlPos                             :: SPos
      -- , vwrlPreGam, vwrlPostGam             :: REGam e
      , vwrlJdBldL                          :: [RlJdBld e]
      , vwrlJdBldDfltL                      :: [RlJdBld e]
      , vwrlFullPreGam, vwrlFullPostGam     :: REGam e
      , vwrlPreScc                          :: [[Nm]]
      , vwrlMbChGam                         :: Maybe RlChGam
      }

emptyVwRlInfo :: VwRlInfo e
emptyVwRlInfo = VwRlInfo nmNone emptySPos [] [] emptyGam emptyGam [] Nothing

vwrlPreGam :: VwRlInfo e -> REGam e
vwrlPreGam v = gamUnions [ g | (RlJdBldDirect g _) <- vwrlJdBldL v ]

vwrlPostGam :: VwRlInfo e -> REGam e
vwrlPostGam v = gamUnions [ g | (RlJdBldDirect _ g) <- vwrlJdBldL v ]

instance Show (VwRlInfo e) where
  show _ = "VwRlInfo"

instance PP e => PP (VwRlInfo e) where
  pp i = "VWRl" >#< pp (vwrlNm i) >#< (ppCommaList (vwrlJdBldL i)
                                       -- >-< ppCommaList (vwrlJdBldDfltL i)
                                       >-< ppGam (vwrlFullPreGam i)
                                       >-< ppGam (vwrlFullPostGam i)
                                       >-< pp (show (vwrlPreScc i))
                                       >-< maybe empty (ppGam . gamMap ppGam) (vwrlMbChGam i)
                                      )

type VwRlGam e = Gam Nm (VwRlInfo e)

vwrlDelEmptyJd :: VwRlInfo e -> VwRlInfo e
vwrlDelEmptyJd i
  = i { vwrlFullPreGam = reGamFilterOutEmpty (vwrlFullPreGam i), vwrlFullPostGam = reGamFilterOutEmpty (vwrlFullPostGam i) }
        
vrwlIsEmpty :: VwRlInfo e -> Bool
vrwlIsEmpty i
  = gamIsEmpty (vwrlFullPreGam i) && gamIsEmpty (vwrlFullPostGam i)

vwrlScc :: VwRlInfo e -> [[Nm]]
vwrlScc 
  = unNm . Scc.scc . concat . dpd . vwrlFullPreGam
  where dpd g
          = d
          where d = [ (jd n,map nm . Set.toList $ is) : zip (map nm . Set.toList $ os) (repeat [jd n])
                    | (REInfoJudge n _ is os _) <- gamElemsShadow g
                    ]
        nm n = nmSetSel n "n"
        jd n = nmSetSel n "j"
        unNm scc = [ l' | l <- scc, let l' = [ nmInit n | n <- l, nmSel n == "j" ], not (null l') ]

vwrlUndefs :: VwRlInfo e -> Set.Set Nm
vwrlUndefs i
  = (prei `Set.union` posto) `Set.difference` (preo `Set.union` posti)
  where nms g
          = (Set.unions iss,Set.unions oss)
          where (iss,oss) = unzip [ (reInNmS i,reOutNmS i) | i <- gamElemsShadow g ]
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
      , rsPos       :: SPos
      , rsScNm      :: Nm
      , rsInclVwS   :: Set.Set Nm
      , rsDescr     :: String
      , rsRlGam     :: RlGam e
      }
  | RsInfoGroup
      { rsNm        :: Nm
      , rsPos       :: SPos
      , rsScNm      :: Nm
      , rsInclVwS   :: Set.Set Nm
      , rsDescr     :: String
      , rsRlNms     :: [(Nm,Nm)]
      }

emptyRsInfo = RsInfo nmUnk emptySPos nmUnk Set.empty "" emptyGam

rsInfoIsPlain :: RsInfo e -> Bool
rsInfoIsPlain (RsInfo _ _ _ _ _ _) = True
rsInfoIsPlain _                    = False

rsInfoIsGroup :: RsInfo e -> Bool
rsInfoIsGroup (RsInfoGroup _ _ _ _ _ _) = True
rsInfoIsGroup _                         = False

instance Show (RsInfo e) where
  show _ = "RsInfo"

instance PP e => PP (RsInfo e) where
  pp (RsInfo      n _ _ _ _ g) = "RS" >#< pp n >#< ppGam g
  pp (RsInfoGroup n _ _ _ _ _) = "RSGrp" >#< pp n

type RsGam e = Gam Nm (RsInfo e)

rsRlOrder :: RsInfo e -> [Nm]
rsRlOrder i
  = case i of
      RsInfo      _ _ _ _ _ g  -> map snd . sort $ [ (rlSeqNr i,rlNm i) | i <- gamElemsShadow g ]
      RsInfoGroup _ _ _ _ _ ns -> map snd ns

rsInfoMbRlGam :: RsInfo e -> Maybe (RlGam e)
rsInfoMbRlGam (RsInfo _ _ _ _ _ g) = Just g
rsInfoMbRlGam _                    = Nothing

rsRlVwGamLookup :: Nm -> Nm -> Nm -> RsGam e -> Maybe (RsInfo e,RlInfo e,VwRlInfo e)
rsRlVwGamLookup = tripleGamLookup rsRlGam rlVwGam

