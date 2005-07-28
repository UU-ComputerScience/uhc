-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerAdmin where

import IO
import Data.Maybe
import Data.Char
import Data.List
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import FPath
import Utils
import Nm
import PPUtils
import UU.Pretty
import qualified UU.DData.Scc as Scc
import UU.Scanner.Position( Pos )
import RulerUtils

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

-------------------------------------------------------------------------
-- WrKind
-------------------------------------------------------------------------

data WrKindInfo
  = WrKindInfo
      { wkBegCmd   :: Nm
      , wkEndCmd   :: Nm
      }

instance Show WrKindInfo where
  show _ = "WrKindInfo"

instance PP WrKindInfo where
  pp i = pp "WK"

type WrKindGam = Gam WrKind WrKindInfo

wrKindGam :: WrKindGam
wrKindGam
  = Map.fromList
      [ (WrIsChanged,WrKindInfo nmCmdBegChng nmCmdEndChng)
      , (WrIsSame   ,WrKindInfo nmCmdBegSame nmCmdEndSame)
      ]

-------------------------------------------------------------------------
-- Attr
-------------------------------------------------------------------------

data AtDir
  = AtInh | AtSyn | AtIn | AtOut | AtInOut
  deriving (Eq,Ord,Show)

data AtProp
  = AtNode | AtThread | AtUpdown | AtRetain
  deriving (Eq,Ord,Show)

data AtInfo
  = AtInfo
      { atNm    :: Nm
      , atDirs  :: [AtDir]
      , atProps :: [AtProp]
      , atTy    :: Nm
      }

instance PP AtProp where
  pp = text . show

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

-------------------------------------------------------------------------
-- View (related to scheme)
-------------------------------------------------------------------------

data VwScInfo e
  = VwScInfo
      { vwscNm          :: Nm
      , vwscJdGam       :: JdGam e
      , vwscAtGam       :: AtGam
      , vwscFullAtGam   :: AtGam
      }

emptyVwScInfo = VwScInfo nmNone emptyGam emptyGam emptyGam

instance Show (VwScInfo e) where
  show _ = "VwScInfo"

instance PP e => PP (VwScInfo e) where
  pp i = "VWSc" >#< pp (vwscNm i) >#< (ppGam (vwscAtGam i) >-< ppGam (vwscFullAtGam i) >-< ppGam (vwscJdGam i))

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

-------------------------------------------------------------------------
-- Formats
-------------------------------------------------------------------------

data FmKind
  = FmTeX | FmAG | FmSpec | FmAll | FmCnstr
  deriving (Show,Eq,Ord)

instance PP FmKind where
  pp = pp . show

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
fmSingleton n k e = Map.singleton n (FmInfo n (Map.singleton k e))

fmNull :: FmGam e -> Bool
fmNull = all (Map.null . fmKdGam) . Map.elems

fmGamFromList' :: FmKind -> [(Nm,e)] -> FmGam e
fmGamFromList' fk = Map.unions . map (\(n,e) -> fmSingleton n fk e)

fmGamFromList :: [(Nm,e)] -> FmGam e
fmGamFromList = fmGamFromList' FmAll

fmGamUnion :: FmGam e -> FmGam e -> FmGam e
fmGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = fmKdGam i1 `Map.union` fmKdGam i2})

fmGamUnions :: [FmGam e] -> FmGam e
fmGamUnions = foldr fmGamUnion emptyGam

{-
fmLGamUnion :: FmGam [e] -> FmGam [e] -> FmGam [e]
fmLGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = Map.unionWith (++) (fmKdGam i1) (fmKdGam i2)})
-}

fmGamLookup :: Nm -> FmKind -> FmGam e -> Maybe e
fmGamLookup n k g
  = case Map.lookup n g of
      Just i
        -> fkGamLookup Nothing Just [k] (fmKdGam i)
      _ -> Nothing

fmGamMap :: (Nm -> a -> b) -> FmGam a -> FmGam b
fmGamMap f = Map.mapWithKey (\n i -> i {fmKdGam = Map.map (\e -> f n e) (fmKdGam i)})

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

-------------------------------------------------------------------------
-- FmGam for FmKind
-------------------------------------------------------------------------

type FmKdGam e = Gam FmKind e

fkGamLookup :: v -> (e -> v) -> [FmKind] -> FmKdGam e -> v
fkGamLookup = gamLookupWithDefault FmAll

-------------------------------------------------------------------------
-- FmGam for AtDir
-------------------------------------------------------------------------

type FmDrGam e = Gam AtDir e

fdGamLookup :: v -> (e -> v) -> [AtDir] -> FmDrGam e -> v
fdGamLookup = gamLookupWithDefault AtInOut

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
rwSingleton n k d e = Map.singleton n (FmInfo n (Map.singleton k (Map.singleton d [e])))

rwGamUnion :: RwGam e -> RwGam e -> RwGam e
rwGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = Map.unionWith (Map.unionWith (++)) (fmKdGam i1) (fmKdGam i2)})

-------------------------------------------------------------------------
-- Child order
-------------------------------------------------------------------------

type ChOrdGam = Gam Nm Int

-------------------------------------------------------------------------
-- Copy rule order, ref to previous node
-------------------------------------------------------------------------

type CrOrdGam = Gam Nm Nm

-------------------------------------------------------------------------
-- Non local attr's defined, threaded?
-------------------------------------------------------------------------

type AtDefdGam = Gam Nm Bool

