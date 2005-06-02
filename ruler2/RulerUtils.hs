module RulerUtils where

import IO
import Data.Maybe
import Data.Char
import Data.List
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import FPath
import Utils
import PPUtils
import UU.Pretty
import qualified UU.DData.Scc as Scc
import UU.Scanner.Position( Pos )

-------------------------------------------------------------------------
-- Symbol position
-------------------------------------------------------------------------

type SPos = (String,Pos)

-------------------------------------------------------------------------
-- Graph for version/view dpd
-------------------------------------------------------------------------

data DpdGr n
  = DpdGr
      { vgDpd   :: [[n]]
      , vgGr    :: Graph
      , vgGrT   :: Graph
      , vgV2N   :: Vertex -> (n, [n])
      , vgK2V   :: n -> Maybe Vertex
      }

vgVsToNs :: DpdGr n -> [Vertex] -> [n]
vgVsToNs g = map (\v -> fst (vgV2N g v))

mkDpdGr :: Ord n => [[n]] -> (Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex)
mkDpdGr
  = graphFromEdges . mkEdges . foldr cmbChain Map.empty . map mkChain
  where mkChain = Map.fromList . fst . foldl (\(c,prev) n -> ((n,prev) : c,[n])) ([],[])
        cmbChain = Map.unionWith (++)
        mkEdges = map (\(n,ns) -> (n,n,ns)) . Map.toList

mkVwDpdGr :: [[Nm]] -> DpdGr Nm
mkVwDpdGr nLL
  = DpdGr nLL g (transposeG g) (\v -> let (n,_,ns) = n2 v in (n,ns)) v2
  where (g,n2,v2) = mkDpdGr nLL

vgTopSort :: DpdGr n -> [n]
vgTopSort g
  = vgVsToNs g . topSort . vgGr $ g

vgVertices :: Ord n => DpdGr n -> Set.Set n
vgVertices g
  = Set.fromList . vgVsToNs g . vertices . vgGr $ g

vgReachable :: Ord n => (DpdGr n -> Graph) -> DpdGr n -> n -> Set.Set n
vgReachable gOf g n
  = case vgK2V g n of
      Just n' -> Set.fromList . vgVsToNs g $ reachable (gOf g) n'
      Nothing -> Set.empty

vgReachableFrom :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableFrom = vgReachable vgGr 

vgReachableTo :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableTo = vgReachable vgGrT 

vgDpdsOn :: DpdGr n -> n -> [n]
vgDpdsOn g n
  = maybe [] (snd . vgV2N g) (vgK2V g n)

-------------------------------------------------------------------------
-- Kind of scheme
-------------------------------------------------------------------------

data ScKind
  = ScJudge | ScRelation
  deriving (Show,Eq,Ord)

instance PP ScKind where
  pp = text . show

-------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------

data Nm' s
  = Nm     { nmStr      :: s }
  | NmSel  { nmNm       :: Nm' s
           , nmMbSel    :: Maybe s
           }
  deriving (Eq,Ord)

type Nm = Nm' String

nmBase' :: Nm -> String
nmBase' (NmSel n _) = nmBase' n
nmBase' (Nm s)      = s

nmBase :: Nm -> Nm
nmBase = Nm . nmBase'

nmSetSuff :: Nm -> String -> Nm
nmSetSuff n s = NmSel (nmBase n) (Just s)

nmSetBase :: Nm -> String -> Nm
nmSetBase n s
  = nmFromL (Just s:nL)
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

nmToL :: Nm -> [String]
nmToL = map (maybe "" id) . nmToMbL

nmFromL :: [Maybe s] -> Nm' s
nmFromL
  = n . reverse
  where n [Just s] = Nm s
        n (s:ss) = NmSel (n ss) s

nmApd :: Nm' s -> Nm' s -> Nm' s
nmApd n1 n2
  = nmFromL (l1 ++ l2)
  where l1 = nmToMbL n1
        l2 = nmToMbL n2

nmStrApd :: Nm -> Nm -> Nm
nmStrApd n1 n2
  = Nm (s1 ++ s2)
  where s1 = show n1
        s2 = show n2

nmShow' :: String -> Nm -> String
nmShow' sep = concat . intersperse sep . nmToL

nmShowAG :: Nm -> String
nmShowAG = nmShow' "_"

instance Show Nm where
  show = nmShow' "."

instance PP Nm where
  pp = ppListSep "" "" "." . nmToL

instance Functor Nm' where
  fmap f (Nm s) = Nm (f s)
  fmap f (NmSel n ms) = NmSel (fmap f n) (fmap f ms)

strVec = "_"

nmVec, nmUnk, nmApp, nmWild :: Nm
nmVec  = Nm strVec
nmWild = nmVec
nmUnk  = Nm "??"
nmApp  = Nm "$"

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm SPos String String [Nm]
  | Err_NoJdSc  SPos String [Nm]
  | Err_Match   SPos String PP_Doc PP_Doc
  deriving Show

ppErr :: SPos -> PP_Doc -> PP_Doc
ppErr (sym,pos) p = "**** ERROR" >#< pp (show pos) >|< ", symbol '" >|< pp sym >|< "':" >-< indent 2 p

instance PP Err where
  pp (Err_UndefNm pos cx knd nmL)
    = ppErr pos ("In" >#< cx >#< knd >|< "(s) are undefined:" >#< ppCommas nmL)
  pp (Err_NoJdSc pos cx nmL)
    = ppErr pos ("In" >#< cx >#< "no (tex) judgement scheme for:" >#< ppCommas nmL)
  pp (Err_Match pos cx given reqd)
    = ppErr pos ("In" >#< cx >#< "could not match"
                 >-< indent 2
                       (    "scheme judgement expr:" >#< reqd
                        >-< "given view expr      :" >#< given
                       )
                )

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
-- Attr
-------------------------------------------------------------------------

data AtDir
  = AtInh | AtSyn | AtIn | AtOut | AtInOut
  deriving (Eq,Ord,Show)

data AtProp
  = AtNode | AtThread | AtUpdown
  deriving (Eq,Ord,Show)

data AtInfo
  = AtInfo
      { atNm    :: Nm
      , atDirs  :: [AtDir]
      , atProps :: [AtProp]
      }

instance PP AtProp where
  pp = text . show

instance Show AtInfo where
  show _ = "AtInfo"

instance PP AtInfo where
  pp i = "AT" >#< pp (atNm i) >#< pp (show (atDirs i)) >#< pp (show (atProps i))

type AtGam = Gam Nm AtInfo

-------------------------------------------------------------------------
-- Judgement formats
-------------------------------------------------------------------------

data JdInfo e
  = JdInfo
      { jdExpr  :: e
      }

type JdGam e = FmKdGam (JdInfo e)

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

emptyVwScInfo = VwScInfo (Nm "") emptyGam emptyGam emptyGam

instance Show (VwScInfo e) where
  show _ = "VwScInfo"

instance PP (VwScInfo e) where
  pp i = "VWSc" >#< pp (vwscNm i) >#< (ppGam (vwscAtGam i) >-< ppGam (vwscFullAtGam i))

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

emptyScInfo = ScInfo (Nm "") Nothing ScJudge emptyGam

instance Show (ScInfo e) where
  show _ = "ScInfo"

instance PP (ScInfo e) where
  pp i = "SC" >#< pp (scNm i) >#< ppGam (scVwGam i)

type ScGam e = Gam Nm (ScInfo e)

scVwGamLookup :: Nm -> Nm -> ScGam e -> Maybe (ScInfo e,VwScInfo e)
scVwGamLookup = dblGamLookup scVwGam

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
fmGamToJaGam fm = Map.fromList . map (\(n,e) -> (n,JAInfo n e Set.empty)) . Map.toList . Map.map (fkGamLookup (panic "fmGamToJaGam") id fm . fmKdGam)

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

instance Show (REInfo e) where
  show _ = "REInfo"

instance PP e => PP (REInfo e) where
  pp (REInfoJudge n sn i o g) = "REJdg" >#< pp n >#< pp sn >#< (pp (show i) >#< pp (show o) >-< ppGam g)
  pp (REInfoDel   ns        ) = "REDel" >#< ppCommas ns

type REGam e = Gam Nm (REInfo e)

-------------------------------------------------------------------------
-- View (related to rule)
-------------------------------------------------------------------------

data VwRlInfo e
  = VwRlInfo
      { vwrlNm                              :: Nm
      , vwrlPreGam, vwrlPostGam             :: REGam e
      , vwrlFullPreGam, vwrlFullPostGam     :: REGam e
      , vwrlPreScc                          :: [[Nm]]
      }

emptyVwRlInfo = VwRlInfo (Nm "") emptyGam emptyGam emptyGam emptyGam []

instance Show (VwRlInfo e) where
  show _ = "VwRlInfo"

instance PP e => PP (VwRlInfo e) where
  pp i = "VWRl" >#< pp (vwrlNm i) >#< (ppGam (vwrlPreGam i)
                                       >-< ppGam (vwrlPostGam i)
                                       >-< ppGam (vwrlFullPreGam i)
                                       >-< ppGam (vwrlFullPostGam i)
                                       >-< pp (show (vwrlPreScc i))
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
      { rsNm    :: Nm
      , rsScNm  :: Nm
      , rsDescr :: String
      , rsRlGam :: RlGam e
      }
  | RsInfoGroup
      { rsNm    :: Nm
      , rsScNm  :: Nm
      , rsDescr :: String
      , rsRlNms :: [(Nm,Nm)]
      }

instance Show (RsInfo e) where
  show _ = "RsInfo"

instance PP e => PP (RsInfo e) where
  pp (RsInfo      n _ _ g) = "RS" >#< pp n >#< ppGam g
  pp (RsInfoGroup n _ _ _) = "RSGrp" >#< pp n

type RsGam e = Gam Nm (RsInfo e)

rsRlOrder :: RsInfo e -> [Nm]
rsRlOrder i
  = case i of
      RsInfo      _ _ _ g  -> map snd . sort $ [ (rlSeqNr i,rlNm i) | i <- Map.elems g ]
      RsInfoGroup _ _ _ ns -> map snd ns

rsInfoMbRlGam :: RsInfo e -> Maybe (RlGam e)
rsInfoMbRlGam (RsInfo _ _ _ g) = Just g
rsInfoMbRlGam _                = Nothing

-------------------------------------------------------------------------
-- Formats
-------------------------------------------------------------------------

data FmKind
  = FmTeX | FmAG | FmAll
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

fmGamFromList :: [(Nm,e)] -> FmGam e
fmGamFromList = Map.unions . map (\(n,e) -> fmSingleton n FmAll e)

fmGamUnion :: FmGam e -> FmGam e -> FmGam e
fmGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = fmKdGam i1 `Map.union` fmKdGam i2})

fmGamUnions :: [FmGam e] -> FmGam e
fmGamUnions = foldr fmGamUnion emptyGam

fmLGamUnion :: FmGam [e] -> FmGam [e] -> FmGam [e]
fmLGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = Map.unionWith (++) (fmKdGam i1) (fmKdGam i2)})

fmGamLookup :: Nm -> FmKind -> FmGam e -> Maybe e
fmGamLookup n k g
  = case Map.lookup n g of
      Just i
        -> fkGamLookup Nothing Just k (fmKdGam i)
      _ -> Nothing

fmGamMap :: (a -> b) -> FmGam a -> FmGam b
fmGamMap f = Map.map (\i -> i {fmKdGam = Map.map f (fmKdGam i)})

-------------------------------------------------------------------------
-- General purpose lookup with a default key
-------------------------------------------------------------------------

gamLookupWithDefault :: Ord k => k -> v -> (e -> v) -> k -> Gam k e -> v
gamLookupWithDefault dfltKey dflt extr key g
  = case Map.lookup key g of
      Just i
        -> extr i
      Nothing | key /= dfltKey
        -> gamLookupWithDefault dfltKey dflt extr dfltKey g
      _ -> dflt

-------------------------------------------------------------------------
-- FmGam for FmKind
-------------------------------------------------------------------------

type FmKdGam e = Gam FmKind e

fkGamLookup :: v -> (e -> v) -> FmKind -> FmKdGam e -> v
fkGamLookup = gamLookupWithDefault FmAll

-------------------------------------------------------------------------
-- FmGam for AtDir
-------------------------------------------------------------------------

type FmDrGam e = Gam AtDir e

fdGamLookup :: v -> (e -> v) -> AtDir -> FmDrGam e -> v
fdGamLookup = gamLookupWithDefault AtInOut

-------------------------------------------------------------------------
-- Rewrite rules
-------------------------------------------------------------------------

type RwGam e = FmGam (FmDrGam [e])

rwGamLookup :: Nm -> FmKind -> AtDir -> RwGam e -> Maybe [e]
rwGamLookup n k d g
  = case fmGamLookup n k g of
      Just g'
        -> fdGamLookup Nothing Just d g'
      _ -> Nothing

rwSingleton :: Nm -> FmKind -> AtDir -> e -> RwGam e
rwSingleton n k d e = Map.singleton n (FmInfo n (Map.singleton k (Map.singleton d [e])))

rwGamUnion :: RwGam e -> RwGam e -> RwGam e
rwGamUnion = Map.unionWith (\i1 i2 -> i1 {fmKdGam = Map.unionWith (Map.unionWith (++)) (fmKdGam i1) (fmKdGam i2)})

-------------------------------------------------------------------------
-- LaTeX
-------------------------------------------------------------------------

mkLaTeXNm :: String -> String
mkLaTeXNm = map (\c -> if isAlphaNum c then c else '-')

nmLaTeX :: Nm -> Nm
nmLaTeX = Nm . mkLaTeXNm . show

strLhs2TeXSafe :: String -> String
strLhs2TeXSafe = concat . map (\c -> if c == '|' then "||" else [c])

nmLhs2TeXSafe :: Nm -> Nm
nmLhs2TeXSafe = fmap strLhs2TeXSafe

mkMBox :: PP a => a -> PP_Doc
mkMBox p = "\\;\\mbox" >|< ppCurly p

mkRuleNm :: String -> String -> PP_Doc
mkRuleNm r v = "\\textsc" >|< ppCurly (mkLaTeXNm r) >|< (if null v then empty else "_" >|< ppCurly v)

mkVerb :: PP_Doc -> PP_Doc
mkVerb p = ppPacked "@" "@" p

switchLaTeXLhs :: PP a => a -> PP_Doc
switchLaTeXLhs p = ppVBar (" " >|< p >|< " ")

mkInLhs2Tex :: PP_Doc -> PP_Doc
mkInLhs2Tex p = ppVBar (p >|< " ")

ensureTeXMath :: PP_Doc -> PP_Doc
ensureTeXMath = mkTexCmdUse "ensuremath"

mkCmdNmDef :: (PP a, PP b) => a -> b -> PP_Doc
mkCmdNmDef = mkTexCmdDef "rulerCmdDef"

mkCmdNmUse :: PP a => a -> PP_Doc
mkCmdNmUse = mkTexCmdUse "rulerCmdUse"

ppNmLaTeX :: Nm -> PP_Doc
ppNmLaTeX n
  = ppSelLaTeX (== strVec) (fromJust base) (map (fmap (\s -> (s,pp s))) sels)
  where (base:sels) = nmToMbL n

ppSelLaTeX :: (PP base,PP sel) => (sel -> Bool) -> base -> [Maybe (sel,PP_Doc)] -> PP_Doc
ppSelLaTeX isVec base sels
  = p
  where sw = (" " >|<) . switchLaTeXLhs
        over n s = if isVec n then sw (text "\\overline{") else sw ("\\stackrel{" >|< sw s >|< "}{")
        p = case (pp base,sels) of
                 (x,[Nothing,Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw (text "}")
                 (x,[Nothing,Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("^{" >|< sw s2 >|< "}}")
                 (x,[Just (_,s1),Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}}")
                 (x,[Just (_,s1),Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}}")
                 (x,(Just (_,s1):Just (_,s2):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}")
                 (x,(Nothing:Just (_,s2):_))
                   -> x >|< sw ("^{" >|< sw s2 >|< "}")
                 (x,(Just (_,s1):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}")
                 (x,_)
                   -> x

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

hdAndTl :: [a] -> (a,[a])
hdAndTl (x:xs) = (x,xs)

maybeHd :: r -> (a -> r) -> [a] -> r
maybeHd n f l = if null l then n else f (head l)

panic m = error ("panic: " ++ m)


