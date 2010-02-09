%%[1 hs module (Admin)
%%]

%%[1 hs export (module FmGam, module JdShpGam)
%%]

%%[1 hs export (DtFldInfo(..), DtFldGam, emptyDtFldInfo, DtAltInfo(..), DtAltGam, emptyDtAltInfo, DtVwInfo(..), DtVwGam, emptyDtVwInfo)
%%]

%%[1 hs export (DtInfo(..), DtGam, emptyDtInfo, dtGamInv, dtVwGamLookup, DtAltInvInfo(..), DtAltInvGam, emptyDtAltInvInfo, DtVwInvInfo(..), DtVwInvGam)
%%]

%%[1 hs export (DtInvInfo(..), DtInvGam, dtVwRlInvGamLookup, dtInvGamRlMbOn, dtInvGamRlVwS, AtInfo(..), AtGam, emptyAtInfo, atGamNode)
%%]

%%[1 hs export (atMbSynInh, atHasDir, atFilterProps, atHasProp, atHasProps, atIsExternUndef, AtDefUse(..), atDefUse, BldRename(..))
%%]

%%[1 hs export (ScAtBld(..), emptyScAtBld, sabFilterScheme, ScAtBldRename(..), sabrGamRename, ExplInfo(..), ExplGam)
%%]

%%[1 hs export (VwScInfo(..), VwScGam, emptyVwScInfo, vwscAtGam, ScInfo(..), ScGam, emptyScInfo, scVwGamLookup, scVwGamNodeAt)
%%]

%%[1 hs export (JAInfo(..), JAGam, mkJAInfo, jaGamToFmGam, fmGamToJaGam)
%%]

%%[1 hs export (REInfo(..), REGam, reMbJAGam, reMbJd, reUpdJAGam, reGamUnionShadow, reGamFilterOutDel, reGamJAGamDifference, reGamJAGamDifferenceOnExpr)
%%]

%%[1 hs export (RlChInfo(..), RlChGam, rcGamUnionShadow, RlJdBld(..), VwRlInfo(..), VwRlGam, emptyVwRlInfo, mkVwRlInfo)
%%]

%%[1 hs export (vwrlPreGam, vwrlPostGam, vwrlExtNmS, vwrlDelEmptyJd, vrwlIsEmpty, vwrlScc, vwrlUndefs)
%%]

%%[1 hs export (RlInfo(..), RlGam, emptyRlInfo, rlVwGamLookup, rlVwRlOnL)
%%]

%%[1 hs export (RsInfo(..), RsGam, emptyRsInfo, rsInfoIsPlain, rsInfoIsGroup, rsRlVwGamLookup, rsRlOrder, rsInfoMbRlGam)
%%]

%%[1 hs import (Data.Maybe, Data.Char, Data.List, qualified Data.Set as Set, qualified Data.Map as Map)
%%]

%%[1 hs import (EH.Util.Utils, EH.Util.Pretty, Common, Gam, FmGam, JdShpGam)
%%]

%%[1 hs import (Expr.Utils, Ty.Utils)
%%]


%%[1 hs

-------------------------------------------------------------------------
-- Data/AST: field
-------------------------------------------------------------------------

data DtFldInfo
  = DtFldInfo
      { dfNm	:: Nm
      , dfTy	:: Ty
      , dfSeqNr	:: Int
      }

emptyDtFldInfo :: DtFldInfo
emptyDtFldInfo = DtFldInfo nmUnk tyUnk 0

instance Show DtFldInfo where
  show _ = "DtFldInfo"

instance PP DtFldInfo where
  pp i = "DtFld" >#< pp (dfNm i) >|< "/" >|< pp (dfSeqNr i) >#< "::" >#< pp (dfTy i)

type DtFldGam = Gam Nm DtFldInfo

-------------------------------------------------------------------------
-- Data/AST: alternative
-------------------------------------------------------------------------

data DtAltInfo
  = DtAltInfo
      { daNm		:: Nm
      , daRlNm		:: Nm
      , daMbOnNm    :: Maybe Nm
      , daFldGam	:: DtFldGam
      }

emptyDtAltInfo :: DtAltInfo
emptyDtAltInfo = DtAltInfo nmUnk nmUnk Nothing emptyGam

instance Show DtAltInfo where
  show _ = "DtAltInfo"

instance PP DtAltInfo where
  pp i = "DtAlt" >#< pp (daRlNm i) >#< ppGam (daFldGam i)

type DtAltGam = Gam Nm DtAltInfo

-------------------------------------------------------------------------
-- Data/AST: view
-------------------------------------------------------------------------

data DtVwInfo
  = DtVwInfo
      { vdNm			:: Nm
      , vdAltGam		:: DtAltGam
      , vdFullAltGam	:: DtAltGam
      }

emptyDtVwInfo :: DtVwInfo
emptyDtVwInfo = DtVwInfo nmUnk emptyGam emptyGam

instance Show DtVwInfo where
  show _ = "DtVwInfo"

instance PP DtVwInfo where
  pp i = "VwDt" >#< pp (vdNm i) >#< (ppGam (vdAltGam i) >-< ppGam (vdFullAltGam i))

type DtVwGam = Gam Nm DtVwInfo

-------------------------------------------------------------------------
-- Data/AST: data
-------------------------------------------------------------------------

data DtInfo
  = DtInfo
      { dtNm		:: Nm
      , dtScNmL		:: [Nm]
      , dtVwGam		:: DtVwGam
      }

emptyDtInfo :: DtInfo
emptyDtInfo = DtInfo nmUnk [] emptyGam

instance Show DtInfo where
  show _ = "DtInfo"

instance PP DtInfo where
  pp i = "Dt" >#< ppCommas' (dtScNmL i) >#< ppGam (dtVwGam i)

type DtGam = Gam Nm DtInfo

dtGamInv :: DtGam -> DtInvGam
dtGamInv dtGam
  = gamFoldWithKey
      (\dn di dg
        -> let vg
                 = gamFoldWithKey
                     (\vn vi vg
                       -> let ag
                                = gamFoldWithKey
                                    (\an ai ag
                                      -> let cg = gamMap dfSeqNr $ gamFilter (\i -> tyTopNm (dfTy i) `gamMember` dtGam) $ daFldGam ai
                                         in  gamInsertShadow (daRlNm ai) (DtAltInvInfo (daRlNm ai) an (daMbOnNm ai) cg) ag
                                    )
                                    emptyGam
                                    (vdFullAltGam vi)
                          in  gamInsertShadow vn (DtVwInvInfo vn ag) vg
                     )
                     emptyGam
                     (dtVwGam di)
           in  foldr (\n -> gamInsertShadow n (DtInvInfo n dn vg)) dg (dtScNmL di)
      )
      emptyGam
      dtGam

dtVwGamLookup :: Nm -> Nm -> DtGam -> Maybe (DtInfo,DtVwInfo)
dtVwGamLookup = dblGamLookup dtVwGam

-------------------------------------------------------------------------
-- Data/AST: alternative (inverse)
-------------------------------------------------------------------------

data DtAltInvInfo
  = DtAltInvInfo
      { daiNm		:: Nm
      , daiAGNm		:: Nm
      , daiMbOnNm   :: Maybe Nm
      , daiChOrdGam :: ChOrdGam
      }

emptyDtAltInvInfo :: DtAltInvInfo
emptyDtAltInvInfo = DtAltInvInfo nmUnk nmUnk Nothing emptyGam

instance Show DtAltInvInfo where
  show _ = "DtAltInvInfo"

instance PP DtAltInvInfo where
  pp i = "DtAltInv" >#< pp (daiAGNm i) >#< pp (daiMbOnNm i) >#< ppGam (daiChOrdGam i)

type DtAltInvGam = Gam Nm DtAltInvInfo

-------------------------------------------------------------------------
-- Data/AST: view (inverse)
-------------------------------------------------------------------------

data DtVwInvInfo
  = DtVwInvInfo
      { vdiNm			:: Nm
      , vdiFullAltGam	:: DtAltInvGam
      }

emptyDtVwInvInfo :: DtVwInvInfo
emptyDtVwInvInfo = DtVwInvInfo nmUnk emptyGam

instance Show DtVwInvInfo where
  show _ = "DtVwInvInfo"

instance PP DtVwInvInfo where
  pp i = "VwDtInv" >#< ppGam (vdiFullAltGam i)

type DtVwInvGam = Gam Nm DtVwInvInfo

-------------------------------------------------------------------------
-- Data/AST: data (inverse)
-------------------------------------------------------------------------

data DtInvInfo
  = DtInvInfo
      { dtiNm		:: Nm
      , dtiAGNm		:: Nm
      , dtiVwGam	:: DtVwInvGam
      }

emptyDtInvInfo :: DtInvInfo
emptyDtInvInfo = DtInvInfo nmUnk nmUnk emptyGam

instance Show DtInvInfo where
  show _ = "DtInvInfo"

instance PP DtInvInfo where
  pp i = "DtInv" >#< pp (dtiAGNm i) >#< ppGam (dtiVwGam i)

type DtInvGam = Gam Nm DtInvInfo

dtVwRlInvGamLookup :: Nm -> Nm -> Nm -> DtInvGam -> Maybe (DtInvInfo,DtVwInvInfo,DtAltInvInfo)
dtVwRlInvGamLookup = tripleGamLookup dtiVwGam vdiFullAltGam

dtInvGamRlMbOn :: DtInvGam -> RlInfo e -> Nm -> Nm -> Maybe Nm
dtInvGamRlMbOn dg rlInfo scNm vwNm
  = case rlMbOnNm rlInfo of
      Just n  -> Just n
      Nothing -> case dtVwRlInvGamLookup scNm vwNm (rlNm rlInfo) dg of
                   Just (_,_,i) -> daiMbOnNm i
                   Nothing      -> Nothing

dtInvGamRlVwS :: Nm -> Nm -> DtInvGam -> Maybe (Set.Set Nm)
dtInvGamRlVwS scNm rlNm g
  = case gamLookup scNm g of
      Just i
        -> Just $ Set.fromList $ [ v | (v,vi) <- gamAssocs $ dtiVwGam i, isJust $ gamLookup rlNm $ vdiFullAltGam vi ]
      _ -> Nothing

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
  = do let aNdGm = gamFilter (\ai -> AtNode `atHasProp` ai) g
       case gamAssocsShadow aNdGm of
         ((na,ai):_) -> return na
         _           -> Nothing

atMbSynInh :: AtInfo -> Maybe Nm
atMbSynInh i
  = if      AtThread `atHasProp` i then Just (nmInit n)
    else if AtUpdown `atHasProp` i then Just (nmInit n)
    else if AtInh    `atHasDir`  i
         && AtSyn    `atHasDir`  i then Just n
                                      else Nothing
  where n = atNm i

atHasProp :: AtProp -> AtInfo -> Bool
atHasProp p i = p `elem` atProps i

atFilterProps :: [AtProp] -> AtInfo -> [AtProp]
atFilterProps ps i = ps `intersect` atProps i

atHasProps :: [AtProp] -> AtInfo -> Bool
atHasProps ps i = not $ null $ ps `atFilterProps` i

atHasDir :: AtProp -> AtInfo -> Bool
atHasDir p i = p `elem` atDirs i

atIsExternUndef :: Bool -> AtInfo -> Bool
atIsExternUndef isPre i
  = isExtern && defUse == ADDef
  where isExtern = AtExtern `atHasProp` i
        defUse   = atDefUse isPre i

-------------------------------------------------------------------------
-- Attr def/use
-------------------------------------------------------------------------

data AtDefUse
  = ADDef | ADUse | ADNode | ADNoDir
  deriving (Show,Eq,Ord)

atDefUse :: Bool -> AtInfo -> AtDefUse
atDefUse isPre atInfo
  = if AtNode `atHasProp` atInfo
    then ADNode
    else if isSyn
    then if isPre then ADUse else ADDef
    else if isInh
    then if isPre then ADDef else ADUse
    else ADNoDir
  where isSyn = AtSyn `atHasDir` atInfo
        isInh = AtInh `atHasDir` atInfo

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
-- Rename of .. during build
-------------------------------------------------------------------------

data BldRename
  = BldRename
      { brNmFrom 	:: Nm
      , brNmTo 		:: Nm
      }

instance Show BldRename where
  show _ = "BldRename"

instance PP BldRename where
  pp (BldRename f t) = "BR" >#< f >#< "->" >#< t


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

sabFilterScheme :: [ScAtBld] -> [ScAtBld]
sabFilterScheme l = [ b | b@(ScAtBldScheme _ _ _) <- l ]

instance Show ScAtBld where
  show _ = "ScAtBld"

instance PP ScAtBld where
  pp   (ScAtBldDirect i    ) = "SAB-D" >#< pp i
  pp i@(ScAtBldScheme _ _ _) = "SAB-S" >#< sabNm i >#< ppBracketsCommas (sabRenameL i)

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
  pp i = "VWSc" >#< pp (vwscNm i) >#< (ppBracketsCommas (vwscAtBldL i) >-< ppGam (vwscFullAtGam i) >-< ppGam (vwscJdShpGam i) >-< ppGam (vwscExplGam i))

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
      , reIsSmall           :: Bool
      }
  | REInfoDel
      { reNms               :: [Nm]
      }

reMbJd :: REInfo e -> Maybe (REInfo e)
reMbJd (REInfoDel _) = Nothing
reMbJd i             = Just i

reNm' :: REInfo e -> Nm
reNm' = maybe nmUnk reNm . reMbJd

reMbJAGam :: REInfo e -> Maybe (JAGam e)
reMbJAGam = fmap reJAGam . reMbJd

reUpdJAGam :: JAGam e -> REInfo e -> REInfo e
reUpdJAGam g i = maybe i (\i -> i {reJAGam = g}) $ reMbJd i

instance Show (REInfo e) where
  show _ = "REInfo"

instance PP e => PP (REInfo e) where
  pp (REInfoJudge n sn i o g _) = "REJdg" >#< pp n >#< pp sn >#< (pp (show i) >#< pp (show o) >-< ppGam g)
  pp (REInfoDel   ns          ) = "REDel" >#< ppCommas ns

type REGam e = Gam Nm (REInfo e)

reGamUnionShadow :: REGam e -> REGam e -> REGam e
reGamUnionShadow g gamPrev
  = gamFoldWithKey
      (\n i gamPrev
        -> case i of
             REInfoJudge _ sn _ _ jg _
               -> gamInsertShadow n (iPrev {reJAGam = jg `gamUnionShadow` jaGamPrev}) gamPrev
               where (jaGamPrev,iPrev)
                       = case gamLookup n gamPrev of
                           Just iPrev@(REInfoJudge _ snPrev _ _ _ _) | snPrev == sn
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
                                 (reNm' i) gamDiff
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
      { rjbExtNmS   	:: Set.Set Nm
      , rjbPreGam   	:: REGam e
      , rjbPostGam  	:: REGam e
      }
  | RlJdBldFromRuleset
      { rjbPos      	:: SPos
      , rjbRsNm     	:: Nm
      , rjbRlNm     	:: Nm
      , rjbScRenameL	:: [BldRename]
      }

instance Show (RlJdBld e) where
  show _ = "RlJdBld"

instance PP e => PP (RlJdBld e) where
  pp   (RlJdBldDirect      _ g1 g2) = "RJB-D"  >#< (ppGam g1 >-< ppGam g2)
  pp i@(RlJdBldFromRuleset _ _ _ _) = "RJB-RS" >#< rjbRsNm i >#< rjbRlNm i >#< ppBracketsCommas (rjbScRenameL i)

-------------------------------------------------------------------------
-- View (related to rule)
-------------------------------------------------------------------------

data VwRlInfo e
  = VwRlInfo
      { vwrlNm                              				:: Nm
      , vwrlPos                             				:: SPos
      , vwrlJdBldL                          				:: [RlJdBld e]
      , vwrlJdBldOnAL, vwrlJdBldOnBL              			:: [RlJdBld e]	-- for debug
      , vwrlFullNoDfltPreGam, vwrlFullNoDfltPostGam     	:: REGam e
      , vwrlFullPreGam, vwrlFullPostGam     				:: REGam e
      , vwrlPreScc                          				:: [[Nm]]
      , vwrlMbChGam                         				:: Maybe RlChGam
      , vwrlAuxGroups                                       :: [[Nm]]
      }

emptyVwRlInfo :: VwRlInfo e
emptyVwRlInfo = VwRlInfo nmNone emptySPos [] [] [] emptyGam emptyGam emptyGam emptyGam [] Nothing []

mkVwRlInfo :: Nm -> SPos -> [RlJdBld e] -> [[Nm]] -> VwRlInfo e
mkVwRlInfo n p b g = emptyVwRlInfo { vwrlNm = n, vwrlPos = p, vwrlJdBldL = b, vwrlAuxGroups = g }

vwrlPreGam :: VwRlInfo e -> REGam e
vwrlPreGam v = gamUnions [ g | (RlJdBldDirect _ g _) <- vwrlJdBldL v ]

vwrlPostGam :: VwRlInfo e -> REGam e
vwrlPostGam v = gamUnions [ g | (RlJdBldDirect _ _ g) <- vwrlJdBldL v ]

vwrlExtNmS :: VwRlInfo e -> Set.Set Nm
vwrlExtNmS v = Set.unions [ e | (RlJdBldDirect e _ _) <- vwrlJdBldL v ]

instance Show (VwRlInfo e) where
  show _ = "VwRlInfo"

instance PP e => PP (VwRlInfo e) where
  pp i = "VWRl" >#< pp (vwrlNm i) >#< (ppBracketsCommas (vwrlJdBldL i)
                                       >-< ppBracketsCommas (vwrlJdBldOnAL i)
                                       >-< ppBracketsCommas (vwrlJdBldOnBL i)
                                       >-< ppGam (vwrlFullNoDfltPreGam i)
                                       >-< ppGam (vwrlFullNoDfltPostGam i)
                                       >-< ppGam (vwrlFullPreGam i)
                                       >-< ppGam (vwrlFullPostGam i)
                                       >-< pp (show (vwrlPreScc i))
                                       >-< maybe empty (ppGam . gamMap ppGam) (vwrlMbChGam i)
                                       >-< pp (show (vwrlAuxGroups i))
                                      )

type VwRlGam e = Gam Nm (VwRlInfo e)

vwrlDelEmptyJd :: VwRlInfo e -> VwRlInfo e
vwrlDelEmptyJd i
  = i { vwrlFullPreGam = reGamFilterOutEmpty (vwrlFullPreGam i), vwrlFullPostGam = reGamFilterOutEmpty (vwrlFullPostGam i) }
        
vrwlIsEmpty :: VwRlInfo e -> Bool
vrwlIsEmpty i
  = gamIsEmpty (vwrlFullPreGam i) && gamIsEmpty (vwrlFullPostGam i)

vwrlScc :: VwRlInfo e -> [[Nm]]
vwrlScc i
  = map reorder . unNm . scc . concat . dpd . vwrlFullPreGam $ i
  where dpd g = d' ++ d
          where d = [ (jd n,map nm . Set.toList $ is) : zip (map nm . Set.toList $ os) (repeat [jd n])
                    | (REInfoJudge n _ is os _ _) <- gamElemsShadow g
                    ]
                d' = [ zipWith (\a b -> (a, [b])) grp (tl ++ [hd])
                     | grp@(hd:tl) <- map (map jd) auxGrps
                     , not (null grp)
                     ]
        nm n = nmSetSel n "n"
        jd n = nmSetSel n "j"
        unNm scc = [ l' | l <- scc, let l' = [ nmInit n | n <- l, nmSel n == "j" ], not (null l') ]
        auxGrps = vwrlAuxGroups i
        reorder cfg = foldr reorderForGroup cfg auxGrps
        reorderForGroup grp cfg
          = let (mem, nmem) = partition (`elem` grp) cfg
                smem = sortBy (\x y -> compare (elemIndex x grp) (elemIndex y grp)) mem
            in smem ++ nmem

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
      , rlMbInclVwS :: Maybe (Set.Set Nm)
      , rlVwGam     :: VwRlGam e
      }

emptyRlInfo = RlInfo nmUnk emptySPos Nothing Nothing 0 Nothing emptyGam

instance Show (RlInfo e) where
  show _ = "RlInfo"

instance PP e => PP (RlInfo e) where
  pp i = "RL" >#< pp (rlNm i) >#< ppGam (rlVwGam i)

type RlGam e = Gam Nm (RlInfo e)

rlVwGamLookup :: Nm -> Nm -> RlGam e -> Maybe (RlInfo e,VwRlInfo e)
rlVwGamLookup = dblGamLookup rlVwGam

rlVwRlOnL :: DtInvGam -> RlGam e -> Nm -> Nm -> RlInfo e -> [VwRlInfo e]
rlVwRlOnL dtInvGam rlGam nSc nVw rlInfo
  = on rlInfo
  where on i
          = case maybe Nothing (\n -> gamLookup n rlGam) (mbOnNm i) of
              Just i -> case gamLookup nVw (rlVwGam i) of
                          Just j -> j : on i
                          _      -> on i
              _      -> []
        mbOnNm i = dtInvGamRlMbOn dtInvGam i nSc nVw

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

%%]
