%%[0
%include lhs2TeX.fmt
%include afp.fmt

%if style == poly
%format sl1
%format sl1'
%format sl2
%format sl2'
%endif
%%]

%%[doesWhat doclatex
A VarMp maps from variables (tvars, ...) to whatever else has to be
mapped to (Ty, ...).

Starting with variant 6 (which introduces kinds) it allows multiple meta
level mapping, in that the VarMp holds mappings for multiple meta
levels. This allows one map to both map to base level info and to higher
levels. In particular this is used by fitsIn which also instantiates
types, and types may quantify over type variables with other kinds than
kind *, which must be propagated. A separate map could have been used,
but this holds the info together and is extendible to more levels.

A multiple level VarMp knows its own absolute metalevel, which is the default to use for lookup.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast) module {%{EH}VarMp} import(Data.List, {%{EH}Base.Common}, {%{EH}Ty}) export(VarMp'(..), VarMp, emptyVarMp, varmpTyLookup)
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Map as Map,qualified Data.Set as Set,Data.Maybe)
%%]

%%[(2 hmtyinfer || hmtyast) import(EH.Util.Pretty, {%{EH}Ty.Pretty}) export(ppVarMpV)
%%]

%%[(4 hmtyinfer || hmtyast) export(varmpFilterTy,varmpDel,(|\>))
%%]

%%[(4 hmtyinfer || hmtyast)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Error})
%%]

%%[(4_2 hmtyinfer || hmtyast) import(Maybe) export(varmpDelAlphaRename,varmpFilterAlphaRename,varmpFilterTyAltsMappedBy)
%%]

%%[(4_2 hmtyinfer || hmtyast) export(tyAsVarMp,varmpTyRevUnit)
%%]

%%[(6 hmtyinfer || hmtyast) import({%{EH}VarLookup}) export(module {%{EH}VarLookup})
%%]

%%[(6 hmtyinfer || hmtyast) import({%{EH}Base.Debug}) export(VarMpInfo(..),varmpToAssocL)
%%]

%%[(50 hmtyinfer || hmtyast) export(varmpKeys)
%%]

%%[(90 hmtyinfer || hmtyast) export(varmpMapTy)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).VarMpQ.Base
newtype VarMp' k v = VarMp (AssocL k v) deriving Show
%%]

%%[(6 hmtyinfer || hmtyast) -2.VarMpQ.Base
data VarMp' k v
  = VarMp
      { varmpMetaLev 	:: !MetaLev				-- the base meta level
      , varmpMpL 		:: [Map.Map k v]		-- for each level a map, starting at the base meta level
      }
%%]

%%[(99 hmtyinfer || hmtyast) export(varmpToMap)
-- get the base meta level map, ignore the others
varmpToMap :: VarMp' k v -> Map.Map k v
varmpToMap (VarMp _ (m:_)) = m
%%]

%%[(6 hmtyinfer || hmtyast) export(mkVarMp)
mkVarMp :: Map.Map k v -> VarMp' k v
mkVarMp m = VarMp 0 [m]
%%]

%%[(2 hmtyinfer || hmtyast).VarMp.emptyVarMp
emptyVarMp :: VarMp' k v
emptyVarMp = VarMp []
%%]

%%[(6 hmtyinfer || hmtyast).VarMp.emptyVarMp -2.VarMp.emptyVarMp
emptyVarMp :: VarMp' k v
emptyVarMp = mkVarMp Map.empty
%%]

%%[(4 hmtyinfer || hmtyast).varmpFilter export(varmpFilter)
varmpFilter :: (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilter f (VarMp l) = VarMp (filter (uncurry f) l)

varmpPartition :: (k -> v -> Bool) -> VarMp' k v -> (VarMp' k v,VarMp' k v)
varmpPartition f (VarMp l)
  = (VarMp p1, VarMp p2)
  where (p1,p2) = partition (uncurry f) l
%%]

%%[(6 hmtyinfer || hmtyast).varmpFilter -4.varmpFilter
varmpFilter :: Ord k => (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilter f (VarMp l c) = VarMp l (map (Map.filterWithKey f) c)

varmpPartition :: Ord k => (k -> v -> Bool) -> VarMp' k v -> (VarMp' k v,VarMp' k v)
varmpPartition f (VarMp l m)
  = (VarMp l p1, VarMp l p2)
  where (p1,p2) = unzip $ map (Map.partitionWithKey f) m
%%]

%%[(4 hmtyinfer || hmtyast).varmpDel
varmpDel :: Ord k => [k] -> VarMp' k v -> VarMp' k v
varmpDel tvL c = varmpFilter (const.not.(`elem` tvL)) c

(|\>) :: Ord k => VarMp' k v -> [k] -> VarMp' k v
(|\>) = flip varmpDel
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: meta level changes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast) export(varmpShiftMetaLev,varmpIncMetaLev,varmpDecMetaLev)
-- shift up the level,
-- or down when negative, throwing away the lower levels
varmpShiftMetaLev :: MetaLev -> VarMp' k v -> VarMp' k v
varmpShiftMetaLev inc (VarMp mlev fm)
  | inc < 0   = let mlev' = mlev+inc in VarMp (mlev' `max` 0) (drop (- (mlev' `min` 0)) fm)
  | otherwise = VarMp (mlev+inc) fm

varmpIncMetaLev :: VarMp' k v -> VarMp' k v
varmpIncMetaLev = varmpShiftMetaLev 1

varmpDecMetaLev :: VarMp' k v -> VarMp' k v
varmpDecMetaLev = varmpShiftMetaLev (-1)
%%]

%%[(6 hmtyinfer || hmtyast) export(varmpSelectMetaLev)
varmpSelectMetaLev :: [MetaLev] -> VarMp' k v -> VarMp' k v
varmpSelectMetaLev mlevs (VarMp mlev ms)
  = (VarMp mlev [ if l `elem` mlevs then m else Map.empty | (l,m) <- zip [mlev..] ms ])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(varmpSize)
varmpSize :: VarMp' k v -> Int
varmpSize (VarMp _ m) = sum $ map Map.size m
%%]

%%[(4 hmtyinfer || hmtyast).varmpKeys export(varmpKeys,varmpKeysSet)
varmpKeys :: VarMp' k v -> [k]
varmpKeys (VarMp l) = assocLKeys l

varmpKeysSet :: Ord k => VarMp' k v -> Set.Set k
varmpKeysSet = Set.fromList . varmpKeys
%%]

%%[(6 hmtyinfer || hmtyast).varmpKeys -4.varmpKeys export(varmpKeys,varmpKeysSet)
varmpKeys :: Ord k => VarMp' k v -> [k]
varmpKeys (VarMp _ fm) = Map.keys $ Map.unions fm

varmpKeysSet :: Ord k => VarMp' k v -> Set.Set k
varmpKeysSet (VarMp _ fm) = Set.unions $ map Map.keysSet fm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: from/to AssocL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer || hmtyast).assocTyLToVarMp export(assocTyLToVarMp,varmpToAssocTyL)
assocTyLToVarMp :: AssocL k v -> VarMp' k v
assocTyLToVarMp = VarMp

varmpToAssocTyL :: VarMp' k v -> AssocL k v
varmpToAssocTyL (VarMp l) = l

varmpToAssocL :: VarMp' k v -> AssocL k v
varmpToAssocL = varmpToAssocTyL
%%]

%%[(6 hmtyinfer || hmtyast).assocTyLToVarMp -4.assocTyLToVarMp export(assocMetaLevTyLToVarMp,assocTyLToVarMp,varmpToAssocTyL)
assocMetaLevTyLToVarMp :: Ord k => AssocL k (MetaLev,Ty) -> VarMp' k VarMpInfo
assocMetaLevTyLToVarMp l = varmpUnions [ varmpMetaLevTyUnit lev v t | (v,(lev,t)) <- l ]

assocTyLToVarMp :: Ord k => AssocL k Ty -> VarMp' k VarMpInfo
assocTyLToVarMp l = mkVarMp (Map.fromList $ assocLMapElt VMITy l)

varmpToAssocL :: VarMp' k VarMpInfo -> AssocL k VarMpInfo
varmpToAssocL (VarMp _ []   ) = []
varmpToAssocL (VarMp _ (l:_)) = Map.toList l

varmpToAssocTyL :: VarMp' k VarMpInfo -> AssocL k Ty
varmpToAssocTyL c = [ (v,t) | (v,VMITy t) <- varmpToAssocL c ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: combine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).varmpPlus export(varmpPlus, (|+>))
infixr 7 `varmpPlus`, |+>

varmpPlus, (|+>) :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpPlus (VarMp l1) (VarMp l2) = VarMp (l1 ++ l2)
(|+>) = varmpPlus
%%]

%%[(6 hmtyinfer || hmtyast).varmpPlus -2.varmpPlus export(varmpPlus)
infixr 7 `varmpPlus`

varmpPlus :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpPlus = (|+>) -- (VarMp l1) (VarMp l2) = VarMp (l1 `Map.union` l2)
%%]

%%[(4 hmtyinfer || hmtyast) export(varmpUnion,varmpUnions)
varmpUnion :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpUnion = varmpPlus

varmpUnions :: Ord k => [VarMp' k v] -> VarMp' k v
varmpUnions [x] = x
varmpUnions l   = foldr varmpPlus emptyVarMp l
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction specific for InstTo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast) export(instToL1VarMp)
instToL1VarMp :: [InstTo] -> VarMp
instToL1VarMp = varmpIncMetaLev . assocMetaLevTyLToVarMp . instToL1AssocL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMpInfo, info varieties in VarMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast)
data VarMpInfo
  = VMITy      !Ty
%%[[9
  | VMIImpls   !Impls
  | VMIScope   !PredScope
  | VMIPred    !Pred
  | VMIAssNm   !VarUIDHsName
%%]]
%%[[10
  | VMILabel   !Label
  | VMIOffset  !LabelOffset
--  | VMIExts    !RowExts
%%]]
%%[[13
  | VMIPredSeq !PredSeq
%%]]
  deriving (Eq,Show)
%%]

%%[(2 hmtyinfer || hmtyast).vmiMbTy export(vmiMbTy)
%%[[2
vmiMbTy      t = Just t
%%][6
vmiMbTy      i = case i of {VMITy      x -> Just x}
%%][9
vmiMbTy      i = case i of {VMITy      x -> Just x; _ -> Nothing}
%%]]
%%]

%%[(9 hmtyinfer || hmtyast) export(vmiMbImpls,vmiMbScope,vmiMbPred,vmiMbAssNm)
vmiMbImpls   i = case i of {VMIImpls   x -> Just x; _ -> Nothing}
vmiMbScope   i = case i of {VMIScope   x -> Just x; _ -> Nothing}
vmiMbPred    i = case i of {VMIPred    x -> Just x; _ -> Nothing}
vmiMbAssNm   i = case i of {VMIAssNm   x -> Just x; _ -> Nothing}
%%]
%%[(10 hmtyinfer || hmtyast) export(vmiMbLabel,vmiMbOffset)
vmiMbLabel   i = case i of {VMILabel   x -> Just x; _ -> Nothing}
vmiMbOffset  i = case i of {VMIOffset  x -> Just x; _ -> Nothing}
%%]
%%[(13 hmtyinfer || hmtyast) export(vmiMbPredSeq)
vmiMbPredSeq i = case i of {VMIPredSeq x -> Just x; _ -> Nothing}
%%] 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).VarMp.Base
type VarMp  = VarMp' TyVarId Ty
%%]

20080610, AD, todo: use TvPurpose

%%[(6 hmtyinfer || hmtyast) -2.VarMp.Base
type VarMp  = VarMp' TyVarId VarMpInfo

instance Show VarMp where
  show (VarMp _ c) = show (map Map.toList c)
%%]

%%[(4 hmtyinfer || hmtyast).varmpFilterTy
varmpFilterTy :: (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilterTy = varmpFilter
%%]

%%[(6 hmtyinfer || hmtyast).varmpFilterTy -4.varmpFilterTy
varmpFilterTy :: Ord k => (k -> Ty -> Bool) -> VarMp' k VarMpInfo -> VarMp' k VarMpInfo
varmpFilterTy f
  = varmpFilter
%%[[6
        (\v i -> case i of {VMITy t -> f v t})
%%][9
        (\v i -> case i of {VMITy t -> f v t ; _ -> True})
%%]]
%%]

%%[(9 hmtyinfer || hmtyast) export(varmpMapThr,varmpMapThrTy)
varmpMapThr :: (MetaLev -> TyVarId -> VarMpInfo -> thr -> (VarMpInfo,thr)) -> thr -> VarMp -> (VarMp,thr)
varmpMapThr f thr (VarMp l ms)
  = (VarMp l ms',thr')
  where (ms',thr') = foldMlev thr ms
        foldMp mlev thr fm
          = Map.foldWithKey
              (\v i (fm,thr)
                 -> let  (i',thr') = f mlev v i thr
                    in   (Map.insert v i' fm,thr')
              )
              (Map.empty,thr) fm
        foldMlev thr ms
          = foldr
              (\(mlev,m) (ms,thr)
                -> let (m',thr') = foldMp mlev thr m
                   in  (m':ms,thr')
              )
              ([],thr) (zip [0..] ms)

varmpMapThrTy :: (MetaLev -> TyVarId -> Ty -> thr -> (Ty,thr)) -> thr -> VarMp -> (VarMp,thr)
varmpMapThrTy f
  = varmpMapThr
      (\mlev v i thr
         -> case i of 
              VMITy t -> (VMITy t,thr')
                      where (t',thr') = f mlev v t thr
              _       -> (i,thr)
      )
%%]

%%[(9 hmtyinfer || hmtyast) hs export(varmpTailAddOcc)
varmpTailAddOcc :: ImplsProveOcc -> Impls -> (Impls,VarMp)
varmpTailAddOcc o (Impls_Tail i os) = (t, varmpImplsUnit i t)
                                    where t = Impls_Tail i (o:os)
varmpTailAddOcc _ x                 = (x,emptyVarMp)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make var counterpart of VarMpInfo, for derivation tree pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer || hmtyast).varmpinfoMkVar export(varmpinfoMkVar)
varmpinfoMkVar :: TyVarId -> VarMpInfo -> Ty
varmpinfoMkVar v i
  = case i of
      VMITy       t -> mkTyVar v
      VMIImpls    i -> mkImplsVar v
      _             -> mkTyVar v					-- rest incomplete
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).VarMp.varmpTyUnit export(varmpTyUnit)
varmpTyUnit :: k -> v -> VarMp' k v
varmpTyUnit tv t = VarMp [(tv,t)]
%%]

%%[(6 hmtyinfer || hmtyast).VarMp.varmpTyUnit -2.VarMp.varmpTyUnit export(varmpMetaLevTyUnit,varmpTyUnit)
varmpMetaLevTyUnit :: Ord k => MetaLev -> k -> Ty -> VarMp' k VarMpInfo
varmpMetaLevTyUnit mlev v t = VarMp mlev [Map.fromList [(v,VMITy t)]]

varmpTyUnit :: Ord k => k -> Ty -> VarMp' k VarMpInfo
varmpTyUnit = varmpMetaLevTyUnit 0
%%]

%%[(4_2 hmtyinfer || hmtyast).varmpTyRevUnit
varmpTyRevUnit :: TyVarId -> Ty -> (Ty,VarMp)
varmpTyRevUnit tv t = maybe (t,varmpTyUnit tv t) (\v -> let t' = mkTyVar tv in (t',varmpTyUnit v t')) . tyMbVar $ t
%%]

%%[(9 hmtyinfer || hmtyast) export(varmpImplsUnit,assocImplsLToVarMp,varmpScopeUnit,varmpPredUnit,varmpAssNmUnit)
varmpImplsUnit :: ImplsVarId -> Impls -> VarMp
varmpImplsUnit v i = mkVarMp (Map.fromList [(v,VMIImpls i)])

varmpScopeUnit :: TyVarId -> PredScope -> VarMp
varmpScopeUnit v sc = mkVarMp (Map.fromList [(v,VMIScope sc)])

varmpPredUnit :: TyVarId -> Pred -> VarMp
varmpPredUnit v p = mkVarMp (Map.fromList [(v,VMIPred p)])

varmpAssNmUnit :: TyVarId -> VarUIDHsName -> VarMp
varmpAssNmUnit v p = mkVarMp (Map.fromList [(v,VMIAssNm p)])

assocImplsLToVarMp :: AssocL ImplsVarId Impls -> VarMp
assocImplsLToVarMp = mkVarMp . Map.fromList . assocLMapElt VMIImpls
%%]


%%[(10 hmtyinfer || hmtyast) export(varmpLabelUnit,varmpOffsetUnit)
varmpLabelUnit :: LabelVarId -> Label -> VarMp
varmpLabelUnit v l = mkVarMp (Map.fromList [(v,VMILabel l)])

varmpOffsetUnit :: UID -> LabelOffset -> VarMp
varmpOffsetUnit v l = mkVarMp (Map.fromList [(v,VMIOffset l)])

%%]
varmpExtsUnit :: UID -> RowExts -> VarMp
varmpExtsUnit v l = mkVarMp (Map.fromList [(v,VMIExts l)])

%%[(13 hmtyinfer || hmtyast) export(varmpPredSeqUnit)
varmpPredSeqUnit :: TyVarId -> PredSeq -> VarMp
varmpPredSeqUnit v l = mkVarMp (Map.fromList [(v,VMIPredSeq l)])
%%]

%%[(6 hmtyinfer || hmtyast) export(tyRestrictKiVarMp)
-- restrict the kinds of tvars bound to value identifiers to kind *
tyRestrictKiVarMp :: [Ty] -> VarMp
tyRestrictKiVarMp ts = varmpIncMetaLev $ assocTyLToVarMp [ (v,kiStar) | t <- ts, v <- maybeToList $ tyMbVar t ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).varmpTyLookup
varmpTyLookup, varmpLookup :: Eq k => k -> VarMp' k v -> Maybe v
varmpTyLookup tv (VarMp s) = lookup tv s

varmpLookup = varmpTyLookup
%%]

%%[(6 hmtyinfer || hmtyast) -2.varmpTyLookup
varmpLookup :: (VarLookup m k VarMpInfo,Ord k) => k -> m -> Maybe VarMpInfo
varmpLookup = varlookupMap (Just . id)

varmpTyLookup :: (VarLookup m k VarMpInfo,Ord k) => k -> m -> Maybe Ty
varmpTyLookup = varlookupMap vmiMbTy
%%]

%%[(9 hmtyinfer || hmtyast) export(varmpImplsLookup,varmpScopeLookup,varmpPredLookup)
varmpImplsLookup :: VarLookup m ImplsVarId VarMpInfo => ImplsVarId -> m -> Maybe Impls
varmpImplsLookup = varlookupMap vmiMbImpls

varmpScopeLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe PredScope
varmpScopeLookup = varlookupMap vmiMbScope

varmpPredLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe Pred
varmpPredLookup = varlookupMap vmiMbPred

varmpAssNmLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe VarUIDHsName
varmpAssNmLookup = varlookupMap vmiMbAssNm
%%]

%%[(10 hmtyinfer || hmtyast) export(varmpLabelLookup,varmpOffsetLookup)
varmpLabelLookup :: VarLookup m LabelVarId VarMpInfo => LabelVarId -> m -> Maybe Label
varmpLabelLookup = varlookupMap vmiMbLabel

varmpOffsetLookup :: VarLookup m UID VarMpInfo => UID -> m -> Maybe LabelOffset
varmpOffsetLookup = varlookupMap vmiMbOffset
%%]

%%[(13 hmtyinfer || hmtyast) export(varmpPredSeqLookup)
varmpPredSeqLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe PredSeq
varmpPredSeqLookup = varlookupMap vmiMbPredSeq
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup as VarLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast)
instance Ord k => VarLookup (VarMp' k v) k v where
  varlookupWithMetaLev l k    (VarMp vmlev ms) = lkup (l-vmlev) ms
                                               where lkup _ []     = Nothing
                                                     lkup 0 (m:_)  = Map.lookup k m
                                                     lkup l (_:ms) = lkup (l-1) ms
  varlookup              k vm@(VarMp vmlev _ ) = varlookupWithMetaLev vmlev k vm
  
  -- combine by taking the lowest level, adapting the lists with maps accordingly
  (VarMp l1 ms1) |+> (VarMp l2 ms2)
    = case compare l1 l2 of
        EQ -> VarMp l1 (cmb                                   ms1                                    ms2 )
        LT -> VarMp l1 (cmb                                   ms1  (replicate (l2 - l1) Map.empty ++ ms2))
        GT -> VarMp l2 (cmb (replicate (l1 - l2) Map.empty ++ ms1)                                   ms2 )
    where cmb (m1:ms1) (m2:ms2) = Map.union m1 m2 : cmb ms1 ms2
          cmb ms1      []       = ms1
          cmb []       ms2      = ms2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp lookup: Cycle check variants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer || hmtyast).varmpTyLookupCyc export(varmpTyLookupCyc)
%%[[4
varmpTyLookupCyc :: TyVarId -> VarMp -> Maybe Ty
%%][9
varmpTyLookupCyc :: VarLookup m ImplsVarId VarMpInfo => TyVarId -> m -> Maybe Ty
%%]]
varmpTyLookupCyc x m = lookupLiftCycMb2 tyMbVar (flip varmpTyLookup m) x
%%]

%%[(9 hmtyinfer || hmtyast) export(varmpImplsLookupImplsCyc,varmpImplsLookupCyc,varmpScopeLookupScopeCyc,varmpAssNmLookupAssNmCyc)
varmpImplsLookupImplsCyc :: VarLookup m ImplsVarId VarMpInfo => Impls -> m -> Maybe Impls
varmpImplsLookupImplsCyc x m = lookupLiftCycMb1 implsMbVar (flip varmpImplsLookup m) x

varmpImplsLookupCyc :: VarLookup m ImplsVarId VarMpInfo => TyVarId -> m -> Maybe Impls
varmpImplsLookupCyc x m = lookupLiftCycMb2 implsMbVar (flip varmpImplsLookup m) x

varmpScopeLookupScopeCyc :: VarLookup m ImplsVarId VarMpInfo => PredScope -> m -> Maybe PredScope
varmpScopeLookupScopeCyc x m = lookupLiftCycMb1 pscpMbVar (flip varmpScopeLookup m) x

varmpAssNmLookupAssNmCyc :: VarLookup m ImplsVarId VarMpInfo => VarUIDHsName -> m -> Maybe VarUIDHsName
varmpAssNmLookupAssNmCyc x m = lookupLiftCycMb1 vunmMbVar (flip varmpAssNmLookup m) x
%%]

%%[(10 hmtyinfer || hmtyast) export(varmpLabelLookupCyc,varmpLabelLookupLabelCyc)
varmpLabelLookupLabelCyc :: VarLookup m ImplsVarId VarMpInfo => Label -> m -> Maybe Label
varmpLabelLookupLabelCyc x m = lookupLiftCycMb1 labelMbVar (flip varmpLabelLookup m) x

varmpLabelLookupCyc :: VarLookup m ImplsVarId VarMpInfo => TyVarId -> m -> Maybe Label
varmpLabelLookupCyc x m = lookupLiftCycMb2 labelMbVar (flip varmpLabelLookup m) x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp lookup: Flipped variants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer || hmtyast) export(varmpTyLookupCyc2)
varmpTyLookupCyc2 :: VarMp -> TyVarId -> Maybe Ty
varmpTyLookupCyc2 x m = varmpTyLookupCyc m x
%%]

%%[(9 hmtyinfer || hmtyast) export(varmpPredLookup2,varmpScopeLookup2,varmpAssNmLookup2,varmpImplsLookupCyc2)
varmpScopeLookup2 :: VarMp -> TyVarId -> Maybe PredScope
varmpScopeLookup2 m v = varmpScopeLookup v m

varmpImplsLookup2 :: VarMp -> ImplsVarId -> Maybe Impls
varmpImplsLookup2 m v = varmpImplsLookup v m

varmpImplsLookupCyc2 :: VarMp -> ImplsVarId -> Maybe Impls
varmpImplsLookupCyc2 m v = varmpImplsLookupCyc v m

varmpPredLookup2 :: VarMp -> TyVarId -> Maybe Pred
varmpPredLookup2 m v = varmpPredLookup v m

varmpAssNmLookup2 :: VarMp -> TyVarId -> Maybe VarUIDHsName
varmpAssNmLookup2 m v = varmpAssNmLookup v m
%%]

%%[(10 hmtyinfer || hmtyast)
varmpLabelLookup2 :: VarMp -> LabelVarId -> Maybe Label
varmpLabelLookup2 m v = varmpLabelLookup v m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Remove alpha rename of tvars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4_2 hmtyinfer || hmtyast)
varmpDelAlphaRename :: VarMp -> VarMp
varmpDelAlphaRename = varmpFilterTy (\_ t -> not (tyIsVar t))

varmpFilterAlphaRename :: VarMp -> VarMp
varmpFilterAlphaRename = varmpFilterTy (\_ t -> case t of {Ty_Var _ TyVarCateg_Plain -> True ; _ -> False})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty as cnstr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4_2 hmtyinfer || hmtyast)
tyAsVarMp :: UID -> Ty -> (Ty,VarMp)
tyAsVarMp u ty
  =  case ty of
        Ty_Var _ TyVarCateg_Plain -> (ty,emptyVarMp)
        _ -> let t = mkNewTyVar u in (t,u `varmpTyUnit` ty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filter cnstr bound to Ty_Alts which has a cnstr in other
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4_2 hmtyinfer || hmtyast)
varmpFilterTyAltsMappedBy :: VarMp -> VarMp -> VarMp
varmpFilterTyAltsMappedBy c cMp
  =  varmpFilterTy (\_ t -> case t of {Ty_Alts v _ -> isJust (varmpTyLookup v cMp) ; _ -> False}) c
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).ppVarMp
ppVarMp :: ([PP_Doc] -> PP_Doc) -> VarMp -> PP_Doc
ppVarMp ppL (VarMp l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) $ l
%%]

%%[(2 hmtyinfer || hmtyast)
ppVarMpV :: VarMp -> PP_Doc
ppVarMpV = ppVarMp vlist
%%]

%%[(6 hmtyinfer || hmtyast).ppVarMp -2.ppVarMp export(ppVarMp)
ppVarMp :: ([PP_Doc] -> PP_Doc) -> VarMp -> PP_Doc
ppVarMp ppL (VarMp mlev ms)
  = ppL [ "@" >|< pp lev >|< ":" >#< ppL [ pp n >|< ":->" >|< pp v | (n,v) <- Map.toList m]
        | (lev,m) <- zip [mlev..] ms
        ]
%%]

%%[(2 hmtyinfer || hmtyast).PP
instance PP VarMp where
  pp = ppVarMp (ppListSepFill "" "" ", ")
%%]

%%[(99 hmtyinfer || hmtyast).ppVarMpInfoCfgTy export(ppVarMpInfoCfgTy,ppVarMpInfoDt)
ppVarMpInfoCfgTy :: CfgPPTy -> VarMpInfo -> PP_Doc
ppVarMpInfoCfgTy c i
  = case i of
      VMITy       t -> ppTyWithCfg    c t
      VMIImpls    i -> ppImplsWithCfg c i
      VMIScope    s -> pp s						-- rest incomplete
      VMIPred     p -> pp p
      VMILabel    x -> pp x
      VMIOffset   x -> pp x
      VMIPredSeq  x -> pp "predseq" -- pp x

ppVarMpInfoDt :: VarMpInfo -> PP_Doc
ppVarMpInfoDt = ppVarMpInfoCfgTy cfgPPTyDT
%%]

%%[(6 hmtyinfer || hmtyast)
instance PP VarMpInfo where
  pp (VMITy       t) = pp t
%%[[9
  pp (VMIImpls    i) = pp i
  pp (VMIScope    s) = pp s
  pp (VMIPred     p) = pp p
%%]]
%%[[10
  pp (VMILabel    x) = pp x
  pp (VMIOffset   x) = pp x
  -- pp (VMIExts     x) = pp "exts" -- pp x
%%]]
%%[[13
  pp (VMIPredSeq  x) = pp "predseq" -- pp x
%%]]
%%]

