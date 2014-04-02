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

%%[2 module {%{EH}VarMp}
%%]

%%[2 import(Data.List, {%{EH}Base.Common})
%%]
%%[2 import({%{EH}Ty})
%%]

%%[2 import(qualified Data.Map as Map,qualified Data.Set as Set,Data.Maybe)
%%]

%%[2 import(UHC.Util.Pretty)
%%]
%%[(2 hmtyinfer || hmtyast) import({%{EH}Ty.Pretty})
%%]

%%[4 import({%{EH}Error})
%%]

%%[(4_2) import(Maybe) export(varmpDelAlphaRename,varmpFilterAlphaRename,varmpFilterTyAltsMappedBy)
%%]

%%[(4_2) export(tyAsVarMp',varmpTyRevUnit)
%%]

%%[6 import(UHC.Util.VarMp) export(module UHC.Util.VarMp)
%%]

%%[6 import({%{EH}VarLookup}) export(module {%{EH}VarLookup})
%%]

%%[6 import({%{EH}Base.Debug})
%%]

%%[8 import(UHC.Util.Utils)
%%]

%%[50 import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[9090 export(varmpMapTy)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.VarMpQ.Base export(VarMp'(..))
newtype VarMp' k v = VarMp (AssocL k v) deriving Show
%%]

%%[6 -2.VarMpQ.Base
-- moved to package uhc-util
%%]

%%[2.VarMp.emptyVarMp export(emptyVarMp)
emptyVarMp :: VarMp' k v
emptyVarMp = VarMp []
%%]

%%[6.VarMp.emptyVarMp -2.VarMp.emptyVarMp
%%]

%%[4.varmpFilter export(varmpFilter)
varmpFilter :: (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilter f (VarMp l) = VarMp (filter (uncurry f) l)

varmpPartition :: (k -> v -> Bool) -> VarMp' k v -> (VarMp' k v,VarMp' k v)
varmpPartition f (VarMp l)
  = (VarMp p1, VarMp p2)
  where (p1,p2) = partition (uncurry f) l
%%]

%%[6.varmpFilter -4.varmpFilter
%%]

%%[4.varmpDel
varmpDel :: Ord k => [k] -> VarMp' k v -> VarMp' k v
varmpDel tvL c = varmpFilter (const.not.(`elem` tvL)) c

(|\>) :: Ord k => VarMp' k v -> [k] -> VarMp' k v
(|\>) = flip varmpDel
%%]

%%[6.varmpDel -4.varmpDel
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.varmpKeys export(varmpKeys,varmpKeysSet)
varmpKeys :: VarMp' k v -> [k]
varmpKeys (VarMp l) = assocLKeys l

varmpKeysSet :: Ord k => VarMp' k v -> Set.Set k
varmpKeysSet = Set.fromList . varmpKeys
%%]

%%[6.varmpKeys -4.varmpKeys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.VarMp.varmpSingleton export(varmpSingleton)
varmpSingleton :: k -> v -> VarMp' k v
varmpSingleton tv t = VarMp [(tv,t)]
%%]

%%[6.VarMp.varmpSingleton -2.VarMp.varmpSingleton
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: from/to AssocL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer || hmtyast).assocTyLToVarMp export(assocTyLToVarMp,varmpToAssocTyL,varmpToAssocL)
assocTyLToVarMp, assocLToVarMp :: AssocL k v -> VarMp' k v
assocLToVarMp   = VarMp
assocTyLToVarMp = VarMp

varmpToAssocTyL :: VarMp' k v -> AssocL k v
varmpToAssocTyL (VarMp l) = l

varmpToAssocL :: VarMp' k v -> AssocL k v
varmpToAssocL = varmpToAssocTyL
%%]

%%[(6 hmtyinfer || hmtyast).assocTyLToVarMp -4.assocTyLToVarMp export(assocMetaLevTyLToVarMp,assocTyLToVarMp,varmpToAssocTyL)
assocMetaLevTyLToVarMp :: Ord k => AssocL k (MetaLev,Ty) -> VarMp' k VarMpInfo
assocMetaLevTyLToVarMp = assocMetaLevLToVarMp . assocLMapElt (\(ml,t) -> (ml, VMITy t)) -- varmpUnions [ varmpMetaLevTyUnit lev v t | (v,(lev,t)) <- l ]

assocTyLToVarMp :: Ord k => AssocL k Ty -> VarMp' k VarMpInfo
assocTyLToVarMp = assocLToVarMp . assocLMapElt VMITy

varmpToAssocTyL :: VarMp' k VarMpInfo -> AssocL k Ty
varmpToAssocTyL c = [ (v,t) | (v,VMITy t) <- varmpToAssocL c ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: combine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.varmpPlus export(varmpPlus, (|+>))
infixr 7 `varmpPlus`, |+>

varmpPlus, (|+>) :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpPlus (VarMp l1) (VarMp l2) = VarMp (l1 ++ l2)
(|+>) = varmpPlus
%%]

%%[6.varmpPlus -2.varmpPlus
%%]

%%[4.varmpUnion export(varmpUnion,varmpUnions)
varmpUnion :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpUnion = varmpPlus

varmpUnions :: Ord k => [VarMp' k v] -> VarMp' k v
varmpUnions [ ] = emptyVarMp
varmpUnions [x] = x
varmpUnions l   = foldr1 varmpPlus l
%%]

%%[6.varmpUnion -4.varmpUnion
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction specific for InstTo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer) export(instToL1VarMp)
instToL1VarMp :: [InstTo] -> VarMp
instToL1VarMp = varmpIncMetaLev . assocMetaLevTyLToVarMp . instToL1AssocL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMpInfo, info varieties in VarMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(VarMpInfo(..))
data VarMpInfo
%%[[(6 hmtyinfer || hmtyast)
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
%%][6
  = VMINone			-- nada
%%]]
  deriving
    ( Eq, Ord, Show
%%[[50
    , Typeable, Data
%%]]
    )
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

%%[2.VarMp.Base export(VarMp)
type VarMp  = VarMp' VarId Ty
%%]

20080610, AD, todo: use TvPurpose

%%[6 -2.VarMp.Base export(VarMp)
type VarMp  = VarMp' VarId VarMpInfo
%%]

%%[(4 hmtyinfer).varmpFilterTy export(varmpFilterTy)
varmpFilterTy :: (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilterTy = varmpFilter
%%]

%%[(6 hmtyinfer).varmpFilterTy -4.varmpFilterTy export(varmpFilterTy)
varmpFilterTy :: Ord k => (k -> Ty -> Bool) -> VarMp' k VarMpInfo -> VarMp' k VarMpInfo
varmpFilterTy f
  = varmpFilter
%%[[6
        (\v i -> case i of {VMITy t -> f v t})
%%][9
        (\v i -> case i of {VMITy t -> f v t ; _ -> True})
%%]]
%%]

%%[(9 hmtyinfer) hs export(varmpTailAddOcc)
varmpTailAddOcc :: ImplsProveOcc -> Impls -> (Impls,VarMp)
varmpTailAddOcc o (Impls_Tail i os) = (t, varmpImplsUnit i t)
                                    where t = Impls_Tail i (o:os)
varmpTailAddOcc _ x                 = (x,emptyVarMp)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fold: map + thread, for ty related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(varmpMapThr,varmpMapThrTy)
varmpMapThr :: (MetaLev -> TyVarId -> VarMpInfo -> thr -> (VarMpInfo,thr)) -> thr -> VarMp -> (VarMp,thr)
varmpMapThr f thr (VarMp l ms)
  = (VarMp l ms',thr')
  where (ms',thr') = foldMlev thr ms
        foldMp mlev thr fm
          = Map.foldrWithKey
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Visit as graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999 export(varmpGraphVisit)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make var counterpart of VarMpInfo, for derivation tree pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer).varmpinfoMkVar export(varmpinfoMkVar)
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
varmpTyUnit = varmpSingleton
%%]

%%[(6 hmtyinfer || hmtyast).VarMp.varmpTyUnit -2.VarMp.varmpTyUnit export(varmpMetaLevTyUnit,varmpTyUnit)
varmpMetaLevTyUnit :: Ord k => MetaLev -> k -> Ty -> VarMp' k VarMpInfo
varmpMetaLevTyUnit mlev v t = varmpMetaLevSingleton mlev v (VMITy t)

varmpTyUnit :: Ord k => k -> Ty -> VarMp' k VarMpInfo
varmpTyUnit = varmpMetaLevTyUnit metaLevVal
%%]

%%[4_2.varmpTyRevUnit
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

%%[(6 hmtyinfer) export(tyRestrictKiVarMp)
-- restrict the kinds of tvars bound to value identifiers to kind *
tyRestrictKiVarMp :: [Ty] -> VarMp
tyRestrictKiVarMp ts = varmpIncMetaLev $ assocTyLToVarMp [ (v,kiStar) | t <- ts, v <- maybeToList $ tyMbVar t ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp: reification as VarMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer) export(tyAsVarMp',tyAsVarMp)
-- | Encode 'ty' as a tvar + VarMp, with additional initial construction
tyAsVarMp' :: (UID -> Ty -> Ty) -> UID -> Ty -> (Ty,VarMp)
tyAsVarMp' f u t
  = case f v1 t of
      t | tyIsVar t -> (t, emptyVarMp)
        | otherwise -> (mkTyVar v2, varmpTyUnit v2 t)
  where [v1,v2] = mkNewLevUIDL 2 u

-- | Encode 'ty' as a tvar + VarMp
tyAsVarMp :: UID -> Ty -> (Ty,VarMp)
tyAsVarMp = tyAsVarMp' (flip const)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).varmpTyLookup export(varmpTyLookup)
varmpTyLookup, varmpLookup :: Eq k => k -> VarMp' k v -> Maybe v
varmpTyLookup tv (VarMp s) = lookup tv s

varmpLookup = varmpTyLookup
%%]

%%[(6 hmtyinfer || hmtyast) -2.varmpTyLookup export(varmpTyLookup)
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
%%% VarMp lookup: Cycle check variants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer || hmtyast).varmpTyLookupCyc export(varmpTyLookupCyc)
%%[[4
varmpTyLookupCyc :: TyVarId -> VarMp -> Maybe Ty
%%][8
varmpTyLookupCyc :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe Ty
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

%%[(4 hmtyinfer) export(varmpTyLookupCyc2)
varmpTyLookupCyc2 :: VarMp -> TyVarId -> Maybe Ty
varmpTyLookupCyc2 x m = varmpTyLookupCyc m x
%%]

%%[(9 hmtyinfer) export(varmpPredLookup2,varmpScopeLookup2,varmpAssNmLookup2,varmpImplsLookupCyc2)
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

%%[(10 hmtyinfer)
varmpLabelLookup2 :: VarMp -> LabelVarId -> Maybe Label
varmpLabelLookup2 m v = varmpLabelLookup v m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.ppVarMp
ppVarMp :: (PP k, PP v) => ([PP_Doc] -> PP_Doc) -> VarMp' k v -> PP_Doc
ppVarMp ppL (VarMp l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) $ l

ppVarMpV :: VarMp -> PP_Doc
ppVarMpV = ppVarMp vlist
%%]

%%[6.ppVarMp -2.ppVarMp
%%]

%%[2.PP
instance (PP k, PP v) => PP (VarMp' k v) where
  pp = ppVarMp (ppBlockWithStrings "" "" ", ")
%%]

%%[6.PP -2.PP
%%]

%%[(99 hmtyinfer).ppVarMpInfoCfgTy export(ppVarMpInfoCfgTy,ppVarMpInfoDt)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer || hmtyast)
instance Serialize VarMpInfo where
  sput (VMITy      a) = sputWord8 0  >> sput a
  sput (VMIImpls   a) = sputWord8 1  >> sput a
  sput (VMIScope   a) = sputWord8 2  >> sput a
  sput (VMIPred    a) = sputWord8 3  >> sput a
  sput (VMIAssNm   a) = sputWord8 4  >> sput a
  sput (VMILabel   a) = sputWord8 5  >> sput a
  sput (VMIOffset  a) = sputWord8 6  >> sput a
  sput (VMIPredSeq a) = sputWord8 7  >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM VMITy      sget
              1 -> liftM VMIImpls   sget
              2 -> liftM VMIScope   sget
              3 -> liftM VMIPred    sget
              4 -> liftM VMIAssNm   sget
              5 -> liftM VMILabel   sget
              6 -> liftM VMIOffset  sget
              7 -> liftM VMIPredSeq sget


%%]
