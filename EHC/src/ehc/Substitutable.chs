%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt

%if style == poly
%format sl1
%format sl1'
%format sl2
%format sl2'
%endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 module {%{EH}Substitutable}
%%]

%%[2 import(Data.List, {%{EH}Base.Common}, {%{EH}VarMp})
%%]

%%[2 import(UHC.Util.Substitutable) export(module UHC.Util.Substitutable)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}Ty}, {%{EH}Ty.Trf.Subst}, {%{EH}Ty.Ftv})
%%]

%%[2 import(qualified Data.Set as Set,UHC.Util.Pretty)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Error})
%%]

%%[6 import(qualified Data.Map as Map)
%%]

%%[6 hs import({%{EH}VarLookup})
%%]

%%[(6 hmtyinfer || hmtyast) hs import({%{EH}Base.TermLike})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 export(substLift)
substLift :: (v' -> v) -> (v' -> v -> v') -> (subst -> v -> (v,r)) -> subst -> v' -> (v',r)
substLift toV updV app s v'
  = (updV v' x,r)
  where (x,r) = app s $ toV v'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable like computations, partially implemented
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer || hmtyast) export(varmpinfoFtvMp)
varmpinfoFtvMp :: VarMpInfo -> TvCatMp
varmpinfoFtvMp i
  = case i of
      VMITy       t  -> tyFtvMp    t
      VMIImpls    i  -> implsFtvMp i
      _              -> emptyTvCatMp		-- incomplete
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable type family instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast)
type instance VarLookupKey VarMp = VarId
%%[[2
type instance VarLookupVal VarMp = Ty
%%][6
type instance VarLookupVal VarMp = VarMpInfo

{-
type instance VarLookupKey (VarMp' k v) = k
type instance VarLookupVal (VarMp' k v) = v
-}
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).SubstitutableTy
%%[[2
instance VarUpdatable Ty VarMp where
%%][6
instance (VarLookup m, VarLookupKey m ~ TyVarId, VarLookupVal m ~ VarMpInfo) => VarUpdatable Ty m where
%%]]
  varUpd     	= tyAppVarLookup
%%[[4
  varUpdCyc    = tyAppVarLookup2
%%]]

type instance ExtrValVarKey Ty = TyVarId

instance VarExtractable Ty where
  varFreeSet    = tyFtv
%%]

%%[(10 hmtyinfer || hmtyast)
-- instance VarUpdatable Label VarMp where
instance (VarLookup m, VarLookupKey m ~ ImplsVarId, VarLookupVal m ~ VarMpInfo) => VarUpdatable Label m where
  s `varUpd` lb          = maybe lb id $ varmpLabelLookupLabelCyc lb s

type instance ExtrValVarKey Label = TyVarId

instance VarExtractable Label where
  varFree (Label_Var v) = [v]
  varFree _             = []

-- instance VarUpdatable LabelOffset VarMp where
instance (VarLookup m, VarLookupKey m ~ UID, VarLookupVal m ~ VarMpInfo) => VarUpdatable LabelOffset m where
  s `varUpd` o@(LabelOffset_Var v) = maybe o id $ varmpOffsetLookup v s
  s `varUpd` o                     = o

type instance ExtrValVarKey LabelOffset = TyVarId

instance VarExtractable LabelOffset where
  varFree (LabelOffset_Var v) = [v]
  varFree _                   = []
%%]

%%[(2 hmtyinfer || hmtyast).SubstitutableList
{- 20160421: in uhc-util
-- instance (VarUpdatable vv subst) => VarUpdatable [vv] subst where
instance (Ord (VarLookupKey subst), VarUpdatable vv subst) => VarUpdatable [vv] subst where
  s      `varUpd`  l   =   map (varUpd s) l
%%[[4
  s      `varUpdCyc` l   =   (l,varmpUnions m)
                  where (l,m) = unzip $ map (varUpdCyc s) l
%%]]
-}

{- 20160411: in uhc-util
instance (VarExtractable vv) => VarExtractable [vv] where
  varFreeSet      l   =   Set.unions $ map varFreeSet l
-}
%%]

%%[(2 hmtyinfer || hmtyast).SubstitutableVarMp
instance Eq k => VarUpdatable (VarMp' k v) (VarMp' k v)where
  s1@(VarMp sl1) `varUpd` s2@(VarMp sl2)
    = VarMp (sl1 ++ map (\(v,t) -> (v,s1 `varUpd` t)) sl2')
    where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1

type instance ExtrValVarKey VarMp = TyVarId

instance VarExtractable VarMp where
  varFreeSet (VarMp sl)
    = varFreeSet . map snd $ sl
%%]

%%[(4 hmtyinfer || hmtyast).SubstitutableVarMp -2.SubstitutableVarMp
instance Ord k => VarUpdatable (VarMp' k v) (VarMp' k v) where
  s1@(VarMp sl1) `varUpd` s2@(VarMp sl2)
    = s1 `varmpPlus` s2

type instance ExtrValVarKey VarMp = TyVarId

instance VarExtractable VarMp where
  varFreeSet (VarMp sl)
    = varFreeSet $ map snd sl
%%]

%%[(6 hmtyinfer || hmtyast).SubstitutableVarMp -4.SubstitutableVarMp
-- instance VarLookupCmb m (VarMp' k v) => VarUpdatable (VarMp' k v) m where
instance VarLookupCmb (VarMp' k v) (VarMp' k v) => VarUpdatable (VarMp' k v) (VarMp' k v) where
  varUpd                                =   (|+>)

type instance ExtrValVarKey VarMp = TyVarId

instance VarExtractable VarMp where
  varFreeSet               (VarMp _ sl)    =   Set.unions $ map (varFreeSet . Map.elems) sl
%%]

%%[(7 hmtyinfer || hmtyast)
-- instance VarUpdatable vv subst => VarUpdatable (HsName,vv) subst where
instance VarUpdatable vv subst => VarUpdatable (HsName,vv) subst where
  s `varUpd`  (k,v) =  (k,s `varUpd` v)

instance (VarExtractable vv, ExtrValVarKey vv ~ ExtrValVarKey (HsName,vv)) => VarExtractable (HsName,vv) where
  varFreeSet (_,v) =  varFreeSet v
%%]

%%[(9 hmtyinfer || hmtyast)
instance VarUpdatable Pred VarMp where
  s `varUpd`  p  =  (\(Ty_Pred p) -> p) (s `varUpd` (Ty_Pred p))

type instance ExtrValVarKey Pred = TyVarId

instance VarExtractable Pred where
  varFreeSet p  =  varFreeSet (Ty_Pred p)

-- instance VarUpdatable PredScope VarMp where
instance (VarLookup m, VarLookupKey m ~ ImplsVarId, VarLookupVal m ~ VarMpInfo) => VarUpdatable PredScope m where
  s `varUpd`  sc                   = maybe sc id $ varmpScopeLookupScopeCyc sc s

type instance ExtrValVarKey PredScope = TyVarId

instance VarExtractable PredScope where
  varFree    (PredScope_Var v)    = [v]
  varFree    _                    = []

instance VarUpdatable CHRPredOccCxt VarMp where
  s `varUpd`  (CHRPredOccCxt_Scope1 sc) = CHRPredOccCxt_Scope1 (s `varUpd` sc)

type instance ExtrValVarKey CHRPredOccCxt = TyVarId

instance VarExtractable CHRPredOccCxt where
  varFree    (CHRPredOccCxt_Scope1 sc) = varFree sc

instance VarUpdatable PredOcc VarMp where
%%[[9
  s `varUpd`  (PredOcc pr id sc)  = PredOcc (s `varUpd` pr) id (s `varUpd` sc)
%%][99
  s `varUpd`  (PredOcc pr id sc r)  = PredOcc (s `varUpd` pr) id (s `varUpd` sc) r
%%]]

type instance ExtrValVarKey PredOcc = TyVarId

instance VarExtractable PredOcc where
%%[[9
  varFreeSet (PredOcc pr id sc)  = varFreeSet pr `Set.union` varFreeSet sc
%%][99
  varFreeSet (PredOcc pr id sc _)  = varFreeSet pr `Set.union` varFreeSet sc
%%]]

instance VarUpdatable CHRPredOcc VarMp where
%%[[9
  s `varUpd`  (CHRPredOcc pr sc)  = CHRPredOcc (s `varUpd` pr) (s `varUpd` sc)
%%][99
  s `varUpd`  (CHRPredOcc pr sc r)  = CHRPredOcc (s `varUpd` pr) (s `varUpd` sc) r
%%]]

type instance ExtrValVarKey CHRPredOcc = TyVarId

instance VarExtractable CHRPredOcc where
%%[[9
  varFreeSet (CHRPredOcc pr sc)  = varFreeSet pr `Set.union` varFreeSet sc
%%][99
  varFreeSet (CHRPredOcc pr sc _)  = varFreeSet pr `Set.union` varFreeSet sc
%%]]

instance VarUpdatable Impls VarMp where
  s `varUpd`  i  =  (\(Ty_Impls i) -> i) (s `varUpd` (Ty_Impls i))

type instance ExtrValVarKey Impls = TyVarId

instance VarExtractable Impls where
  varFreeSet i  =  varFreeSet (Ty_Impls i)
%%]

%%[(6 hmtyinfer || hmtyast)
instance VarUpdatable VarMpInfo VarMp where
  s `varUpd` vmi =  case vmi of
                 VMITy       t  -> VMITy (s `varUpd` t)
%%[[9
                 VMIImpls    i  -> VMIImpls (s `varUpd` i)
                 VMIPred     i  -> VMIPred (s `varUpd` i)
                 VMIScope    sc -> VMIScope (s `varUpd` sc)
%%]]
%%[[13
                 VMIPredSeq  x  -> VMIPredSeq (s `varUpd` x)
%%]]
%%[[10
                 -- VMIExts     x  -> VMIExts (s `varUpd` x)
                 vmi            -> vmi
%%]]
%%]

%%[6
type instance ExtrValVarKey VarMpInfo = VarId

instance VarExtractable VarMpInfo where
  varFreeSet vmi = case vmi of
%%[[(6 hmtyinfer || hmtyast)
                 VMITy       t  -> varFreeSet t
%%[[9
                 VMIImpls    i  -> varFreeSet i
                 VMIPred     i  -> varFreeSet i
                 VMIScope    sc -> varFreeSet sc
%%]]
%%[[13
                 VMIPredSeq  x  -> varFreeSet x
%%]]
%%[[10
                 -- VMIExts     x  -> varFreeSet x
                 vmi            -> Set.empty
%%]]
%%][6
                 _              -> Set.empty
%%]]
%%]

This is still/regretfully duplicated in Ty/Subst.cag, Ty/Ftv.cag

%%[(10 hmtyinfer || hmtyast)
%%]
instance Substitutable RowExts TyVarId VarMp where
  s |=>  e@(RowExts_Var  v) = maybe e id $ varmpExtsLookup v s
  s |=>    (RowExts_Exts x) = RowExts_Exts $ assocLMapElt (s |=>) x
  ftv      (RowExts_Var  v) = [v]
  ftv    _                  = []

And this too...

%%[(13 hmtyinfer || hmtyast)
instance VarUpdatable PredSeq VarMp where
  s `varUpd`  a@(PredSeq_Var  v  ) = maybe a id $ varmpPredSeqLookup v s
  s `varUpd`    (PredSeq_Cons h t) = PredSeq_Cons (s `varUpd` h) (s `varUpd` t)
  _ `varUpd`    x                  = x

type instance ExtrValVarKey PredSeq = TyVarId

instance VarExtractable PredSeq where
  varFreeSet   (PredSeq_Var  v  ) = Set.singleton v
  varFreeSet   (PredSeq_Cons h t) = varFreeSet h `Set.union` varFreeSet t
  varFreeSet _                    = Set.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixating free type vars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(tyFixTyVars,tyMetaTyVars)
-- | Construct varmp for fixing tvars to new fresh fixed tvars + varmp for unfixing those to (again) fresh tvars, resp meta tvars
fixTyVarsVarMp :: UID -> Ty -> (VarMp,VarMp,VarMp,VarMp)
fixTyVarsVarMp uniq t
  = ( mk TyVarCateg_Fixed fv rv
    , mk TyVarCateg_Meta  fv rv
    , mk TyVarCateg_Plain rv rv2
    , mk TyVarCateg_Meta  rv rv2
    )
  where fv = varFree t
        l  = length fv
        (rv,rv2) = splitAt l $ mkNewUIDL (2*l) uniq
        mk cat fv rv = mkVarMp $ Map.fromList $ zipWith (\v r -> (v,VMITy (Ty_Var r cat))) fv rv

tyFixTyVars :: UID -> Ty -> (Ty,VarMp,VarMp,VarMp)
tyFixTyVars uniq t
  = (sTo `varUpd` t, sTo, sFr, smFr)
  where (sTo,_,sFr,smFr) = fixTyVarsVarMp uniq t

-- | replace tvars with tvars having TyVarCateg_Meta
tyMetaTyVars :: UID -> Ty -> Ty
tyMetaTyVars uniq t
  = smTo `varUpd` t
  where (_,smTo,_,_) = fixTyVarsVarMp uniq t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast) export(setSubst)
setSubst :: VarMp -> TyVarIdS -> TyVarIdS
setSubst m s = varFreeSet $ (varUpd m) $ map mkTyVar $ Set.toList s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% map VarMp keys to another key, filtering out non-keys.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast) export(varmpMapTyVarKey)
varmpMapTyVarKey :: VarMp -> VarMp -> VarMp
varmpMapTyVarKey mMap m
  = varmpUnions [ varmpTyUnit v x | (Ty_Var v _,x) <- assocLMapKey (\v -> fst $ appUnAnnCanon $ mMap `varUpd` mkTyVar v) $ varmpToAssocTyL m ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract tyvars mapped to tyvars from m2, and build a mapping for the mapped tyvars from m1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast)
%%]
varmpForward :: VarMp -> VarMp -> VarMp
varmpForward m1 m2
  =  varmpFilterTy (\_ t -> case t of {Ty_Alts v _ -> isJust (varmpTyLookup v cMp) ; _ -> False}) c

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast) hs export(ppS)
ppS :: VarUpdatable x m => (x -> PP_Doc) -> m -> x -> PP_Doc
ppS pp c x = (pp $ c `varUpd` x) >#< ppParens (pp x)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in module VarMp, but because of use of |=> cannot.

%%[(4 hmtyinfer || hmtyast).varmpOccurErr export(varmpOccurErr)
varmpOccurErr :: VarMp -> VarMp -> [Err]
varmpOccurErr m mc = [ Err_OccurCycle v (varmpDel [v] m `varUpd` t) | (v,t) <- varmpToAssocTyL mc ]
%%]
varmpOccurErr m = map (uncurry Err_OccurCycle) $ varmpToAssocTyL m

%%[(99 hmtyinfer || hmtyast) -4.varmpOccurErr export(varmpOccurErr)
varmpOccurErr :: Range -> VarMp -> VarMp -> [Err]
varmpOccurErr r m mc = [ Err_OccurCycle r v (varmpDel [v] m `varUpd` t) | (v,t) <- varmpToAssocTyL mc ]
%%]
varmpOccurErr r m = map (uncurry (Err_OccurCycle r)) $ varmpToAssocTyL m

