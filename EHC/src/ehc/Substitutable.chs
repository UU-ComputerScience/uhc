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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast) module {%{EH}Substitutable} import(Data.List, {%{EH}Base.Common}, {%{EH}Ty}, {%{EH}VarMp},{%{EH}Ty.Trf.Subst},{%{EH}Ty.Ftv}) export(Substitutable(..))
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set,EH.Util.Pretty)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Error})
%%]

%%[(6 hmtyinfer || hmtyast) import(qualified Data.Map as Map)
%%]

%%[(6 hmtyinfer || hmtyast) hs import({%{EH}VarLookup})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable
infixr 6 |=>
%%]

%%[(4 hmtyinfer || hmtyast)
infixr 6 |==>
%%]

%%[(2 hmtyinfer || hmtyast).Substitutable
%%[[2
class Ord k => Substitutable vv k subst | vv -> subst k where
%%][9999
class VarLookup subst k vv => Substitutable vv k subst where
%%]]
  (|=>)         ::  subst -> vv -> vv
%%[[4
  (|==>)        ::  subst -> vv -> (vv,VarMp)
%%]]
  ftv           ::  vv -> [k]
  ftvSet        ::  vv -> Set.Set k
  
  -- default
  ftv           =   Set.toList . ftvSet
  ftvSet        =   Set.fromList . ftv


%%[[4
  s |==> x = (s |=> x,emptyVarMp)
%%]]
%%]

%%[(4 hmtyinfer || hmtyast) export(substLift)
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
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).SubstitutableTy
%%[[2
instance Substitutable Ty TyVarId VarMp where
%%][9999
instance VarLookup m TyVarId VarMpInfo => Substitutable Ty TyVarId m where
%%]]
  (|=>)     = tyAppVarLookup
%%[[4
  (|==>)    = tyAppVarLookup2
%%]]
  ftvSet    = tyFtv
%%]

%%[(10 hmtyinfer || hmtyast)
instance Substitutable Label TyVarId VarMp where
  s |=> lb          = maybe lb id $ varmpLabelLookupLabelCyc lb s
  ftv (Label_Var v) = [v]
  ftv _             = []

instance Substitutable LabelOffset TyVarId VarMp where
  s |=> o@(LabelOffset_Var v) = maybe o id $ varmpOffsetLookup v s
  s |=> o                     = o
  ftv (LabelOffset_Var v) = [v]
  ftv _                   = []
%%]
instance Substitutable Label TyVarId VarMp where
  (|=>)             = labelAppVarMp
  ftv (Label_Var v) = [v]
  ftv _             = []

%%[(2 hmtyinfer || hmtyast).SubstitutableList
instance (Ord k,Substitutable vv k subst) => Substitutable [vv] k subst where
  s      |=>  l   =   map (s |=>) l
%%[[4
  s      |==> l   =   (l,varmpUnions m)
                  where (l,m) = unzip $ map (s |==>) l
%%]]
  ftvSet      l   =   Set.unions $ map ftvSet l
%%]

%%[(2 hmtyinfer || hmtyast).SubstitutableVarMp
instance Substitutable VarMp TyVarId VarMp where
  s1@(VarMp sl1) |=> s2@(VarMp sl2)
    = VarMp (sl1 ++ map (\(v,t) -> (v,s1 |=> t)) sl2')
    where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
  ftvSet (VarMp sl)
    = ftvSet . map snd $ sl
%%]

%%[(4 hmtyinfer || hmtyast).SubstitutableVarMp -2.SubstitutableVarMp
instance Substitutable VarMp TyVarId VarMp where
  s1@(VarMp sl1) |=> s2@(VarMp sl2)
    = s1 `varmpPlus` s2
  ftvSet (VarMp sl)
    = ftvSet $ map snd sl
%%]

%%[(6 hmtyinfer || hmtyast).SubstitutableVarMp -4.SubstitutableVarMp
instance Substitutable VarMp TyVarId VarMp where
  (|=>)                                =   varmpPlus
  ftvSet               (VarMp _ sl)    =   Set.unions $ map (ftvSet . Map.elems) sl
%%]

%%[(7 hmtyinfer || hmtyast)
instance Substitutable vv k subst => Substitutable (HsName,vv) k subst where
  s |=>  (k,v) =  (k,s |=> v)
  ftvSet (_,v) =  ftvSet v
%%]

%%[(9 hmtyinfer || hmtyast)
instance Substitutable Pred TyVarId VarMp where
  s |=>  p  =  (\(Ty_Pred p) -> p) (s |=> (Ty_Pred p))
  ftvSet p  =  ftvSet (Ty_Pred p)

instance Substitutable PredScope TyVarId VarMp where
  s |=>  sc                   = maybe sc id $ varmpScopeLookupScopeCyc sc s
  ftv    (PredScope_Var v)    = [v]
  ftv    _                    = []

instance Substitutable CHRPredOccCxt TyVarId VarMp where
  s |=>  (CHRPredOccCxt_Scope1 sc) = CHRPredOccCxt_Scope1 (s |=> sc)
  ftv    (CHRPredOccCxt_Scope1 sc) = ftv sc

instance Substitutable PredOcc TyVarId VarMp where
%%[[9
  s |=>  (PredOcc pr id sc)  = PredOcc (s |=> pr) id (s |=> sc)
  ftvSet (PredOcc pr id sc)  = ftvSet pr `Set.union` ftvSet sc
%%][99
  s |=>  (PredOcc pr id sc r)  = PredOcc (s |=> pr) id (s |=> sc) r
  ftvSet (PredOcc pr id sc _)  = ftvSet pr `Set.union` ftvSet sc
%%]]

instance Substitutable CHRPredOcc TyVarId VarMp where
%%[[9
  s |=>  (CHRPredOcc pr sc)  = CHRPredOcc (s |=> pr) (s |=> sc)
  ftvSet (CHRPredOcc pr sc)  = ftvSet pr `Set.union` ftvSet sc
%%][99
  s |=>  (CHRPredOcc pr sc r)  = CHRPredOcc (s |=> pr) (s |=> sc) r
  ftvSet (CHRPredOcc pr sc _)  = ftvSet pr `Set.union` ftvSet sc
%%]]

instance Substitutable Impls TyVarId VarMp where
  s |=>  i  =  (\(Ty_Impls i) -> i) (s |=> (Ty_Impls i))
  ftvSet i  =  ftvSet (Ty_Impls i)
%%]
  s |=>  sc@(PredScope_Var v) = maybe sc id $ varmpScopeLookup v s
  s |=>  sc                   = sc


%%[(6 hmtyinfer || hmtyast)
instance Substitutable VarMpInfo TyVarId VarMp where
  s |=> vmi =  case vmi of
                 VMITy       t  -> VMITy (s |=> t)
%%[[9
                 VMIImpls    i  -> VMIImpls (s |=> i)
                 VMIPred     i  -> VMIPred (s |=> i)
                 VMIScope    sc -> VMIScope (s |=> sc)
%%]]
%%[[13
                 VMIPredSeq  x  -> VMIPredSeq (s |=> x)
%%]]
%%[[10
                 -- VMIExts     x  -> VMIExts (s |=> x)
                 vmi            -> vmi
%%]]
  ftvSet vmi = case vmi of
                 VMITy       t  -> ftvSet t
%%[[9
                 VMIImpls    i  -> ftvSet i
                 VMIPred     i  -> ftvSet i
                 VMIScope    sc -> ftvSet sc
%%]]
%%[[13
                 VMIPredSeq  x  -> ftvSet x
%%]]
%%[[10
                 -- VMIExts     x  -> ftvSet x
                 vmi            -> Set.empty
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
instance Substitutable PredSeq TyVarId VarMp where
  s |=>  a@(PredSeq_Var  v  ) = maybe a id $ varmpPredSeqLookup v s
  s |=>    (PredSeq_Cons h t) = PredSeq_Cons (s |=> h) (s |=> t)
  _ |=>    x                  = x
  ftvSet   (PredSeq_Var  v  ) = Set.singleton v
  ftvSet   (PredSeq_Cons h t) = ftvSet h `Set.union` ftvSet t
  ftvSet _                    = Set.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixating free type vars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(tyFixTyVars)
-- | Construct varmp for fixing tvars to new fresh fixed tvars + varmp for unfixing those to (again) fresh tvars
fixTyVarsVarMp :: UID -> Ty -> (VarMp,VarMp)
fixTyVarsVarMp uniq t
  = (mk TyVarCateg_Fixed fv rv,mk TyVarCateg_Plain rv rv2)
  where fv = ftv t
        l  = length fv
        (rv,rv2) = splitAt l $ mkNewUIDL (2*l) uniq
        mk cat fv rv = mkVarMp $ Map.fromList $ zipWith (\v r -> (v,VMITy (Ty_Var r cat))) fv rv

tyFixTyVars :: UID -> Ty -> (Ty,VarMp,VarMp)
tyFixTyVars uniq t
  = (sTo |=> t, sTo, sFr)
  where (sTo,sFr) = fixTyVarsVarMp uniq t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast) export(setSubst)
setSubst :: VarMp -> TyVarIdS -> TyVarIdS
setSubst m s = ftvSet $ (m |=>) $ map mkTyVar $ Set.toList s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% map VarMp keys to another key, filtering out non-keys.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast) export(varmpMapTyVarKey)
varmpMapTyVarKey :: VarMp -> VarMp -> VarMp
varmpMapTyVarKey mMap m
  = varmpUnions [ varmpTyUnit v x | (Ty_Var v _,x) <- assocLMapKey (\v -> tyUnAnn $ mMap |=> mkTyVar v) $ varmpToAssocTyL m ]
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
ppS :: Substitutable x TyVarId VarMp => (x -> PP_Doc) -> VarMp -> x -> PP_Doc
ppS pp c x = (pp $ c |=> x) >#< ppParens (pp x)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in module VarMp, but because of use of |=> cannot.

%%[(4 hmtyinfer || hmtyast).varmpOccurErr export(varmpOccurErr)
varmpOccurErr :: VarMp -> VarMp -> [Err]
varmpOccurErr m mc = [ Err_OccurCycle v (varmpDel [v] m |=> t) | (v,t) <- varmpToAssocTyL mc ]
%%]
varmpOccurErr m = map (uncurry Err_OccurCycle) $ varmpToAssocTyL m

%%[(99 hmtyinfer || hmtyast) -4.varmpOccurErr export(varmpOccurErr)
varmpOccurErr :: Range -> VarMp -> VarMp -> [Err]
varmpOccurErr r m mc = [ Err_OccurCycle r v (varmpDel [v] m |=> t) | (v,t) <- varmpToAssocTyL mc ]
%%]
varmpOccurErr r m = map (uncurry (Err_OccurCycle r)) $ varmpToAssocTyL m

