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

%%[2 module {%{EH}Substitutable} import(Data.List, {%{EH}Base.Common}, {%{EH}Ty}, {%{EH}VarMp},{%{EH}Ty.Trf.Subst},{%{EH}Ty.Ftv}) export(Substitutable(..))
%%]

%%[2 import(qualified Data.Set as Set)
%%]

%%[9 import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Substitutable
infixr 6 |=>
%%]

%%[4
infixr 6 |==>
%%]

%%[2.Substitutable
class Substitutable vv k subst | vv -> subst k where
  (|=>)         ::  subst -> vv -> vv
%%[[4
  (|==>)        ::  subst -> vv -> (vv,VarMp)
%%]]
  ftv           ::  vv -> [k]

%%[[4
  s |==> x = (s |=> x,emptyVarMp)
%%]]
%%]

%%[4 export(substLift)
substLift :: (v' -> v) -> (v' -> v -> v') -> (subst -> v -> (v,r)) -> subst -> v' -> (v',r)
substLift toV updV app s v'
  = (updV v' x,r)
  where (x,r) = app s $ toV v'
%%]

%%[2 export(ftvSet)
ftvSet :: (Ord k,Substitutable x k subst) => x -> Set.Set k
ftvSet = Set.fromList . ftv
%%]

%%[9 export(ftvClosureSet)
ftvClosureSet :: (Substitutable x TyVarId VarMp) => VarMp -> x -> Set.Set TyVarId
ftvClosureSet varmp x
  = fvs `Set.union` fv
  where fv = ftvSet x
        (fvs,_,mcyc) = varmpClosure (`Set.member` fv) ftvSet varmp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.SubstitutableTy
instance Substitutable Ty TyVarId VarMp where
  (|=>)     = tyAppVarMp
%%[[4
  (|==>)    = tyAppVarMp2
%%]]
  ftv       = tyFtv
%%]

%%[10
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

%%[2.SubstitutableList
instance (Ord k,Substitutable vv k subst) => Substitutable [vv] k subst where
  s      |=>  l   =   map (s |=>) l
%%[[4
  s      |==> l   =   (l,varmpUnions m)
                  where (l,m) = unzip $ map (s |==>) l
%%]]
  ftv         l   =   unions $ map ftv l
%%]

%%[2.SubstitutableVarMp
instance Substitutable (VarMp' TyVarId Ty) TyVarId VarMp where
  s1@(VarMp sl1) |=> s2@(VarMp sl2)
    = VarMp (sl1 ++ map (\(v,t) -> (v,s1 |=> t)) sl2')
    where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
  ftv (VarMp sl)
    = ftv . map snd $ sl
%%]

%%[4.SubstitutableVarMp -2.SubstitutableVarMp
instance Substitutable (VarMp' TyVarId Ty) TyVarId VarMp where
  s1@(VarMp sl1) |=> s2@(VarMp sl2)
    = s1 `varmpPlus` s2
  ftv (VarMp sl)
    = ftv $ map snd sl
%%]

%%[9.SubstitutableVarMp -4.SubstitutableVarMp
instance Substitutable (VarMp' TyVarId (VarMpInfo Ty)) TyVarId VarMp where
  s1@(VarMp sl1) |=>   s2@(VarMp sl2)  =   VarMp (sl1 `Map.union` {- Map.map (s1 |=>) -} sl2)
  ftv                  (VarMp sl)      =   ftv $ Map.elems sl
%%]

%%[7
instance Substitutable vv k subst => Substitutable (HsName,vv) k subst where
  s |=>  (k,v) =  (k,s |=> v)
  ftv    (_,v) =  ftv v
%%]

%%[9
instance Substitutable Pred TyVarId VarMp where
  s |=>  p  =  (\(Ty_Pred p) -> p) (s |=> (Ty_Pred p))
  ftv    p  =  ftv (Ty_Pred p)

instance Substitutable PredScope TyVarId VarMp where
  s |=>  sc                   = maybe sc id $ varmpScopeLookupScopeCyc sc s
  ftv    (PredScope_Var v)    = [v]
  ftv    _                    = []

instance Substitutable PredOcc TyVarId VarMp where
  s |=>  (PredOcc pr id sc)  = PredOcc (s |=> pr) id (s |=> sc)
  ftv    (PredOcc pr id sc)  = unions [ftv pr,ftv sc]

instance Substitutable CHRPredOcc TyVarId VarMp where
  s |=>  (CHRPredOcc pr sc)  = CHRPredOcc (s |=> pr) (s |=> sc)
  ftv    (CHRPredOcc pr sc)  = unions [ftv pr,ftv sc]

instance Substitutable Impls TyVarId VarMp where
  s |=>  i  =  (\(Ty_Impls i) -> i) (s |=> (Ty_Impls i))
  ftv    i  =  ftv (Ty_Impls i)
%%]
  s |=>  sc@(PredScope_Var v) = maybe sc id $ varmpScopeLookup v s
  s |=>  sc                   = sc


%%[9
instance Substitutable (VarMpInfo Ty) TyVarId VarMp where
  s |=> vmi =  case vmi of
                 VMITy       t  -> VMITy (s |=> t)
                 VMIImpls    i  -> VMIImpls (s |=> i)
                 VMIPred     i  -> VMIPred (s |=> i)
                 VMIScope    sc -> VMIScope (s |=> sc)
%%[[13
                 VMIPredSeq  x  -> VMIPredSeq (s |=> x)
%%]]
%%[[10
                 -- VMIExts     x  -> VMIExts (s |=> x)
                 vmi            -> vmi
%%]]
  ftv   vmi =  case vmi of
                 VMITy       t  -> ftv t
                 VMIImpls    i  -> ftv i
                 VMIPred     i  -> ftv i
                 VMIScope    sc -> ftv sc
%%[[13
                 VMIPredSeq  x  -> ftv x
%%]]
%%[[10
                 -- VMIExts     x  -> ftv x
                 vmi            -> []
%%]]
%%]

This is still/regretfully duplicated in Ty/Subst.cag, Ty/Ftv.cag

%%[10
%%]
instance Substitutable RowExts TyVarId VarMp where
  s |=>  e@(RowExts_Var  v) = maybe e id $ varmpExtsLookup v s
  s |=>    (RowExts_Exts x) = RowExts_Exts $ assocLMapElt (s |=>) x
  ftv      (RowExts_Var  v) = [v]
  ftv    _                  = []

And this too...

%%[13
instance Substitutable PredSeq TyVarId VarMp where
  s |=>  a@(PredSeq_Var  v  ) = maybe a id $ varmpPredSeqLookup v s
  s |=>    (PredSeq_Cons h t) = PredSeq_Cons (s |=> h) (s |=> t)
  _ |=>    x                  = x
  ftv      (PredSeq_Var  v  ) = [v]
  ftv      (PredSeq_Cons h t) = unions [ftv h, ftv t]
  ftv    _                    = []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixating free type vars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(tyFixTyVars)
fixTyVarsVarMp :: Ty -> (VarMp,VarMp)
fixTyVarsVarMp t
  = (mk TyVarCateg_Fixed fv,mk TyVarCateg_Plain fv)
  where fv = ftv t
        mk cat fv = VarMp $ Map.fromList $ map (\v -> (v,VMITy (Ty_Var v cat))) $ fv

tyFixTyVars :: Ty -> (Ty,VarMp)
tyFixTyVars t
  = (sTo |=> t, sFr)
  where (sTo,sFr) = fixTyVarsVarMp t
%%]


