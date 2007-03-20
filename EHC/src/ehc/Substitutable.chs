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

%%[2 module {%{EH}Substitutable} import(Data.List, {%{EH}Base.Common}, {%{EH}Ty}, {%{EH}Cnstr},{%{EH}Ty.Trf.Subst},{%{EH}Ty.Ftv}) export(Substitutable(..))
%%]

%%[2 import(qualified Data.Set as Set)
%%]

%%[4_2 export((|>>))
%%]

%%[9 import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Substitutable
infixr 6 |=>
%%]

%%[2.Substitutable
class Substitutable vv k subst | vv -> subst k where
  (|=>)         ::  subst -> vv -> vv
  ftv           ::  vv -> [k]
%%]

%%[2 export(ftvSet)
ftvSet :: (Ord k,Substitutable vv k subst) => vv -> Set.Set k
ftvSet = Set.fromList . ftv
%%]

%%[4_2.partialSubstApp
infixr 6 |>>

(|>>) :: Cnstr -> Cnstr -> Cnstr
c1 |>> c2 = cnstrMapTy (const (c1 |=>)) c2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.SubstitutableTy
instance Substitutable Ty TyVarId Cnstr where
  (|=>)  = tyAppCnstr
  ftv    = tyFtv
%%]

%%[9.SubstitutableTy -2.SubstitutableTy
instance Substitutable Ty TyVarId Cnstr where
  (|=>)  = tyAppCnstr
  ftv    = tyFtv
%%]

%%[10
instance Substitutable Label TyVarId Cnstr where
  (|=>)             = labelAppCnstr
  ftv (Label_Var v) = [v]
  ftv _             = []

instance Substitutable LabelOffset TyVarId Cnstr where
  s |=> o@(LabelOffset_Var v) = maybe o id $ cnstrOffsetLookup v s
  s |=> o                     = o
  ftv (LabelOffset_Var v) = [v]
  ftv _                   = []
%%]

%%[2.SubstitutableList
instance (Ord k,Substitutable vv k subst) => Substitutable [vv] k subst where
  s      |=>  l   =   map (s |=>) l
  ftv         l   =   unions . map ftv $ l
%%]

%%[2.SubstitutableCnstr
instance Substitutable (Cnstr' TyVarId Ty) TyVarId Cnstr where
  s1@(Cnstr sl1) |=> s2@(Cnstr sl2)
    = Cnstr (sl1 ++ map (\(v,t) -> (v,s1 |=> t)) sl2')
    where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
  ftv (Cnstr sl)
    = ftv . map snd $ sl
%%]
instance (Ord k,Substitutable v k subst) => Substitutable (Cnstr' k v) k subst where
  s1@(Cnstr sl1) |=> s2@(Cnstr sl2)
    = Cnstr (sl1 ++ map (\(v,t) -> (v,s1 |=> t)) sl2')
    where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
  ftv (Cnstr sl)
    = ftv . map snd $ sl

%%[7
instance Substitutable vv k subst => Substitutable (HsName,vv) k subst where
  s |=>  (k,v) =  (k,s |=> v)
  ftv    (_,v) =  ftv v
%%]

%%[9.SubstitutableCnstr -2.SubstitutableCnstr
instance Substitutable (Cnstr' TyVarId (CnstrInfo Ty)) TyVarId Cnstr where
  s1@(Cnstr sl1) |=>   s2@(Cnstr sl2)  =   Cnstr (sl1 `Map.union` Map.map (s1 |=>) sl2)
  ftv                  (Cnstr sl)      =   ftv $ Map.elems sl
%%]
instance (Ord k,Substitutable v k subst) => Substitutable (Cnstr' k v) k subst where
  s1@(Cnstr sl1) |=>   s2@(Cnstr sl2)  =   Cnstr (sl1 `Map.union` Map.map (s1 |=>) sl2)
  ftv                  (Cnstr sl)      =   ftv $ Map.elems sl

%%[9
instance Substitutable Pred TyVarId Cnstr where
  s |=>  p  =  (\(Ty_Pred p) -> p) (s |=> (Ty_Pred p))
  ftv    p  =  ftv (Ty_Pred p)

instance Substitutable PredScope TyVarId Cnstr where
  s |=>  sc@(PredScope_Var v) = maybe sc id $ cnstrScopeLookup v s
  s |=>  sc                   = sc
  ftv    (PredScope_Var v)    = [v]
  ftv    _                    = []

instance Substitutable PredOcc TyVarId Cnstr where
  s |=>  (PredOcc pr id sc)  = PredOcc (s |=> pr) id (s |=> sc)
  ftv    (PredOcc pr id sc)  = unions [ftv pr,ftv sc]

instance Substitutable CHRPredOcc TyVarId Cnstr where
  s |=>  (CHRPredOcc pr sc)  = CHRPredOcc (s |=> pr) (s |=> sc)
  ftv    (CHRPredOcc pr sc)  = unions [ftv pr,ftv sc]

instance Substitutable Impls TyVarId Cnstr where
  s |=>  i  =  (\(Ty_Impls i) -> i) (s |=> (Ty_Impls i))
  ftv    i  =  ftv (Ty_Impls i)
%%]
instance Substitutable PredOccId UID Cnstr where
  s |=>  i@(PredOccId_Var v) = maybe i id $ cnstrPoiLookup v s
  s |=>  i                   = i
  ftv    (PredOccId_Var v)   = [v]
  ftv    _                   = []


%%[9
instance Substitutable (CnstrInfo Ty) TyVarId Cnstr where
  s |=>  ci =  case ci of
                 CITy       t  -> CITy (s |=> t)
                 CIImpls    i  -> CIImpls (s |=> i)
                 CIPred     i  -> CIPred (s |=> i)
                 CIScope    sc -> CIScope (s |=> sc)
%%[[13
                 CIPredSeq  x  -> CIPredSeq (s |=> x)
%%]]
%%[[10
                 CIExts     x  -> CIExts (s |=> x)
                 ci            -> ci
%%]]
  ftv    ci =  case ci of
                 CITy       t  -> ftv t
                 CIImpls    i  -> ftv i
                 CIPred     i  -> ftv i
                 CIScope    sc -> ftv sc
%%[[13
                 CIPredSeq  x  -> ftv x
%%]]
%%[[10
                 CIExts     x  -> ftv x
                 ci            -> []
%%]]
%%]

This is still/regretfully duplicated in Ty/Subst.cag, Ty/Ftv.cag

%%[10
instance Substitutable RowExts TyVarId Cnstr where
  s |=>  e@(RowExts_Var  v) = maybe e id $ cnstrExtsLookup v s
  s |=>    (RowExts_Exts x) = RowExts_Exts $ assocLMapElt (s |=>) x
  ftv      (RowExts_Var  v) = [v]
  ftv    _                  = []
%%]

And this too...

%%[13
instance Substitutable PredSeq TyVarId Cnstr where
  s |=>  a@(PredSeq_Var  v  ) = maybe a id $ cnstrPredSeqLookup v s
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
fixTyVarsCnstr :: Ty -> (Cnstr,Cnstr)
fixTyVarsCnstr t
  = (mk TyVarCateg_Fixed fv,mk TyVarCateg_Plain fv)
  where fv = ftv t
        mk cat fv = Cnstr $ Map.fromList $ map (\v -> (v,CITy (Ty_Var v cat))) $ fv

tyFixTyVars :: Ty -> (Ty,Cnstr)
tyFixTyVars t
  = (sTo |=> t, sFr)
  where (sTo,sFr) = fixTyVarsCnstr t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tvar under constr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_1 hs
%%]
tvUnderCnstr :: Cnstr -> TyVarId -> TyVarId
tvUnderCnstr c v
  =  case c |=> mkTyVar v of
		Ty_Var   v' TyVarCateg_Plain  -> v'
		Ty_Alts  v' _                 -> v'
		_                             -> v

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Are tvars alpha renaming of eachother?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_1 hs
%%]
tvLMbAlphaRename :: Cnstr -> TyVarIdL -> TyVarIdL -> Maybe TyVarIdL
tvLMbAlphaRename c l1 l2
  =  if l1' == l2' && length l1' == length l1 then Just l1' else Nothing
  where  r = sort . nub . map (tvUnderCnstr c)
         l1' = r l1
         l2' = r l2

