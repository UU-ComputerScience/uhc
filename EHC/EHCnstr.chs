% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

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

%%[2 import(List, EHCommon, EHTy, UU.Pretty, EHTyPretty) export(Cnstr, emptyCnstr, cnstrTyUnit, Substitutable(..), unionL)
%%]

%%[4 export(cnstrFilter)
%%]

%%[9 import(FiniteMap,EHDebug) export(CnstrInfo(..),fixTyVarsCnstr,tyFixTyVars,cnstrImplsLookup,cnstrImplsUnit,listToCnstrImpls,cnstrToAssocL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cnstr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Cnstr.Base
newtype Cnstr  = Cnstr (AssocL TyVarId Ty) deriving Show

cnstrTyLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrTyLookup tv (Cnstr s) = lookup tv s
%%]

%%[9 -2.Cnstr.Base
data CnstrInfo  = CITy Ty
                | CIImpls Impls
                deriving (Eq,Show)

newtype Cnstr   = Cnstr (FiniteMap TyVarId CnstrInfo)

instance Show Cnstr where
  show (Cnstr c) = show (fmToList c)

cnstrTyLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrTyLookup v (Cnstr s)
  = case lookupFM s v of
      Just (CITy t)  -> Just t
      _              -> Nothing
%%]

%%[2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr
emptyCnstr = Cnstr []
%%]

%%[9.Cnstr.emptyCnstr -2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr
emptyCnstr = Cnstr emptyFM
%%]

%%[2.Cnstr.cnstrTyUnit
cnstrTyUnit :: TyVarId -> Ty -> Cnstr
cnstrTyUnit tv t = Cnstr [(tv,t)]
%%]

%%[9.Cnstr.cnstrTyUnit -2.Cnstr.cnstrTyUnit
cnstrTyUnit :: TyVarId -> Ty -> Cnstr
cnstrTyUnit v t = Cnstr (listToFM [(v,CITy t)])
%%]

%%[4.cnstrFilter
cnstrFilter :: (TyVarId -> Ty -> Bool) -> Cnstr -> Cnstr
cnstrFilter f (Cnstr l) = Cnstr (filter (uncurry f) l)
%%]

%%[9.cnstrFilter -4.cnstrFilter
cnstrFilter :: (TyVarId -> CnstrInfo -> Bool) -> Cnstr -> Cnstr
cnstrFilter f (Cnstr c) = Cnstr (filterFM f c)
%%]

%%[9
cnstrImplsUnit :: ImplsVarId -> Impls -> Cnstr
cnstrImplsUnit v i = Cnstr (listToFM [(v,CIImpls i)])

listToCnstrImpls :: AssocL ImplsVarId Impls -> Cnstr
listToCnstrImpls = Cnstr . listToFM . assocLMapSnd CIImpls

cnstrImplsLookup :: ImplsVarId -> Cnstr -> Maybe Impls
cnstrImplsLookup v (Cnstr s)
  = case lookupFM s v of
      Just (CIImpls i)  -> Just i
      _                 -> Nothing

cnstrToAssocL :: Cnstr -> AssocL UID CnstrInfo
cnstrToAssocL (Cnstr l) = fmToList l
%%]

%%[9
cnstrSize (Cnstr m) = sizeFM m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Substitutable
infixr 6 |=>

class Substitutable s where
  (|=>)         ::  Cnstr -> s -> s
  ftv           ::  s -> TyVarIdL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[SubstitutableTyHead.2
instance Substitutable Ty where
  s |=> ty = st ty
    where st t            =  case t of
                               Ty_App fn arg    ->  Ty_App (st fn ) (st arg)
%%]

%%[SubstitutableTyVar.2
                               Ty_Var tvar      ->  maybe t id (cnstrTyLookup tvar s)
%%]

%%[SubstitutableTyTVarsHead
  ftv t =  case t of
                 Ty_App fn arg                  -> ftv fn `union` ftv arg
%%]

%%[2.SubstitutableTy
%%@SubstitutableTyHead.2
%%@SubstitutableTyVar.2
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
                 Ty_Var tv                      -> [tv]
                 otherwise                      -> []
%%]

%%[SubstitutableTyHead.3
instance Substitutable Ty where
  s |=> ty = st (const False) ty
    where st isBound t    =  case t of
                               Ty_App fn arg    ->  Ty_App (st isBound fn ) (st isBound arg)
%%]

%%[SubstitutableTyVar.3
                               Ty_Var tvar f    ->  if isBound tvar then t else maybe t id (cnstrTyLookup tvar s)
%%]

%%[SubstitutableTyTVarsVar.3
                 Ty_Var tv _                    ->  [tv]
%%]

%%[3.SubstitutableTy -2.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
                               Ty_Quant tv tp   ->  Ty_Quant tv (st (\v -> v == tv || isBound v) tp)
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
                 Ty_Quant tv tp                 ->  tv `delete` ftv tp
                 otherwise                      ->  []
%%]

%%[SubstitutableTyQuant.4
                               Ty_Quant q tv tp
                                                ->  Ty_Quant q tv (st (\v -> v == tv || isBound v) tp)
%%]

%%[SubstitutableTyTVarsQuant.4
                 Ty_Quant _ tv tp               ->  tv `delete` ftv tp
%%]

%%[4.SubstitutableTy -3.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
%%@SubstitutableTyQuant.4
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
%%@SubstitutableTyTVarsQuant.4
                 otherwise                      ->  []
%%]

%%[7.SubstitutableTy -4.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
%%@SubstitutableTyQuant.4
                               Ty_Ext t l e     ->  Ty_Ext (st isBound t) l (st isBound e)
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
%%@SubstitutableTyTVarsQuant.4
                 Ty_Ext t _ e                   ->  ftv t `union` ftv e
                 otherwise                      ->  []
%%]

%%[9.SubstitutableTy -7.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
%%@SubstitutableTyQuant.4
                               Ty_Ext t l e     ->  Ty_Ext (st isBound t) l (st isBound e)
                               Ty_Pred p        ->  Ty_Pred (sp isBound p)
                               Ty_Impls i       ->  Ty_Impls (si isBound i)
                               otherwise        ->  t
          sp isBound p    =  case p of
                               Pred_Class t     ->  Pred_Class (st isBound t)
                               Pred_Lacks t n   ->  Pred_Lacks (st isBound t) n
                               Pred_Equal v t   ->  Pred_Equal v (st isBound t)
          si isBound i    =  case i of
                               Impls_Cons v p pv t  -> Impls_Cons v (sp isBound p) pv (si isBound t)
                               Impls_Tail v     ->  maybe i id (cnstrImplsLookup v s)
                               _                ->  i
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
%%@SubstitutableTyTVarsQuant.4
                 Ty_Ext t _ e                   ->  ftv t `union` ftv e
                 Ty_Pred p                      ->  fpv p
                 Ty_Impls i                     ->  fiv i
                 otherwise                      ->  []
    where fpv p           =  case p of
                               Pred_Class t     ->  ftv t
                               Pred_Lacks t n   ->  ftv t
                               Pred_Equal v t   ->  ftv t
          fiv i           =  case i of
                               Impls_Cons _ p _ t  -> fpv p `union` fiv t
                               _                ->  []
%%]

%%[2.SubstitutableList
instance Substitutable a => Substitutable [a] where
  s      |=>  l   =   map (s |=>) l
  ftv         l   =   unionL . map ftv $ l
%%]

%%[2.SubstitutableCnstr
instance Substitutable Cnstr where
  s1@(Cnstr sl1) |=>   s2@(Cnstr sl2)  =   Cnstr (sl1 ++ map (\(v,t) -> (v,s1 |=> t)) sl2')
                                           where sl2' = deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
  ftv                  (Cnstr sl)      =   ftv . map snd $ sl
%%]

%%[7
instance Substitutable v => Substitutable (HsName,v) where
  s |=>  (k,v) =  (k,s |=> v)
  ftv    (_,v) =  ftv v
%%]

%%[9.SubstitutableCnstr -2.SubstitutableCnstr
instance Substitutable Cnstr where
  s1@(Cnstr sl1) |=>   s2@(Cnstr sl2)  =   Cnstr (mapFM (\v t -> s1 |=> t) (sl2) `plusFM` (sl1))
  ftv                  (Cnstr sl)      =   ftv . eltsFM $ sl
%%]

%%[9
instance Substitutable Pred where
  s |=>  p  =  (\(Ty_Pred p) -> p) (s |=> (Ty_Pred p))
  ftv    p  =  ftv (Ty_Pred p)

instance Substitutable PredOcc where
  s |=>  (PredOcc pr id)  = PredOcc (tyPred (s |=> Ty_Pred pr)) id
  ftv    (PredOcc pr _)   = ftv (Ty_Pred pr)

instance Substitutable Impls where
  s |=>  i  =  (\(Ty_Impls i) -> i) (s |=> (Ty_Impls i))
  ftv    i  =  ftv (Ty_Impls i)

instance Substitutable CnstrInfo where
  s |=>  ci =  case ci of
                 CITy     t -> CITy (s |=> t)
                 CIImpls  i -> CIImpls (s |=> i)
  ftv    ci =  case ci of
                 CITy     t -> ftv t
                 CIImpls  i -> ftv i
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixating free type vars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
fixTyVarsCnstr :: Substitutable s => s -> Cnstr
fixTyVarsCnstr = Cnstr . listToFM . map (\v -> (v,CITy (Ty_Var v TyVarCateg_Fixed))) . ftv

tyFixTyVars :: Substitutable s => s -> s
tyFixTyVars s = fixTyVarsCnstr s |=> s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.PP
instance PP Cnstr where
  pp (Cnstr l) = ppListSepFill "" "" ", " (map (\(n,v) -> pp n >|< ":->" >|< pp v) l)
%%]

%%[9 -2.PP
instance PP Cnstr where
  pp (Cnstr l) = ppListSepFill "" "" ", " (map (\(n,v) -> pp n >|< ":->" >|< pp v) . fmToList $ l)
%%]

%%[9
instance PP CnstrInfo where
  pp (CITy t) = pp t
  pp (CIImpls i) = pp i
%%]

