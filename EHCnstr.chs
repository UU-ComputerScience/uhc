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

%%[2 import(List, EHCommon, EHTy, UU.Pretty, EHTyPretty) export(Cnstr, emptyCnstr, cnstrUnit, Substitutable(..), unionL)
%%]

%%[4 export(cnstrFilter)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cnstr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Cnstr.Base
newtype Cnstr  = Cnstr (AssocL TyVarId Ty) deriving Show

cnstrLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrLookup tv (Cnstr s) = lookup tv s
%%]

%%[2.Cnstr.Rest
emptyCnstr :: Cnstr
emptyCnstr = Cnstr []

cnstrUnit :: TyVarId -> Ty -> Cnstr
cnstrUnit tv t = Cnstr [(tv,t)]
%%]

%%[4
cnstrFilter :: (TyVarId -> Ty -> Bool) -> Cnstr -> Cnstr
cnstrFilter f (Cnstr l) = Cnstr (filter (uncurry f) l)
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
                               Ty_Var tvar      ->  maybe t id (cnstrLookup tvar s)
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
                               Ty_Var tvar f
                                 | f == TyVarCateg_Plain
                                                ->  if isBound tvar then t else maybe t id (cnstrLookup tvar s)
%%]

%%[SubstitutableTyTVarsVar.3
                 Ty_Var tv _                    -> [tv]
%%]

%%[3.SubstitutableTy -2.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
                               Ty_Quant tv tp   ->  Ty_Quant tv (st (\v -> v == tv || isBound v) tp)
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
                 Ty_Quant tv tp                 -> tv `delete` ftv tp
                 otherwise                      -> []
%%]

%%[SubstitutableTyQuant.4
                               Ty_Quant q tv tp
                                                ->  Ty_Quant q tv (st (\v -> v == tv || isBound v) tp)
%%]

%%[SubstitutableTyTVarsQuant.4
                 Ty_Quant _ tv tp               -> tv `delete` ftv tp
%%]

%%[4.SubstitutableTy -3.SubstitutableTy
%%@SubstitutableTyHead.3
%%@SubstitutableTyVar.3
%%@SubstitutableTyQuant.4
                               otherwise        ->  t
%%@SubstitutableTyTVarsHead
%%@SubstitutableTyTVarsVar.3
%%@SubstitutableTyTVarsQuant.4
                 otherwise                      -> []
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
                 Ty_Ext t _ e                   -> ftv t `union` ftv e
                 otherwise                      -> []
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2
instance PP Cnstr where
  pp (Cnstr l) = ppListSepFill "" "" ", " (map (\(n,v) -> pp n >|< ":->" >|< ppTy v) l)
%%]

