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

%%[2 import(List, EHCommon, EHTy, UU.Pretty, EHTyPretty) export(Cnstr(..), emptyCnstr, cnstrTyUnit, cnstrTyLookup, unionL)
%%]

%%[4 export(cnstrFilter)
%%]

%%[4_1 export(assocLToCnstr,cnstrToAssocTyL)
%%]

%%[9 import(FiniteMap,EHDebug) export(CnstrInfo(..),cnstrImplsLookup,cnstrImplsUnit,assocLToCnstrImpls,cnstrToAssocL)
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

%%[4_1.assocLToCnstr
assocLToCnstr :: AssocL TyVarId Ty -> Cnstr
assocLToCnstr = Cnstr

cnstrToAssocTyL :: Cnstr -> AssocL TyVarId Ty
cnstrToAssocTyL (Cnstr l) = l
%%]

%%[9.assocLToCnstr -4_1.assocLToCnstr
assocLToCnstr :: AssocL TyVarId Ty -> Cnstr
assocLToCnstr = Cnstr . listToFM . assocLMapSnd CITy

cnstrToAssocTyL :: Cnstr -> AssocL TyVarId Ty
cnstrToAssocTyL c = [ (v,t) | (v,CITy t) <- cnstrToAssocL c ]
%%]

%%[9
cnstrImplsUnit :: ImplsVarId -> Impls -> Cnstr
cnstrImplsUnit v i = Cnstr (listToFM [(v,CIImpls i)])

assocLToCnstrImpls :: AssocL ImplsVarId Impls -> Cnstr
assocLToCnstrImpls = Cnstr . listToFM . assocLMapSnd CIImpls

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

