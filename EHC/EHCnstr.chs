% $Id$

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

%%[2 import(List, EHCommon, EHTy) export(Cnstr(..), emptyCnstr, cnstrTyUnit, cnstrTyLookup, unionL)
%%]

%%[2 import(UU.Pretty, EHTyPretty) export(ppCnstrV)
%%]

%%[4 export(cnstrFilter,cnstrDel,cnstrPlus,cnstrMap)
%%]

%%[4 export(assocLToCnstr,cnstrToAssocTyL)
%%]

%%[4_2 export(tyAsCnstr)
%%]

%%[9 import(FiniteMap,EHDebug) export(CnstrInfo(..),cnstrImplsLookup,cnstrImplsUnit,assocLToCnstrImpls,cnstrToAssocL)
%%]

%%[11 export(cnstrKeys)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operator prio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
infixr `cnstrPlus`
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

%%[4.cnstrDel
cnstrDel :: TyVarIdL -> Cnstr -> Cnstr
cnstrDel tvL c = cnstrFilter (const.not.(`elem` tvL)) c
%%]

%%[2.cnstrPlus
cnstrPlus :: Cnstr -> Cnstr -> Cnstr
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l1 ++ l2)
%%]

%%[9.cnstrPlus -2.cnstrPlus
cnstrPlus :: Cnstr -> Cnstr -> Cnstr
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l2 `plusFM` l1)
%%]

%%[2.cnstrMap
cnstrMap :: ((TyVarId,Ty) -> (TyVarId,Ty)) -> Cnstr -> Cnstr
cnstrMap f (Cnstr l) = Cnstr (map f l)
%%]

%%[9.cnstrMap -2.cnstrMap
cnstrMap :: ((TyVarId,CnstrInfo) -> (TyVarId,CnstrInfo)) -> Cnstr -> Cnstr
cnstrMap f (Cnstr l) = Cnstr (mapFM (\k e -> snd (f (k,e))) l)
%%]

%%[4.assocLToCnstr
assocLToCnstr :: AssocL TyVarId Ty -> Cnstr
assocLToCnstr = Cnstr

cnstrToAssocTyL :: Cnstr -> AssocL TyVarId Ty
cnstrToAssocTyL (Cnstr l) = l
%%]

%%[9.assocLToCnstr -4.assocLToCnstr
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
cnstrSize :: Cnstr -> Int
cnstrSize (Cnstr m) = sizeFM m
%%]

%%[11
cnstrKeys :: Cnstr -> TyVarIdL
cnstrKeys = assocLKeys . cnstrToAssocL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty as cnstr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
tyAsCnstr :: UID -> Ty -> (Ty,Cnstr)
tyAsCnstr u ty
  =  case ty of
        Ty_Var _ TyVarCateg_Plain -> (ty,emptyCnstr)
        _ -> let t = mkNewTyVar u in (t,u `cnstrTyUnit` ty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.ppCnstr
ppCnstr :: (PP_DocL -> PP_Doc) -> Cnstr -> PP_Doc
ppCnstr ppL (Cnstr l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) $ l
%%]

%%[2
ppCnstrV :: Cnstr -> PP_Doc
ppCnstrV = ppCnstr vlist
%%]

%%[9.ppCnstr -2.ppCnstr
ppCnstr :: (PP_DocL -> PP_Doc) -> Cnstr -> PP_Doc
ppCnstr ppL (Cnstr l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) . fmToList $ l
%%]

%%[2.PP
instance PP Cnstr where
  pp = ppCnstr (ppListSepFill "" "" ", ")
%%]

%%[9
instance PP CnstrInfo where
  pp (CITy t) = pp t
  pp (CIImpls i) = pp i
%%]

