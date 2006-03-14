% $Id: EHCnstr.chs 269 2005-08-14 12:49:00Z cddouma $

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

%%[2 import(Data.List, EHCommon, EHTy) export(Cnstr(..), emptyCnstr, cnstrTyUnit, cnstrTyLookup)
%%]

%%[2 import(UU.Pretty, EHTyPretty) export(ppCnstrV)
%%]

%%[4 export(cnstrFilterTy,cnstrDel,(|\>) ,cnstrPlus)
%%]

%%[4 export(assocLToCnstr,cnstrToAssocTyL,cnstrKeys)
%%]

%%[4_2 import(Maybe) export(cnstrMapThrTy,cnstrMapTy,cnstrDelAlphaRename,cnstrFilterAlphaRename,cnstrFilterTyAltsMappedBy)
%%]

%%[4_2 export(tyAsCnstr,cnstrTyRevUnit)
%%]

%%[9 import(qualified Data.Map as Map,EHDebug) export(CnstrInfo(..),cnstrImplsLookup,cnstrImplsUnit,assocLToCnstrImpls,cnstrToAssocL)
%%]

%%[11 export(cnstrKeys)
%%]

%%[99 export(cnstrMapTy)
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
%%]

%%[2.cnstrTyLookup
cnstrTyLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrTyLookup tv (Cnstr s) = lookup tv s
%%]

%%[9 -(2.Cnstr.Base 2.cnstrTyLookup)
data CnstrInfo  = CITy Ty
                | CIImpls Impls
                deriving (Eq,Show)

newtype Cnstr   = Cnstr (Map.Map TyVarId CnstrInfo)

instance Show Cnstr where
  show (Cnstr c) = show (Map.toList c)

cnstrTyLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrTyLookup v (Cnstr s)
  = case Map.lookup v s of
      Just (CITy t)  -> Just t
      _              -> Nothing
%%]

%%[2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr
emptyCnstr = Cnstr []
%%]

%%[9.Cnstr.emptyCnstr -2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr
emptyCnstr = Cnstr Map.empty
%%]

%%[2.Cnstr.cnstrTyUnit
cnstrTyUnit :: TyVarId -> Ty -> Cnstr
cnstrTyUnit tv t = Cnstr [(tv,t)]
%%]

%%[4_2.cnstrTyRevUnit
cnstrTyRevUnit :: TyVarId -> Ty -> (Ty,Cnstr)
cnstrTyRevUnit tv t = maybe (t,cnstrTyUnit tv t) (\v -> let t' = mkTyVar tv in (t',cnstrTyUnit v t')) . tyMbVar $ t
%%]

%%[9.Cnstr.cnstrTyUnit -2.Cnstr.cnstrTyUnit
cnstrTyUnit :: TyVarId -> Ty -> Cnstr
cnstrTyUnit v t = Cnstr (Map.fromList [(v,CITy t)])
%%]

%%[4.cnstrFilterTy
cnstrFilterTy :: (TyVarId -> Ty -> Bool) -> Cnstr -> Cnstr
cnstrFilterTy f (Cnstr l) = Cnstr (filter (uncurry f) l)
%%]

%%[9.cnstrFilterTy -4.cnstrFilterTy
cnstrFilter :: (TyVarId -> CnstrInfo -> Bool) -> Cnstr -> Cnstr
cnstrFilter f (Cnstr c) = Cnstr (Map.filterWithKey f c)

cnstrFilterTy :: (TyVarId -> Ty -> Bool) -> Cnstr -> Cnstr
cnstrFilterTy f = cnstrFilter (\v i -> case i of {CITy t -> f v t ; _ -> True})
%%]

%%[4.cnstrDel
cnstrDel :: TyVarIdL -> Cnstr -> Cnstr
cnstrDel tvL c = cnstrFilterTy (const.not.(`elem` tvL)) c

(|\>) :: Cnstr -> TyVarIdL -> Cnstr
(|\>) = flip cnstrDel
%%]

%%[2.cnstrPlus
cnstrPlus :: Cnstr -> Cnstr -> Cnstr
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l1 ++ l2)
%%]

%%[9.cnstrPlus -2.cnstrPlus
cnstrPlus :: Cnstr -> Cnstr -> Cnstr
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l1 `Map.union` l2)
%%]

%%[4_2.cnstrMapThr
cnstrMapThrTy :: (TyVarId -> Ty -> thr -> (Ty,thr)) -> thr -> Cnstr -> (Cnstr,thr)
cnstrMapThrTy f thr (Cnstr l)
  =  let (l',thr')
           =  foldr    (\(v,t) (l,thr)
           				  ->  let  (t',thr') = f v t thr
           				      in   ((v,t'):l,thr')
                       )
                       ([],thr) l
     in  (Cnstr l',thr')

cnstrMapTy :: (TyVarId -> Ty -> Ty) -> Cnstr -> Cnstr
cnstrMapTy f = fst . cnstrMapThrTy (\v t _ -> (f v t,())) ()
%%]

%%[9 -4_2.cnstrMapThr
cnstrMapThr :: (TyVarId -> CnstrInfo -> thr -> (CnstrInfo,thr)) -> thr -> Cnstr -> (Cnstr,thr)
cnstrMapThr f thr (Cnstr fm)
  =  let (fm',thr')
           =  Map.foldWithKey
                (\v i (fm,thr)
           		  ->  let  (i',thr') = f v i thr
           		      in   (Map.insert v i' fm,thr')
                )
                (Map.empty,thr) fm
     in  (Cnstr fm',thr')

cnstrMapThrTy :: (TyVarId -> Ty -> thr -> (Ty,thr)) -> thr -> Cnstr -> (Cnstr,thr)
cnstrMapThrTy f = cnstrMapThr (\v i thr -> case i of {CITy t -> let (t',thr') = f v t thr in (CITy t,thr'); _ -> (i,thr)})
%%]

%%[99.cnstrMapTy
cnstrMap :: (TyVarId -> CnstrInfo -> CnstrInfo) -> Cnstr -> Cnstr
cnstrMap f (Cnstr l) = Cnstr (Map.mapWithKey f l)

cnstrMapTy :: (TyVarId -> Ty -> Ty) -> Cnstr -> Cnstr
cnstrMapTy f = cnstrMap (\v i -> case i of {CITy t -> CITy (f v t); _ -> i})
%%]

%%[4.assocLToCnstr
assocLToCnstr :: AssocL TyVarId Ty -> Cnstr
assocLToCnstr = Cnstr

cnstrToAssocTyL :: Cnstr -> AssocL TyVarId Ty
cnstrToAssocTyL (Cnstr l) = l
%%]

%%[9.assocLToCnstr -4.assocLToCnstr
assocLToCnstr :: AssocL TyVarId Ty -> Cnstr
assocLToCnstr = Cnstr . Map.fromList . assocLMapElt CITy

cnstrToAssocTyL :: Cnstr -> AssocL TyVarId Ty
cnstrToAssocTyL c = [ (v,t) | (v,CITy t) <- cnstrToAssocL c ]
%%]

%%[4.cnstrKeys
cnstrKeys :: Cnstr -> TyVarIdL
cnstrKeys (Cnstr l) = assocLKeys l
%%]

%%[9.cnstrKeys -4.cnstrKeys
cnstrKeys :: Cnstr -> TyVarIdL
cnstrKeys (Cnstr fm) = Map.keys fm
%%]

%%[9
cnstrImplsUnit :: ImplsVarId -> Impls -> Cnstr
cnstrImplsUnit v i = Cnstr (Map.fromList [(v,CIImpls i)])

assocLToCnstrImpls :: AssocL ImplsVarId Impls -> Cnstr
assocLToCnstrImpls = Cnstr . Map.fromList . assocLMapElt CIImpls

cnstrImplsLookup :: ImplsVarId -> Cnstr -> Maybe Impls
cnstrImplsLookup v (Cnstr s)
  = case Map.lookup v s of
      Just (CIImpls i)  -> Just i
      _                 -> Nothing

cnstrToAssocL :: Cnstr -> AssocL UID CnstrInfo
cnstrToAssocL (Cnstr l) = Map.toList l
%%]

%%[9
cnstrSize :: Cnstr -> Int
cnstrSize (Cnstr m) = Map.size m
%%]

%%[11
cnstrKeys :: Cnstr -> TyVarIdL
cnstrKeys = assocLKeys . cnstrToAssocL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Remove alpha rename of tvars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
cnstrDelAlphaRename :: Cnstr -> Cnstr
cnstrDelAlphaRename = cnstrFilterTy (\_ t -> not (tyIsVar t))

cnstrFilterAlphaRename :: Cnstr -> Cnstr
cnstrFilterAlphaRename = cnstrFilterTy (\_ t -> case t of {Ty_Var _ TyVarCateg_Plain -> True ; _ -> False})
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
%%% Filter cnstr bound to Ty_Alts which has a cnstr in other
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
cnstrFilterTyAltsMappedBy :: Cnstr -> Cnstr -> Cnstr
cnstrFilterTyAltsMappedBy c cMp
  =  cnstrFilterTy (\_ t -> case t of {Ty_Alts v _ -> isJust (cnstrTyLookup v cMp) ; _ -> False}) c
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
ppCnstr ppL (Cnstr l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) . Map.toList $ l
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

