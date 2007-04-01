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

%%[2 module {%{EH}Cnstr} import(Data.List, {%{EH}Base.Common}, {%{EH}Ty}) export(Cnstr'(..), Cnstr, emptyCnstr, cnstrTyUnit, cnstrTyLookup)
%%]

%%[2 import(UU.Pretty, {%{EH}Ty.Pretty}) export(ppCnstrV)
%%]

%%[4 export(cnstrFilterTy,cnstrDel,(|\>) ,cnstrPlus)
%%]

%%[4 export(assocLToCnstr,cnstrToAssocTyL,cnstrKeys)
%%]

%%[4_2 import(Maybe) export(cnstrMapThrTy,cnstrMapTy,cnstrDelAlphaRename,cnstrFilterAlphaRename,cnstrFilterTyAltsMappedBy)
%%]

%%[4_2 export(tyAsCnstr,cnstrTyRevUnit)
%%]

%%[9 import(qualified Data.Map as Map,{%{EH}Base.Debug}) export(CnstrInfo(..),cnstrToAssocL)
%%]

%%[50 export(cnstrKeys)
%%]

%%[90 export(cnstrMapTy)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operator prio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
infixr `cnstrPlus`
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cnstr'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.CnstrQ.Base
newtype Cnstr' k v = Cnstr (AssocL k v) deriving Show
%%]

%%[9 -2.CnstrQ.Base
newtype Cnstr' k v = Cnstr (Map.Map k v)
%%]

%%[2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr' k v
emptyCnstr = Cnstr []
%%]

%%[9.Cnstr.emptyCnstr -2.Cnstr.emptyCnstr
emptyCnstr :: Cnstr' k v
emptyCnstr = Cnstr Map.empty
%%]

%%[2.Cnstr.cnstrTyUnit
cnstrTyUnit :: k -> v -> Cnstr' k v
cnstrTyUnit tv t = Cnstr [(tv,t)]
%%]

%%[9.Cnstr.cnstrTyUnit -2.Cnstr.cnstrTyUnit
cnstrTyUnit :: Ord k => k -> v -> Cnstr' k (CnstrInfo v)
cnstrTyUnit v t = Cnstr (Map.fromList [(v,CITy t)])
%%]

%%[4.cnstrFilter
cnstrFilter :: (k -> v -> Bool) -> Cnstr' k v -> Cnstr' k v
cnstrFilter f (Cnstr l) = Cnstr (filter (uncurry f) l)
%%]

%%[9.cnstrFilter -4.cnstrFilter
cnstrFilter :: Ord k => (k -> v -> Bool) -> Cnstr' k v -> Cnstr' k v
cnstrFilter f (Cnstr c) = Cnstr (Map.filterWithKey f c)
%%]

%%[2.cnstrPlus
cnstrPlus :: Cnstr' k v -> Cnstr' k v -> Cnstr' k v
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l1 ++ l2)
%%]

%%[9.cnstrPlus -2.cnstrPlus
cnstrPlus :: Ord k => Cnstr' k v -> Cnstr' k v -> Cnstr' k v
cnstrPlus (Cnstr l1) (Cnstr l2) = Cnstr (l1 `Map.union` l2)
%%]

%%[4.cnstrDel
cnstrDel :: Ord k => [k] -> Cnstr' k v -> Cnstr' k v
cnstrDel tvL c = cnstrFilter (const.not.(`elem` tvL)) c

(|\>) :: Ord k => Cnstr' k v -> [k] -> Cnstr' k v
(|\>) = flip cnstrDel
%%]

%%[4.assocLToCnstr
assocLToCnstr :: AssocL k v -> Cnstr' k v
assocLToCnstr = Cnstr

cnstrToAssocTyL :: Cnstr' k v -> AssocL k v
cnstrToAssocTyL (Cnstr l) = l
%%]

%%[9.assocLToCnstr -4.assocLToCnstr
assocLToCnstr :: Ord k => AssocL k v -> Cnstr' k (CnstrInfo v)
assocLToCnstr = Cnstr . Map.fromList . assocLMapElt CITy

cnstrToAssocL :: Cnstr' k (CnstrInfo v) -> AssocL k (CnstrInfo v)
cnstrToAssocL (Cnstr l) = Map.toList l

cnstrToAssocTyL :: Cnstr' k (CnstrInfo v) -> AssocL k v
cnstrToAssocTyL c = [ (v,t) | (v,CITy t) <- cnstrToAssocL c ]
%%]

%%[9
cnstrSize :: Cnstr' k v -> Int
cnstrSize (Cnstr m) = Map.size m
%%]

%%[4.cnstrKeys
cnstrKeys :: Cnstr' k v -> [k]
cnstrKeys (Cnstr l) = assocLKeys l
%%]

%%[9.cnstrKeys -4.cnstrKeys
cnstrKeys :: Cnstr' k v -> [k]
cnstrKeys (Cnstr fm) = Map.keys fm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cnstr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Cnstr.Base
type Cnstr  = Cnstr' TyVarId Ty
%%]

%%[2.cnstrTyLookup
cnstrTyLookup :: Eq k => k -> Cnstr' k v -> Maybe v
cnstrTyLookup tv (Cnstr s) = lookup tv s
%%]

%%[9 -(2.Cnstr.Base 2.cnstrTyLookup)
data CnstrInfo v
  = CITy     v
  | CIImpls  Impls
  | CIScope  !PredScope
  | CIPred   !Pred
%%[[10
  | CILabel  !Label
  | CIOffset !LabelOffset
  | CIExts   RowExts
%%]]
%%[[13
  | CIPredSeq PredSeq
%%]]
  deriving (Eq,Show)
%%]

%%[9
type Cnstr  = Cnstr' TyVarId (CnstrInfo Ty)

instance Show Cnstr where
  show (Cnstr c) = show (Map.toList c)

cnstrLookup' :: Ord k => (CnstrInfo v -> Maybe res) -> k -> Cnstr' k (CnstrInfo v) -> Maybe res
cnstrLookup' get v (Cnstr s)
  = do { ci <- Map.lookup v s
       ; get ci
       }

cnstrTyLookup :: Ord k => k -> Cnstr' k (CnstrInfo v) -> Maybe v
cnstrTyLookup = cnstrLookup' (\ci -> case ci of {CITy t -> Just t; _ -> Nothing})
%%]

%%[4_2.cnstrTyRevUnit
cnstrTyRevUnit :: TyVarId -> Ty -> (Ty,Cnstr)
cnstrTyRevUnit tv t = maybe (t,cnstrTyUnit tv t) (\v -> let t' = mkTyVar tv in (t',cnstrTyUnit v t')) . tyMbVar $ t
%%]

%%[4.cnstrFilterTy
cnstrFilterTy :: (k -> v -> Bool) -> Cnstr' k v -> Cnstr' k v
cnstrFilterTy = cnstrFilter
%%]

%%[9.cnstrFilterTy -4.cnstrFilterTy
cnstrFilterTy :: Ord k => (k -> v -> Bool) -> Cnstr' k (CnstrInfo v) -> Cnstr' k (CnstrInfo v)
cnstrFilterTy f = cnstrFilter (\v i -> case i of {CITy t -> f v t ; _ -> True})
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
cnstrMapThr :: (TyVarId -> CnstrInfo Ty -> thr -> (CnstrInfo Ty,thr)) -> thr -> Cnstr -> (Cnstr,thr)
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

%%[9 export(cnstrImplsUnit,assocLToCnstrImpls,cnstrScopeUnit,cnstrPredUnit)
cnstrImplsUnit :: ImplsVarId -> Impls -> Cnstr
cnstrImplsUnit v i = Cnstr (Map.fromList [(v,CIImpls i)])

cnstrScopeUnit :: TyVarId -> PredScope -> Cnstr
cnstrScopeUnit v sc = Cnstr (Map.fromList [(v,CIScope sc)])

cnstrPredUnit :: TyVarId -> Pred -> Cnstr
cnstrPredUnit v p = Cnstr (Map.fromList [(v,CIPred p)])

assocLToCnstrImpls :: AssocL ImplsVarId Impls -> Cnstr
assocLToCnstrImpls = Cnstr . Map.fromList . assocLMapElt CIImpls
%%]
cnstrPoiUnit :: UID -> PredOccId -> Cnstr
cnstrPoiUnit v sc = Cnstr (Map.fromList [(v,CIPoi sc)])


%%[10 export(cnstrLabelUnit,cnstrOffsetUnit,cnstrExtsUnit)
cnstrLabelUnit :: LabelVarId -> Label -> Cnstr
cnstrLabelUnit v l = Cnstr (Map.fromList [(v,CILabel l)])

cnstrOffsetUnit :: UID -> LabelOffset -> Cnstr
cnstrOffsetUnit v l = Cnstr (Map.fromList [(v,CIOffset l)])

cnstrExtsUnit :: UID -> RowExts -> Cnstr
cnstrExtsUnit v l = Cnstr (Map.fromList [(v,CIExts l)])
%%]

%%[13 export(cnstrPredSeqUnit)
cnstrPredSeqUnit :: TyVarId -> PredSeq -> Cnstr
cnstrPredSeqUnit v l = Cnstr (Map.fromList [(v,CIPredSeq l)])
%%]

%%[9 hs export(cnstrTailAddOcc)
cnstrTailAddOcc :: ImplsProveOcc -> Impls -> (Impls,Cnstr)
cnstrTailAddOcc o (Impls_Tail i os) = (t, cnstrImplsUnit i t)
                                    where t = Impls_Tail i (o:os)
cnstrTailAddOcc _ x                 = (x,emptyCnstr)
%%]

%%[9 export(cnstrImplsLookup,cnstrScopeLookup,cnstrPredLookup)
cnstrImplsLookup :: ImplsVarId -> Cnstr -> Maybe Impls
cnstrImplsLookup = cnstrLookup' (\ci -> case ci of {CIImpls i -> Just i; _ -> Nothing})

cnstrScopeLookup :: TyVarId -> Cnstr -> Maybe PredScope
cnstrScopeLookup = cnstrLookup' (\ci -> case ci of {CIScope s -> Just s; _ -> Nothing})

cnstrPredLookup :: TyVarId -> Cnstr -> Maybe Pred
cnstrPredLookup = cnstrLookup' (\ci -> case ci of {CIPred p -> Just p; _ -> Nothing})
%%]
cnstrPoiLookup :: UID -> Cnstr -> Maybe PredOccId
cnstrPoiLookup = cnstrLookup' (\ci -> case ci of {CIPoi p -> Just p; _ -> Nothing})


%%[10 export(cnstrLabelLookup,cnstrOffsetLookup,cnstrExtsLookup)
cnstrLabelLookup :: LabelVarId -> Cnstr -> Maybe Label
cnstrLabelLookup = cnstrLookup' (\ci -> case ci of {CILabel l -> Just l; _ -> Nothing})

cnstrOffsetLookup :: UID -> Cnstr -> Maybe LabelOffset
cnstrOffsetLookup = cnstrLookup' (\ci -> case ci of {CIOffset l -> Just l; _ -> Nothing})

cnstrExtsLookup :: UID -> Cnstr -> Maybe RowExts
cnstrExtsLookup = cnstrLookup' (\ci -> case ci of {CIExts l -> Just l; _ -> Nothing})
%%]

%%[13 export(cnstrPredSeqLookup)
cnstrPredSeqLookup :: TyVarId -> Cnstr -> Maybe PredSeq
cnstrPredSeqLookup = cnstrLookup' (\ci -> case ci of {CIPredSeq a -> Just a; _ -> Nothing})
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
ppCnstr :: ([PP_Doc] -> PP_Doc) -> Cnstr -> PP_Doc
ppCnstr ppL (Cnstr l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) $ l
%%]

%%[2
ppCnstrV :: Cnstr -> PP_Doc
ppCnstrV = ppCnstr vlist
%%]

%%[9.ppCnstr -2.ppCnstr
ppCnstr :: ([PP_Doc] -> PP_Doc) -> Cnstr -> PP_Doc
ppCnstr ppL (Cnstr l) = ppL . map (\(n,v) -> pp n >|< ":->" >|< pp v) . Map.toList $ l
%%]

%%[2.PP
instance PP Cnstr where
  pp = ppCnstr (ppListSepFill "" "" ", ")
%%]

%%[9
instance PP v => PP (CnstrInfo v) where
  pp (CITy       t) = pp t
  pp (CIImpls    i) = pp i
  pp (CIScope    s) = pp s
  pp (CIPred     p) = pp p
%%[[10
  pp (CILabel    x) = pp x
  pp (CIOffset   x) = pp x
  pp (CIExts     x) = pp "exts" -- pp x
%%]]
%%[[13
  pp (CIPredSeq  x) = pp "predseq" -- pp x
%%]]
%%]

