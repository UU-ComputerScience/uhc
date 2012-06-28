module EH101.Substitutable
( VarUpdatable (..)
, VarExtractable (..)
, ppS
, substLift
, varmpMapTyVarKey
, setSubst
, tyFixTyVars, tyMetaTyVars
, varmpinfoFtvMp
, varmpOccurErr )
where
import Data.List
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Ty.Trf.Subst
import EH101.Ty.Ftv
import qualified Data.Set as Set
import EH.Util.Pretty
import EH101.Error
import qualified Data.Map as Map
import EH101.VarLookup




{-# LINE 37 "src/ehc/Substitutable.chs" #-}
infixr 6 {- |=>, -} `varUpd`

{-# LINE 41 "src/ehc/Substitutable.chs" #-}
infixr 6 {- |==>, -} `varUpdCyc`

{-# LINE 45 "src/ehc/Substitutable.chs" #-}
class VarUpdatable vv subst where
  varUpd         	::  subst -> vv -> vv
  varUpdCyc        ::  subst -> vv -> (vv,VarMp)
  s `varUpdCyc` x = (s `varUpd` x,emptyVarMp)

{-# LINE 56 "src/ehc/Substitutable.chs" #-}
class Ord k => VarExtractable vv k | vv -> k where
  varFree           ::  vv -> [k]
  varFreeSet        ::  vv -> Set.Set k

  -- default
  varFree           =   Set.toList . varFreeSet
  varFreeSet        =   Set.fromList . varFree

{-# LINE 85 "src/ehc/Substitutable.chs" #-}
substLift :: (v' -> v) -> (v' -> v -> v') -> (subst -> v -> (v,r)) -> subst -> v' -> (v',r)
substLift toV updV app s v'
  = (updV v' x,r)
  where (x,r) = app s $ toV v'

{-# LINE 96 "src/ehc/Substitutable.chs" #-}
varmpinfoFtvMp :: VarMpInfo -> TvCatMp
varmpinfoFtvMp i
  = case i of
      VMITy       t  -> tyFtvMp    t
      VMIImpls    i  -> implsFtvMp i
      _              -> emptyTvCatMp		-- incomplete

{-# LINE 109 "src/ehc/Substitutable.chs" #-}
instance VarLookup m TyVarId VarMpInfo => VarUpdatable Ty m where
  varUpd     	= tyAppVarLookup
  varUpdCyc    = tyAppVarLookup2

instance VarExtractable Ty TyVarId where
  varFreeSet    = tyFtv

{-# LINE 124 "src/ehc/Substitutable.chs" #-}
-- instance VarUpdatable Label VarMp where
instance VarLookup m ImplsVarId VarMpInfo => VarUpdatable Label m where
  s `varUpd` lb          = maybe lb id $ varmpLabelLookupLabelCyc lb s

instance VarExtractable Label TyVarId where
  varFree (Label_Var v) = [v]
  varFree _             = []

-- instance VarUpdatable LabelOffset VarMp where
instance VarLookup m UID VarMpInfo => VarUpdatable LabelOffset m where
  s `varUpd` o@(LabelOffset_Var v) = maybe o id $ varmpOffsetLookup v s
  s `varUpd` o                     = o

instance VarExtractable LabelOffset TyVarId where
  varFree (LabelOffset_Var v) = [v]
  varFree _                   = []

{-# LINE 143 "src/ehc/Substitutable.chs" #-}
instance (VarUpdatable vv subst) => VarUpdatable [vv] subst where
  s      `varUpd`  l   =   map (varUpd s) l
  s      `varUpdCyc` l   =   (l,varmpUnions m)
                  where (l,m) = unzip $ map (varUpdCyc s) l

instance (VarExtractable vv k) => VarExtractable [vv] k where
  varFreeSet      l   =   Set.unions $ map varFreeSet l

{-# LINE 176 "src/ehc/Substitutable.chs" #-}
instance VarLookupCmb m (VarMp' k v) => VarUpdatable (VarMp' k v) m where
  varUpd                                =   (|+>)

instance VarExtractable VarMp TyVarId where
  varFreeSet               (VarMp _ sl)    =   Set.unions $ map (varFreeSet . Map.elems) sl

{-# LINE 186 "src/ehc/Substitutable.chs" #-}
instance VarUpdatable vv subst => VarUpdatable (HsName,vv) subst where
  s `varUpd`  (k,v) =  (k,s `varUpd` v)

instance VarExtractable vv k => VarExtractable (HsName,vv) k where
  varFreeSet (_,v) =  varFreeSet v

{-# LINE 194 "src/ehc/Substitutable.chs" #-}
instance VarUpdatable Pred VarMp where
  s `varUpd`  p  =  (\(Ty_Pred p) -> p) (s `varUpd` (Ty_Pred p))

instance VarExtractable Pred TyVarId where
  varFreeSet p  =  varFreeSet (Ty_Pred p)

-- instance VarUpdatable PredScope VarMp where
instance VarLookup m ImplsVarId VarMpInfo => VarUpdatable PredScope m where
  s `varUpd`  sc                   = maybe sc id $ varmpScopeLookupScopeCyc sc s

instance VarExtractable PredScope TyVarId where
  varFree    (PredScope_Var v)    = [v]
  varFree    _                    = []

instance VarUpdatable CHRPredOccCxt VarMp where
  s `varUpd`  (CHRPredOccCxt_Scope1 sc) = CHRPredOccCxt_Scope1 (s `varUpd` sc)

instance VarExtractable CHRPredOccCxt TyVarId where
  varFree    (CHRPredOccCxt_Scope1 sc) = varFree sc

instance VarUpdatable PredOcc VarMp where
  s `varUpd`  (PredOcc pr id sc r)  = PredOcc (s `varUpd` pr) id (s `varUpd` sc) r

instance VarExtractable PredOcc TyVarId where
  varFreeSet (PredOcc pr id sc _)  = varFreeSet pr `Set.union` varFreeSet sc

instance VarUpdatable CHRPredOcc VarMp where
  s `varUpd`  (CHRPredOcc pr sc r)  = CHRPredOcc (s `varUpd` pr) (s `varUpd` sc) r

instance VarExtractable CHRPredOcc TyVarId where
  varFreeSet (CHRPredOcc pr sc _)  = varFreeSet pr `Set.union` varFreeSet sc

instance VarUpdatable Impls VarMp where
  s `varUpd`  i  =  (\(Ty_Impls i) -> i) (s `varUpd` (Ty_Impls i))

instance VarExtractable Impls TyVarId where
  varFreeSet i  =  varFreeSet (Ty_Impls i)

{-# LINE 250 "src/ehc/Substitutable.chs" #-}
instance VarUpdatable VarMpInfo VarMp where
  s `varUpd` vmi =  case vmi of
                 VMITy       t  -> VMITy (s `varUpd` t)
                 VMIImpls    i  -> VMIImpls (s `varUpd` i)
                 VMIPred     i  -> VMIPred (s `varUpd` i)
                 VMIScope    sc -> VMIScope (s `varUpd` sc)
                 VMIPredSeq  x  -> VMIPredSeq (s `varUpd` x)
                 -- VMIExts     x  -> VMIExts (s `varUpd` x)
                 vmi            -> vmi

instance VarExtractable VarMpInfo TyVarId where
  varFreeSet vmi = case vmi of
                 VMITy       t  -> varFreeSet t
                 VMIImpls    i  -> varFreeSet i
                 VMIPred     i  -> varFreeSet i
                 VMIScope    sc -> varFreeSet sc
                 VMIPredSeq  x  -> varFreeSet x
                 -- VMIExts     x  -> varFreeSet x
                 vmi            -> Set.empty


{-# LINE 296 "src/ehc/Substitutable.chs" #-}
instance VarUpdatable PredSeq VarMp where
  s `varUpd`  a@(PredSeq_Var  v  ) = maybe a id $ varmpPredSeqLookup v s
  s `varUpd`    (PredSeq_Cons h t) = PredSeq_Cons (s `varUpd` h) (s `varUpd` t)
  _ `varUpd`    x                  = x

instance VarExtractable PredSeq TyVarId where
  varFreeSet   (PredSeq_Var  v  ) = Set.singleton v
  varFreeSet   (PredSeq_Cons h t) = varFreeSet h `Set.union` varFreeSet t
  varFreeSet _                    = Set.empty

{-# LINE 312 "src/ehc/Substitutable.chs" #-}
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

{-# LINE 342 "src/ehc/Substitutable.chs" #-}
setSubst :: VarMp -> TyVarIdS -> TyVarIdS
setSubst m s = varFreeSet $ (varUpd m) $ map mkTyVar $ Set.toList s

{-# LINE 351 "src/ehc/Substitutable.chs" #-}
varmpMapTyVarKey :: VarMp -> VarMp -> VarMp
varmpMapTyVarKey mMap m
  = varmpUnions [ varmpTyUnit v x | (Ty_Var v _,x) <- assocLMapKey (\v -> tyUnAnn $ mMap `varUpd` mkTyVar v) $ varmpToAssocTyL m ]


{-# LINE 371 "src/ehc/Substitutable.chs" #-}
ppS :: VarUpdatable x m => (x -> PP_Doc) -> m -> x -> PP_Doc
ppS pp c x = (pp $ c `varUpd` x) >#< ppParens (pp x)

{-# LINE 388 "src/ehc/Substitutable.chs" #-}
varmpOccurErr :: Range -> VarMp -> VarMp -> [Err]
varmpOccurErr r m mc = [ Err_OccurCycle r v (varmpDel [v] m `varUpd` t) | (v,t) <- varmpToAssocTyL mc ]

