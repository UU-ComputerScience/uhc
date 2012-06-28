module EH101.VarMp
( VarMp' (..), VarMp
, ppVarMpV
, vmiMbTy
, varmpFilterTy, varmpDel, (|\>)
, varmpUnion, varmpUnions
, varmpTyLookupCyc
, varmpTyLookupCyc2
, module EH101.VarLookup
, VarMpInfo (..)
, mkVarMp
, emptyVarMp, varmpIsEmpty
, varmpShiftMetaLev, varmpIncMetaLev, varmpDecMetaLev
, varmpSelectMetaLev
, varmpKeys, varmpKeysSet
, assocMetaLevTyLToVarMp, assocTyLToVarMp, varmpToAssocTyL, varmpToAssocL
, varmpPlus
, instToL1VarMp
, varmpMetaLevTyUnit, varmpTyUnit
, tyRestrictKiVarMp
, varmpLookup, varmpTyLookup
, ppVarMp
, VarMpStk'
, emptyVarMpStk, varmpstkUnit
, varmpstkPushEmpty, varmpstkPop
, varmpstkToAssocL, varmpstkKeysSet
, varmpstkUnions
, varmpSize
, vmiMbImpls, vmiMbScope, vmiMbPred, vmiMbAssNm
, varmpTailAddOcc
, varmpMapThr, varmpMapThrTy
, varmpImplsUnit, assocImplsLToVarMp, varmpScopeUnit, varmpPredUnit, varmpAssNmUnit
, varmpImplsLookup, varmpScopeLookup, varmpPredLookup
, varmpImplsLookupImplsCyc, varmpImplsLookupCyc, varmpScopeLookupScopeCyc, varmpAssNmLookupAssNmCyc
, varmpPredLookup2, varmpScopeLookup2, varmpAssNmLookup2, varmpImplsLookupCyc2
, vmiMbLabel, vmiMbOffset
, varmpLabelUnit, varmpOffsetUnit
, varmpLabelLookup, varmpOffsetLookup
, varmpLabelLookupCyc, varmpLabelLookupLabelCyc
, vmiMbPredSeq
, varmpPredSeqUnit
, varmpPredSeqLookup
, varmpToMap
, varmpinfoMkVar
, ppVarMpInfoCfgTy, ppVarMpInfoDt )
where
import Data.List
import EH101.Base.Common
import EH101.Ty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import EH.Util.Pretty
import EH101.Ty.Pretty
import EH101.Error
import EH101.VarLookup
import EH101.Base.Debug
import EH.Util.Utils
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize






{-# LINE 80 "src/ehc/VarMp.chs" #-}
data VarMp' k v
  = VarMp
      { varmpMetaLev 	:: !MetaLev				-- the base meta level
      , varmpMpL 		:: [Map.Map k v]		-- for each level a map, starting at the base meta level
      }
  deriving ( Eq, Ord
           , Typeable, Data
           )

{-# LINE 93 "src/ehc/VarMp.chs" #-}
-- get the base meta level map, ignore the others
varmpToMap :: VarMp' k v -> Map.Map k v
varmpToMap (VarMp _ (m:_)) = m

{-# LINE 99 "src/ehc/VarMp.chs" #-}
mkVarMp :: Map.Map k v -> VarMp' k v
mkVarMp m = VarMp 0 [m]

{-# LINE 109 "src/ehc/VarMp.chs" #-}
emptyVarMp :: VarMp' k v
emptyVarMp = mkVarMp Map.empty

varmpIsEmpty :: VarMp' k v -> Bool
varmpIsEmpty (VarMp {varmpMpL=l}) = all Map.null l

instance VarLookupBase (VarMp' k v) k v where
  varlookupEmpty = emptyVarMp

{-# LINE 130 "src/ehc/VarMp.chs" #-}
varmpFilter :: Ord k => (k -> v -> Bool) -> VarMp' k v -> VarMp' k v
varmpFilter f (VarMp l c) = VarMp l (map (Map.filterWithKey f) c)

varmpPartition :: Ord k => (k -> v -> Bool) -> VarMp' k v -> (VarMp' k v,VarMp' k v)
varmpPartition f (VarMp l m)
  = (VarMp l p1, VarMp l p2)
  where (p1,p2) = unzip $ map (Map.partitionWithKey f) m

{-# LINE 140 "src/ehc/VarMp.chs" #-}
varmpDel :: Ord k => [k] -> VarMp' k v -> VarMp' k v
varmpDel tvL c = varmpFilter (const.not.(`elem` tvL)) c

(|\>) :: Ord k => VarMp' k v -> [k] -> VarMp' k v
(|\>) = flip varmpDel

{-# LINE 152 "src/ehc/VarMp.chs" #-}
-- shift up the level,
-- or down when negative, throwing away the lower levels
varmpShiftMetaLev :: MetaLev -> VarMp' k v -> VarMp' k v
varmpShiftMetaLev inc (VarMp mlev fm)
  | inc < 0   = let mlev' = mlev+inc in VarMp (mlev' `max` 0) (drop (- (mlev' `min` 0)) fm)
  | otherwise = VarMp (mlev+inc) fm

varmpIncMetaLev :: VarMp' k v -> VarMp' k v
varmpIncMetaLev = varmpShiftMetaLev 1

varmpDecMetaLev :: VarMp' k v -> VarMp' k v
varmpDecMetaLev = varmpShiftMetaLev (-1)

{-# LINE 167 "src/ehc/VarMp.chs" #-}
varmpSelectMetaLev :: [MetaLev] -> VarMp' k v -> VarMp' k v
varmpSelectMetaLev mlevs (VarMp mlev ms)
  = (VarMp mlev [ if l `elem` mlevs then m else Map.empty | (l,m) <- zip [mlev..] ms ])

{-# LINE 177 "src/ehc/VarMp.chs" #-}
varmpSize :: VarMp' k v -> Int
varmpSize (VarMp _ m) = sum $ map Map.size m

{-# LINE 190 "src/ehc/VarMp.chs" #-}
varmpKeys :: Ord k => VarMp' k v -> [k]
varmpKeys (VarMp _ fm) = Map.keys $ Map.unions fm

varmpKeysSet :: Ord k => VarMp' k v -> Set.Set k
varmpKeysSet (VarMp _ fm) = Set.unions $ map Map.keysSet fm

{-# LINE 213 "src/ehc/VarMp.chs" #-}
assocMetaLevTyLToVarMp :: Ord k => AssocL k (MetaLev,Ty) -> VarMp' k VarMpInfo
assocMetaLevTyLToVarMp l = varmpUnions [ varmpMetaLevTyUnit lev v t | (v,(lev,t)) <- l ]

assocTyLToVarMp :: Ord k => AssocL k Ty -> VarMp' k VarMpInfo
assocTyLToVarMp l = mkVarMp (Map.fromList $ assocLMapElt VMITy l)

varmpToAssocL :: VarMp' k i -> AssocL k i
varmpToAssocL (VarMp _ []   ) = []
varmpToAssocL (VarMp _ (l:_)) = Map.toList l

varmpToAssocTyL :: VarMp' k VarMpInfo -> AssocL k Ty
varmpToAssocTyL c = [ (v,t) | (v,VMITy t) <- varmpToAssocL c ]

{-# LINE 240 "src/ehc/VarMp.chs" #-}
infixr 7 `varmpPlus`

varmpPlus :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpPlus = (|+>) -- (VarMp l1) (VarMp l2) = VarMp (l1 `Map.union` l2)

{-# LINE 247 "src/ehc/VarMp.chs" #-}
varmpUnion :: Ord k => VarMp' k v -> VarMp' k v -> VarMp' k v
varmpUnion = varmpPlus

varmpUnions :: Ord k => [VarMp' k v] -> VarMp' k v
varmpUnions [x] = x
varmpUnions l   = foldr varmpPlus emptyVarMp l

{-# LINE 260 "src/ehc/VarMp.chs" #-}
instToL1VarMp :: [InstTo] -> VarMp
instToL1VarMp = varmpIncMetaLev . assocMetaLevTyLToVarMp . instToL1AssocL

{-# LINE 269 "src/ehc/VarMp.chs" #-}
data VarMpInfo
  = VMITy      !Ty
  | VMIImpls   !Impls
  | VMIScope   !PredScope
  | VMIPred    !Pred
  | VMIAssNm   !VarUIDHsName
  | VMILabel   !Label
  | VMIOffset  !LabelOffset
--  | VMIExts    !RowExts
  | VMIPredSeq !PredSeq
  deriving
    ( Eq, Ord, Show
    , Typeable, Data
    )

{-# LINE 294 "src/ehc/VarMp.chs" #-}
vmiMbTy      i = case i of {VMITy      x -> Just x; _ -> Nothing}

{-# LINE 304 "src/ehc/VarMp.chs" #-}
vmiMbImpls   i = case i of {VMIImpls   x -> Just x; _ -> Nothing}
vmiMbScope   i = case i of {VMIScope   x -> Just x; _ -> Nothing}
vmiMbPred    i = case i of {VMIPred    x -> Just x; _ -> Nothing}
vmiMbAssNm   i = case i of {VMIAssNm   x -> Just x; _ -> Nothing}
{-# LINE 310 "src/ehc/VarMp.chs" #-}
vmiMbLabel   i = case i of {VMILabel   x -> Just x; _ -> Nothing}
vmiMbOffset  i = case i of {VMIOffset  x -> Just x; _ -> Nothing}
{-# LINE 314 "src/ehc/VarMp.chs" #-}
vmiMbPredSeq i = case i of {VMIPredSeq x -> Just x; _ -> Nothing}

{-# LINE 328 "src/ehc/VarMp.chs" #-}
type VarMp  = VarMp' TyVarId VarMpInfo

instance Show (VarMp' k v) where
  show _ = "VarMp"

{-# LINE 340 "src/ehc/VarMp.chs" #-}
varmpFilterTy :: Ord k => (k -> Ty -> Bool) -> VarMp' k VarMpInfo -> VarMp' k VarMpInfo
varmpFilterTy f
  = varmpFilter
        (\v i -> case i of {VMITy t -> f v t ; _ -> True})

{-# LINE 351 "src/ehc/VarMp.chs" #-}
varmpTailAddOcc :: ImplsProveOcc -> Impls -> (Impls,VarMp)
varmpTailAddOcc o (Impls_Tail i os) = (t, varmpImplsUnit i t)
                                    where t = Impls_Tail i (o:os)
varmpTailAddOcc _ x                 = (x,emptyVarMp)

{-# LINE 362 "src/ehc/VarMp.chs" #-}
varmpMapThr :: (MetaLev -> TyVarId -> VarMpInfo -> thr -> (VarMpInfo,thr)) -> thr -> VarMp -> (VarMp,thr)
varmpMapThr f thr (VarMp l ms)
  = (VarMp l ms',thr')
  where (ms',thr') = foldMlev thr ms
        foldMp mlev thr fm
          = Map.foldrWithKey
              (\v i (fm,thr)
                 -> let  (i',thr') = f mlev v i thr
                    in   (Map.insert v i' fm,thr')
              )
              (Map.empty,thr) fm
        foldMlev thr ms
          = foldr
              (\(mlev,m) (ms,thr)
                -> let (m',thr') = foldMp mlev thr m
                   in  (m':ms,thr')
              )
              ([],thr) (zip [0..] ms)

varmpMapThrTy :: (MetaLev -> TyVarId -> Ty -> thr -> (Ty,thr)) -> thr -> VarMp -> (VarMp,thr)
varmpMapThrTy f
  = varmpMapThr
      (\mlev v i thr
         -> case i of
              VMITy t -> (VMITy t,thr')
                      where (t',thr') = f mlev v t thr
              _       -> (i,thr)
      )

{-# LINE 404 "src/ehc/VarMp.chs" #-}
varmpinfoMkVar :: TyVarId -> VarMpInfo -> Ty
varmpinfoMkVar v i
  = case i of
      VMITy       t -> mkTyVar v
      VMIImpls    i -> mkImplsVar v
      _             -> mkTyVar v					-- rest incomplete

{-# LINE 422 "src/ehc/VarMp.chs" #-}
varmpMetaLevTyUnit :: Ord k => MetaLev -> k -> Ty -> VarMp' k VarMpInfo
varmpMetaLevTyUnit mlev v t = VarMp mlev [Map.fromList [(v,VMITy t)]]

varmpTyUnit :: Ord k => k -> Ty -> VarMp' k VarMpInfo
varmpTyUnit = varmpMetaLevTyUnit 0

{-# LINE 435 "src/ehc/VarMp.chs" #-}
varmpImplsUnit :: ImplsVarId -> Impls -> VarMp
varmpImplsUnit v i = mkVarMp (Map.fromList [(v,VMIImpls i)])

varmpScopeUnit :: TyVarId -> PredScope -> VarMp
varmpScopeUnit v sc = mkVarMp (Map.fromList [(v,VMIScope sc)])

varmpPredUnit :: TyVarId -> Pred -> VarMp
varmpPredUnit v p = mkVarMp (Map.fromList [(v,VMIPred p)])

varmpAssNmUnit :: TyVarId -> VarUIDHsName -> VarMp
varmpAssNmUnit v p = mkVarMp (Map.fromList [(v,VMIAssNm p)])

assocImplsLToVarMp :: AssocL ImplsVarId Impls -> VarMp
assocImplsLToVarMp = mkVarMp . Map.fromList . assocLMapElt VMIImpls

{-# LINE 453 "src/ehc/VarMp.chs" #-}
varmpLabelUnit :: LabelVarId -> Label -> VarMp
varmpLabelUnit v l = mkVarMp (Map.fromList [(v,VMILabel l)])

varmpOffsetUnit :: UID -> LabelOffset -> VarMp
varmpOffsetUnit v l = mkVarMp (Map.fromList [(v,VMIOffset l)])


{-# LINE 464 "src/ehc/VarMp.chs" #-}
varmpPredSeqUnit :: TyVarId -> PredSeq -> VarMp
varmpPredSeqUnit v l = mkVarMp (Map.fromList [(v,VMIPredSeq l)])

{-# LINE 469 "src/ehc/VarMp.chs" #-}
-- restrict the kinds of tvars bound to value identifiers to kind *
tyRestrictKiVarMp :: [Ty] -> VarMp
tyRestrictKiVarMp ts = varmpIncMetaLev $ assocTyLToVarMp [ (v,kiStar) | t <- ts, v <- maybeToList $ tyMbVar t ]

{-# LINE 486 "src/ehc/VarMp.chs" #-}
varmpLookup :: (VarLookup m k i,Ord k) => k -> m -> Maybe i
varmpLookup = varlookupMap (Just . id)

varmpTyLookup :: (VarLookup m k VarMpInfo,Ord k) => k -> m -> Maybe Ty
varmpTyLookup = varlookupMap vmiMbTy

{-# LINE 494 "src/ehc/VarMp.chs" #-}
varmpImplsLookup :: VarLookup m ImplsVarId VarMpInfo => ImplsVarId -> m -> Maybe Impls
varmpImplsLookup = varlookupMap vmiMbImpls

varmpScopeLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe PredScope
varmpScopeLookup = varlookupMap vmiMbScope

varmpPredLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe Pred
varmpPredLookup = varlookupMap vmiMbPred

varmpAssNmLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe VarUIDHsName
varmpAssNmLookup = varlookupMap vmiMbAssNm

{-# LINE 508 "src/ehc/VarMp.chs" #-}
varmpLabelLookup :: VarLookup m LabelVarId VarMpInfo => LabelVarId -> m -> Maybe Label
varmpLabelLookup = varlookupMap vmiMbLabel

varmpOffsetLookup :: VarLookup m UID VarMpInfo => UID -> m -> Maybe LabelOffset
varmpOffsetLookup = varlookupMap vmiMbOffset

{-# LINE 516 "src/ehc/VarMp.chs" #-}
varmpPredSeqLookup :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe PredSeq
varmpPredSeqLookup = varlookupMap vmiMbPredSeq

{-# LINE 525 "src/ehc/VarMp.chs" #-}
instance Ord k => VarLookup (VarMp' k v) k v where
  varlookupWithMetaLev l k    (VarMp vmlev ms) = lkup (l-vmlev) ms
                                               where lkup _ []     = Nothing
                                                     lkup 0 (m:_)  = Map.lookup k m
                                                     lkup l (_:ms) = lkup (l-1) ms
  varlookup              k vm@(VarMp vmlev _ ) = varlookupWithMetaLev vmlev k vm


instance Ord k => VarLookupCmb (VarMp' k v) (VarMp' k v) where
  -- combine by taking the lowest level, adapting the lists with maps accordingly
  (VarMp l1 ms1) |+> (VarMp l2 ms2)
    = case compare l1 l2 of
        EQ -> VarMp l1 (cmb                                   ms1                                    ms2 )
        LT -> VarMp l1 (cmb                                   ms1  (replicate (l2 - l1) Map.empty ++ ms2))
        GT -> VarMp l2 (cmb (replicate (l1 - l2) Map.empty ++ ms1)                                   ms2 )
    where cmb (m1:ms1) (m2:ms2) = Map.union m1 m2 : cmb ms1 ms2
          cmb ms1      []       = ms1
          cmb []       ms2      = ms2

{-# LINE 550 "src/ehc/VarMp.chs" #-}
varmpTyLookupCyc :: VarLookup m TyVarId VarMpInfo => TyVarId -> m -> Maybe Ty
varmpTyLookupCyc x m = lookupLiftCycMb2 tyMbVar (flip varmpTyLookup m) x

{-# LINE 559 "src/ehc/VarMp.chs" #-}
varmpImplsLookupImplsCyc :: VarLookup m ImplsVarId VarMpInfo => Impls -> m -> Maybe Impls
varmpImplsLookupImplsCyc x m = lookupLiftCycMb1 implsMbVar (flip varmpImplsLookup m) x

varmpImplsLookupCyc :: VarLookup m ImplsVarId VarMpInfo => TyVarId -> m -> Maybe Impls
varmpImplsLookupCyc x m = lookupLiftCycMb2 implsMbVar (flip varmpImplsLookup m) x

varmpScopeLookupScopeCyc :: VarLookup m ImplsVarId VarMpInfo => PredScope -> m -> Maybe PredScope
varmpScopeLookupScopeCyc x m = lookupLiftCycMb1 pscpMbVar (flip varmpScopeLookup m) x

varmpAssNmLookupAssNmCyc :: VarLookup m ImplsVarId VarMpInfo => VarUIDHsName -> m -> Maybe VarUIDHsName
varmpAssNmLookupAssNmCyc x m = lookupLiftCycMb1 vunmMbVar (flip varmpAssNmLookup m) x

{-# LINE 573 "src/ehc/VarMp.chs" #-}
varmpLabelLookupLabelCyc :: VarLookup m ImplsVarId VarMpInfo => Label -> m -> Maybe Label
varmpLabelLookupLabelCyc x m = lookupLiftCycMb1 labelMbVar (flip varmpLabelLookup m) x

varmpLabelLookupCyc :: VarLookup m ImplsVarId VarMpInfo => TyVarId -> m -> Maybe Label
varmpLabelLookupCyc x m = lookupLiftCycMb2 labelMbVar (flip varmpLabelLookup m) x

{-# LINE 585 "src/ehc/VarMp.chs" #-}
varmpTyLookupCyc2 :: VarMp -> TyVarId -> Maybe Ty
varmpTyLookupCyc2 x m = varmpTyLookupCyc m x

{-# LINE 590 "src/ehc/VarMp.chs" #-}
varmpScopeLookup2 :: VarMp -> TyVarId -> Maybe PredScope
varmpScopeLookup2 m v = varmpScopeLookup v m

varmpImplsLookup2 :: VarMp -> ImplsVarId -> Maybe Impls
varmpImplsLookup2 m v = varmpImplsLookup v m

varmpImplsLookupCyc2 :: VarMp -> ImplsVarId -> Maybe Impls
varmpImplsLookupCyc2 m v = varmpImplsLookupCyc v m

varmpPredLookup2 :: VarMp -> TyVarId -> Maybe Pred
varmpPredLookup2 m v = varmpPredLookup v m

varmpAssNmLookup2 :: VarMp -> TyVarId -> Maybe VarUIDHsName
varmpAssNmLookup2 m v = varmpAssNmLookup v m

{-# LINE 607 "src/ehc/VarMp.chs" #-}
varmpLabelLookup2 :: VarMp -> LabelVarId -> Maybe Label
varmpLabelLookup2 m v = varmpLabelLookup v m

{-# LINE 616 "src/ehc/VarMp.chs" #-}
newtype VarMpStk' k v
  = VarMpStk [VarMp' k v]
  deriving (Show)

{-# LINE 622 "src/ehc/VarMp.chs" #-}
emptyVarMpStk :: VarMpStk' k v
emptyVarMpStk = VarMpStk [emptyVarMp]

varmpstkUnit :: Ord k => k -> v -> VarMpStk' k v
varmpstkUnit k v = VarMpStk [mkVarMp (Map.fromList [(k,v)])]

{-# LINE 630 "src/ehc/VarMp.chs" #-}
varmpstkPushEmpty :: VarMpStk' k v -> VarMpStk' k v
varmpstkPushEmpty (VarMpStk s) = VarMpStk (emptyVarMp : s)

varmpstkPop :: VarMpStk' k v -> (VarMpStk' k v, VarMpStk' k v)
varmpstkPop (VarMpStk (s:ss)) = (VarMpStk [s], VarMpStk ss)
varmpstkPop _                 = panic "varmpstkPop: empty"

{-# LINE 639 "src/ehc/VarMp.chs" #-}
varmpstkToAssocL :: VarMpStk' k v -> AssocL k v
varmpstkToAssocL (VarMpStk s) = concatMap varmpToAssocL s

varmpstkKeysSet :: Ord k => VarMpStk' k v -> Set.Set k
varmpstkKeysSet (VarMpStk s) = Set.unions $ map varmpKeysSet s

{-# LINE 647 "src/ehc/VarMp.chs" #-}
varmpstkUnions :: Ord k => [VarMpStk' k v] -> VarMpStk' k v
varmpstkUnions [x] = x
varmpstkUnions l   = foldr (|+>) emptyVarMpStk l

{-# LINE 653 "src/ehc/VarMp.chs" #-}
instance Ord k => VarLookup (VarMpStk' k v) k v where
  varlookupWithMetaLev l k (VarMpStk s) = varlookupWithMetaLev l k s

instance Ord k => VarLookupCmb (VarMpStk' k v) (VarMpStk' k v) where
  (VarMpStk s1) |+> (VarMpStk s2) = VarMpStk (s1 |+> s2)

{-# LINE 670 "src/ehc/VarMp.chs" #-}
ppVarMpV :: VarMp -> PP_Doc
ppVarMpV = ppVarMp vlist

{-# LINE 675 "src/ehc/VarMp.chs" #-}
ppVarMp :: (PP k, PP v) => ([PP_Doc] -> PP_Doc) -> VarMp' k v -> PP_Doc
ppVarMp ppL (VarMp mlev ms)
  = ppL [ "@" >|< pp lev >|< ":" >#< ppL [ pp n >|< ":->" >|< pp v | (n,v) <- Map.toList m]
        | (lev,m) <- zip [mlev..] ms
        ]

{-# LINE 683 "src/ehc/VarMp.chs" #-}
instance (PP k, PP v) => PP (VarMp' k v) where
  pp = ppVarMp (ppListSepFill "" "" ", ")

{-# LINE 688 "src/ehc/VarMp.chs" #-}
instance (PP k, PP v) => PP (VarMpStk' k v) where
  pp (VarMpStk s) = ppListSepFill "" "" "; " $ map pp s

{-# LINE 693 "src/ehc/VarMp.chs" #-}
ppVarMpInfoCfgTy :: CfgPPTy -> VarMpInfo -> PP_Doc
ppVarMpInfoCfgTy c i
  = case i of
      VMITy       t -> ppTyWithCfg    c t
      VMIImpls    i -> ppImplsWithCfg c i
      VMIScope    s -> pp s						-- rest incomplete
      VMIPred     p -> pp p
      VMILabel    x -> pp x
      VMIOffset   x -> pp x
      VMIPredSeq  x -> pp "predseq" -- pp x

ppVarMpInfoDt :: VarMpInfo -> PP_Doc
ppVarMpInfoDt = ppVarMpInfoCfgTy cfgPPTyDT

{-# LINE 709 "src/ehc/VarMp.chs" #-}
instance PP VarMpInfo where
  pp (VMITy       t) = pp t
  pp (VMIImpls    i) = pp i
  pp (VMIScope    s) = pp s
  pp (VMIPred     p) = pp p
  pp (VMILabel    x) = pp x
  pp (VMIOffset   x) = pp x
  -- pp (VMIExts     x) = pp "exts" -- pp x
  pp (VMIPredSeq  x) = pp "predseq" -- pp x

{-# LINE 731 "src/ehc/VarMp.chs" #-}
instance Serialize VarMpInfo where
  sput (VMITy      a) = sputWord8 0  >> sput a
  sput (VMIImpls   a) = sputWord8 1  >> sput a
  sput (VMIScope   a) = sputWord8 2  >> sput a
  sput (VMIPred    a) = sputWord8 3  >> sput a
  sput (VMIAssNm   a) = sputWord8 4  >> sput a
  sput (VMILabel   a) = sputWord8 5  >> sput a
  sput (VMIOffset  a) = sputWord8 6  >> sput a
  sput (VMIPredSeq a) = sputWord8 7  >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM VMITy      sget
              1 -> liftM VMIImpls   sget
              2 -> liftM VMIScope   sget
              3 -> liftM VMIPred    sget
              4 -> liftM VMIAssNm   sget
              5 -> liftM VMILabel   sget
              6 -> liftM VMIOffset  sget
              7 -> liftM VMIPredSeq sget

instance (Ord k, Serialize k, Serialize v) => Serialize (VarMp' k v) where
  sput (VarMp a b) = sput a >> sput b
  sget = liftM2 VarMp sget sget

