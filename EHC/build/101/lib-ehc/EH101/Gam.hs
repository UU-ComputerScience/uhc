module EH101.Gam
( module EH101.Gam.Base
, ppGam, ppGamDup
, IdDefOccGam, IdDefOccAsc
, gamDoTyWithVarMp
, SoGam, SoGamInfo (..)
, initSoGam
, idDefOccGamPartitionByKind
, idDefOccGamByKind
, idDefOccGamStrip
, IdQualGam
, idGam2QualGam, idQualGamReplacement )
where
import EH101.Gam.Base
import Data.List
import EH.Util.Utils
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.NameAspect
import EH101.Error
import EH.Util.Pretty
import EH101.Ty.Pretty
import qualified Data.Set as Set
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty
import EH101.Opts.Base
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.Core
import EH101.Gam.ScopeMapGam
import EH101.Base.Debug
import EH101.Core.Subst
import EH101.VarLookup















{-# LINE 95 "src/ehc/Gam.chs" #-}
-- Do something with each Ty in a Gam.
-- The global VarMp is kept separately so a new tyvar binding can be computed, which is threaded separatedly and also returned.
-- This allows the retainment of the original tyvar in the Gam, which is required when used twice with different VarMp's.
gamDoTyWithVarMp
  :: Ord key =>
     (info -> Ty,Ty -> info -> info)									-- get/set from/into info in Gam
     																	-- do whatever must be done given
     -> (key															-- 	 name in gam
         -> (Ty,VarMp)													--   Ty + cycles
         -> VarMp														--   new subst
         -> thr															--   thread
         -> (Ty,VarMp,thr))												--   result: new Ty, new subst, thread
     -> VarMp															-- subst for Gam entries
     -> thr																-- initial value for thread
     -> Gam key info													-- the Gam (env)
     -> (Gam key info,VarMp,thr)										-- result: new gam, new subst, thread
gamDoTyWithVarMp (get,set) f gamVarMp thr gam
  = (g,c,thr')
  where (g,(thr',c))
           = gamMapThr
               (\(n,gi) (thr,c)
                   -> let t = get gi
                          (t',c',thr') = f n (gamVarMp `varUpdCyc` t) c thr
                          (tg,cg)      = case (tyUnAnn t,tyUnAnn t') of
                                           (Ty_Var v1 _  ,Ty_Var v2 _) | v1 == v2
                                             -> dflt
                                           (Ty_Var v  cat,_          ) | not (tvCatIsFixed cat)
                                             -> (t ,v `varmpTyUnit` t')
                                           _ -> dflt
                                       where dflt = (t',emptyVarMp)
                      in  ((n,set ({- tr "gamDoTyWithVarMp.set" (ppTy tg) $ -} tg) gi),(thr',{- (\v -> tr "gamDoTyWithVarMp" (pp v) v) $ -} cg `varmpPlus` c'))
               )
               (thr,emptyVarMp) gam

{-# LINE 143 "src/ehc/Gam.chs" #-}
data SoGamInfo
  = SoGamInfo
      { sgiSo :: Ty }
      deriving Show

type SoGam = Gam HsName SoGamInfo

{-# LINE 156 "src/ehc/Gam.chs" #-}
type IdDefOccGam = Gam    IdOcc  IdDefOcc
type IdDefOccAsc = AssocL IdOcc [IdDefOcc]

{-# LINE 161 "src/ehc/Gam.chs" #-}
idDefOccGamPartitionByKind :: [IdOccKind] -> IdDefOccGam -> (IdDefOccAsc,IdDefOccAsc)
idDefOccGamPartitionByKind ks
  = partition (\(IdOcc n k',_) -> k' `elem` ks) . gamToAssocDupL

{-# LINE 167 "src/ehc/Gam.chs" #-}
idDefOccGamByKind :: IdOccKind -> IdDefOccGam -> AssocL HsName IdDefOcc
idDefOccGamByKind k g = [ (n,head i) | (IdOcc n _,i) <- fst (idDefOccGamPartitionByKind [k] g) ]

{-# LINE 172 "src/ehc/Gam.chs" #-}
-- | Strip references to original source file location
idDefOccGamStrip :: IdDefOccGam -> IdDefOccGam
idDefOccGamStrip g = gamMap (\(k,v) -> (k,doccStrip v)) g

{-# LINE 193 "src/ehc/Gam.chs" #-}
type IdQualGam = Gam IdOcc HsName

{-# LINE 197 "src/ehc/Gam.chs" #-}
idGam2QualGam :: IdDefOccGam -> IdQualGam
idGam2QualGam = gamMap (\(iocc,docc) -> (iocc {ioccNm = hsnQualified $ ioccNm iocc},ioccNm $ doccOcc $ docc))

idQualGamReplacement :: IdQualGam -> IdOccKind -> HsName -> HsName
idQualGamReplacement g k n = maybe n id $ gamLookup (IdOcc n k) g

{-# LINE 221 "src/ehc/Gam.chs" #-}
instance (Ord tk,VarUpdatable vv subst) => VarUpdatable (SGam tk vv) subst where
  s `varUpd`  g    =   gamMapElts (s `varUpd`) g
  s `varUpdCyc` g    =   (g',varmpUnions $ gamElts gm)
              where (g',gm) = sgamUnzip $ gamMapElts (s `varUpdCyc`) g

instance (Ord tk,Ord k,VarExtractable vv k) => VarExtractable (SGam tk vv) k where
  varFreeSet g    =   Set.unions $ map varFreeSet $ gamElts g

{-# LINE 237 "src/ehc/Gam.chs" #-}
ppGam :: (Ord k, PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)

{-# LINE 242 "src/ehc/Gam.chs" #-}
ppGamDup :: (Ord k,PP k, PP v) => Gam k v -> PP_Doc
ppGamDup g = ppAssocL $ map (\(k,v) -> (k,ppBracketsCommas v)) $ gamToAssocDupL $ g

{-# LINE 252 "src/ehc/Gam.chs" #-}
instance (Ord k, PP k, PP v) => PP (SGam k v) where
  pp g = ppGam g

{-# LINE 281 "src/ehc/Gam.chs" #-}
initSoGam :: SoGam
initSoGam
  = assocLToGam
      [ (hsnKindStar,   SoGamInfo kiStar)
      ]

