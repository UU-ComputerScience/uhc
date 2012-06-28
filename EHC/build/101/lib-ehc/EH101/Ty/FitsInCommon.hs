module EH101.Ty.FitsInCommon
( FIOut (..), emptyFO, foHasErrs
, foErrSq
, FitsIn, FitsIn'
, trfit, trfitIn, trfitOu
, foAppSpineInfo
, foPlusVarMp, foSetVarMp, foBindTyVar
, AppSpineVertebraeInfo (..)
, unknownAppSpineVertebraeInfo
, unknownAppSpineVertebraeInfoL
, AppSpineInfo (asgiVertebraeL), emptyAppSpineInfo, asgiShift1SpinePos, asgiSpine
, fitsInLWith
, AppSpineFOUpdCoe
, asUpdateByPolarity )
where
import EH101.Base.Common
import EH101.Ty
import EH101.Error
import qualified EH.Util.FastSeq as Seq
import EH101.VarMp
import EH101.Opts
import EH101.Substitutable
import EH.Util.Pretty
import qualified Data.Set as Set
import EH101.AbstractCore
import EH101.Pred.CommonCHR
import EH101.Core
import EH101.Core.Coercion
import EH101.Core.Subst








{-# LINE 48 "src/ehc/Ty/FitsInCommon.chs" #-}
trfit :: String -> String -> PP_Doc -> PP_Doc
trfit dir msg rest =  dir >|< "." >|< msg >|< ":" >#< rest

trfitIn = trfit ">"
trfitOu = trfit "<"

{-# LINE 66 "src/ehc/Ty/FitsInCommon.chs" #-}
foErrSq :: FIOut -> ErrSq
foErrSq = Seq.fromList . foErrL

{-# LINE 79 "src/ehc/Ty/FitsInCommon.chs" #-}
data FIOut
  =  FIOut
       {  foVarMp           :: !VarMp					-- tvar bindings found during fitsIn
       ,  foTy              :: !Ty						-- the unified type
       ,  foUniq            :: !UID						-- uniq value seed for fresh tvars
       ,  foMbAppSpineInfo  :: !(Maybe AppSpineInfo)	-- Ty_App spine info
       ,  foErrL            :: !ErrL					-- errors
       ,  foTrace           :: [PP_Doc]					-- trace
       ,  foLInstToL        :: [InstTo]					-- instantiation over arrow '->' of left ty
       ,  foRInstToL        :: [InstTo]					-- instantiation over arrow '->' of right ty
       ,  foDontBind        :: !TyVarIdS				-- output variant of fioDontBind
       ,  foCSubst          :: !CSubst					-- subst for holes in the Core
       ,  foLRCoe           :: !LRCoe					-- coercion over arrow structure
       ,  foPredOccL        :: ![PredOcc]				-- arisen predicates (to be obsolete)
       ,  foGathCnstrMp     :: !CHRPredOccCnstrMp		-- arisen predicates
       ,  foRowCoeL         :: !(AssocL HsName Coe)		-- internal, coercions for row fields
       }

{-# LINE 124 "src/ehc/Ty/FitsInCommon.chs" #-}
emptyFO
  =  FIOut
       {  foVarMp           =   emptyVarMp
       ,  foTy              =   Ty_Any
       ,  foUniq            =   uidStart
       ,  foMbAppSpineInfo  =   Nothing
       ,  foErrL            =   []
       ,  foTrace           =   []
       ,  foLInstToL        =   []
       ,  foRInstToL        =   []
       ,  foDontBind        =	Set.empty
       ,  foCSubst          =   emptyCSubst
       ,  foLRCoe           =   emptyLRCoe
       ,  foPredOccL        =   []
       ,  foGathCnstrMp     =   emptyCnstrMp
       ,  foRowCoeL         =   []
       }

{-# LINE 166 "src/ehc/Ty/FitsInCommon.chs" #-}
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL

{-# LINE 171 "src/ehc/Ty/FitsInCommon.chs" #-}
foAppSpineInfo :: FIOut -> AppSpineInfo
foAppSpineInfo fo = maybe emptyAppSpineInfo id $ foMbAppSpineInfo fo

{-# LINE 180 "src/ehc/Ty/FitsInCommon.chs" #-}
foPlusVarMp :: VarMp -> FIOut -> FIOut
foPlusVarMp c fo = fo {foVarMp = c |+> foVarMp fo}

foSetVarMp :: VarMp -> FIOut -> FIOut
foSetVarMp  c fo = fo {foVarMp = c}

foBindTyVar :: TyVarId -> Ty -> FIOut -> FIOut
foBindTyVar v t = foPlusVarMp (v `varmpTyUnit` t)

{-# LINE 195 "src/ehc/Ty/FitsInCommon.chs" #-}
type AppSpineFOUpdCoe = EHCOpts -> [FIOut] -> FIOut

{-# LINE 199 "src/ehc/Ty/FitsInCommon.chs" #-}
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asPolarity     :: Polarity						-- the polarity on this spine position
       , asFIO          :: FIOpts -> FIOpts				-- how to update the context (swap ...)
       , asFO			:: FIOut -> FIOut -> FIOut		-- \ffo afo -> afo, update app function arg FIOut with app function FIOut
       , asMbFOUpdCoe   :: Maybe AppSpineFOUpdCoe		-- possibly update coercion
       }

{-# LINE 211 "src/ehc/Ty/FitsInCommon.chs" #-}
instance Show AppSpineVertebraeInfo where
  show _ = "AppSpineVertebraeInfo"

instance PP AppSpineVertebraeInfo where
  pp = pp . asPolarity

{-# LINE 219 "src/ehc/Ty/FitsInCommon.chs" #-}
unknownAppSpineVertebraeInfo :: AppSpineVertebraeInfo
unknownAppSpineVertebraeInfo
  = AppSpineVertebraeInfo
      polInvariant fioMkUnify
      asFODflt
      Nothing

{-# LINE 230 "src/ehc/Ty/FitsInCommon.chs" #-}
asFODflt :: FIOut -> FIOut -> FIOut
asFODflt _ afo = afo

{-# LINE 235 "src/ehc/Ty/FitsInCommon.chs" #-}
unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat unknownAppSpineVertebraeInfo

{-# LINE 240 "src/ehc/Ty/FitsInCommon.chs" #-}
asUpdateByPolarity :: Polarity -> AppSpineVertebraeInfo -> AppSpineVertebraeInfo
asUpdateByPolarity pol as
  = as {asPolarity = pol, asFIO = mkfio}
  where mkfio | polIsContravariant pol = fioMkStrong
              | polIsCovariant     pol = id
              | otherwise              = fioMkUnify

{-# LINE 249 "src/ehc/Ty/FitsInCommon.chs" #-}
data AppSpineInfo
  = AppSpineInfo
      { asgiSpinePos   :: Int
      , asgiVertebraeL :: [AppSpineVertebraeInfo]
      }

instance Show AppSpineInfo where
  show _ = "AppSpineInfo"

instance PP AppSpineInfo where
  pp i = ppBracketsCommas (take 5 $ asgiVertebraeL i) >|< "@" >|< asgiSpinePos i

emptyAppSpineInfo :: AppSpineInfo
emptyAppSpineInfo = AppSpineInfo 0 unknownAppSpineVertebraeInfoL

asgiShift1SpinePos :: AppSpineInfo -> AppSpineInfo
asgiShift1SpinePos i = i {asgiSpinePos = asgiSpinePos i + 1}

asgiSpine :: AppSpineInfo -> [AppSpineVertebraeInfo]
asgiSpine i = drop (asgiSpinePos i) $ asgiVertebraeL i

{-# LINE 276 "src/ehc/Ty/FitsInCommon.chs" #-}
type FitsIn' = FIOpts -> UID -> VarMp -> Ty -> Ty -> FIOut
type FitsIn = FIOpts -> UID -> VarMp -> Ty -> Ty -> (Ty,VarMp,ErrL)

{-# LINE 281 "src/ehc/Ty/FitsInCommon.chs" #-}
fitsInLWith :: (FIOut -> FIOut -> FIOut) -> FitsIn' -> FIOpts -> UID -> VarMp -> TyL -> TyL -> ([FIOut],FIOut)
fitsInLWith foCmb elemFits opts uniq varmp tyl1 tyl2
  = (foL,fo)
  where ((_,fo),foL)
          = foldr  (\(t1,t2) ((u,foThr),foL)
                      -> let  (u',ue) = mkNewLevUID u
                              fo = elemFits opts ue (foVarMp foThr `varUpd` varmp) (foVarMp foThr `varUpd` t1) (foVarMp foThr `varUpd` t2)
                         in   ((u',foCmb fo foThr),fo:foL)
                   )
                   ((uniq,emptyFO),[])
            . zip tyl1
            $ tyl2

