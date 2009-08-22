%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared structures for fitsIn and related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(1 hmtyinfer) module {%{EH}Ty.FitsInCommon} import({%{EH}Base.Common}, {%{EH}Ty}, {%{EH}Error}) export (FIOut(..), emptyFO, foHasErrs)
%%]

%%[(1 hmtyinfer) import(qualified EH.Util.FastSeq as Seq)
%%]

%%[(2 hmtyinfer) import({%{EH}VarMp})
%%]

%%[(4 hmtyinfer) import({%{EH}Base.Opts})
%%]

%%[(4 hmtyinfer) import({%{EH}Substitutable}) export(FitsIn, FitsIn',fitsInLWith)
%%]

%%[(8 codegen hmtyinfer) import(qualified {%{EH}TyCore.Full0} as C)
%%]

%%[(9 hmtyinfer) import(qualified Data.Set as Set)
%%]

%%[(9 hmtyinfer) import({%{EH}Pred.CommonCHR})
%%]

%%[(9 codegen hmtyinfer) import({%{EH}Core},{%{EH}Core.Coercion},{%{EH}Core.Subst})
%%]

For debug/trace:
%%[(4 hmtyinfer) import(EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing info, specialized  for fitsIn and related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(trfit,trfitIn,trfitOu)
trfit :: String -> String -> PP_Doc -> PP_Doc
trfit dir msg rest =  dir >|< "." >|< msg >|< ":" >#< rest

trfitIn = trfit ">"
trfitOu = trfit "<"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to result/output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(1 hmtyinfer).FIOut
data FIOut  =   FIOut   { foTy     ::  Ty      ,  foErrL   ::  ErrL    }

emptyFO     =   FIOut   { foTy     =   Ty_Any  ,  foErrL   =   []      }
%%]

%%[(1 hmtyinfer) export(foErrSq)
foErrSq :: FIOut -> ErrSq
foErrSq = Seq.fromList . foErrL
%%]

%%[(2 hmtyinfer).FIOut -1.FIOut
data FIOut  =  FIOut  {  foTy     ::  Ty      ,  foErrL   ::  ErrL  ,  foVarMp           ::  VarMp           }
%%]

%%[(2 hmtyinfer).FIOut.empty
emptyFO     =  FIOut  {  foTy     =   Ty_Any  ,  foErrL   =   []    ,  foVarMp           =   emptyVarMp      }
%%]

%%[(4 hmtyinfer).FIOut -(2.FIOut 2.FIOut.empty)
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
%%[[(8 codegen)
       ,  foTCSubst         :: !(C.CSubst)				-- subst for holes in the Core
       ,  foLRTCoe          :: !(C.LRCoe)				-- coercion over arrow structure
%%]]
%%[[(9 codegen)
       ,  foCSubst          :: !CSubst					-- subst for holes in the Core
       ,  foLRCoe           :: !LRCoe					-- coercion over arrow structure
%%]]
%%[[9
       ,  foPredOccL        :: ![PredOcc]				-- arisen predicates (to be obsolete)
       ,  foGathCnstrMp     :: !CHRPredOccCnstrMp		-- arisen predicates
%%]]
%%[[(10 codegen)
       ,  foRowCoeL         :: !(AssocL HsName Coe)		-- internal, coercions for row fields
       ,  foRowTCoeL        :: !(AssocL HsName C.Coe)	-- 
%%]]
%%[[50
       ,  foEqVarMp         :: !VarMp
%%]]
%%[[99
       -- top binding -> format (for DT) -> final inference VarMp -> threaded pretty print tyvar VarMp
       --   -> (rule, threaded ...)
       ,  foMkDT            :: Maybe PP_Doc -> String -> VarMp -> VarMp -> (PP_Doc,VarMp)
%%][100
%%]]
       }
%%]

%%[(4 hmtyinfer).emptyFO
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
%%[[6
       -- ,  foTvKiVarMp       =   emptyVarMp
%%]]
%%[[(8 codegen)
       ,  foTCSubst         =   C.emptyCSubst
       ,  foLRTCoe          =   C.emptyLRCoe
%%]]
%%[[(9 codegen)
       ,  foCSubst          =   emptyCSubst
       ,  foLRCoe           =   emptyLRCoe
%%]]
%%[[9
       ,  foPredOccL        =   []
       ,  foGathCnstrMp     =   emptyCnstrMp
%%]]
%%[[(10 codegen)
       ,  foRowCoeL         =   []
       ,  foRowTCoeL        =   []
%%]]
%%[[50
       ,  foEqVarMp         =   emptyVarMp
%%]]
%%[[99
       ,  foMkDT            =   \_ _ m dm -> (empty,dm)
%%][100
%%]]
       }
%%]

%%[(1 hmtyinfer).foHasErrs
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL
%%]

%%[(4 hmtyinfer) export(foAppSpineInfo)
foAppSpineInfo :: FIOut -> AppSpineInfo
foAppSpineInfo fo = maybe emptyAppSpineInfo id $ foMbAppSpineInfo fo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bind type var
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(foPlusVarMp,foSetVarMp,foBindTyVar)
foPlusVarMp :: VarMp -> FIOut -> FIOut
foPlusVarMp c fo = fo {foVarMp = c |+> foVarMp fo}

foSetVarMp :: VarMp -> FIOut -> FIOut
foSetVarMp  c fo = fo {foVarMp = c}

foBindTyVar :: TyVarId -> Ty -> FIOut -> FIOut
foBindTyVar v t = foPlusVarMp (v `varmpTyUnit` t)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer) export(AppSpineFOUpdCoe)
type AppSpineFOUpdCoe = EHCOpts -> [FIOut] -> FIOut
%%]

%%[(4 hmtyinfer).AppSpine export(AppSpineVertebraeInfo(..))
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asPolarity     :: Polarity						-- the polarity on this spine position
       , asFIO          :: FIOpts -> FIOpts				-- how to update the context (swap ...)
       , asFO			:: FIOut -> FIOut -> FIOut		-- \ffo afo -> afo, update app function arg FIOut with app function FIOut
%%[[(8 codegen)
       , asMbFOUpdCoe   :: Maybe AppSpineFOUpdCoe		-- possibly update coercion
%%]]
       }
%%]

%%[(4 hmtyinfer)
instance Show AppSpineVertebraeInfo where
  show _ = "AppSpineVertebraeInfo"

instance PP AppSpineVertebraeInfo where
  pp = pp . asPolarity
%%]

%%[(4 hmtyinfer) export(unknownAppSpineVertebraeInfo)
unknownAppSpineVertebraeInfo :: AppSpineVertebraeInfo
unknownAppSpineVertebraeInfo
  = AppSpineVertebraeInfo
      polInvariant fioMkUnify
      asFODflt
%%[[(8 codegen)
      Nothing
%%]]
%%]

%%[(4 hmtyinfer)
asFODflt :: FIOut -> FIOut -> FIOut
asFODflt _ afo = afo
%%]

%%[(4 hmtyinfer) export(unknownAppSpineVertebraeInfoL)
unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat unknownAppSpineVertebraeInfo
%%]

%%[(17 hmtyinfer) export(asUpdateByPolarity)
asUpdateByPolarity :: Polarity -> AppSpineVertebraeInfo -> AppSpineVertebraeInfo
asUpdateByPolarity pol as
  = as {asPolarity = pol, asFIO = mkfio}
  where mkfio | polIsContravariant pol = fioMkStrong
              | polIsCovariant     pol = id
              | otherwise              = fioMkUnify
%%]

%%[(4 hmtyinfer).AppSpineGam export(AppSpineInfo(asgiVertebraeL), emptyAppSpineInfo, asgiShift1SpinePos, asgiSpine)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wrapper around fitsIn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).FitsIn
type FitsIn' = FIOpts -> UID -> VarMp -> Ty -> Ty -> FIOut
type FitsIn = FIOpts -> UID -> VarMp -> Ty -> Ty -> (Ty,VarMp,ErrL)
%%]

%%[(4 hmtyinfer).fitsInLWith
fitsInLWith :: (FIOut -> FIOut -> FIOut) -> FitsIn' -> FIOpts -> UID -> VarMp -> TyL -> TyL -> (FIOut,[FIOut])
fitsInLWith foCmb elemFits opts uniq varmp tyl1 tyl2
  = (fo,foL)
  where ((_,fo),foL)
          = foldr  (\(t1,t2) ((u,foThr),foL)
                      -> let  (u',ue) = mkNewLevUID u
                              fo = elemFits opts u (foVarMp foThr |=> varmp) (foVarMp foThr |=> t1) (foVarMp foThr |=> t2)
                         in   ((u',foCmb fo foThr),fo:foL)
                   )
                   ((uniq,emptyFO),[])
            . zip tyl1
            $ tyl2
%%]

