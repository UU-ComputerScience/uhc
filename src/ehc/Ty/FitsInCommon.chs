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
data FIOut  =  FIOut    {  foVarMp           :: !VarMp               ,  foTy              :: !Ty
                        ,  foUniq            :: !UID                 ,  foMbAppSpineInfo  :: !(Maybe AppSpineInfo)
                        ,  foErrL            :: !ErrL                ,  foTrace           :: [PP_Doc]
%%[[(9 codegen)
                        ,  foCSubst          :: !CSubst              ,  foLRCoe           :: !LRCoe
%%]]
%%[[9
                        ,  foPredOccL        :: ![PredOcc]
                        ,  foGathCnstrMp     :: !CHRPredOccCnstrMp
%%]]
%%[[(10 codegen)
                        ,  foRowCoeL         :: !(AssocL HsName Coe)
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
emptyFO     =  FIOut    {  foVarMp           =   emptyVarMp          ,  foTy              =   Ty_Any
                        ,  foUniq            =   uidStart            ,  foMbAppSpineInfo  =   Nothing
                        ,  foErrL            =   []                  ,  foTrace           =   []
%%[[(9 codegen)
                        ,  foCSubst          =   emptyCSubst         ,  foLRCoe           =   emptyLRCoe
%%]]
%%[[9
                        ,  foPredOccL        =   []
                        ,  foGathCnstrMp     =   emptyCnstrMp
%%]]
%%[[(10 codegen)
                        ,  foRowCoeL         =   []
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

%%[(9 hmtyinfer) export(AppSpineFOUpdCoe)
type AppSpineFOUpdCoe = EHCOpts -> [FIOut] -> FIOut
%%]

%%[(4 hmtyinfer).AppSpine export(AppSpineVertebraeInfo(..), unknownAppSpineVertebraeInfoL, arrowAppSpineVertebraeInfoL, prodAppSpineVertebraeInfoL)
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asPolarity     :: Polarity
       , asFIO          :: FIOpts -> FIOpts
%%[[(9 codegen)
       , asMbFOUpdCoe   :: Maybe AppSpineFOUpdCoe
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
%%[[(9 codegen)
      Nothing
%%]]
%%]

%%[(9 codegen hmtyinfer) export(asFOUpdCoe)
dfltFOUpdCoe :: AppSpineFOUpdCoe
dfltFOUpdCoe _ x = last x

asFOUpdCoe :: AppSpineVertebraeInfo -> AppSpineFOUpdCoe
asFOUpdCoe = maybe dfltFOUpdCoe id . asMbFOUpdCoe
%%]

%%[(4 hmtyinfer)
unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat unknownAppSpineVertebraeInfo
%%]

%%[(4 hmtyinfer).vertebraeInfoL
arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL = [AppSpineVertebraeInfo polContravariant fioMkStrong, AppSpineVertebraeInfo polCovariant id]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL = repeat $ AppSpineVertebraeInfo polCovariant id
%%]

%%[(9 hmtyinfer).vertebraeInfoL -4.vertebraeInfoL
arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL
  = [ AppSpineVertebraeInfo polContravariant fioMkStrong
%%[[(9 codegen)
          (Just dfltFOUpdCoe)
%%]]
    , AppSpineVertebraeInfo polCovariant id
%%[[(9 codegen)
          (Just (\opts [ffo,afo]
                  -> let (u',u1) = mkNewUID (foUniq afo)
                         -- c = lrcoeForLamTyApp opts u1 (foCSubst afo) (foLRCoe ffo) (foLRCoe afo)
                         (c,s) = lrcoeForLamTyAppAsSubst opts u1 (foLRCoe ffo) (foLRCoe afo)
                     in  afo { foLRCoe = c, foUniq = u', foCSubst = foCSubst afo `cSubstApp` s }
          )     )
%%]]
    ]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL
  = repeat
    $ AppSpineVertebraeInfo polCovariant id
%%[[(9 codegen)
          (Just dfltFOUpdCoe)
%%]]
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

