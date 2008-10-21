%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared structures for fitsIn and related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Ty.FitsInCommon} import({%{EH}Base.Common}, {%{EH}Ty}, {%{EH}Error}) export (FIOut(..), emptyFO, foHasErrs)
%%]

%%[1 import(qualified EH.Util.FastSeq as Seq)
%%]

%%[2 import({%{EH}VarMp})
%%]

%%[4 import({%{EH}Base.Opts})
%%]

%%[4 import({%{EH}Substitutable}) export(FitsIn, FitsIn',fitsInLWith)
%%]

%%[9 import(qualified Data.Set as Set)
%%]

%%[9 import({%{EH}Pred.CommonCHR})
%%]

%%[(9 codegen) import({%{EH}Core},{%{EH}Core.Coercion},{%{EH}Core.Subst})
%%]

For debug/trace:
%%[4 import(EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing info, specialized  for fitsIn and related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 export(trfit,trfitIn,trfitOu)
trfit :: String -> String -> PP_Doc -> PP_Doc
trfit dir msg rest =  dir >|< "." >|< msg >|< ":" >#< rest

trfitIn = trfit ">"
trfitOu = trfit "<"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to result/output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.FIOut
data FIOut  =   FIOut   { foTy     ::  Ty      ,  foErrL   ::  ErrL    }

emptyFO     =   FIOut   { foTy     =   Ty_Any  ,  foErrL   =   []      }
%%]

%%[1 export(foErrSq)
foErrSq :: FIOut -> ErrSq
foErrSq = Seq.fromList . foErrL
%%]

%%[2.FIOut -1.FIOut
data FIOut  =  FIOut  {  foTy     ::  Ty      ,  foErrL   ::  ErrL  ,  foVarMp           ::  VarMp           }
%%]

%%[2.FIOut.empty
emptyFO     =  FIOut  {  foTy     =   Ty_Any  ,  foErrL   =   []    ,  foVarMp           =   emptyVarMp      }
%%]

%%[4.FIOut -(2.FIOut 2.FIOut.empty)
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

%%[4.emptyFO
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

%%[1.foHasErrs
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL
%%]

%%[4 export(foAppSpineInfo)
foAppSpineInfo :: FIOut -> AppSpineInfo
foAppSpineInfo fo = maybe emptyAppSpineInfo id $ foMbAppSpineInfo fo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bind type var
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 export(foPlusVarMp,foSetVarMp,foBindTyVar)
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

%%[9 export(AppSpineFOUpdCoe)
type AppSpineFOUpdCoe = EHCOpts -> [FIOut] -> FIOut
%%]

%%[4.AppSpine export(AppSpineVertebraeInfo(..), unknownAppSpineVertebraeInfoL, arrowAppSpineVertebraeInfoL, prodAppSpineVertebraeInfoL)
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asPolarity     :: Polarity
       , asFIO          :: FIOpts -> FIOpts
%%[[(9 codegen)
       , asMbFOUpdCoe   :: Maybe AppSpineFOUpdCoe
%%]]
       }
%%]

%%[(9 codegen) export(asFOUpdCoe)
dfltFOUpdCoe :: AppSpineFOUpdCoe
dfltFOUpdCoe _ x = last x

asFOUpdCoe :: AppSpineVertebraeInfo -> AppSpineFOUpdCoe
asFOUpdCoe = maybe dfltFOUpdCoe id . asMbFOUpdCoe
%%]

%%[4.vertebraeInfoL
unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat (AppSpineVertebraeInfo polInvariant fioMkUnify)

arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL = [AppSpineVertebraeInfo polContravariant fioMkStrong, AppSpineVertebraeInfo polCovariant id]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL = repeat $ AppSpineVertebraeInfo polCovariant id
%%]

%%[9.vertebraeInfoL -4.vertebraeInfoL
unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL
  = repeat
    $ AppSpineVertebraeInfo polInvariant fioMkUnify
%%[[(9 codegen)
          Nothing
%%]]

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
                         c = lrcoeForLamTyApp opts u1 (foCSubst afo) (foLRCoe ffo) (foLRCoe afo)
                     in  afo { foLRCoe = c, foUniq = u' }
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

%%[17 export(asUpdateByPolarity)
asUpdateByPolarity :: Polarity -> AppSpineVertebraeInfo -> AppSpineVertebraeInfo
asUpdateByPolarity pol as
  = as {asPolarity = pol, asFIO = mkfio}
  where mkfio | polIsContravariant pol = fioMkStrong
              | polIsCovariant     pol = id
              | otherwise              = fioMkUnify
%%]

%%[4.AppSpineGam export(AppSpineInfo(asgiVertebraeL), emptyAppSpineInfo, asgiShift1SpinePos, asgiSpine)
data AppSpineInfo
  = AppSpineInfo
      { asgiSpinePos   :: Int
      , asgiVertebraeL :: [AppSpineVertebraeInfo]
      }

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

%%[4.FitsIn
type FitsIn' = FIOpts -> UID -> VarMp -> Ty -> Ty -> FIOut
type FitsIn = FIOpts -> UID -> VarMp -> Ty -> Ty -> (Ty,VarMp,ErrL)
%%]

%%[4.fitsInLWith
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

