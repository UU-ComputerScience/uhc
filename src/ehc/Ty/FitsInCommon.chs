%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption (fitting in) for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Ty.FitsInCommon} import({%{EH}Base.Common}, {%{EH}Ty}, {%{EH}Error}) export (FIOut(..), emptyFO, foHasErrs)
%%]

%%[1 import(qualified EH.Util.FastSeq as Seq)
%%]

%%[2 import({%{EH}VarMp})
%%]

%%[4 import({%{EH}Base.Opts}) export(AppSpineVertebraeInfo(..), unknownAppSpineVertebraeInfoL, arrowAppSpineVertebraeInfoL, prodAppSpineVertebraeInfoL)
%%]

%%[4 import({%{EH}Substitutable}) export(FitsIn, FitsIn',fitsInLWith)
%%]

%%[9 import({%{EH}Core},{%{EH}Core.Subst})
%%]

%%[9 import({%{EH}Pred.CommonCHR})
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
                        ,  foErrL            :: !ErrL  
%%]
%%[9
                        ,  foCSubst          :: !CSubst              ,  foPredOccL        :: ![PredOcc]
                        ,  foLRCoe           :: !LRCoe
                        ,  foGathCnstrMp     :: !CHRPredOccCnstrMp
%%]
%%[10
                        ,  foRowCoeL         :: !(AssocL HsName Coe)
%%]
%%[50
                        ,  foEqVarMp         :: !VarMp
%%]
%%[4.FIOut.tl
                        }
%%]

%%[4.emptyFO
emptyFO     =  FIOut    {  foVarMp           =   emptyVarMp          ,  foTy              =   Ty_Any
                        ,  foUniq            =   uidStart            ,  foMbAppSpineInfo  =   Nothing
                        ,  foErrL            =   []         
%%]
%%[9
                        ,  foCSubst          =   emptyCSubst         ,  foPredOccL        =   []
                        ,  foLRCoe           =   emptyLRCoe
                        ,  foGathCnstrMp     =   emptyCnstrMp
%%]
%%[10
                        ,  foRowCoeL         =   []
%%]
%%[50
                        ,  foEqVarMp         =   emptyVarMp
%%]
%%[4.emptyFO.tl
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
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.AppSpine
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asCoCo         :: CoContraVariance
       , asFIO          :: FIOpts -> FIOpts
       }

unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat (AppSpineVertebraeInfo CoContraVariant fioMkUnify)

arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL = [AppSpineVertebraeInfo ContraVariant fioMkStrong, AppSpineVertebraeInfo CoVariant id]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL = repeat $ AppSpineVertebraeInfo CoVariant id
%%]

%%[9.AppSpine -4.AppSpine
data AppSpineVertebraeInfo
  =  AppSpineVertebraeInfo
       { asCoCo         :: CoContraVariance
       , asFIO          :: FIOpts -> FIOpts
       , asFOUpdCoe     :: EHCOpts -> [FIOut] -> FIOut
       }

dfltFOUpdCoe _ x = last x

unknownAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
unknownAppSpineVertebraeInfoL = repeat $ AppSpineVertebraeInfo CoContraVariant fioMkUnify dfltFOUpdCoe

arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL
  = [ AppSpineVertebraeInfo ContraVariant fioMkStrong
          dfltFOUpdCoe
    , AppSpineVertebraeInfo CoVariant id
          (\opts [ffo,afo]
              -> let (u',u1) = mkNewUID (foUniq afo)
                     c = lrcoeForLamTyApp opts u1 (foCSubst afo) (foLRCoe ffo) (foLRCoe afo)
                 in  afo { foLRCoe = c, foUniq = u' }
          )
    ]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL = repeat $ AppSpineVertebraeInfo CoVariant id dfltFOUpdCoe
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

