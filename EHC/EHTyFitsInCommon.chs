% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption (fitting in) for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 import(EHCommon, EHTy, EHError) export (FIOut(..), emptyFO, foHasErrs)
%%]

%%[2 import(EHCnstr)
%%]

%%[4 import(EHOpts) export(AppSpineInfo(..), unknownAppSpineInfoL, arrowAppSpineInfoL, prodAppSpineInfoL)
%%]

%%[9 import(EHCore,EHCoreSubst)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to result/output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.FIOut
data FIOut  =   FIOut   { foTy     ::  Ty      ,  foErrL   ::  ErrL    }

emptyFO     =   FIOut   { foTy     =   Ty_Any  ,  foErrL   =   []      }
%%]

%%[2.FIOut -1.FIOut
data FIOut  =  FIOut  {  foTy     ::  Ty      ,  foErrL   ::  ErrL  ,  foCnstr           ::  Cnstr           }
%%]

%%[2.FIOut.empty
emptyFO     =  FIOut  {  foTy     =   Ty_Any  ,  foErrL   =   []    ,  foCnstr           =   emptyCnstr      }
%%]

%%[4.FIOut -(2.FIOut 2.FIOut.empty)
data FIOut  =  FIOut    {  foCnstr           ::  Cnstr               ,  foTy              ::  Ty
                        ,  foUniq            ::  UID                 ,  foAppSpineL       ::  [AppSpineInfo]
                        ,  foErrL            ::  ErrL  
%%]
%%[9
                        ,  foCSubst          ::  CSubst              ,  foPredOccL        ::  [PredOcc]
                        ,  foLCoeL           ::  [Coe]               ,  foRCoeL           ::  [Coe]
%%]
%%[10
                        ,  foRowCoeL         ::  AssocL HsName Coe
%%]
%%[11
                        ,  foEqCnstr         ::  Cnstr
%%]
%%[4.FIOut.tl
                        }
%%]

%%[4.emptyFO
emptyFO     =  FIOut    {  foCnstr           =   emptyCnstr          ,  foTy              =   Ty_Any
                        ,  foUniq            =   uidStart            ,  foAppSpineL       =   []
                        ,  foErrL            =   []         
%%]
%%[9
                        ,  foCSubst          =   emptyCSubst         ,  foPredOccL        =   []
                        ,  foLCoeL           =   []                  ,  foRCoeL           =   []
%%]
%%[10
                        ,  foRowCoeL         =   []
%%]
%%[11
                        ,  foEqCnstr         =   emptyCnstr
%%]
%%[4.emptyFO.tl
                        }
%%]

%%[1.foHasErrs
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.AppSpine
data AppSpineInfo
  =  AppSpineInfo
       { asCoCo         :: CoContraVariance
       , asFIO          :: FIOpts -> FIOpts
       }

unknownAppSpineInfoL :: [AppSpineInfo]
unknownAppSpineInfoL = repeat (AppSpineInfo CoContraVariant fioMkUnify)

arrowAppSpineInfoL :: [AppSpineInfo]
arrowAppSpineInfoL = [AppSpineInfo ContraVariant fioMkStrong, AppSpineInfo CoVariant id]

prodAppSpineInfoL :: [AppSpineInfo]
prodAppSpineInfoL = repeat (AppSpineInfo CoVariant id)
%%]

%%[9.AppSpine -4.AppSpine
data AppSpineInfo
  =  AppSpineInfo
       { asCoCo         :: CoContraVariance
       , asFIO          :: FIOpts -> FIOpts
       , asFOUpdCoe     :: FIOut -> FIOut -> FIOut
       }

unknownAppSpineInfoL :: [AppSpineInfo]
unknownAppSpineInfoL = repeat (AppSpineInfo CoContraVariant fioMkUnify (\_ x -> x))

arrowAppSpineInfoL :: [AppSpineInfo]
arrowAppSpineInfoL
  =  [  AppSpineInfo ContraVariant fioMkStrong
            (\_ x -> x)
     ,  AppSpineInfo CoVariant id
            (\ffo afo
                ->  let  (u',u1) = mkNewUID (foUniq afo)
                         n = uidHNm u1
                         r = mkCoe (\e ->  CExpr_Lam n e)
                         l = mkCoe (\e ->  CExpr_App e
                                             (coeWipeWeave emptyCnstr (foCSubst afo) (foLCoeL ffo) (foRCoeL ffo)
                                               `coeEvalOn` CExpr_Var n)
                                   )
                    in   afo  { foRCoeL = r : foRCoeL afo, foLCoeL = l : foLCoeL afo
                              , foUniq = u'
                              }
            )
     ]

prodAppSpineInfoL :: [AppSpineInfo]
prodAppSpineInfoL = repeat (AppSpineInfo CoVariant id (\_ x -> x))
%%]

