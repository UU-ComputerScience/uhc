%%[1 hs import(EH4x2.Base.Common(UID(..),uidStart))
%%]

%%[1 hs import(EH4x2.Ty(TyVarId,TyVarIdL))
%%]

%%[1 hs export(module EH4x2.Base.Common)
%%]

%%[1 hs export(module EH4x2.Ty)
%%]

%%[1

%%]

%%[3 hs import(UU.Pretty) export(Err,ErrL)
%%]

%%[3 hs import(EH4x2.Base.Common(FIMode(..),fimOpp,fimSwapCoCo))
%%]

%%[3 hs import(EH4x2.Base.Opts(fioIsMeetJoin,fioIsSubsume))
%%]

%%[3 hs import(EH4x2.Base.Common(CoContraVariance(..),cocoOpp))
%%]

%%[3 hs import(EH4x2.Base.Opts(FIOpts(..),strongFIOpts,joinFIOpts,meetFIOpts,fioSwapOpts,fioSwapCoCo))
%%]

%%[3 hs import(Debug.Trace) export(tr)
%%]

%%[3 hs export(module EH4x2.Base.Opts)
%%]


-------------------------------------------------------------------------
-- Unique identifier
-------------------------------------------------------------------------

%%[1
%%]
newtype UID = UID [Int] deriving (Eq,Ord)

instance Show UID where
  show (UID l)
    = concat .  intersperse "_" . map show . reverse $ l

uidStart = UID [0]


-------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------

%%[1 hs
%%]
type TyVarId  = UID
type TyVarIdL = [TyVarId]

-------------------------------------------------------------------------
-- Error
-------------------------------------------------------------------------

%%[3
type Err = PP_Doc
type ErrL = [Err]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Co/Contra variance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3
%%]
data CoContraVariance =  CoVariant | ContraVariant | CoContraVariant deriving Eq

%%[3
%%]
instance Show CoContraVariance where
  show CoVariant        = "CC+"
  show ContraVariant    = "CC-"
  show CoContraVariant  = "CCo"

%%[3
%%]
cocoOpp :: CoContraVariance -> CoContraVariance
cocoOpp  CoVariant      =   ContraVariant
cocoOpp  ContraVariant  =   CoVariant
cocoOpp  _              =   CoContraVariant

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting mode (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3
%%]
data FIMode  =  FitSubLR
             |  FitSubRL
             |  FitUnify
             |  FitMeet
             |  FitJoin
             deriving (Eq,Ord)

%%[3
fimOpp :: FIMode -> FIMode
fimOpp m
  =  case m of
       FitSubLR  -> FitSubRL
       FitSubRL  -> FitSubLR
       FitMeet   -> FitJoin
       FitJoin   -> FitMeet
       _         -> m
%%]

%%[3
%%]
fimSwapCoCo :: CoContraVariance -> FIMode -> FIMode
fimSwapCoCo coco m = case coco of {ContraVariant -> fimOpp m; _ -> m}

%%[3
%%]
instance Show FIMode where
  show FitSubLR  = "<="
  show FitSubRL  = ">="
  show FitUnify  = "=="
  show FitMeet   = "=^="
  show FitJoin   = "=v="

%%[3
%%]
instance PP FIMode where
  pp m = pp (show m)

%%[3
%%]
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}

fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3.FIOpts.hd
%%]
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioBindLBeforeR         ::  Bool
                        ,  fioMode           ::  FIMode              ,  fioUniq                 ::  UID
                        ,  fioBindToTyAlts   ::  Bool
                        ,  fioDontBind       ::  TyVarIdL
                        }

%%[3.strongFIOpts.hd
%%]
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
                        ,  fioBindToTyAlts   =   False
                        ,  fioDontBind       =   []
                        }

unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

meetFIOpts :: FIOpts
meetFIOpts = unifyFIOpts {fioMode = FitMeet}

joinFIOpts :: FIOpts
joinFIOpts = unifyFIOpts {fioMode = FitJoin}

%%[3
%%]
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio = fio { fioBindRFirst = fioBindLFirst fio, fioBindLFirst = fioBindRFirst fio, fioBindLBeforeR = not (fioBindLBeforeR fio) }

fioSwapCoCo :: CoContraVariance -> FIOpts -> FIOpts
fioSwapCoCo coco fio = fio {fioMode = fimSwapCoCo coco (fioMode fio)}

%%[3
%%]
instance Show FIOpts where
  show o =  "FIOpts"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3
showPP :: PP a => a -> String
showPP p = disp (pp p) 1000 ""

tr :: String -> [String] -> a -> a
tr msg sl x = trace (showPP (msg >#< (vlist $ map pp $ sl))) x
%%]
