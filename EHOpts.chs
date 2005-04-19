% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 import(GetOpt,EHCommon) export(EHCOpts(..), defaultEHCOpts, ehcCmdLineOpts)
%%]

%%[4 import(EHTy) export(FIOpts(..), fioSwapCoCo, fioSwapOpts, strongFIOpts, instFIOpts, instLFIOpts, fioMkStrong, fioMkUnify)
%%]

%%[4_2 export(unifyFIOpts,meetFIOpts,joinFIOpts,impredFIOpts)
%%]

%%[5 export(weakFIOpts)
%%]

%%[8 import(List,Char) export(cmdLineTrfs,trfOptOverrides)
%%]

%%[9 export(predFIOpts,implFIOpts)
%%]

%%[11 import(EHTyInstantiate)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data TrfOpt = TrfYes String | TrfNo String | TrfAllYes | TrfAllNo

cmdLineTrfs :: AssocL String String
cmdLineTrfs
  = [ ("CCP"    , "Core Constant Propagation (simple ones introduced by frontend)")
    , ("CRU"    , "Core Rename Unique (all identifiers)")
    , ("CLU"    , "Core Let Unrec (remove unnecessary recursive defs)")
    , ("CILA"   , "Core Inline Let Alias (remove unnecessary alpha renamings)")
    , ("CFL"    , "Core Full Laziness (give names to all expressions and float them outwards)")
    , ("CLL"    , "Core Lambda Lift")
    ]

trfOptOverrides :: [TrfOpt] -> String -> Maybe Bool
trfOptOverrides opts trf
  =  ovr opts
  where  ovr [] = Nothing
         ovr (TrfYes s   :os) | trf == s  = Just True
         ovr (TrfNo s    :os) | trf == s  = Just False
         ovr (TrfAllYes  :os)             = Just True
         ovr (TrfAllNo   :os)             = Just False
         ovr (_          :os)             = ovr os
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[EHCOpts.1
data EHCOpts    = EHCOptions    {  ehcoptDumpPP         ::  Maybe String
                                ,  ehcoptShowTopTyPP    ::  Bool
                                ,  ehcoptHelp           ::  Bool
                                ,  ehcoptDebug          ::  Bool
%%]

%%[EHCOpts.8
                                ,  ehcoptCore           ::  Bool
                                ,  ehcoptCoreJava       ::  Bool
                                ,  ehcoptCoreGrin       ::  Bool
                                ,  ehcoptSearchPath     ::  [String]
                                ,  ehcoptVerbosity      ::  Verbosity
                                ,  ehcoptTrf            ::  [TrfOpt]
%%]

%%[defaultEHCOpts.1
defaultEHCOpts  = EHCOptions    {  ehcoptDumpPP         =   Just "pp"
                                ,  ehcoptShowTopTyPP    =   False
                                ,  ehcoptHelp           =   False
                                ,  ehcoptDebug          =   False
%%]

%%[defaultEHCOpts.8
                                ,  ehcoptCore           =   True
                                ,  ehcoptCoreJava       =   False
                                ,  ehcoptCoreGrin       =   False
                                ,  ehcoptSearchPath     =   []
                                ,  ehcoptVerbosity      =   VerboseQuiet
                                ,  ehcoptTrf            =   []
%%]

%%[ehcCmdLineOptsA.1
ehcCmdLineOpts  
  =  [  Option "p"  ["pretty"]        (OptArg oPretty "pp|ast|no|off")
          "do output pretty printed version of src (pp), abstract syntax tree (ast) or nothing (no|off), default=pp"
     ,  Option "d"  ["debug"]         (NoArg oDebug)
          "same as --pretty=ast + extra debug info"
     ,  Option ""   ["show-top-ty"]   (OptArg oShowTopTy "yes|no")
          "show top ty, default=no"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
%%]

%%[ehcCmdLineOptsA.8
     ,  Option "c"  ["code"]          (OptArg oCode "java|grin")
          "dump code (java- > .java, grin -> .grin, - -> none) on file, default=core (-> .core)"
     ,  Option ""   ["trf"]           (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
          "switch on/off transformations"
     ,  Option "v"  ["verbose"]       (OptArg oVerbose "0|1|2")
          "be verbose, 0=quiet 1=normal 2=noisy, default=1"
%%]

%%[ehcCmdLineOptsB.1
  where  oPretty     ms  o =  case ms of
                                Just "no"   -> o { ehcoptDumpPP        = Nothing   }
                                Just "off"  -> o { ehcoptDumpPP        = Nothing   }
                                Just p      -> o { ehcoptDumpPP        = Just p    }
                                _           -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcoptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcoptHelp          = True    }
         oDebug          o =  (oPretty (Just "ast") o) { ehcoptDebug         = True    }
%%]

%%[ehcCmdLineOptsB.8
         oCode       ms  o =  case ms of
                                Just "java"  -> o { ehcoptCoreJava     = True      }
                                Just "grin"  -> o { ehcoptCoreGrin     = True      }
                                Just "-"     -> o { ehcoptCore         = False     }
                                _            -> o { ehcoptCore         = True      }
         oTrf        s   o =  o { ehcoptTrf           = opt s   }
                           where  opt "" =  []
                                  opt o  =  let  (pm,o2) = span (\c -> c == '+' || c == '-') o
                                                 (tr,o3) = span isAlpha o2
                                                 opt2    = opt o3
                                            in   case (pm,tr) of
                                                   ("+",_:_)  -> TrfYes tr : opt2
                                                   ("-",_:_)  -> TrfNo tr : opt2
                                                   ("+",_)    -> [TrfAllYes]
                                                   ("-",_)    -> [TrfAllNo]
                                                   _          -> []
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcoptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcoptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { ehcoptVerbosity     = VerboseALot        }
                                Nothing     -> o { ehcoptVerbosity     = VerboseNormal      }
                                _           -> o
%%]

%%[1.Options
%%@EHCOpts.1
                                }

%%@defaultEHCOpts.1
                                }

%%@ehcCmdLineOptsA.1
     ]
%%@ehcCmdLineOptsB.1
%%]

%%[8.Options -1.Options
%%@EHCOpts.1
%%@EHCOpts.8
                                }

%%@defaultEHCOpts.1
%%@defaultEHCOpts.8
                                }

%%@ehcCmdLineOptsA.1
%%@ehcCmdLineOptsA.8
     ]
%%@ehcCmdLineOptsB.1
%%@ehcCmdLineOptsB.8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIOpts.hd
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioUniq                 ::  UID
                        ,  fioMode           ::  FIMode
%%]
%%[4_2
                        ,  fioBindToTyAlts   ::  Bool
                        ,  fioDontBind       ::  TyVarIdL
%%]
%%[7.FIOpts
                        ,  fioNoRLabElimFor  ::  [HsName]
%%]
%%[9.FIOpts
                        ,  fioPredAsTy       ::  Bool                ,  fioAllowRPredElim       ::  Bool
                        ,  fioDontBind       ::  TyVarIdL
%%]
%%[11.FIOpts
                        ,  fioAllowEqOpen    ::  Bool                ,  fioInstCoConst          ::  HowToInst
%%]
%%[4.FIOpts.tl
                        }
%%]

%%[4.strongFIOpts.hd
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioUniq                 =   uidStart
                        ,  fioMode           =   FitSubLR
%%]
%%[4_2
                        ,  fioBindToTyAlts   =   False
                        ,  fioDontBind       =   []
%%]
%%[7.strongFIOpts
                        ,  fioNoRLabElimFor  =   []
%%]
%%[9.strongFIOpts
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
                        ,  fioDontBind       =   []
%%]
%%[11.FIOpts
                        ,  fioAllowEqOpen    =   False               ,  fioInstCoConst          =   instCoConst
%%]
%%[4.strongFIOpts.tl
                        }
%%]

%%[4
instance Show FIOpts where
  show o =  "FIOpts{"
            ++ " fioLeaveRInst=" ++ show (fioLeaveRInst o)
            ++ " }"
%%]

%%[4.FIOpts.defaults
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]
instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}

%%[4_2.FIOpts.defaults
unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

meetFIOpts :: FIOpts
meetFIOpts = unifyFIOpts {fioMode = FitMeet}

joinFIOpts :: FIOpts
joinFIOpts = unifyFIOpts {fioMode = FitJoin}

impredFIOpts :: FIOpts
impredFIOpts = strongFIOpts {fioBindToTyAlts = True}
%%]

%%[5
weakFIOpts :: FIOpts
weakFIOpts = strongFIOpts {fioLeaveRInst = True, fioBindRFirst = False}
%%]

%%[9
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}

implFIOpts  :: FIOpts
implFIOpts = strongFIOpts {fioAllowRPredElim = False}
%%]

%%[4
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio = fio { fioBindRFirst = fioBindLFirst fio, fioBindLFirst = fioBindRFirst fio }

fioSwapCoCo :: CoContraVariance -> FIOpts -> FIOpts
fioSwapCoCo coco fio = fio {fioMode = fimSwapCoCo coco (fioMode fio)}
%%]

%%[4.fioMkStrong
fioMkStrong :: FIOpts -> FIOpts
fioMkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}
%%]

%%[4.fioMkUnify
fioMkUnify :: FIOpts -> FIOpts
fioMkUnify fi = fi {fioMode = FitUnify}
%%]

