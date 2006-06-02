%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Opts} import(System.Console.GetOpt,{%{EH}Base.Common}) export(EHCOpts(..), defaultEHCOpts, ehcCmdLineOpts)
%%]

%%[4 import({%{EH}Ty},UU.Pretty,EH.Util.PPUtils) export(FIOpts(..), fioSwapCoCo, fioSwapOpts, strongFIOpts, instFIOpts, instLRFIOpts, instLFIOpts, fioMkStrong, fioMkUnify)
%%]

%%[4 export(fioIsSubsume)
%%]

%%[4_2 export(unifyFIOpts,meetFIOpts,joinFIOpts,impredFIOpts)
%%]

%%[4_2 export(fioIsMeetJoin)
%%]

%%[5 export(weakFIOpts)
%%]

%%[8 import(Data.List,Data.Char) export(cmdLineTrfs,trfOptOverrides)
%%]

%%[9 export(predFIOpts,implFIOpts)
%%]

%%[11 import({%{EH}Ty.Instantiate})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data TrfOpt = TrfYes String | TrfNo String | TrfAllYes | TrfAllNo

cmdLineTrfs :: AssocL String String
cmdLineTrfs
  = [ ("CER"    , "Core Eta Reduction")
    , ("CCP"    , "Core Constant Propagation (simple ones introduced by frontend)")
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

%%[1.EHCOpts
data EHCOpts
  = EHCOpts
      {  ehcOptDumpPP         ::  Maybe String
      ,  ehcOptShowTopTyPP    ::  Bool
      ,  ehcOptHelp           ::  Bool
      ,  ehcOptVersion        ::  Bool
      ,  ehcOptDebug          ::  Bool
%%]
%%[8.EHCOpts
      ,  ehcOptDumpCallGraph  ::  Bool
      ,  ehcOptDumpTrfGrin    ::  Maybe String
      ,  ehcOptTimeCompile    ::  Bool
      ,  ehcOptGenTrace       ::  Bool
      ,  ehcOptCore           ::  Bool
      ,  ehcOptCoreJava       ::  Bool
      ,  ehcOptCoreGrin       ::  Bool
      ,  ehcOptCoreCmm        ::  Bool
      ,  ehcOptSearchPath     ::  [String]
      ,  ehcOptVerbosity      ::  Verbosity
      ,  ehcOptTrf            ::  [TrfOpt]
%%]
%%[9.EHCOpts
      ,  ehcOptPrfCutOffAt    ::  Int
%%]
%%[1
      }
%%]

%%[1.defaultEHCOpts
defaultEHCOpts
  = EHCOpts
      {  ehcOptDumpPP         =   Just "pp"
      ,  ehcOptShowTopTyPP    =   False
      ,  ehcOptHelp           =   False
      ,  ehcOptVersion        =   False
      ,  ehcOptDebug          =   False
%%]
%%[8.defaultEHCOpts
      ,  ehcOptDumpCallGraph  =   False
      ,  ehcOptDumpTrfGrin    =   Nothing
      ,  ehcOptTimeCompile    =   False
      ,  ehcOptGenTrace       =   False
      ,  ehcOptCore           =   True
      ,  ehcOptCoreJava       =   False
      ,  ehcOptCoreGrin       =   False
      ,  ehcOptCoreCmm        =   False
      ,  ehcOptSearchPath     =   []
      ,  ehcOptVerbosity      =   VerboseQuiet
      ,  ehcOptTrf            =   []
%%]
%%[9.defaultEHCOpts
      ,  ehcOptPrfCutOffAt    =   20
%%]
%%[1
      }
%%]

%%[1.ehcCmdLineOptsA
ehcCmdLineOpts  
  =  [  Option "p"  ["pretty"]        (OptArg oPretty "pp|ast|grin|no|off")
          "do output pretty printed version of src (pp), abstract syntax tree (ast) or nothing (no|off), default=pp"
     ,  Option "d"  ["debug"]         (NoArg oDebug)
          "same as --pretty=ast + extra debug info"
     ,  Option ""   ["show-top-ty"]   (OptArg oShowTopTy "yes|no")
          "show top ty, default=no"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
     ,  Option ""   ["version"]       (NoArg oVersion)
          "print version info"
%%]
%%[8.ehcCmdLineOptsA
     ,  Option "c"  ["code"]          (OptArg oCode "core|java|grin|cmm")
          "dump code (java- > .java, grin -> .grin, cmm -> .grin + .cmm, - -> none) on file, default=core (-> .core)"
     ,  Option ""   ["gen-trace"]     (NoArg oGenTrace)
          "emit trace info into C-- code (grin only)"
     ,  Option ""   ["trf"]           (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
          "switch on/off transformations"
     ,  Option ""   ["time-compilation"]  (NoArg oTimeCompile)
          "show CPU usage for each compilation phase (grin only, with -v2)"
     ,  Option ""   ["dump-call-graph"]   (NoArg oDumpCallGraph)
          "dump call graph as dot file (grin only)"
     ,  Option ""   ["dump-trf-grin"]     (OptArg oDumpTrfGrin "basename")
          "dump grin code after transformation (grin only)"
     ,  Option "v"  ["verbose"]       (OptArg oVerbose "0|1|2")
          "be verbose, 0=quiet 1=normal 2=noisy, default=1"
%%]
%%[1
     ]
%%]
%%[1.ehcCmdLineOptsB
  where  oPretty     ms  o =  case ms of
                                Just "no"   -> o { ehcOptDumpPP        = Nothing   }
                                Just "off"  -> o { ehcOptDumpPP        = Nothing   }
                                Just p      -> o { ehcOptDumpPP        = Just p    }
                                _           -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcOptHelp          = True    }
         oVersion        o =  o { ehcOptVersion       = True    }
         oDebug          o =  (oPretty (Just "ast") o) { ehcOptDebug         = True    }
%%]
%%[8.ehcCmdLineOptsB
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
         oGenTrace       o =  o { ehcOptGenTrace          = True    }
         oDumpTrfGrin ms o =  o { ehcOptDumpTrfGrin       = maybe (Just "") (const ms) ms }
         oDumpCallGraph  o =  o { ehcOptDumpCallGraph     = True }
         oCode       ms  o =  case ms of
                                Just "java"  -> o { ehcOptCoreJava     = True      }
                                Just "grin"  -> o { ehcOptCoreGrin     = True      }
                                Just "cmm"   -> o { ehcOptCoreGrin     = True, ehcOptCoreCmm      = True      }
                                Just "-"     -> o { ehcOptCore         = False     }
                                _            -> o { ehcOptCore         = True      }
         oTrf        s   o =  o { ehcOptTrf           = opt s   }
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
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Nothing     -> o { ehcOptVerbosity     = VerboseNormal      }
                                _           -> o
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIOpts.hd
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioBindLBeforeR         ::  Bool
                        ,  fioMode           ::  FIMode              ,  fioUniq                 ::  UID
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
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
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
  show o =  "FIOpts"
%%]

%%[4
instance PP FIOpts where
  pp   o =  "FIOpts{"
            >#< "leaveRInst=" >|< pp (fioLeaveRInst o)
            >#< "bindLFirst=" >|< pp (fioBindLFirst o)
            >#< "bindRFirst=" >|< pp (fioBindRFirst o)
%%]
%%[9
            >#< "allowRPredElim=" >|< pp (fioAllowRPredElim o)
%%]
%%[4
            >#< "}"
%%]

%%[4.FIOpts.instLFIOpts
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}
%%]

%%[4.FIOpts.instLRFIOpts
instLRFIOpts :: FIOpts
instLRFIOpts = strongFIOpts {fioBindRFirst = False, fioBindLFirst = False}
%%]

%%[4.FIOpts.instFIOpts
instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]

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
fioSwapOpts fio = fio { fioBindRFirst = fioBindLFirst fio, fioBindLFirst = fioBindRFirst fio, fioBindLBeforeR = not (fioBindLBeforeR fio) }

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn opts related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}
%%]

%%[4_2
fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}
%%]


