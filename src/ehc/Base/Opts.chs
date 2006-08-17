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

%%[50 import({%{EH}Ty.Instantiate})
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
      {  ehcOptShowHS         ::  Bool
      ,  ehcOptShowEH         ::  Bool
      ,  ehcOptShowAst        ::  Bool
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
      ,  ehcOptShowGrin       ::  Bool
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptEmitCore       ::  Bool
      ,  ehcOptEmitJava       ::  Bool
      ,  ehcOptEmitGrin       ::  Bool
      ,  ehcOptEmitCmm        ::  Bool
      ,  ehcOptEmitLlc        ::  Bool
      ,  ehcOptSearchPath     ::  [String]
      ,  ehcOptVerbosity      ::  Verbosity
      ,  ehcOptTrf            ::  [TrfOpt]
%%]
%%[9.EHCOpts
      ,  ehcOptPrfCutOffAt    ::  Int
%%]
%%[99
      ,  ehcProgName          ::  String
%%]
%%[1
      }
%%]

%%[1.defaultEHCOpts
defaultEHCOpts
  = EHCOpts
      {  ehcOptShowHS         =   False
      ,  ehcOptShowEH         =   True
      ,  ehcOptShowAst        =   False
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
      ,  ehcOptShowGrin       =   False
      ,  ehcOptEmitHS         =   False
      ,  ehcOptEmitEH         =   False
      ,  ehcOptEmitCore       =   True
      ,  ehcOptEmitJava       =   False
      ,  ehcOptEmitGrin       =   False
      ,  ehcOptEmitCmm        =   False
      ,  ehcOptEmitLlc        =   False
      ,  ehcOptSearchPath     =   []
      ,  ehcOptVerbosity      =   VerboseQuiet
      ,  ehcOptTrf            =   []
%%]
%%[9.defaultEHCOpts
      ,  ehcOptPrfCutOffAt    =   20
%%]
%%[99
      ,  ehcProgName          =   ""
%%]
%%[1
      }
%%]

%%[1.ehcCmdLineOptsA
ehcCmdLineOpts  
  =  [  Option "p"  ["pretty"]        (OptArg oPretty "hs|eh|grin|ast|-")
          "show pretty printed EH/Grin source or EH abstract syntax tree, default=eh, -=off, (hs only for .hs files)"
     ,  Option "d"  ["debug"]         (NoArg oDebug)
          "show extra debug information"
     ,  Option ""   ["show-top-ty"]   (OptArg oShowTopTy "yes|no")
          "show top ty, default=no"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
     ,  Option ""   ["version"]       (NoArg oVersion)
          "print version info"
%%]
%%[8.ehcCmdLineOptsA
     ,  Option "c"  ["code"]          (OptArg oCode "eh|core|java|grin|cmm|llc|-")
          "write code to file, default=core"
     ,  Option ""   ["gen-trace"]     (NoArg oGenTrace)
          "emit trace info into cmm code"
     ,  Option ""   ["trf"]           (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
          "switch on/off transformations"
     ,  Option ""   ["time-compilation"]  (NoArg oTimeCompile)
          "show grin compiler CPU usage for each compilation phase (only with -v2)"
     ,  Option ""   ["dump-call-graph"]   (NoArg oDumpCallGraph)
          "output grin call graph as dot file"
     ,  Option ""   ["dump-trf-grin"]     (OptArg oDumpTrfGrin "basename")
          "dump intermediate grin code after transformation"
     ,  Option "v"  ["verbose"]       (OptArg oVerbose "0|1|2")
          "be verbose, 0=quiet 1=normal 2=noisy, default=1"
%%]
%%[1
     ]
%%]
%%[1.ehcCmdLineOptsB
  where  oPretty     ms  o =  case ms of
                                Just "-"     -> o { ehcOptShowEH       = False     }
                                Just "no"    -> o { ehcOptShowEH       = False     }
                                Just "off"   -> o { ehcOptShowEH       = False     }
                                Just "hs"    -> o { ehcOptShowHS       = True      }
                                Just "eh"    -> o { ehcOptShowEH       = True      }
                                Just "pp"    -> o { ehcOptShowEH       = True      }
                                Just "ast"   -> o { ehcOptShowAst      = True      }
%%]
%%[8.ehcCmdLineOptsB
                                Just "grin"  -> o { ehcOptShowGrin     = True      }
%%]
%%[1.ehcCmdLineOptsB
                                _            -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcOptHelp          = True    }
         oVersion        o =  o { ehcOptVersion       = True    }
         oDebug          o =  o { ehcOptDebug         = True
                                , ehcOptShowEH        = True
                                }
%%]
%%[8.ehcCmdLineOptsB
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
         oGenTrace       o =  o { ehcOptGenTrace          = True    }
         oDumpTrfGrin ms o =  o { ehcOptDumpTrfGrin       = maybe (Just "") (const ms) ms }
         oDumpCallGraph  o =  o { ehcOptDumpCallGraph     = True }
         oCode       ms  o =  case ms of
                                Just "-"     -> o { ehcOptEmitCore     = False     }
                                Just "hs"    -> o { ehcOptEmitHS       = True      }
                                Just "eh"    -> o { ehcOptEmitEH       = True      }
                                Just "core"  -> o { ehcOptEmitCore     = True      }
                                Just "java"  -> o { ehcOptEmitJava     = True      }
                                Just "grin"  -> o { ehcOptEmitGrin     = True      }
                                Just "cmm"   -> o { ehcOptEmitCmm      = True      }
                                Just "llc"   -> o { ehcOptEmitLlc      = True      }
                                _            -> o
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
%%[50.FIOpts
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
%%[50.FIOpts
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
