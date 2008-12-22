%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Opts} import(System.Console.GetOpt,{%{EH}Base.Common}) export(EHCOpts(..), defaultEHCOpts, ehcCmdLineOpts)
%%]

%%[4 import(EH.Util.Pretty)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Ty})
%%]

%%[8 import(Data.List,Data.Char,{%{EH}Base.Builtin})
%%]

%%[8 import(EH.Util.FPath)
%%]

%%[9 import(qualified Data.Set as Set)
%%]

%%[99 import(EH.Util.Utils)
%%]

%%[50 import({%{EH}Ty.Trf.Instantiate})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cmdLineTrfs)
data TrfOpt = TrfYes String | TrfNo String | TrfAllYes | TrfAllNo

cmdLineTrfs :: AssocL String String
cmdLineTrfs
  = [ ("CER"    , "Core Eta Reduction")
    , ("CETA"   , "Core Eliminate Trivial Applications")
    , ("CCP"    , "Core Constant Propagation (simple ones introduced by frontend)")
    , ("CRU"    , "Core Rename Unique (all identifiers)")
    , ("CLU"    , "Core Let Unrec (remove unnecessary recursive defs)")
    , ("CILA"   , "Core Inline Let Alias (remove unnecessary alpha renamings)")
    , ("CFL"    , "Core Full Laziness (give names to all expressions and float them outwards)")
    , ("CLL"    , "Core Lambda Lift")
    , ("CLGA"   , "Core Lambda Global as Arg")
    , ("CCGA"   , "Core CAF Global as Arg")
    , ("CLFG"   , "Core Lambda Float to Global")
%%[[9
    , ("CLDF"   , "Core Lift Dictionary Fields")
%%]]
%%[[102
    , ("CS"     , "Core Strip (debug)")
%%]]
    ]
%%]

%%[(8 codegen) export(trfOptOverrides)
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

Convention: most option names/fields start with 'ehcOpt'

%%[1.EHCOpts
data EHCOpts
  = EHCOpts
      {  ehcOptAspects        ::  String            -- which aspects are included in this compiler
      ,  ehcOptShowHS         ::  Bool              -- show HS pretty print on stdout
      ,  ehcOptShowEH         ::  Bool              -- show EH pretty print on stdout
      ,  ehcOptPriv           ::  Bool				-- privately used (in general during switch between 2 impls of 1 feature)
%%[[1
      ,  ehcOptShowAst        ::  Bool              -- show decorated EH AST on stdout
%%][100
%%]]
%%[[(1 hmtyinfer)
      ,  ehcOptShowTopTyPP    ::  Bool              -- show EH type of expression
%%]]
      ,  ehcOptHelp           ::  Bool              -- print help
      ,  ehcOptVersion        ::  Bool              -- print version info
      ,  ehcOptDebug          ::  Bool              -- debug info
      ,  ehcStopAtPoint       ::  CompilePoint      -- stop at (after) compile phase
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[(8 codegen)
      ,  ehcOptEmitCore       ::  Bool
      ,  ehcOptOptimise       ::  Optimise			-- optimisation level
      ,  ehcOptDumpCoreStages ::  Bool				-- dump intermediate Core transformation stages
      ,  ehcOptTrf            ::  [TrfOpt]
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptOwn            ::  Int
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool				-- generate runtime debug info
      ,  ehcOptGenTrace       ::  Bool

      ,  ehcOptEmitGrin       ::  Bool
      ,  ehcOptEmitC          ::  Bool
      ,  ehcOptEmitJVM        ::  Bool              -- Emit a .class file for JVM processing
      ,  ehcOptEmitLLVM       ::  Bool              -- Emit a .ll file for LLVM processing
      ,  ehcOptEmitExecLLVM   ::  Bool              -- Emit an executable created via LLVM
      ,  ehcOptEmitBytecode   ::  Bool
      ,  ehcOptEmitExecC      ::  Bool
      ,  ehcOptEmitExecBytecode:: Bool
      ,  ehcOptGenRTSInfo     ::  Int				-- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptFullProgAnalysis ::  Bool				-- do full GRIN program analysis
      ,  ehcOptDumpGrinStages ::  Bool				-- dump intermediate Grin transformation stages
      ,  ehcOptErrAboutBytecode ::  Bool				-- report when Grin ByteCode errors occur
%%]]
%%[[(8 codegen java)
      ,  ehcOptEmitJava       ::  Bool
%%]]
%%[[8
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptSearchPath     ::  [String]
      ,  ehcOptVerbosity      ::  Verbosity			-- verbosity level

      ,  ehcOptBuiltinNames   ::  EHBuiltinNames
      ,  ehcOptUseInplace     ::  Bool              -- use inplace runtime libraries
      
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf::  Bool				-- functions/fields of instance get as arg the dictionary as well
      ,  ehcOptPrfCutOffAt    ::  Int				-- cut off limit for context reduction
      ,  ehcCfgClassViaRec    ::  Bool				-- instance representation via record instead of data
      -- ,  ehcCfgCHRScoped      ::  CHRScoped			-- how to gen scoped CHR's (option is used only for paper writing + experimenting)
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt					-- cut off for type lambda expansion
                              ::  Int
%%]]
%%[[(20 codegen)
      ,  ehcDebugStopAtCoreError
                              ::  Bool              -- stop when Core parse error occurs (otherwise errors are ignored, repaired .core is used)
%%]]
%%[[20
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcDebugStopAtHIError::  Bool              -- stop when HI parse error occurs (otherwise it is ignored, .hi thrown away)
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree  ::  DerivTreeWay      -- show derivation tree on stdout
      ,  ehcOptEmitDerivTreePaperSize
      						  ::  String            -- the paper size to be used
      ,  ehcOptEmitDerivFitsIn
      						  ::  Bool              -- show fitsIn derivation tree as well
%%]]
%%[[99
      ,  ehcProgName          ::  FPath  			-- name of this program
      ,  ehcOptShowNumVersion ::  Bool				-- numerical version, for external version comparison
      ,  ehcOptCPP            ::  Bool				-- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude						-- use & assume presence of prelude
                              ::  Bool
%%]]
      }
%%]

%%[1.defaultEHCOpts
defaultEHCOpts
  = EHCOpts
      {  ehcOptAspects          =   "%%@{%{ASPECTS}%%}"
      ,  ehcOptShowHS           =   False
      ,  ehcOptPriv             =   False
%%[[1
      ,  ehcOptShowEH           =   True
%%][99
      ,  ehcOptShowEH           =   False
%%]]
%%[[1
      ,  ehcOptShowAst          =   False
%%][100
%%]]
%%[[(1 hmtyinfer)
      ,  ehcOptShowTopTyPP      =   False
%%]]
      ,  ehcOptHelp             =   False
      ,  ehcOptVersion          =   False
      ,  ehcOptDebug            =   False
      ,  ehcStopAtPoint         =   CompilePoint_All
%%[[7_2
      ,  ehcOptUniqueness       =   True
%%]]
%%[[(8 codegen)
      ,  ehcOptEmitCore         =   True
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimise         =   OptimiseNormal
      ,  ehcOptTrf              =   []
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile      =   False

      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptOwn              =   3
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace         =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptEmitGrin         =   False
      ,  ehcOptEmitJVM          =   False
      ,  ehcOptEmitLLVM         =   False
      ,  ehcOptEmitExecLLVM     =   False
      ,  ehcOptEmitC            =   False
      ,  ehcOptEmitExecC        =   False
      ,  ehcOptFullProgAnalysis =   False
      ,  ehcOptDumpGrinStages   =   False
%%]]
%%[[(8 codegen java)
      ,  ehcOptEmitJava         =   False
%%]]
%%[[8
      ,  ehcOptEmitHS           =   False
      ,  ehcOptEmitEH           =   False
      
      ,  ehcOptSearchPath       =   []
      ,  ehcOptVerbosity        =   VerboseNormal
      ,  ehcOptBuiltinNames     =   mkEHBuiltinNames (const id)
      ,  ehcOptUseInplace       =   True
      
%%]]
%%[[(8 codegen grin)
      ,  ehcOptEmitBytecode     =   False
      ,  ehcOptEmitExecBytecode =   False
      ,  ehcOptErrAboutBytecode =   False
      ,  ehcOptGenCmt           =   True
%%][(99 codegen grin)
      ,  ehcOptEmitBytecode     =   True
      ,  ehcOptEmitExecBytecode =   True
      ,  ehcOptErrAboutBytecode =   True
      ,  ehcOptGenCmt           =   False
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf  =   False
      ,  ehcOptPrfCutOffAt      =   20
      ,  ehcCfgClassViaRec      =   False -- True
      -- ,  ehcCfgCHRScoped     =   CHRScopedAll
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt
                                =   10
%%]]
%%[[(20 codegen)
      ,  ehcDebugStopAtCoreError=   False
%%]]
%%[[20
      ,  ehcOptCheckRecompile   =   True
      ,  ehcDebugStopAtHIError  =   False
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree	=	DerivTreeWay_None
      ,  ehcOptEmitDerivTreePaperSize
      						    =   "2"
      ,  ehcOptEmitDerivFitsIn  =   False
%%]]
%%[[99
      ,  ehcProgName            =   emptyFPath
      ,  ehcOptShowNumVersion   =   False
      ,  ehcOptCPP              =   False
      ,  ehcOptUseAssumePrelude =   True
%%]]
      }
%%]

%%[1
ehcCmdLineOpts
  =  [  Option "d"  ["debug"]            (NoArg oDebug)                       "show debug information"
     ,  Option "p"  ["pretty"]           (OptArg oPretty "hs|eh|ast|-")       "show pretty printed source or EH abstract syntax tree, default=eh, -=off, (downstream only)"
     ,  Option ""   ["priv"]             (boolArg oPriv)                      "private flag, used during development of 2 impls of 1 feature"
%%[[(1 hmtyinfer)
     ,  Option ""   ["show-top-ty"]      (OptArg oShowTopTy "yes|no")         "show top ty, default=no"
%%]]
     ,  Option "h"  ["help"]             (NoArg oHelp)                        "only show this help"
     ,  Option ""   ["version"]          (NoArg oVersion)                     "only show version info"
     ,  Option ""   ["stopat"]
%%[[1
                                         (ReqArg oStopAt "0|1|2|3")           "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh"
%%][8
                                         (ReqArg oStopAt "0|1|2|3|4")         "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh, 4=core"
%%]]
%%[[7_2
     ,  Option ""   ["nounique"]         (NoArg oUnique)                      "do not compute uniqueness solution"
%%]]
%%[[(8 codegen)
     ,  Option ""   ["trf"]              (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
                                                                              "switch on/off core transformations"
     ,  Option ""   ["dump-core-stages"] (boolArg optDumpCoreStages)          "dump intermediate Core transformation stages (no)"
%%]]
%%[[(8 codegen grin)
     ,  Option ""   ["dump-grin-stages"] (boolArg optDumpGrinStages)          "dump intermediate Grin and Silly transformation stages (no)"
     ,  Option "O"  ["optimise"]         (OptArg oOptimise "0|1|2")           "optimise, 0=none 1=normal 2=more, default=1"
     ,  Option ""   ["time-compilation"] (NoArg oTimeCompile)                 "show grin compiler CPU usage for each compilation phase (only with -v2)"

     ,  Option ""   ["gen-casedefault"]  (boolArg optSetGenCaseDefault)       "trap wrong casedistinction in C (no)"
     ,  Option "g"  ["gen-own"]          (OptArg  oOwn "0|1|2|3|4")           "generate own 1=parameters/tailjumps, 2=locals, 3=calls, 4=stack (3)"
     ,  Option ""   ["gen-cmt"]          (boolArg optSetGenCmt)               "include comment about code in generated code"
     ,  Option ""   ["gen-debug"]        (boolArg optSetGenDebug)             "include debug info in generated code (yes)"
%%]]
%%[[(8 codegen java)
%%]]
%%[[8
     ,  Option "c"  ["code"]             (OptArg oCode "hs|eh|core|java|grin|c|exe[c]|jvm|llvm|lexe[c]|bc|bexe[c]|-")  "write code to file, default=core (downstream only)"
     ,  Option "v"  ["verbose"]          (OptArg oVerbose "0|1|2|3")          "be verbose, 0=quiet 1=normal 2=noisy 3=debug-noisy, default=1"
%%]]
%%[[(8 codegen grin)
     ,  Option ""   ["gen-trace"]        (boolArg optSetGenTrace)             "trace functioncalls in C (no)"
     ,  Option ""   ["gen-rtsinfo"]      (ReqArg oRTSInfo "<nr>")             "flags for rts info dumping (default=0)"
%%][100
%%]]
%%[[9
     -- ,  Option ""   ["chr-scoped"]       (ReqArg  oCHRScoped "0|1|2")         "scoped CHR gen: 0=inst, 1=super, 2=all (default=2)"
%%]]
%%[[(20 codegen)
     ,  Option ""   ["debug-stopat-core-error"]
                                         (boolArg oStopAtCoreError)           "debug: stop at .core parse error (default=off)"
%%]]
%%[[20
     ,  Option ""   ["no-recomp"]        (NoArg oNoRecomp)                    "turn off recompilation check (force recompile)"
     ,  Option ""   ["debug-stopat-hi-error"]
                                         (boolArg oStopAtHIError)             "debug: stop at .hi parse error (default=off)"
%%]]
%%[[99
     ,  Option ""   ["numeric-version"]  (NoArg oNumVersion)                  "only show numeric version"
     ,  Option "P"  ["search-path"]      (ReqArg oSearchPath "path")          "search path for all files, path separators=';', appended to previous"
     ,  Option ""   ["no-prelude"]       (NoArg oNoPrelude)                   "do not assume presence of Prelude"
     ,  Option ""   ["cpp"]              (NoArg oCPP)                         "preprocess source with CPP"
     ,  Option ""   ["limit-tysyn-expand"]
                                         (intArg oLimitTyBetaRed)             "type synonym expansion limit"
     -- 20071002: limiting the number of context reduction steps is not supported starting with the use of CHRs
     -- ,  Option ""   ["limit-ctxt-red"]   (intArg oLimitCtxtRed)               "context reduction steps limit"
     
     ,  Option ""   ["use-inplace"]      (boolArg oUseInplace)                "use the inplace runtime libraries"
%%]]
%%[[(99 hmtyinfer)
     ,  Option ""   ["deriv-tree"]       (OptArg oDerivTree ("f|i[,p=[{0,1,2,3,4,5}|<n>m]][,f=" ++ boolArgStr ++ "]"))
                                                                              "emit derivation tree on .lhs file; f=final, i=infer, default=f; p=paper size (0=a0,...; <n>m=2^<n> meter), dflt=2; f=show subsumption"
%%][100
%%]]
     ]
%%]
%%[1
  where  oPretty     ms  o =  case ms of
                                Just "-"     -> o { ehcOptShowEH       = False     }
                                Just "no"    -> o { ehcOptShowEH       = False     }
                                Just "off"   -> o { ehcOptShowEH       = False     }
                                Just "hs"    -> o { ehcOptShowHS       = True      }
                                Just "eh"    -> o { ehcOptShowEH       = True      }
                                Just "pp"    -> o { ehcOptShowEH       = True      }
%%[[1
                                Just "ast"   -> o { ehcOptShowAst      = True      }
%%][100
%%]]
                                _            -> o
%%[[(1 hmtyinfer)
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
%%]]
         oHelp           o =  o { ehcOptHelp          = True    }
         oVersion        o =  o { ehcOptVersion       = True    }
         oDebug          o =  o { ehcOptDebug         = True
%%[[1
                                , ehcOptShowAst       = True
%%][100
%%]]
                                }
         oStopAt       s o =  o { ehcStopAtPoint       =
                                    case s of
                                      "0" -> CompilePoint_Imports
                                      "1" -> CompilePoint_Parse
                                      "2" -> CompilePoint_AnalHS
                                      "3" -> CompilePoint_AnalEH
%%[[(8 codegen)
                                      "4" -> CompilePoint_Core
%%]]
                                      _   -> CompilePoint_All
                                }
%%[[7_2
         oUnique         o =  o { ehcOptUniqueness    = False   }
%%]]
%%[[(8 codegen)
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
%%]]
%%[[8
         oCode       ms  o =  case ms of
                                Just "hs"    -> o { ehcOptEmitHS           = True   }
                                Just "eh"    -> o { ehcOptEmitEH           = True   }
%%[[(8 codegen)
                                Just "-"     -> o { ehcOptEmitCore         = False  }
                                Just "core"  -> o { ehcOptEmitCore         = True   }
%%]]
%%[[(8 codegen java)
                                Just "java"  -> o { ehcOptEmitJava         = True   }
%%]]
%%[[(8 codegen grin)
                                Just "grin"  -> o { ehcOptEmitGrin         = True   }
                                Just "bc"    -> o { ehcOptEmitBytecode     = True 
                                                  , ehcOptFullProgAnalysis = False
                                                  }
                                Just m | m `elem` ["bexe","bexec"]
                                             -> o { ehcOptEmitBytecode     = True
                                                  , ehcOptEmitExecBytecode = True
                                                  , ehcOptFullProgAnalysis = False
                                                  }

                                Just "c"     -> o { ehcOptEmitC            = True
                                                  , ehcOptFullProgAnalysis = True
                                                  , ehcOptEmitExecBytecode = False
                                                  , ehcOptEmitBytecode     = False
                                                  , ehcOptErrAboutBytecode = False
                                                  }

                                Just m | m `elem` ["exe","exec"]
                                             -> o { ehcOptEmitC            = True
                                                  , ehcOptEmitExecC        = True
                                                  , ehcOptFullProgAnalysis = True
                                                  , ehcOptEmitExecBytecode = False
                                                  , ehcOptEmitBytecode     = False
                                                  , ehcOptErrAboutBytecode = False
                                                  }

                                Just "jvm"   -> o { ehcOptEmitJVM          = True
                                                  , ehcOptFullProgAnalysis = True
                                                  , ehcOptEmitExecBytecode = False
                                                  , ehcOptEmitBytecode     = False
                                                  , ehcOptErrAboutBytecode = False
                                                  }

                                Just "llvm"  -> o { ehcOptEmitLLVM         = True
                                                  , ehcOptFullProgAnalysis = True
                                                  , ehcOptEmitExecBytecode = False
                                                  , ehcOptEmitBytecode     = False
                                                  , ehcOptErrAboutBytecode = False
                                                  }
                                Just m | m `elem` ["lexe", "lexec"]
                                             -> o { ehcOptEmitLLVM         = True
                                                  , ehcOptEmitExecLLVM     = True
                                                  , ehcOptFullProgAnalysis = True
                                                  , ehcOptEmitExecBytecode = False
                                                  , ehcOptEmitBytecode     = False
                                                  , ehcOptErrAboutBytecode = False
                                                  }                   
%%]]
%%[[(99 hmtyinfer)
                                Just "dt"    -> o { ehcOptEmitDerivTree    = DerivTreeWay_Final   }
%%]]
                                _            -> o

%%[[(8 codegen)
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
%%]]
%%[[(8 codegen grin)
         oOwn        ms  o =  case ms of
                                Just "0"    -> o { ehcOptOwn     = 0       }
                                Just "1"    -> o { ehcOptOwn     = 1       }
                                Just "2"    -> o { ehcOptOwn     = 2       }
                                Just "3"    -> o { ehcOptOwn     = 3       }
                                Just "4"    -> o { ehcOptOwn     = 4       }
                                Just "5"    -> o { ehcOptOwn     = 5       }
                                Nothing     -> o { ehcOptOwn     = 3       }
                                _           -> o { ehcOptOwn     = 3       }
         oRTSInfo    s   o =  o { ehcOptGenRTSInfo     = read s       }
%%]]
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Just "3"    -> o { ehcOptVerbosity     = VerboseDebug       }
                                Nothing     -> o { ehcOptVerbosity     = VerboseALot        }
                                _           -> o
%%[[(8 codegen grin)
         oOptimise   ms  o =  case ms of
                                Just "0"    -> o { ehcOptOptimise      = OptimiseNone       }
                                Just "1"    -> o { ehcOptOptimise      = OptimiseNormal     }
                                Just "2"    -> o { ehcOptOptimise      = OptimiseALot       }
                                Nothing     -> o { ehcOptOptimise      = OptimiseALot       }
                                _           -> o
%%]]
%%]]
%%[[9
{-
         oCHRScoped    s o =  o { ehcCfgCHRScoped       =
                                    case s of
                                      "0" -> CHRScopedInstOnly
                                      "1" -> CHRScopedMutualSuper
                                      "2" -> CHRScopedAll
                                      _   -> CHRScopedAll
                                }
-}
%%]]
%%[[20
         oNoRecomp       o =  o { ehcOptCheckRecompile             = False   }
%%]]
%%[[99
         oNumVersion     o =  o { ehcOptShowNumVersion          = True    }
         oSearchPath  s  o =  o { ehcOptSearchPath = ehcOptSearchPath o ++ wordsBy (==';') s }
         oNoPrelude      o =  o { ehcOptUseAssumePrelude        = False   }
         oCPP            o =  o { ehcOptCPP                     = True    }
         oLimitTyBetaRed o l = o { ehcOptTyBetaRedCutOffAt = l }
         oLimitCtxtRed   o l = o { ehcOptPrfCutOffAt       = l }
         oUseInplace     o b = o { ehcOptUseInplace = b }
%%]]
%%[[(99 hmtyinfer)
         oDerivTree  ms  o =  case ms of
                                Just ('f':a) -> opts a $ o { ehcOptEmitDerivTree    = DerivTreeWay_Final  }
                                Just ('i':a) -> opts a $ o { ehcOptEmitDerivTree    = DerivTreeWay_Infer  }
                                Nothing      ->          o { ehcOptEmitDerivTree    = DerivTreeWay_Final  }
                                _            ->          o
                           where opts (',':'p':'=':sz:'m':r) o = opts r $ o { ehcOptEmitDerivTreePaperSize = ['m',sz] }
                                 opts (',':'p':'=':sz    :r) o = opts r $ o { ehcOptEmitDerivTreePaperSize = [sz] }
                                 opts (',':'f':'='       :r) o = maybe o (\(b,r) -> opts r $ o {ehcOptEmitDerivFitsIn = b}) (optBooleanTake r)
                                 opts _                      o = o
%%][100
%%]]
%%]

%%[99
intArg  tr = ReqArg (optInt tr) "<nr>"

optInt :: (EHCOpts -> Int -> EHCOpts) -> String -> EHCOpts -> EHCOpts
optInt tr s o
 = tr o $ read s
%%]

%%[1
optBooleanTake :: String -> Maybe (Bool,String)
optBooleanTake s
  = case s of
      ('-':r)           -> Just (False,r)
      ('n':'o':r)       -> Just (False,r)
      ('o':'f':'f':r)   -> Just (False,r)
      ('0':r)           -> Just (False,r)
      ('+':r)           -> Just (True ,r)
      ('y':'e':'s':r)   -> Just (True ,r)
      ('o':'n':r)       -> Just (True ,r)
      ('1':r)           -> Just (True ,r)
      _                 -> Nothing

optBoolean :: (EHCOpts -> Bool -> EHCOpts) -> Maybe String -> EHCOpts -> EHCOpts
optBoolean tr ms o
 = case ms of
     Just s -> maybe o (tr o . fst) (optBooleanTake s)
     _      -> o

boolArgStr = "0|1|no|yes|off|on|-|+"
boolArg tr = OptArg (optBoolean tr) boolArgStr
%%]

%%[1
oPriv                o b = o { ehcOptPriv           = b }
%%]

%%[(8 codegen)
optDumpCoreStages    o b = o { ehcOptDumpCoreStages = b }
%%]

%%[(8 codegen grin)
optSetGenTrace       o b = o { ehcOptGenTrace       = b }
optSetGenRTSInfo     o b = o { ehcOptGenRTSInfo     = b }
optSetGenCaseDefault o b = o { ehcOptGenCaseDefault = b }
optSetGenCmt         o b = o { ehcOptGenCmt         = b }
optSetGenDebug       o b = o { ehcOptGenDebug       = b }
optDumpGrinStages    o b = o { ehcOptDumpGrinStages = b, ehcOptEmitGrin = b }
%%]

%%[(20 codegen)
oStopAtCoreError     o b = o { ehcDebugStopAtCoreError     = b }
%%]

%%[20
oStopAtHIError       o b = o { ehcDebugStopAtHIError       = b }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Discrimination options for recompile, represent as string, difference means recompile
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(optsDiscrRecompileRepr)
optsDiscrRecompileRepr :: EHCOpts -> String
optsDiscrRecompileRepr opts
  = concat
    $ intersperse " "
    $ [ show (ehcOptAspects opts)
%%[[(20 codegen)
      , o "clsrec"          (ehcCfgClassViaRec      opts)
      , o "fullproggrin"    (ehcOptFullProgAnalysis opts)
      , o "exec"            (ehcOptEmitExecC        opts)
      , o "bexec"           (ehcOptEmitExecBytecode opts)
      , show (ehcOptOptimise opts)
%%]]
      ]
  where o m v = if v then m else ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Difference strong/weak:

strong: in a context where information is known (i.e. type signature)
strong allows impredicative binding whereas weak will instantiate quantifiers

%%[(9 hmtyinfer) export(FIOBind(..),fioBindIsYes,fioBindNoSet)
data FIOBind = FIOBindYes | FIOBindNoBut TyVarIdS

fioBindNoSet :: FIOBind -> TyVarIdS
fioBindNoSet (FIOBindNoBut s) = s

fioBindIsYes :: FIOBind -> Bool
fioBindIsYes FIOBindYes = True
fioBindIsYes _          = False
%%]

%%[(4 hmtyinfer).FIOpts.hd export(FIOpts(..))
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioBindLBeforeR         ::  Bool
                        ,  fioMode           ::  FIMode              ,  fioUniq                 ::  UID
%%[[7
                        ,  fioNoRLabElimFor  ::  [HsName]            ,  fioNoLLabElimFor        ::  [HsName]
%%]]
%%[[9
                        ,  fioPredAsTy       ::  Bool                ,  fioAllowRPredElim       ::  Bool
                        ,  fioDontBind       ::  TyVarIdS
                        ,  fioBindLVars      ::  FIOBind             ,  fioBindRVars            ::  FIOBind
%%]]
%%[[16
                        ,  fioFitFailureToProveObl    :: Bool
                        ,  fioFitVarFailureToProveObl :: Bool
%%]]
%%[[50
                        ,  fioAllowEqOpen    ::  Bool                ,  fioInstCoConst          ::  HowToInst
%%]]
                        }
%%]

%%[(4 hmtyinfer).strongFIOpts.hd export(strongFIOpts)
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
%%[[7
                        ,  fioNoRLabElimFor  =   []                  ,  fioNoLLabElimFor        =   []
%%]]
%%[[9
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
                        ,  fioDontBind       =   Set.empty
                        ,  fioBindLVars      =   FIOBindYes          ,  fioBindRVars            =   FIOBindYes
%%]]
%%[[16
                        ,  fioFitFailureToProveObl    = False
                        ,  fioFitVarFailureToProveObl = False
%%]]
%%[[50
                        ,  fioAllowEqOpen    =   False               ,  fioInstCoConst          =   instCoConst
%%]]
                        }
%%]

%%[(4 hmtyinfer)
instance Show FIOpts where
  show o =  "FIOpts"
%%]

%%[(4 hmtyinfer)
instance PP FIOpts where
  pp   o =  "FIOpts{"
            >#< "leaveRInst=" >|< pp (fioLeaveRInst o)
            >#< "bindLFirst=" >|< pp (fioBindLFirst o)
            >#< "bindRFirst=" >|< pp (fioBindRFirst o)
%%[[7
            >#< "fioNoLLabElimFor=" >|< pp (show $ fioNoLLabElimFor o)
            >#< "fioNoRLabElimFor=" >|< pp (show $ fioNoRLabElimFor o)
%%]]
%%[[9
            >#< "allowRPredElim=" >|< pp (fioAllowRPredElim o)
%%]]
            >#< "}"
%%]

%%[(4 hmtyinfer).FIOpts.instLFIOpts export(instLFIOpts)
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}
%%]

%%[(4 hmtyinfer).FIOpts.instLRFIOpts export(instLRFIOpts)
instLRFIOpts :: FIOpts
instLRFIOpts = strongFIOpts {fioBindRFirst = False, fioBindLFirst = False}
%%]

%%[(4 hmtyinfer).FIOpts.instFIOpts export(unifyFIOpts,instFIOpts)
unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]

%%[(4_2 hmtyinfer).FIOpts.defaults export(meetFIOpts,joinFIOpts,impredFIOpts)
meetFIOpts :: FIOpts
meetFIOpts = unifyFIOpts {fioMode = FitMeet}

joinFIOpts :: FIOpts
joinFIOpts = unifyFIOpts {fioMode = FitJoin}

impredFIOpts :: FIOpts
impredFIOpts = strongFIOpts {fioBindToTyAlts = True}
%%]

%%[(5 hmtyinfer) export(weakFIOpts)
weakFIOpts :: FIOpts
weakFIOpts = strongFIOpts {fioLeaveRInst = True, fioBindRFirst = False}
%%]

%%[(9 hmtyinfer) export(predFIOpts,implFIOpts)
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}

implFIOpts  :: FIOpts
implFIOpts = strongFIOpts {fioAllowRPredElim = False}
%%]

%%[(4 hmtyinfer) export(fioSwapPolarity, fioSwapOpts)
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio
  = fio
      { fioBindRFirst   = fioBindLFirst fio
      , fioBindLFirst   = fioBindRFirst fio
      , fioBindLBeforeR = not (fioBindLBeforeR fio)
%%[[9
      , fioBindLVars    = fioBindRVars fio
      , fioBindRVars    = fioBindLVars fio
%%]]
      }

fioSwapPolarity :: Polarity -> FIOpts -> FIOpts
fioSwapPolarity pol fio = fio {fioMode = fimSwapPol pol (fioMode fio)}
%%]

%%[(4 hmtyinfer).fioMkStrong export(fioMkStrong)
fioMkStrong :: FIOpts -> FIOpts
fioMkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}
%%]

%%[(4 hmtyinfer).fioMkUnify export(fioMkUnify)
fioMkUnify :: FIOpts -> FIOpts
fioMkUnify fi = fi {fioMode = FitUnify}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn opts related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fioIsSubsume)
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}
%%]

%%[(4_2 hmtyinfer) export(fioIsMeetJoin)
fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}
%%]
