module EH101.Opts
( module EH101.Opts.Base
, optOptsIsYes
, defaultEHCOpts
, ehcCmdLineOpts
, FIOpts (..)
, strongFIOpts
, instLFIOpts
, instLRFIOpts
, unifyFIOpts, instFIOpts
, fioSwapPolarity, fioSwapOpts
, fioMkStrong
, fioMkWeak
, fioMkUnify
, fioIsSubsume
, weakFIOpts
, Optimize (..), OptimizationLevel (..)
, module EH101.Base.FileSearchLocation
, ehcOptWholeProgHPTAnalysis
, ehcOptErrAboutBytecode
, ehcOptEmitExecBytecode, ehcOptEmitBytecode
, ehcOptEmitC
, ehcOptEmitCore
, ehcOptCoreSysF
, ehcOptOptimizes
, FIOBind (..)
, fioBindIsYes, fioBindNoSet
, predFIOpts, implFIOpts
, ehcOptWholeProgOptimizationScope
, ehcOptEarlyModMerge
, optsDiscrRecompileRepr
, ehcOptUpdateWithPragmas )
where
import System.Console.GetOpt
import EH101.Base.Common
import EH101.Opts.Base
import EH.Util.Utils
import Data.Maybe
import qualified Data.Map as Map
import Data.Char
import EH.Util.Pretty
import EH101.Ty
import qualified Data.Set as Set
import Data.List
import EH101.Base.Builtin
import EH.Util.FPath
import EH101.EHC.Environment
import EH101.Base.Target
import EH101.Base.Optimize
import EH101.Base.FileSearchLocation
import EH101.Error
import qualified EH101.ConfigInstall as Cfg
import EH101.Base.Pragma
import EH101.Base.Parser
import EH101.Base.Parser2









{-# LINE 66 "src/ehc/Opts.chs" #-}
-- | possibly adapt with pragmas
ehcOptUpdateWithPragmas :: Set.Set Pragma -> EHCOpts -> (EHCOpts,Bool)
ehcOptUpdateWithPragmas pragmas opts
  = foldr (\p om@(o,modf) -> maybe om (\o -> (o,True)) $ upd p o) (opts,False) (Set.toList pragmas)
  where upd pragma opts
          = case pragma of
              Pragma_NoGenericDeriving  -> Just $ opts { ehcOptGenGenerics 			= False }
              Pragma_GenericDeriving    -> Just $ opts { ehcOptGenGenerics 			= True  }
              Pragma_NoBangPatterns  	-> Just $ opts { ehcOptBangPatterns 		= False }
              Pragma_BangPatterns    	-> Just $ opts { ehcOptBangPatterns 		= True  }
              Pragma_ExtensibleRecords  -> Just $ opts { ehcOptExtensibleRecords 	= True  }
              Pragma_Fusion             -> Just $ opts { ehcOptFusion 				= True  }
              _                         -> Nothing

{-# LINE 86 "src/ehc/Opts.chs" #-}
mkStringPath :: String -> [String]
mkStringPath = wordsBy (`elem` ";,")

mkFileLocPath :: String -> FileLocPath
mkFileLocPath = map mkDirFileLoc . mkStringPath

{-# LINE 98 "src/ehc/Opts.chs" #-}
optOpts :: Map.Map String opt -> String -> [opt]
optOpts m s = catMaybes $ map (\os -> Map.lookup os m) $ wordsBy (==',') s

optOptsIsYes :: Eq opt => Maybe [opt] -> opt -> Bool
optOptsIsYes mos o = maybe False (o `elem`) mos

{-# LINE 106 "src/ehc/Opts.chs" #-}
instance Show CoreOpt where
  show CoreOpt_SysF = "sysf"

coreOptMp :: Map.Map String CoreOpt
coreOptMp = Map.fromList [ (show o, o) | o <- [minBound .. maxBound] ]

{-# LINE 137 "src/ehc/Opts.chs" #-}
-- do something with whole program
ehcOptWholeProgOptimizationScope :: EHCOpts -> Bool
ehcOptWholeProgOptimizationScope opts
  = ehcOptOptimizationScope opts >= OptimizationScope_WholeGrin

{-# LINE 144 "src/ehc/Opts.chs" #-}
-- compatibility option
ehcOptEarlyModMerge :: EHCOpts -> Bool
ehcOptEarlyModMerge opts
  = ehcOptOptimizationScope opts >= OptimizationScope_WholeCore

{-# LINE 151 "src/ehc/Opts.chs" #-}
-- do whole program analysis, with HPT
ehcOptWholeProgHPTAnalysis :: EHCOpts -> Bool
ehcOptWholeProgHPTAnalysis opts
  =  targetDoesHPTAnalysis (ehcOptTarget opts)
  || ehcOptWholeProgOptimizationScope opts

{-# LINE 161 "src/ehc/Opts.chs" #-}
-- report when Grin ByteCode errors occur
ehcOptErrAboutBytecode :: EHCOpts -> Bool
ehcOptErrAboutBytecode   = targetIsGrinBytecode . ehcOptTarget

{-# LINE 171 "src/ehc/Opts.chs" #-}
-- generate bytecode
ehcOptEmitExecBytecode :: EHCOpts -> Bool
ehcOptEmitExecBytecode = targetIsGrinBytecode . ehcOptTarget

ehcOptEmitBytecode :: EHCOpts -> Bool
ehcOptEmitBytecode = ehcOptEmitExecBytecode

{-# LINE 180 "src/ehc/Opts.chs" #-}
-- generate C
ehcOptEmitC :: EHCOpts -> Bool
ehcOptEmitC = targetIsC . ehcOptTarget

{-# LINE 204 "src/ehc/Opts.chs" #-}
-- generate Core
ehcOptEmitCore :: EHCOpts -> Bool
ehcOptEmitCore opts
  = ehcOptWholeProgHPTAnalysis opts || targetIsCore (ehcOptTarget opts)

{-# LINE 222 "src/ehc/Opts.chs" #-}
-- | Generate system F (20120421 AD: very much under construction)
ehcOptCoreSysF :: EHCOpts -> Bool
ehcOptCoreSysF _    = False

{-# LINE 232 "src/ehc/Opts.chs" #-}
-- | optimizes a particular option
ehcOptOptimizes :: Optimize -> EHCOpts -> Bool
ehcOptOptimizes o opts = o `Set.member` ehcOptOptimizations opts

{-# LINE 242 "src/ehc/Opts.chs" #-}
defaultEHCOpts
  = emptyEHCOpts

{-# LINE 251 "src/ehc/Opts.chs" #-}
ehcCmdLineOpts
  =  [  Option "h"  ["help"]             	(NoArg oHelp)                        	"print this help (then stop)"
     ,  Option ""   ["version"]          	(NoArg oVersion)                     	"print version info (then stop)"
     ,  Option ""   ["version-dotted"]   	(NoArg oNumVersion)                  	("print version in \"x.y.z\" style (then stop)")
     ,  Option ""   ["version-asnumber"] 	(NoArg oVersionAsNumber)             	("print version in \"xyz\" style (then stop)")
     ,  Option ""   ["numeric-version"]  	(NoArg oNumVersion)                  	"see --version-dotted (to become obsolete)"
     ,  Option "v"  ["verbose"]          	(OptArg oVerbose "0|1|2|3|4")        	(   "be verbose, 0=quiet, 4=debug, "
                                                                               		++ "default=1"
                                                                              		)
     ,  Option "t"  ["target"]           	(ReqArg oTarget (showSupportedTargets'  "|"))
     																				("generate code for target, default=" ++ show defaultTarget)
     ,  Option ""   ["target-flavor"]    	(ReqArg oTargetFlavor (showAllTargetFlavors' "|"))
     																				("generate code for target flavor, default=" ++ show defaultTargetFlavor)
     ,  Option "p"  ["pretty"]           	(OptArg oPretty "hs|eh|ast|-")       	"show pretty printed source or EH abstract syntax tree, default=eh, -=off, (downstream only)"


     ,  Option "O"  ["optimise"]         	(OptArg oOptimization ("0|1|2|3|<opt>[=" ++ boolArgStr ++ "]"))
                                                                              		"optimise with level or specific by name, default=1"
     ,  Option ""   ["no-recomp"]        	(NoArg oNoRecomp)                    	"turn off recompilation check (force recompile)"
     ,  Option ""   ["no-prelude"]       	(NoArg oNoPrelude)                   	"do not assume presence of Prelude"
     ,  Option ""   ["no-hi-check"]      	(NoArg oNoHiCheck)                   	"no check on .hi files not matching the compiler version"
     ,  Option "c"  ["compile-only"]     	(NoArg oCompileOnly)                 	"compile only, do not link"
     ,  Option "i"  ["import-path"]         (ReqArg oUsrFileLocPath "path")       	"search path for user files, separators=';', appended to previous"
     ,  Option "L"  ["lib-search-path"]     (ReqArg oLibFileLocPath "path")       	"search path for library files, see also --import-path"
     ,  Option ""   ["cpp"]                 (NoArg oCPP)                         	"preprocess source with CPP"
     ,  Option ""   ["limit-tysyn-expand"]  (intArg oLimitTyBetaRed)             	"type synonym expansion limit"
     ,  Option ""   ["odir"]                (ReqArg oOutputDir "dir")            	"base directory for generated files. Implies --compile-only"
     ,  Option "o"  ["output"]              (ReqArg oOutputFile "file")          	"file to generate executable to"
     ,  Option ""   ["keep-intermediate-files"] (NoArg oKeepIntermediateFiles) 		"keep intermediate files (default=off)"
     ,  Option ""   ["meta-variant"]        (NoArg oVariant)                     	"meta: print variant (then stop)"
     ,  Option ""   ["meta-target-default"] (NoArg oTargetDflt)                  	"meta: print the default codegeneration target (then stop)"
     ,  Option ""   ["meta-targets"]        (NoArg oTargets)                     	"meta: print supported codegeneration targets (then stop)"
     ,  Option ""   ["meta-optimizations"]  (NoArg oOptimizations)               	"meta: print optimization names (then stop)"
     ,  Option ""   ["meta-pkgdir-system"]  (NoArg oMetaPkgdirSys)               	"meta: print system package dir (then stop)"
     ,  Option ""   ["meta-pkgdir-user"]    (NoArg oMetaPkgdirUser)              	"meta: print user package dir (then stop)"
     ,  Option ""   ["package"]             (ReqArg oExposePackage "package")    	"see --pkg-expose"
     ,  Option ""   ["hide-all-packages"]   (NoArg oHideAllPackages)             	"see --pkg-hide-all"
     ,  Option ""   ["pkg-build"]           (ReqArg oPkgBuild "package")         	"pkg: build package from files. Implies --compile-only"
     ,  Option ""   ["pkg-expose"]          (ReqArg oExposePackage "package")    	"pkg: expose/use package"
     ,  Option ""   ["pkg-hide"]            (ReqArg oHidePackage   "package")    	"pkg: hide package"
     ,  Option ""   ["pkg-hide-all"]        (NoArg oHideAllPackages)             	"pkg: hide all (implicitly) assumed/used packages"
     ,  Option ""   ["pkg-searchpath"]      (ReqArg oPkgdirLocPath "path")       	"pkg: package search directories, each dir has <pkg>/<variant>/<target>/<flavor>"
     ,  Option ""   ["cfg-install-root"]    (ReqArg oCfgInstallRoot "dir")        	"cfg: installation root (to be used only by wrapper script)"
     ,  Option ""   ["cfg-install-variant"] (ReqArg oCfgInstallVariant "variant") 	"cfg: installation variant (to be used only by wrapper script)"
     ,  Option ""   ["coreopt"]             (ReqArg oOptCore "opt[,...]")        	("core opts: " ++ (concat $ intersperse " " $ Map.keys coreOptMp))
     ]
{-# LINE 389 "src/ehc/Opts.chs" #-}
  where  oPretty     ms  o =  case ms of
                                Just "-"     -> o { ehcOptShowEH       = False     }
                                Just "no"    -> o { ehcOptShowEH       = False     }
                                Just "off"   -> o { ehcOptShowEH       = False     }
                                Just "hs"    -> o { ehcOptShowHS       = True      }
                                Just "eh"    -> o { ehcOptShowEH       = True      }
                                Just "pp"    -> o { ehcOptShowEH       = True      }
                                _            -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Help    }
         oVersion        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Version }
         oVariant        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_Variant }
         oDebug          o =  o { ehcOptDebug         = True
                                }
         oStopAt       s o =  o { ehcStopAtPoint       =
                                    case s of
                                      "0" -> CompilePoint_Imports
                                      "1" -> CompilePoint_Parse
                                      "2" -> CompilePoint_AnalHS
                                      "3" -> CompilePoint_AnalEH
                                      "4" -> CompilePoint_Core
                                      _   -> CompilePoint_All
                                }
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
         oOptCore    s   o =  o { ehcOptCoreOpts = optOpts coreOptMp s }
         oTarget        s o =  o { ehcOptMbTarget          = mbtarget
                                 , ehcOptOptimizationScope = if isJustOk mbtarget && targetDoesHPTAnalysis (fromJustOk mbtarget)
                                                             then max oscope OptimizationScope_WholeGrin
                                                             else oscope
                                 }
                            where mbtarget = maybe (NotOk s) JustOk $ Map.lookup s supportedTargetMp
                                  oscope = ehcOptOptimizationScope o
         oTargetFlavor  s o =  o { ehcOptMbTargetFlavor  = maybe (NotOk s) JustOk $ Map.lookup s allTargetFlavorMp }
         oOptimizations   o =  o { ehcOptImmQuit         = Just ImmediateQuitOption_Meta_Optimizations       }
         oTargets        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_Targets       }
         oTargetDflt     o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_TargetDefault  }

         oCode       ms  o =  case ms of
                                Just "hs"    -> o { ehcOptEmitHS           = True   }
                                Just "eh"    -> o { ehcOptEmitEH           = True   }
                                Just "-"     -> o -- { ehcOptEmitCore         = False  }
                                Just "core"  -> o { ehcOptMbTarget         = JustOk Target_None_Core_None
                                                  }
                                Just "tycore"-> o { ehcOptMbTarget         = JustOk Target_None_TyCore_None
                                                  }
                                Just "grin"  -> o -- { ehcOptEmitGrin         = True   }
                                Just "bc"    -> o -- { ehcOptEmitBytecode     = True
                                                  -- , ehcOptWholeProgHPTAnalysis = False
                                                  -- }
                                Just m | m `elem` ["bexe","bexec"]
                                             -> o { ehcOptMbTarget         = JustOk Target_Interpreter_Grin_C
                                                  }

                                Just "c"     -> o -- { ehcOptEmitC            = True
                                                  -- , ehcOptWholeProgHPTAnalysis = True
                                                  -- , ehcOptEmitExecBytecode = False
                                                  -- , ehcOptEmitBytecode     = False
                                                  -- , ehcOptErrAboutBytecode = False
                                                  -- }



                                _            -> o

         oRTSInfo    s   o =  o { ehcOptGenRTSInfo     = read s       }
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseMinimal     }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "3"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Just "4"    -> o { ehcOptVerbosity     = VerboseDebug       }
                                Nothing     -> o { ehcOptVerbosity     = succ (ehcOptVerbosity o)}
                                _           -> o
         oOptimization ms o
                           = o' {ehcOptOptimizations = optimizeRequiresClosure os}
                           where (o',doSetOpts)
                                    = case ms of
                                        Just (clevel:',':cscope:_)
                                          | isJust mbO -> (fromJust mbO o, True)
                                          where mbO = mbLevelScope (Just clevel) (Just cscope)
                                        Just (',':cscope:_)
                                          | isJust mbO -> (fromJust mbO o, True)
                                          where mbO = mbLevelScope Nothing (Just cscope)
                                        Just olevel@(clevel:_)
                                          | isDigit clevel && l >= 0 && l < (maxscp * maxlev)
                                            -> ( o { ehcOptOptimizationLevel = toEnum lev, ehcOptOptimizationScope = toEnum sc }
                                               , True
                                               )
                                          where l = read olevel :: Int
                                                (sc,lev) = quotRem l maxlev
                                        Just optname@(_:_)
                                          -> case break (== '=') optname of
                                               (nm, yesno)
                                                 -> ( o { ehcOptOptimizations = os
                                                        , ehcOptOptimizeOptionMp = osmp `Map.union` ehcOptOptimizeOptionMp o
                                                        }
                                                    , False
                                                    )
                                                 where set True  opt = Set.insert opt $ ehcOptOptimizations o
                                                       set False opt = Set.delete opt $ ehcOptOptimizations o
                                                       (os,osmp)
                                                          = -- lookup name, and attempt to extract boolean of assumedly '=' prefixed string, or if not a boolean try to extract specific config whilst also assuming True for the boolean
                                                            case (Map.lookup nm allOptimizeMp, optArgTake optArgAllAllow $ drop 1 yesno) of
                                                              (Just opt, Just (OptArg_Bool b,_ ))   -> (set b     opt           , Map.empty)
                                                              (Just opt, Just (OptArg_Int  i,_ ))   -> (set True  opt           , optimizeOptionMpSingleton opt optopt v)
                                                                                                    where (optopt,optdflt) = allOptimizeOptionMpAnyOption opt
                                                                                                          v = maybe optdflt (\(_,(lo,_)) -> toEnum $ fromEnum lo + i)
                                                                                                              $ mapLookup2 opt optopt allOptimizeOptionMp
                                                              (Just opt, _                      )   -> (set True  opt           , Map.empty)
                                                              _                                     -> (ehcOptOptimizations o   , Map.empty)
                                        Nothing
                                          -> (o { ehcOptOptimizationLevel      = OptimizationLevel_Much       }, True)
                                        _ -> (o, False)
                                 os | doSetOpts = Map.findWithDefault Set.empty (ehcOptOptimizationLevel o') optimizationLevelMp
                                    | otherwise = ehcOptOptimizations o'
                                 maxlev = fromEnum (maxBound :: OptimizationLevel) + 1
                                 maxscp = fromEnum (maxBound :: OptimizationScope) + 1
                                 mbLevelScope ml ms
                                   | isJust l && isJust s = Just (\o -> o { ehcOptOptimizationLevel = toEnum (fromJust l), ehcOptOptimizationScope = toEnum (fromJust s) })
                                   | otherwise            = Nothing
                                   where l = r ehcOptOptimizationLevel maxlev ml
                                         s = r ehcOptOptimizationScope maxscp ms
                                         r dflt mx m
                                           | x >= 0 && x < mx = Just x
                                           | otherwise        = Nothing
                                           where x = (maybe (fromEnum $ dflt o) (\c -> read [c]) m) :: Int
         oNoRecomp              o   = o { ehcOptCheckRecompile              = False    }
         oCompileOnly           o   = o { ehcOptDoLinking                   = False    }
         oNoHiCheck             o   = o { ehcOptHiValidityCheck             = False    }
         oNumVersion            o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_VersionDotted }
         oVersionAsNumber       o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_VersionAsNumber }
         oUsrFileLocPath      s o   = o { ehcOptImportFileLocPath           = ehcOptImportFileLocPath o ++ mkFileLocPath s }
         oLibFileLocPath      s o   = o { ehcOptLibFileLocPath              = ehcOptLibFileLocPath o ++ mkFileLocPath s }
         oPkgdirLocPath       s o   = o { ehcOptPkgdirLocPath               = ehcOptPkgdirLocPath o ++ mkStringPath s }
         oNoPrelude             o   = o { ehcOptUseAssumePrelude            = False   }
         oCPP                   o   = o { ehcOptCPP                         = True    }
         oLimitTyBetaRed        o l = o { ehcOptTyBetaRedCutOffAt           = l }
         oLimitCtxtRed          o l = o { ehcOptPrfCutOffAt                 = l }
         oMetaPkgdirSys         o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_Meta_Pkgdir_System }
         oMetaPkgdirUser        o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_Meta_Pkgdir_User }
         oExposePackage       s o   = o { ehcOptLibPackages                 = ehcOptLibPackages   o ++ [s]
                                        , ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ pkgSearchFilter parsePkgKey PackageSearchFilter_ExposePkg [s]
                                        }
         oHidePackage         s o   = o { ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ pkgSearchFilter parsePkgKey PackageSearchFilter_HidePkg [s]
                                        }
         oHideAllPackages       o   = o { ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ [PackageSearchFilter_HideAll]
                                        -- , ehcOptHideAllPackages             = True
                                        }
         oOutputDir           s o   = o { ehcOptOutputDir                   = Just s
                                          -- no linking when no output file is generated. This is not failsafe, requires better solution as now no executable is generated when no --output is specified. Should depend on existence of main.
                                        , ehcOptDoLinking                   = isJust (ehcOptMbOutputFile o)
                                        }
         oOutputFile          s o   = o { ehcOptMbOutputFile                = Just (mkFPath s)
                                        , ehcOptDoLinking                   = True
                                        }
         oKeepIntermediateFiles o   = o { ehcOptKeepIntermediateFiles       = True }
         oPkgBuild            s o   = o { ehcOptPkg                         = Just (PkgOption_Build s)
                                        , ehcOptDoLinking                   = False
                                        }
         oCfgInstallRoot      s o   = o { ehcOptCfgInstallRoot              = Just s }
         oCfgInstallVariant   s o   = o { ehcOptCfgInstallVariant           = Just s }

{-# LINE 658 "src/ehc/Opts.chs" #-}
intArg  tr = ReqArg (optInt tr) "<nr>"

optInt :: (EHCOpts -> Int -> EHCOpts) -> String -> EHCOpts -> EHCOpts
optInt tr s o
 = tr o $ read s

{-# LINE 666 "src/ehc/Opts.chs" #-}
-- | What kind of optional args are allowed
data OptArgAllow
  = OptArgAllow_Bool
  | OptArgAllow_Int
  deriving (Eq,Enum,Bounded)

optArgAllAllow :: [OptArgAllow]
optArgAllAllow = [minBound .. maxBound]

{-# LINE 677 "src/ehc/Opts.chs" #-}
-- | An optional arg, universal type for all occurring variants
data OptArg
  = OptArg_Bool     Bool
  | OptArg_Int      Int

{-# LINE 684 "src/ehc/Opts.chs" #-}
optArgTake :: [OptArgAllow] -> String -> Maybe (OptArg,String)
optArgTake allow s
  = case s of
      ('-':r)           -> Just (OptArg_Bool False,r)
      ('n':'o':r)       -> Just (OptArg_Bool False,r)
      ('n':r)           -> Just (OptArg_Bool False,r)
      ('o':'f':'f':r)   -> Just (OptArg_Bool False,r)
      ('0':r) | noInt   -> Just (OptArg_Bool False,r)
      ('+':r)           -> Just (OptArg_Bool True ,r)
      ('y':'e':'s':r)   -> Just (OptArg_Bool True ,r)
      ('y':r)           -> Just (OptArg_Bool True ,r)
      ('o':'n':r)       -> Just (OptArg_Bool True ,r)
      ('1':r) | noInt   -> Just (OptArg_Bool True ,r)
      ( c :_) | yesInt && isDigit c
                        -> Just (OptArg_Int (read d) ,r)
                        where (d,r) = span isDigit s
      _                 -> Nothing
  where yesInt = OptArgAllow_Int `elem` allow
        noInt  = not yesInt

{-# LINE 706 "src/ehc/Opts.chs" #-}
optBooleanTake :: String -> Maybe (Bool,String)
optBooleanTake s
  = case optArgTake [OptArgAllow_Bool] s of
      Just (OptArg_Bool b, r)   -> Just (b,r)
      _                         -> Nothing

optBoolean :: (EHCOpts -> Bool -> EHCOpts) -> Maybe String -> EHCOpts -> EHCOpts
optBoolean tr ms o
 = case ms of
     Just s -> maybe o (tr o . fst) (optBooleanTake s)
     _      -> o

boolArgStr = "Bool"
boolArg tr = OptArg (optBoolean tr) boolArgStr

{-# LINE 727 "src/ehc/Opts.chs" #-}
oPriv                o b = o { ehcOptPriv           = b }

{-# LINE 731 "src/ehc/Opts.chs" #-}
optDumpCoreStages    o b = o { ehcOptDumpCoreStages = b }

{-# LINE 735 "src/ehc/Opts.chs" #-}
optSetGenTrace       o b = o { ehcOptGenTrace       = b }
optSetGenTrace2      o b = o { ehcOptGenTrace2      = b }
optSetGenRTSInfo     o b = o { ehcOptGenRTSInfo     = b }
optSetGenCaseDefault o b = o { ehcOptGenCaseDefault = b }
optSetGenCmt         o b = o { ehcOptGenCmt         = b }
optSetGenDebug       o b = o { ehcOptGenDebug       = b }
optDumpGrinStages    o b = o { ehcOptDumpGrinStages = b {-, ehcOptEmitGrin = b -} }
-- optEarlyModMerge     o b = o { ehcOptEarlyModMerge  = b }

{-# LINE 746 "src/ehc/Opts.chs" #-}
oStopAtCoreError     o b = o { ehcDebugStopAtCoreError     = b }

{-# LINE 750 "src/ehc/Opts.chs" #-}
oStopAtHIError       o b = o { ehcDebugStopAtHIError       = b }

{-# LINE 758 "src/ehc/Opts.chs" #-}
optsDiscrRecompileRepr :: EHCOpts -> String
optsDiscrRecompileRepr opts
  = concat
    $ intersperse " "
    $ [ show (ehcOptAspects opts)
      , o "clsrec"          (ehcCfgClassViaRec      opts)
      -- , o "exec"            (ehcOptEmitExecC        opts)
      -- , o "bexec"           (ehcOptEmitExecBytecode opts)
      , show (ehcOptTarget opts)
      , show (ehcOptOptimizationLevel opts)
      ]
  where o m v = if v then m else ""

{-# LINE 779 "src/ehc/Opts.chs" #-}
data FIOBind
  = FIOBindYes | FIOBindNoBut TyVarIdS
  deriving (Show)

{-# LINE 785 "src/ehc/Opts.chs" #-}
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  !Bool                ,  fioBindRFirst           ::  !Bool
                        ,  fioBindLFirst     ::  !Bool                ,  fioBindLBeforeR         ::  !Bool
                        ,  fioMode           ::  !FIMode              ,  fioUniq                 ::  !UID
                        ,  fioBindCategs     ::  ![TyVarCateg]
                        ,  fioNoRLabElimFor  ::  ![HsName]            ,  fioNoLLabElimFor        ::  ![HsName]
                        ,  fioDontBind       ::  !TyVarIdS
                        ,  fioExpandEqTyVar  ::  !Bool                -- expand tyvars also when equal. Required for Sys F translation.
                        ,  fioPredAsTy       ::  !Bool                ,  fioAllowRPredElim       ::  !Bool
                        ,  fioBindLVars      ::  !FIOBind             ,  fioBindRVars            ::  !FIOBind
      					,  fiMbMkErrClash    ::  Maybe (Ty -> Ty -> Err) -- alternate error construction for type clash
                        }

{-# LINE 819 "src/ehc/Opts.chs" #-}
fioBindNoSet :: FIOBind -> TyVarIdS
fioBindNoSet (FIOBindNoBut s) = s
fioBindNoSet _                = Set.empty

fioBindIsYes :: FIOBind -> Bool
fioBindIsYes FIOBindYes = True
fioBindIsYes _          = False

{-# LINE 829 "src/ehc/Opts.chs" #-}
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
                        ,  fioBindCategs     =   [TyVarCateg_Plain]
                        ,  fioNoRLabElimFor  =   []                  ,  fioNoLLabElimFor        =   []
                        ,  fioDontBind       =   Set.empty
                        ,  fioExpandEqTyVar  =   False
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
                        ,  fioBindLVars      =   FIOBindYes          ,  fioBindRVars            =   FIOBindYes
      					,  fiMbMkErrClash    =   Nothing
                        }

{-# LINE 859 "src/ehc/Opts.chs" #-}
instance Show FIOpts where
  show o =  "FIOpts"

{-# LINE 864 "src/ehc/Opts.chs" #-}
instance PP FIOpts where
  pp   o =  "FIOpts{"
            >#< "leaveRInst=" >|< pp (fioLeaveRInst o)
            >#< "bindLFirst=" >|< pp (fioBindLFirst o)
            >#< "bindRFirst=" >|< pp (fioBindRFirst o)
            >#< "fioNoLLabElimFor=" >|< pp (show $ fioNoLLabElimFor o)
            >#< "fioNoRLabElimFor=" >|< pp (show $ fioNoRLabElimFor o)
            >#< "allowRPredElim=" >|< pp (fioAllowRPredElim o)
            >#< "}"

{-# LINE 880 "src/ehc/Opts.chs" #-}
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}

{-# LINE 885 "src/ehc/Opts.chs" #-}
instLRFIOpts :: FIOpts
instLRFIOpts = strongFIOpts {fioBindRFirst = False, fioBindLFirst = False}

{-# LINE 890 "src/ehc/Opts.chs" #-}
unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}

{-# LINE 909 "src/ehc/Opts.chs" #-}
weakFIOpts :: FIOpts
weakFIOpts = fioMkWeak strongFIOpts

{-# LINE 914 "src/ehc/Opts.chs" #-}
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}

implFIOpts  :: FIOpts
implFIOpts = strongFIOpts {fioAllowRPredElim = False}

{-# LINE 922 "src/ehc/Opts.chs" #-}
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio
  = fio
      { fioBindRFirst   = fioBindLFirst fio
      , fioBindLFirst   = fioBindRFirst fio
      , fioBindLBeforeR = not (fioBindLBeforeR fio)
      , fioBindLVars    = fioBindRVars fio
      , fioBindRVars    = fioBindLVars fio
      }

fioSwapPolarity :: Polarity -> FIOpts -> FIOpts
fioSwapPolarity pol fio = fio {fioMode = fimSwapPol pol (fioMode fio)}

{-# LINE 939 "src/ehc/Opts.chs" #-}
fioMkStrong :: FIOpts -> FIOpts
fioMkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}

{-# LINE 944 "src/ehc/Opts.chs" #-}
fioMkWeak :: FIOpts -> FIOpts
fioMkWeak fi = fi {fioLeaveRInst = True, fioBindRFirst = False}

{-# LINE 949 "src/ehc/Opts.chs" #-}
fioMkUnify :: FIOpts -> FIOpts
fioMkUnify fi = fi {fioMode = FitUnify}

{-# LINE 958 "src/ehc/Opts.chs" #-}
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}

