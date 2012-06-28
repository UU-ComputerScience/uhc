module EH101.Opts.Base
( ImmediateQuitOption (..)
, EHCOpts (..)
, emptyEHCOpts
, ehcOptFromJust
, InOrOutputFor (..)
, CoreOpt (..)
, ehcOptTarget, ehcOptTargetFlavor
, ehcOptBuiltin, ehcOptBuiltin2
, PkgOption (..) )
where
import EH101.Base.Common
import EH.Util.Utils
import Data.Maybe
import qualified Data.Map as Map
import EH.Util.Pretty
import qualified Data.Set as Set
import Data.List
import Data.Char
import EH101.Base.Builtin
import EH.Util.FPath
import EH101.EHC.Environment
import EH101.Base.Target
import EH101.Base.Optimize
import EH101.Base.FileSearchLocation
import qualified EH101.ConfigInstall as Cfg
import EH101.Base.Pragma








{-# LINE 53 "src/ehc/Opts/Base.chs" #-}
data ImmediateQuitOption
  = ImmediateQuitOption_Help                                -- print help
  | ImmediateQuitOption_Version                             -- print version info
  | ImmediateQuitOption_Meta_Variant                        -- print variant number
  | ImmediateQuitOption_Meta_Targets                        -- print all codegeneration targets (empty if no codegen)
  | ImmediateQuitOption_Meta_TargetDefault                  -- print the default codegeneration target (dummy if no codegen)
  | ImmediateQuitOption_Meta_Optimizations                  -- print all optimizations
  | ImmediateQuitOption_Meta_Pkgdir_System                  -- print system package dir
  | ImmediateQuitOption_Meta_Pkgdir_User                    -- print user package dir
  | ImmediateQuitOption_VersionDotted                       -- print version in dotted style, for external version comparison
  | ImmediateQuitOption_VersionAsNumber                     -- print version as number, for external version comparison
  -- | ImmediateQuitOption_Meta_ExportEnv (Maybe String)       -- export (write) environmental info of installation
  -- | ImmediateQuitOption_Meta_DirEnv                         -- print dir of environmental info of installation

{-# LINE 77 "src/ehc/Opts/Base.chs" #-}
data InOrOutputFor
  = OutputFor_Module
  | OutputFor_Pkg
  | InputFrom_Loc FileLoc

{-# LINE 90 "src/ehc/Opts/Base.chs" #-}
data PkgOption
  = PkgOption_Build PkgName                         -- build a package

{-# LINE 99 "src/ehc/Opts/Base.chs" #-}
-- | Core options
data CoreOpt
  = CoreOpt_SysF			-- 20120419, work in startup/progress: generate System F
  deriving (Eq,Enum,Bounded)

{-# LINE 121 "src/ehc/Opts/Base.chs" #-}
data EHCOpts
  = EHCOpts
      {  ehcOptTrace          ::  forall a . String -> a -> a            -- tracing
      ,  ehcOptAspects        ::  String            -- which aspects are included in this compiler
      ,  ehcOptShowHS         ::  Bool              -- show HS pretty print on stdout
      ,  ehcOptShowEH         ::  Bool              -- show EH pretty print on stdout
      ,  ehcOptPriv           ::  Bool              -- privately used (in general during switch between 2 impls of 1 feature)
      ,  ehcOptHsChecksInEH   ::  Bool              -- do checks in EH which already have been done in HS (usually related to name absence/duplication). This is used for EH compilation only.
      ,  ehcOptShowTopTyPP    ::  Bool              -- show EH type of expression
      ,  ehcOptImmQuit        ::  Maybe ImmediateQuitOption
      ,  ehcOptDebug          ::  Bool              -- debug info
      ,  ehcStopAtPoint       ::  CompilePoint      -- stop at (after) compile phase
      ,  ehcOptExtensibleRecords
      						  ::  Bool
      ,  ehcOptOptimizations  ::  OptimizeS         -- individual optimizations to be done, derived from level + scope
      ,  ehcOptOptimizeOptionMp
                              ::  OptimizeOptionMp  -- optimization specific configuration
      ,  ehcOptOptimizationLevel
                              ::  OptimizationLevel          -- optimisation level
      ,  ehcOptOptimizationScope
                              ::  OptimizationScope          -- optimisation scope
      ,  ehcOptDumpCoreStages ::  Bool              -- dump intermediate Core transformation stages
      ,  ehcOptMbTarget       ::  MaybeOk Target            -- code generation target
      ,  ehcOptMbTargetFlavor ::  MaybeOk TargetFlavor      -- code generation target flavor
      ,  ehcOptCoreOpts       ::  [CoreOpt]  	    -- Core options
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool              -- generate runtime debug info
      ,  ehcOptGenTrace       ::  Bool
      ,  ehcOptGenTrace2      ::  Bool

      ,  ehcOptGenRTSInfo     ::  Int               -- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptDumpGrinStages ::  Bool              -- dump intermediate Grin transformation stages
      -- ,  ehcOptEarlyModMerge  ::  Bool              -- produce OneBigCore instead of OneBigGrin; useful for future Core-only optimizations
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptImportFileLocPath
                              ::  FileLocPath
      ,  ehcOptVerbosity      ::  Verbosity         -- verbosity level

      ,  ehcOptBuiltinNames   ::  EHBuiltinNames
      ,  ehcOptEnvironment    ::  EHCEnvironment    -- runtime environment
      ,  ehcOptBangPatterns   ::  Bool              -- allow bang patterns

      ,  ehcCfgInstFldHaveSelf::  Bool              -- functions/fields of instance get as arg the dictionary as well
      ,  ehcOptPrfCutOffAt    ::  Int               -- cut off limit for context reduction
      ,  ehcCfgClassViaRec    ::  Bool              -- instance representation via record instead of data
      -- ,  ehcCfgCHRScoped      ::  CHRScoped          -- how to gen scoped CHR's (option is used only for paper writing + experimenting)
      ,  ehcOptTyBetaRedCutOffAt                    -- cut off for type lambda expansion
                              ::  Int
      ,  ehcDebugStopAtCoreError
                              ::  Bool              -- stop when Core parse error occurs (otherwise errors are ignored, repaired .core is used)
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcDebugStopAtHIError::  Bool              -- stop when HI parse error occurs (otherwise it is ignored, .hi thrown away)
      ,  ehcOptDoLinking      ::  Bool              -- do link, if False compile only
      ,  ehcOptGenGenerics    ::  Bool              -- generate for use of generics
      ,  ehcOptFusion   	  ::  Bool				-- allow fusion syntax, the optimization itself is triggered by optimize flags
      ,  ehcOptHiValidityCheck::  Bool              -- when .hi and compiler are out of sync w.r.t. timestamp and checksum, recompile
      ,  ehcOptLibFileLocPath ::  FileLocPath
      ,  ehcOptPkgdirLocPath  ::  StringPath
      ,  ehcOptPkgDb          ::  PackageDatabase   -- package database to be used for searching packages
      ,  ehcOptLibPackages    ::  [String]
      ,  ehcProgName          ::  FPath             -- name of this program
      ,  ehcOptUserDir        ::  String            -- user dir for storing user specific stuff
      ,  ehcOptMbOutputFile   ::  Maybe FPath       -- in which file to put generated output/executable
      ,  ehcOptCPP            ::  Bool              -- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude                     -- use & assume presence of prelude
                              ::  Bool
      ,  ehcOptPackageSearchFilter   ::  [PackageSearchFilter]  -- description of what to expose from package database
      ,  ehcOptOutputDir      ::  Maybe String      -- where to put output, instead of same dir as input file
      ,  ehcOptKeepIntermediateFiles
                              ::  Bool              -- keep intermediate files
      ,  ehcOptPkg            ::  Maybe PkgOption   -- package building (etc) option
      ,  ehcOptCfgInstallRoot        ::  Maybe String      -- the directory where the installation resides; overrides ehcenvInstallRoot
      ,  ehcOptCfgInstallVariant     ::  Maybe String      -- the installation variant; overrides ehcenvVariant
      }

{-# LINE 252 "src/ehc/Opts/Base.chs" #-}
emptyEHCOpts
  = EHCOpts
      {  ehcOptTrace            =   \_ x -> x
      ,  ehcOptAspects          =   "base hmtyinfer codegen grin noHmTyRuler javascript"
      ,  ehcOptShowHS           =   False
      ,  ehcOptPriv             =   False
      ,  ehcOptHsChecksInEH     =   False
      ,  ehcOptShowEH           =   False
      ,  ehcOptShowTopTyPP      =   False
      ,  ehcOptImmQuit          =   Nothing
      ,  ehcOptDebug            =   False
      ,  ehcStopAtPoint         =   CompilePoint_All
      ,  ehcOptExtensibleRecords=   False
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimizations    =   optimizeRequiresClosure $ Map.findWithDefault Set.empty OptimizationLevel_Normal optimizationLevelMp
      ,  ehcOptOptimizeOptionMp =   Map.empty
      ,  ehcOptOptimizationLevel=   OptimizationLevel_Normal
      ,  ehcOptOptimizationScope=   OptimizationScope_PerModule
      ,  ehcOptMbTarget         =   JustOk defaultTarget
      ,  ehcOptMbTargetFlavor   =   JustOk defaultTargetFlavor
      ,  ehcOptCoreOpts         =   []
      ,  ehcOptTimeCompile      =   False
      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace         =   False
      ,  ehcOptGenTrace2        =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptDumpGrinStages   =   False
      -- ,  ehcOptEarlyModMerge    =   False
      ,  ehcOptBangPatterns		= 	True
      ,  ehcOptVerbosity        =   VerboseMinimal
      ,  ehcOptEmitHS           =   False
      ,  ehcOptEmitEH           =   False

      ,  ehcOptImportFileLocPath=   []
      ,  ehcOptBuiltinNames     =   mkEHBuiltinNames (const id)
      ,  ehcOptEnvironment      =   undefined   -- filled in at toplevel

      ,  ehcOptGenCmt           =   False
      ,  ehcCfgInstFldHaveSelf  =   False
      ,  ehcOptPrfCutOffAt      =   20
      ,  ehcCfgClassViaRec      =   False -- True
      -- ,  ehcCfgCHRScoped     =   CHRScopedAll
      ,  ehcOptTyBetaRedCutOffAt
                                =   10
      ,  ehcDebugStopAtCoreError=   False
      ,  ehcOptCheckRecompile   =   True
      ,  ehcDebugStopAtHIError  =   False
      ,  ehcOptDoLinking        =   True
      ,  ehcOptGenGenerics      =   True
      ,  ehcOptFusion			=   False
      ,  ehcOptHiValidityCheck  =   True
      ,  ehcOptLibFileLocPath   =   []
      ,  ehcOptPkgdirLocPath    =   []
      ,  ehcOptPkgDb            =   emptyPackageDatabase
      ,  ehcOptLibPackages      =   []
      ,  ehcProgName            =   emptyFPath
      ,  ehcOptUserDir          =   ""
      ,  ehcOptMbOutputFile     =   Nothing
      ,  ehcOptCPP              =   False
      ,  ehcOptUseAssumePrelude =   True
      ,  ehcOptPackageSearchFilter
                                =   [] -- pkgSearchFilter parsePkgKey PackageSearchFilter_ExposePkg Cfg.ehcAssumedPackages
      ,  ehcOptOutputDir        =   Nothing
      ,  ehcOptKeepIntermediateFiles
                                =   False
      ,  ehcOptPkg              =   Nothing
      ,  ehcOptCfgInstallRoot   =   Nothing
      ,  ehcOptCfgInstallVariant=   Nothing
      }

{-# LINE 391 "src/ehc/Opts/Base.chs" #-}
ehcOptTarget :: EHCOpts -> Target
ehcOptTarget = maybeOk (\s -> panic ("ehcOptTarget: " ++ s)) id  . ehcOptMbTarget

ehcOptTargetFlavor :: EHCOpts -> TargetFlavor
ehcOptTargetFlavor = maybeOk (\s -> panic ("ehcOptTargetFlavor: " ++ s)) id . ehcOptMbTargetFlavor

{-# LINE 403 "src/ehc/Opts/Base.chs" #-}
ehcOptBuiltin :: EHCOpts -> (EHBuiltinNames -> x) -> x
ehcOptBuiltin o f = f $ ehcOptBuiltinNames o

ehcOptBuiltin2 :: EHCOpts -> (EHBuiltinNames -> Int -> HsName) -> Int -> HsName
ehcOptBuiltin2 o f i = f (ehcOptBuiltinNames o) i

{-# LINE 415 "src/ehc/Opts/Base.chs" #-}
-- | Either fromJust with a possible panic, or with a default value (when debugging)
ehcOptFromJust :: EHCOpts -> String -> a -> Maybe a -> a
ehcOptFromJust opts panicMsg n m
  | ehcOptDebug opts = maybe n id m
  | otherwise        = panicJust panicMsg m

