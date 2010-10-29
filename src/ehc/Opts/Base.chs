%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Opts.Base} import({%{EH}Base.Common})
%%]

%%[1 import(EH.Util.Utils)
%%]

%%[1 import(Data.Maybe,qualified Data.Map as Map)
%%]

%%[4 import(EH.Util.Pretty)
%%]

%%[7 import(qualified Data.Set as Set)
%%]

%%[8 import(Data.List,Data.Char,{%{EH}Base.Builtin})
%%]

%%[8 import(EH.Util.FPath)
%%]
%%[8 import({%{EH}EHC.Environment})
%%]

%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%[(8 codegen) import({%{EH}Base.Optimize})
%%]

%%[8 import({%{EH}Base.FileSearchLocation})
%%]

%%[99 import(qualified {%{EH}ConfigInstall} as Cfg)
%%]

%%[99 import({%{EH}Base.Pragma})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Option after which its handling the compiler quits immediately
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(ImmediateQuitOption(..))
data ImmediateQuitOption
  = ImmediateQuitOption_Help                                -- print help
  | ImmediateQuitOption_Version                             -- print version info
  | ImmediateQuitOption_Meta_Variant                        -- print variant number
  | ImmediateQuitOption_Meta_Targets                        -- print all codegeneration targets (empty if no codegen)
  | ImmediateQuitOption_Meta_TargetDefault                  -- print the default codegeneration target (dummy if no codegen)
%%[[(8 codegen)
  | ImmediateQuitOption_Meta_Optimizations                  -- print all optimizations
%%]]
%%[[99
  | ImmediateQuitOption_Meta_Pkgdir_System                  -- print system package dir
  | ImmediateQuitOption_Meta_Pkgdir_User                    -- print user package dir
  | ImmediateQuitOption_VersionDotted                       -- print version in dotted style, for external version comparison
  | ImmediateQuitOption_VersionAsNumber                     -- print version as number, for external version comparison
  -- | ImmediateQuitOption_Meta_ExportEnv (Maybe String)       -- export (write) environmental info of installation
  -- | ImmediateQuitOption_Meta_DirEnv                         -- print dir of environmental info of installation
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Category of output, used for specifying which location something should be put
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(InOrOutputFor(..))
data InOrOutputFor
  = OutputFor_Module
%%[[99
  | OutputFor_Pkg
  | InputFrom_Loc FileLoc
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package option, other than just using it. Similar to ghc-pkg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(PkgOption(..))
data PkgOption
  = PkgOption_Build PkgName                         -- build a package
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Option specific options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) export(TyCoreOpt(..))
data TyCoreOpt
  = TyCoreOpt_Sugar         -- produce/accept sugared version
  | TyCoreOpt_Unicode       -- produce/accept unicode, implies sugar
  deriving Eq

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Convention: most option names/fields start with 'ehcOpt'

%%[1.EHCOpts export(EHCOpts(..))
data EHCOpts
  = EHCOpts
      {  ehcOptAspects        ::  String            -- which aspects are included in this compiler
      ,  ehcOptShowHS         ::  Bool              -- show HS pretty print on stdout
      ,  ehcOptShowEH         ::  Bool              -- show EH pretty print on stdout
%%[[(8 codegen tycore)
      ,  ehcOptShowTyCore     ::  Bool              -- show TyCore ast on stout
%%]]
      ,  ehcOptPriv           ::  Bool              -- privately used (in general during switch between 2 impls of 1 feature)
      ,  ehcOptHsChecksInEH   ::  Bool              -- do checks in EH which already have been done in HS (usually related to name absence/duplication). This is used for EH compilation only.
%%[[1
      ,  ehcOptShowAst        ::  Bool              -- show decorated EH AST on stdout
%%][100
%%]]
%%[[(1 hmtyinfer)
      ,  ehcOptShowTopTyPP    ::  Bool              -- show EH type of expression
%%]]
      ,  ehcOptImmQuit        ::  Maybe ImmediateQuitOption
      ,  ehcOptDebug          ::  Bool              -- debug info
      ,  ehcStopAtPoint       ::  CompilePoint      -- stop at (after) compile phase
%%[[7
      ,  ehcOptExtensibleRecords
      						  ::  Bool
%%]]
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[(8 codegen)
      ,  ehcOptOptimizations  ::  OptimizeS         -- individual optimizations to be done, derived from level + scope
      ,  ehcOptOptimizationLevel
                              ::  OptimizationLevel          -- optimisation level
      ,  ehcOptOptimizationScope
                              ::  OptimizationScope          -- optimisation scope
      ,  ehcOptDumpCoreStages ::  Bool              -- dump intermediate Core transformation stages
      ,  ehcOptMbTarget       ::  MaybeOk Target            -- code generation target
      ,  ehcOptMbTargetFlavor ::  MaybeOk TargetFlavor      -- code generation target flavor
%%]]
%%[[(8 codegen tycore)
      ,  ehcOptUseTyCore      ::  Maybe [TyCoreOpt] -- use TyCore instead of Core (temporary option until Core is obsolete)
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool              -- generate runtime debug info
      ,  ehcOptGenTrace       ::  Bool
      ,  ehcOptGenTrace2      ::  Bool

      ,  ehcOptGenRTSInfo     ::  Int               -- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptDumpGrinStages ::  Bool              -- dump intermediate Grin transformation stages
      -- ,  ehcOptEarlyModMerge  ::  Bool              -- produce OneBigCore instead of OneBigGrin; useful for future Core-only optimizations
%%]]
%%[[8
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptImportFileLocPath
                              ::  FileLocPath
      ,  ehcOptVerbosity      ::  Verbosity         -- verbosity level

      ,  ehcOptBuiltinNames   ::  EHBuiltinNames
      ,  ehcOptEnvironment    ::  EHCEnvironment    -- runtime environment
      
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf::  Bool              -- functions/fields of instance get as arg the dictionary as well
      ,  ehcOptPrfCutOffAt    ::  Int               -- cut off limit for context reduction
      ,  ehcCfgClassViaRec    ::  Bool              -- instance representation via record instead of data
      -- ,  ehcCfgCHRScoped      ::  CHRScoped          -- how to gen scoped CHR's (option is used only for paper writing + experimenting)
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt                    -- cut off for type lambda expansion
                              ::  Int
%%]]
%%[[(20 codegen)
      ,  ehcDebugStopAtCoreError
                              ::  Bool              -- stop when Core parse error occurs (otherwise errors are ignored, repaired .core is used)
%%]]
%%[[20
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcDebugStopAtHIError::  Bool              -- stop when HI parse error occurs (otherwise it is ignored, .hi thrown away)
      ,  ehcOptDoLinking      ::  Bool              -- do link, if False compile only
%%]]
%%[[92
      ,  ehcOptGenGenerics    ::  Bool              -- generate for use of generics
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree  ::  DerivTreeWay      -- show derivation tree on stdout
      ,  ehcOptEmitDerivTreePaperSize
                              ::  String            -- the paper size to be used
      ,  ehcOptEmitDerivFitsIn
                              ::  Bool              -- show fitsIn derivation tree as well
%%]]
%%[[99
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
%%]]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Empty compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.defaultEHCOpts export(emptyEHCOpts)
emptyEHCOpts
  = EHCOpts
      {  ehcOptAspects          =   "%%@{%{ASPECTS}%%}"
      ,  ehcOptShowHS           =   False
%%[[(8 codegen tycore)
      ,  ehcOptShowTyCore       =   False
%%]]
      ,  ehcOptPriv             =   True
      ,  ehcOptHsChecksInEH     =   False
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
      ,  ehcOptImmQuit          =   Nothing
      ,  ehcOptDebug            =   False
      ,  ehcStopAtPoint         =   CompilePoint_All
%%[[7
      ,  ehcOptExtensibleRecords=   True
%%][99
      ,  ehcOptExtensibleRecords=   False
%%]]
%%[[7_2
      ,  ehcOptUniqueness       =   True
%%]]
%%[[(8 codegen)
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimizations    =   optimizeRequiresClosure $ Map.findWithDefault Set.empty OptimizationLevel_Normal optimizationLevelMp
      ,  ehcOptOptimizationLevel=   OptimizationLevel_Normal
      ,  ehcOptOptimizationScope=   OptimizationScope_PerModule
      ,  ehcOptMbTarget         =   JustOk defaultTarget
      ,  ehcOptMbTargetFlavor   =   JustOk defaultTargetFlavor
%%]]
%%[[(8 codegen tycore)
      ,  ehcOptUseTyCore        =   Nothing
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile      =   False
      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace         =   False
      ,  ehcOptGenTrace2        =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptDumpGrinStages   =   False
      -- ,  ehcOptEarlyModMerge    =   False
%%]]
%%[[8
      ,  ehcOptVerbosity        =   VerboseNormal
%%][100
      ,  ehcOptVerbosity        =   VerboseMinimal
%%]]
%%[[8
      ,  ehcOptEmitHS           =   False
      ,  ehcOptEmitEH           =   False
      
      ,  ehcOptImportFileLocPath=   []
      ,  ehcOptBuiltinNames     =   mkEHBuiltinNames (const id)
      ,  ehcOptEnvironment      =   undefined   -- filled in at toplevel
      
%%]]
%%[[(8 codegen grin)
      ,  ehcOptGenCmt           =   True
%%][(99 codegen grin)
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
      ,  ehcOptDoLinking        =   True
%%]]
%%[[92
      ,  ehcOptGenGenerics      =   True
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree    =   DerivTreeWay_None
      ,  ehcOptEmitDerivTreePaperSize
                                =   "2"
      ,  ehcOptEmitDerivFitsIn  =   False
%%]]
%%[[99
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
%%]]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived accessors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ehcOptTarget,ehcOptTargetFlavor)
ehcOptTarget :: EHCOpts -> Target
ehcOptTarget = maybeOk (\s -> panic ("ehcOptTarget: " ++ s)) id  . ehcOptMbTarget

ehcOptTargetFlavor :: EHCOpts -> TargetFlavor
ehcOptTargetFlavor = maybeOk (\s -> panic ("ehcOptTargetFlavor: " ++ s)) id . ehcOptMbTargetFlavor
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Getting a builtin name via EHCOpts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ehcOptBuiltin,ehcOptBuiltin2)
ehcOptBuiltin :: EHCOpts -> (EHBuiltinNames -> x) -> x
ehcOptBuiltin o f = f $ ehcOptBuiltinNames o

ehcOptBuiltin2 :: EHCOpts -> (EHBuiltinNames -> Int -> HsName) -> Int -> HsName
ehcOptBuiltin2 o f i = f (ehcOptBuiltinNames o) i
%%]

