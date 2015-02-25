%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[0 hs
-- {-# LANGUAGE TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Opts.Base} import({%{EH}Base.Common})
%%]

%%[1 import(UHC.Util.Utils)
%%]

%%[1 import(Data.Typeable, Data.Maybe, qualified Data.Map as Map)
%%]

%%[4 import(UHC.Util.Pretty)
%%]

%%[7 import(qualified Data.Set as Set)
%%]

%%[8 import(Data.List,Data.Char,{%{EH}Base.HsName.Builtin})
%%]

%%[8 import(UHC.Util.FPath)
%%]

%%[8888 import(UHC.Util.Lens)
%%]

%%[8 import({%{EH}EHC.Environment})
%%]

%%[8 import({%{EH}Base.Target})
%%]

%%[8 import({%{EH}Base.Optimize})
%%]

%%[8 import({%{EH}Base.FileSearchLocation})
%%]

%%[99 import(qualified {%{EH}ConfigInstall} as Cfg)
%%]

%%[99 import({%{EH}Base.Pragma})
%%]
%%[99 import({%{EH}Opts.CommandLine})
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
  -- -| ImmediateQuitOption_Meta_ExportEnv (Maybe String)       -- export (write) environmental info of installation
  -- -| ImmediateQuitOption_Meta_DirEnv                         -- print dir of environmental info of installation
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

%%[99 export(PkgOption(..), emptyPkgOption)
-- | Build pkg options, all (except obligatory name) wrapped in Maybe/[] because of possible absence.
-- 20140829 AD: will be used to construct config file
data PkgOption
  = PkgOption
      { pkgoptName 				:: PkgName					-- ^ build a package with name
      , pkgoptExposedModules	:: [String]					-- ^ 20140829 AD not yet used: exposed modules
      , pkgoptBuildDepends		:: [PkgName]				-- ^ 20140829 AD not yet used: depends on pkgs
      }

emptyPkgOption :: PkgOption
emptyPkgOption = PkgOption emptyPkgName [] []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Option specific options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(CoreOpt(..))
-- | Core options
data CoreOpt
  = CoreOpt_NONE				-- no-op option
%%[[(8 coreout)
--  | CoreOpt_PPParseable			-- pretty print parseable, negation means just make it readable
  | CoreOpt_Dump 				-- dump textual core output
%%[[50
  | CoreOpt_DumpBinary			-- dump binary core output
%%]]
  | CoreOpt_DumpAlsoNonParseable-- dump also the parts which are not parseable
%%]]
%%[[(8 corerun)
  | CoreOpt_Run					-- run after compilation
  | CoreOpt_RunDump				-- dump CoreRun
  | CoreOpt_RunDumpVerbose		-- dump CoreRun, more verbose
  | CoreOpt_RunTrace			-- trace during running CoreRun
  | CoreOpt_RunTraceExtensive	-- trace during running CoreRun, with extensive info, implies CoreOpt_RunTrace
  | CoreOpt_RunPPNames			-- when dump/run CoreRun print names instead of
  | CoreOpt_RunPPVerbose		-- when dump CoreRun print more verbose info in comment
%%]]
%%[[(8 coresysf)
  | CoreOpt_SysF				-- 20120419, work in startup/progress: generate System F
  | CoreOpt_SysFCheck			-- 20120419, work in startup/progress: typecheck generated System F
  | CoreOpt_SysFCheckOnlyVal	-- 20120419, work in startup/progress: only check values (not types and higher meta defs)
  | CoreOpt_SysFOnlyHi			-- 20120419, work in startup/progress: no codegen, only .hi info propagation
%%]]
  deriving (Eq,Enum,Bounded)
%%]

%%[(8 codegen javascript) export(JavaScriptOpt(..))
-- | JavaScript options
data JavaScriptOpt
  = JavaScriptOpt_Debug			-- not used
%%[[(8 cmm)
  | JavaScriptOpt_ViaCMM		-- 20130912, temporary to take different route via CMM
%%]]
  deriving (Eq,Enum,Bounded)

instance Show JavaScriptOpt where
  show JavaScriptOpt_Debug      = "debug"
%%[[(8 cmm)
  show JavaScriptOpt_ViaCMM     = "cmm"
%%]]
%%]

%%[(8 codegen tycore) export(TyCoreOpt(..))
-- | TyCore options
data TyCoreOpt
  = TyCoreOpt_Sugar         -- produce/accept sugared version
  | TyCoreOpt_Unicode       -- produce/accept unicode, implies sugar
  deriving Eq

%%]

%%[(8 codegen cmm) export(CmmOpt(..))
-- | Cmm options
data CmmOpt
  = CmmOpt_Check			-- name (and other, later perhaps) check code
  deriving (Eq,Ord,Enum,Bounded)

%%]

%%[99 export(PgmExec(..))
-- | Pgm (internal program used) options, in particular alternate internal shell commands
data PgmExec
  = PgmExec_CPP				-- alternate CPP
  | PgmExec_C				-- alternate C compiler
  | PgmExec_Linker			-- alternate linker
  deriving (Eq,Ord,Enum,Bounded)
%%]

%%[99 export(ExecOpt(..),execOptsPlain)
-- | Wrapper around options, adding semantics for adapting cmd specific behavior
data ExecOpt
  = ExecOpt_Plain String						-- ^ plain option
  | ExecOpt_Output (String -> String)			-- ^ output file

execOptsPlain :: [ExecOpt] -> [String]
execOptsPlain o = [ s | ExecOpt_Plain s <- o ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Convention: most option names/fields start with 'ehcOpt'

%%[1.EHCOpts export(EHCOpts(..))
-- | The options to use.
data EHCOpts
  = EHCOpts
      {  ehcOptTrace          ::  forall a . String -> a -> a            -- tracing
      ,  ehcOptAspects        ::  String            -- which aspects are included in this compiler
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
%%[[6
      ,  ehcOptPolyKinds	  ::  Bool              -- allow kind polymorphism
%%]]
%%[[7
      ,  ehcOptExtensibleRecords
      						  ::  Bool
%%]]
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[8
      ,  ehcOptMbTarget       ::  MaybeOk Target            -- code generation target
      ,  ehcOptMbTargetFlavor ::  MaybeOk TargetFlavor      -- code generation target flavor
      ,  ehcOptBangPatterns   ::  Bool              -- allow bang patterns
      ,  ehcOptOptimizationLevel
                              ::  OptimizationLevel          -- optimisation level
      ,  ehcOptOptimizationScope
                              ::  OptimizationScope          -- optimisation scope
%%]]
%%[[(8 codegen)
      ,  ehcOptOptimizations  ::  OptimizeS         -- individual optimizations to be done, derived from level + scope
      ,  ehcOptOptimizeOptionMp
                              ::  OptimizeOptionMp  -- optimization specific configuration
      ,  ehcOptDumpCoreStages ::  Bool              -- dump intermediate Core transformation stages
      ,  ehcOptCoreOpts       ::  [CoreOpt]  	    -- Core options
%%]]
%%[[(8 codegen cmm)
      ,  ehcOptUseCmm		  ::  Maybe [CmmOpt]	-- use cmm? + options
      ,  ehcOptCmmOpts        ::  [CmmOpt]  	    -- Cmm options
%%]]
%%[[(8 codegen javascript)
      ,  ehcOptJavaScriptOpts ::  [JavaScriptOpt]	-- javascript options
%%]]
%%[[(8 javascript)
      ,  ehcOptDumpJavaScriptStages ::  Bool              -- dump intermediate JavaScript transformation stages
%%]]
%%[[(8 codegen tycore)
      ,  ehcOptUseTyCore      ::  Maybe [TyCoreOpt] -- use TyCore instead of Core (temporary option until Core is obsolete)
%%]]
%%[[(8 codegen)
      ,  ehcOptGenTrampoline_ ::  Bool              -- gen trampoline with (tail) calls
      ,  ehcOptGenTrace       ::  Bool
%%]]
%%[[(8 grin)
      ,  ehcOptGenBoxGrin_	  ::  Bool				-- gen simplified grin delaying (un)boxing
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeGrinCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool              -- generate runtime debug info
      ,  ehcOptGenTrace2      ::  Bool

      ,  ehcOptGenRTSInfo     ::  Int               -- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptDumpGrinStages ::  Bool              -- dump intermediate Grin transformation stages
      -- ,  ehcOptEarlyModMerge  ::  Bool              -- produce OneBigCore instead of OneBigGrin; useful for future Core-only optimizations
%%]]
%%[[(8 codegen cmm)
      ,  ehcOptDumpCmmStages  ::  Bool              -- dump intermediate Cmm transformation stages
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
%%[[(50 codegen)
      ,  ehcDebugStopAtCoreError
                              ::  Bool              -- stop when Core parse error occurs (otherwise errors are ignored, repaired .core is used)
%%]]
%%[[50
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcDebugStopAtHIError::  Bool              -- stop when HI parse error occurs (otherwise it is ignored, .hi thrown away)
      -- ,  ehcOptDoExecLinking      ::  Bool              -- do link, if False compile only
      ,  ehcOptLinkingStyle   ::  LinkingStyle      -- how to link, possibly no linking (e.g. when compile only)
%%]]
%%[[92
      ,  ehcOptGenGenerics    ::  Bool              -- generate for use of generics
%%]]
%%[[93
      ,  ehcOptFusion   	  ::  Bool				-- allow fusion syntax, the optimization itself is triggered by optimize flags
%%]]
%%[[(99 hmtyinfer tyderivtree)
      ,  ehcOptEmitDerivTree  ::  DerivTreeWay      -- show derivation tree on stdout
      ,  ehcOptEmitDerivTreePaperSize
                              ::  String            -- the paper size to be used
      ,  ehcOptEmitDerivFitsIn
                              ::  Bool              -- show fitsIn derivation tree as well
%%]]
%%[[8
      ,  ehcOptAltDriver 	  ::  Bool				-- alternate (build function based) compiler driver
%%]]
%%[[99
      ,  ehcOptHiValidityCheck::  Bool              -- when .hi and compiler are out of sync w.r.t. timestamp and checksum, recompile
      ,  ehcOptLibFileLocPath ::  FileLocPath
      ,  ehcOptPkgdirLocPath  ::  StringPath
      ,  ehcOptPkgDb          ::  PackageDatabase   -- package database to be used for searching packages
      ,  ehcProgName          ::  FPath             -- name of this program
      ,  ehcCurDir            ::  String            -- current dir (not an option, but set initially)
      ,  ehcOptUserDir        ::  String            -- user dir for storing user specific stuff
      ,  ehcOptMbOutputFile   ::  Maybe FPath       -- in which file to put generated output/executable
      ,  ehcOptCPP            ::  Bool              -- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude                     -- use & assume presence of prelude
                              ::  Bool
      ,  ehcOptPackageSearchFilter 
      					 	  ::  [PackageSearchFilter]  -- description of what to expose from package database
      ,  ehcOptOutputDir      ::  Maybe String      -- where to put output, instead of same dir as input file
      ,  ehcOptKeepIntermediateFiles
                              ::  Bool              -- keep intermediate files
      ,  ehcOptPkgOpt         ::  Maybe PkgOption   -- package building (etc) option
      ,  ehcOptCfgInstallRoot        ::  Maybe String      -- the directory where the installation resides; overrides ehcenvInstallRoot
      ,  ehcOptCfgInstallVariant     ::  Maybe String      -- the installation variant; overrides ehcenvVariant
      ,  ehcOptCmdLineOpts    ::  CmdLineOpts       -- options from the commandline and pragma for such options
      ,  ehcOptCmdLineOptsDoneViaPragma
      						  ::  Bool       		-- options via OPTIONS_UHC pragma have been set
      ,  ehcOptOverloadedStrings
      						  ::  Bool              -- allow overloaded strings
      ,  ehcOptPgmExecMp	  ::  Map.Map PgmExec FilePath
      												-- alternate executables for program
      ,  ehcOptExecOptsMp	  ::  Map.Map FilePath [ExecOpt]
      												-- default options for commands
%%]]
      }
      deriving Typeable
%%]

%%[8888
mkLabel ''EHCOpts
%%]

%%[9999 export(ehcOptAltDriver)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Empty compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.defaultEHCOpts export(emptyEHCOpts)
emptyEHCOpts
  = EHCOpts
      {  ehcOptTrace            =   \_ x -> x
      ,  ehcOptAspects          =   "%%@{%{ASPECTS}%%}"
      ,  ehcOptShowHS           =   False
%%[[(8 codegen tycore)
      ,  ehcOptShowTyCore       =   False
%%]]
      ,  ehcOptPriv             =   False
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
%%[[6
      ,  ehcOptPolyKinds	    =   True
%%][99
      ,  ehcOptPolyKinds	    =   False
%%]]
%%[[7
      ,  ehcOptExtensibleRecords=   True
%%][99
      ,  ehcOptExtensibleRecords=   False
%%]]
%%[[7_2
      ,  ehcOptUniqueness       =   True
%%]]
%%[[8
      ,  ehcOptMbTarget         =   JustOk defaultTarget
      ,  ehcOptMbTargetFlavor   =   JustOk defaultTargetFlavor
      ,  ehcOptBangPatterns		= 	True
      ,  ehcOptOptimizationLevel=   OptimizationLevel_Normal
      ,  ehcOptOptimizationScope=   OptimizationScope_PerModule
%%]]
%%[[(8 codegen)
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimizations    =   optimizeRequiresClosure $ Map.findWithDefault Set.empty OptimizationLevel_Normal optimizationLevelMp
      ,  ehcOptOptimizeOptionMp =   Map.empty
      ,  ehcOptCoreOpts         =   []
%%]]
%%[[(8 codegen cmm)
      ,  ehcOptUseCmm           =	Nothing
      ,  ehcOptCmmOpts          =   []
%%]]
%%[[(8 codegen javascript)
      ,  ehcOptJavaScriptOpts   =   []
%%]]
%%[[(8 javascript)
      ,  ehcOptDumpJavaScriptStages
                                =   False
%%]]
%%[[(8 codegen tycore)
      ,  ehcOptUseTyCore        =   Nothing
%%]]
%%[[(8 codegen)
      ,  ehcOptGenTrampoline_  	=	False
      ,  ehcOptGenTrace         =   False
%%]]
%%[[(8 grin)
      ,  ehcOptGenBoxGrin_  	=	False
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeGrinCompile      =   False
      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace2        =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptDumpGrinStages   =   False
      -- ,  ehcOptEarlyModMerge    =   False
%%]]
%%[[(8 codegen cmm)
      ,  ehcOptDumpCmmStages    =   False
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
%%[[(50 codegen)
      ,  ehcDebugStopAtCoreError=   False
%%]]
%%[[50
      ,  ehcOptCheckRecompile   =   True
      ,  ehcDebugStopAtHIError  =   False
      -- ,  ehcOptDoExecLinking        =   True
      ,  ehcOptLinkingStyle     =   LinkingStyle_Exec      -- how to link, possibly no linking (e.g. when compile only)
%%]]
%%[[92
      ,  ehcOptGenGenerics      =   True
%%]]
%%[[93
      ,  ehcOptFusion			=   True
%%][99
      ,  ehcOptFusion			=   False
%%]]
%%[[(99 hmtyinfer tyderivtree)
      ,  ehcOptEmitDerivTree    =   DerivTreeWay_None
      ,  ehcOptEmitDerivTreePaperSize
                                =   "2"
      ,  ehcOptEmitDerivFitsIn  =   False
%%]]
%%[[8
      ,  ehcOptAltDriver 	    =   False
%%]]
%%[[99
      ,  ehcOptHiValidityCheck  =   True
      ,  ehcOptLibFileLocPath   =   []
      ,  ehcOptPkgdirLocPath    =   []
      ,  ehcOptPkgDb            =   emptyPackageDatabase
      ,  ehcProgName            =   emptyFPath
      ,  ehcCurDir              =   ""
      ,  ehcOptUserDir          =   ""
      ,  ehcOptMbOutputFile     =   Nothing
      ,  ehcOptCPP              =   False
      ,  ehcOptUseAssumePrelude =   True
      ,  ehcOptPackageSearchFilter
                                =   [] -- pkgSearchFilter parsePkgKey PackageSearchFilter_ExposePkg Cfg.ehcAssumedPackages
      ,  ehcOptOutputDir        =   Nothing
      ,  ehcOptKeepIntermediateFiles
                                =   False
      ,  ehcOptPkgOpt           =   Nothing
      ,  ehcOptCfgInstallRoot   =   Nothing
      ,  ehcOptCfgInstallVariant=   Nothing
      ,  ehcOptCmdLineOpts      =   []
      ,  ehcOptCmdLineOptsDoneViaPragma
                                =   False
      ,  ehcOptOverloadedStrings=   False
      ,  ehcOptPgmExecMp		= 	Map.empty
      ,  ehcOptExecOptsMp		=   Map.empty
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

%%[(8 codegen) export(ehcOptCoreSysF,ehcOptCoreSysFCheck,ehcOptCoreSysFGen,ehcOptCoreSysFCheckOnlyVal)
-- | Generate system F (20120421 AD: very much under construction)
ehcOptCoreSysF :: EHCOpts -> Bool
%%[[(8 coresysf)
ehcOptCoreSysF opts = CoreOpt_SysF `elem` ehcOptCoreOpts opts
%%][8
ehcOptCoreSysF _    = False
%%]]

-- | Typecheck system F (20120421 AD: very much under construction)
ehcOptCoreSysFCheck :: EHCOpts -> Bool
%%[[(8 coresysf)
ehcOptCoreSysFCheck opts = ehcOptCoreSysF opts && CoreOpt_SysFCheck `elem` ehcOptCoreOpts opts
%%][8
ehcOptCoreSysFCheck _    = False
%%]]

-- | Typecheck system F (20120421 AD: very much under construction)
ehcOptCoreSysFGen :: EHCOpts -> Bool
%%[[(8 coresysf)
ehcOptCoreSysFGen opts = ehcOptCoreSysF opts && not (CoreOpt_SysFOnlyHi `elem` ehcOptCoreOpts opts)
%%][8
ehcOptCoreSysFGen opts = ehcOptCoreSysF opts
%%]]

-- | Typecheck system F (20120421 AD: very much under construction)
ehcOptCoreSysFCheckOnlyVal :: EHCOpts -> Bool
%%[[(8 coresysf)
ehcOptCoreSysFCheckOnlyVal opts = ehcOptCoreSysFCheck opts && CoreOpt_SysFCheckOnlyVal `elem` ehcOptCoreOpts opts
%%][8
ehcOptCoreSysFCheckOnlyVal opts = ehcOptCoreSysFCheck opts
%%]]
%%]

%%[8 export(ehcOptEmitExecBytecode, ehcOptEmitBytecode)
-- generate bytecode
ehcOptEmitExecBytecode :: EHCOpts -> Bool
%%[[(8 codegen grin)
ehcOptEmitExecBytecode = targetIsGrinBytecode . ehcOptTarget
%%][8
ehcOptEmitExecBytecode _ = False
%%]]

ehcOptEmitBytecode :: EHCOpts -> Bool
%%[[(8 codegen grin)
ehcOptEmitBytecode = ehcOptEmitExecBytecode
%%][8
ehcOptEmitBytecode _ = False
%%]]
%%]

%%[(8 codegen javascript) export(ehcOptJavaScriptViaCMM)
-- | CMM route for JavaScript backend?
ehcOptJavaScriptViaCMM :: EHCOpts -> Bool
%%[[(8 cmm)
ehcOptJavaScriptViaCMM opts = JavaScriptOpt_ViaCMM `elem` ehcOptJavaScriptOpts opts
%%][8
ehcOptJavaScriptViaCMM _    = False
%%]]
%%]

Some are there for (temporary) backwards compatibility.

%%[(8 codegen grin) export(ehcOptGenBoxGrin, ehcOptGenTrampoline)
-- | Generate new impl of boxing (20130912 AD: temporary for development)
ehcOptGenBoxGrin :: EHCOpts -> Bool
ehcOptGenBoxGrin opts
  =  ehcOptGenBoxGrin_ opts || ehcOptIsViaGrinCmmJavaScript opts

-- | Generate new impl of boxing (20130912 AD: temporary for development)
ehcOptGenTrampoline :: EHCOpts -> Bool
ehcOptGenTrampoline opts
  =  ehcOptGenTrampoline_ opts || ehcOptIsViaGrinCmmJavaScript opts
%%]

%%[(8 codegen javascript) export(ehcOptEmitJavaScript)
-- | Do we generate JavaScript?
ehcOptEmitJavaScript :: EHCOpts -> Bool
ehcOptEmitJavaScript = targetIsJavaScript . ehcOptTarget
%%]

%%[(8888 codegen) export(ehcOptCmm)
-- use Cmm ?
ehcOptCmm :: EHCOpts -> Bool
%%[[(8 cmm)
ehcOptCmm opts = isJust (ehcOptUseCmm opts)
%%][8
ehcOptCmm opts = isJust (ehcOptUseCmm opts)
%%]]
%%]

%%[(8 codegen) export(ehcOptCmmCheck)
-- | Check Cmm
ehcOptCmmCheck :: EHCOpts -> Bool
%%[[(8 cmm)
ehcOptCmmCheck opts = CmmOpt_Check `elem` ehcOptCmmOpts opts
%%][8
ehcOptCmmCheck _    = False
%%]]
%%]

%%[(8 codegen) export(ehcOptIsViaGrinCmmJavaScript, ehcOptIsViaCoreJavaScript)
-- | Via Core -> Grin -> CMM -> JS ?
ehcOptIsViaGrinCmmJavaScript :: EHCOpts -> Bool
ehcOptIsViaGrinCmmJavaScript opts
%%[[(8 javascript cmm)
  = targetIsViaGrinCmmJavaScript t || targetIsViaCoreJavaScript t && ehcOptJavaScriptViaCMM opts
  where t = ehcOptTarget opts
%%][8
  = False
%%]]

-- | Via Core -> JS ?
ehcOptIsViaCoreJavaScript :: EHCOpts -> Bool
ehcOptIsViaCoreJavaScript opts
  = targetIsViaCoreJavaScript t
%%[[(8 javascript cmm)
    && not (ehcOptJavaScriptViaCMM opts)
%%]]
  where t = ehcOptTarget opts
%%]

%%[(8 codegen) export(ehcOptIsViaCmm)
ehcOptIsViaCmm :: EHCOpts -> Bool
ehcOptIsViaCmm opts = ehcOptIsViaGrinCmmJavaScript opts
{-# INLINE ehcOptIsViaCmm #-}
%%]

%%[(8 codegen) export(ehcOptIsViaGrin)
ehcOptIsViaGrin :: EHCOpts -> Bool
ehcOptIsViaGrin opts = ehcOptIsViaGrinCmmJavaScript opts || targetIsGrinBytecode t || targetDoesHPTAnalysis t
  where t = ehcOptTarget opts
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Variation of maybe for allowing either debugging or panic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(ehcOptFromJust)
-- | Either fromJust with a possible panic, or with a default value (when debugging)
ehcOptFromJust :: EHCOpts -> String -> a -> Maybe a -> a
ehcOptFromJust opts panicMsg n m
  | ehcOptDebug opts = maybe n id m
  | otherwise        = panicJust panicMsg m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Do linking?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(ehcOptDoExecLinking)
-- | Do linking into executable?
ehcOptDoExecLinking :: EHCOpts -> Bool
ehcOptDoExecLinking opts = ehcOptLinkingStyle opts == LinkingStyle_Exec
%%]


