%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Opts} import(System.Console.GetOpt,{%{EH}Base.Common}) export(EHCOpts(..), ehcCmdLineOpts)
%%]

%%[1 import(EH.Util.Utils)
%%]

%%[1 import(Data.Maybe,qualified Data.Map as Map)
%%]

%%[4 import(EH.Util.Pretty)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Ty})
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

%%[50 import({%{EH}Ty.Trf.Instantiate})
%%]

%%[8 import({%{EH}Base.FileSearchLocation}) export(module {%{EH}Base.FileSearchLocation})
%%]

%%[99 import(qualified {%{EH}ConfigInstall} as Cfg)
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
%%[[99
  | ImmediateQuitOption_Meta_Pkgdir_System                  -- print system package dir
  | ImmediateQuitOption_Meta_Pkgdir_User                    -- print user package dir
  | ImmediateQuitOption_VersionDotted                      -- print version in dotted style, for external version comparison
  | ImmediateQuitOption_VersionAsNumber                     -- print version as number, for external version comparison
  | ImmediateQuitOption_Meta_ExportEnv (Maybe String)       -- export (write) environmental info of installation
  | ImmediateQuitOption_Meta_DirEnv                         -- print dir of environmental info of installation
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
  = PkgOption_Build	PkgName							-- build a package
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) export(cmdLineTrfs)
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

%%[(8888 codegen) export(trfOptOverrides)
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
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
mkStringPath :: String -> [String]
mkStringPath = wordsBy (`elem` ";,")

mkFileLocPath :: String -> FileLocPath
mkFileLocPath = map mkDirFileLoc . mkStringPath
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Option specific options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(optOptsIsYes)
optOpts :: Map.Map String opt -> String -> [opt]
optOpts m s = catMaybes $ map (\os -> Map.lookup os m) $ wordsBy (==',') s

optOptsIsYes :: Eq opt => Maybe [opt] -> opt -> Bool
optOptsIsYes mos o = maybe False (o `elem`) mos
%%]

%%[(8 codegen) export(TyCoreOpt(..))
data TyCoreOpt
  = TyCoreOpt_Sugar			-- produce/accept sugared version
  | TyCoreOpt_Unicode		-- produce/accept unicode, implies sugar
  deriving Eq

instance Show TyCoreOpt where
  show TyCoreOpt_Sugar		= "sugar"		-- first letters of alternatives must be unique
  show TyCoreOpt_Unicode	= "unicode"

tycoreOpts :: [TyCoreOpt]
tycoreOpts = [TyCoreOpt_Sugar, TyCoreOpt_Unicode]

tycoreOptMp :: Map.Map String TyCoreOpt
tycoreOptMp
  = Map.fromList $ concat
    $ [ [ (s, o), ([head s], o) ]
      | o <- tycoreOpts
      , let s = show o
      ]
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
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[(8 codegen)
      ,  ehcOptOptimise       ::  Optimise          -- optimisation level
      ,  ehcOptDumpCoreStages ::  Bool              -- dump intermediate Core transformation stages
      ,  ehcOptTarget         ::  Target            -- code generation target
      ,  ehcOptTargetFlavor   ::  TargetFlavor      -- code generation target flavor
      ,  ehcOptUseTyCore      ::  Maybe [TyCoreOpt] -- use TyCore instead of Core (temporary option until Core is obsolete)
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenOwn         ::  Bool
      ,  ehcOptGenRVS         ::  Bool
      ,  ehcOptGenLink        ::  Bool
      ,  ehcOptGenLocReg      ::  Bool
      ,  ehcOptGenDebug       ::  Bool              -- generate runtime debug info
      ,  ehcOptGenTrace       ::  Bool
      ,  ehcOptGenTrace2      ::  Bool

      ,  ehcOptGenRTSInfo     ::  Int               -- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptDumpGrinStages ::  Bool              -- dump intermediate Grin transformation stages
      ,  ehcOptEarlyModMerge  ::  Bool              -- produce OneBigCore instead of OneBigGrin; useful for future Core-only optimizations
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
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree  ::  DerivTreeWay      -- show derivation tree on stdout
      ,  ehcOptEmitDerivTreePaperSize
                              ::  String            -- the paper size to be used
      ,  ehcOptEmitDerivFitsIn
                              ::  Bool              -- show fitsIn derivation tree as well
%%]]
%%[[99
      ,  ehcOptHiValidityCheck::  Bool				-- when .hi and compiler are out of sync w.r.t. timestamp and checksum, recompile
      ,  ehcOptLibFileLocPath ::  FileLocPath
      ,  ehcOptPkgdirLocPath  ::  StringPath
      ,  ehcOptPkgDb          ::  PackageDatabase	-- package database to be used for searching packages
      ,  ehcOptLibPackages    ::  [String]
      ,  ehcProgName          ::  FPath             -- name of this program
      ,  ehcOptUserDir        ::  String            -- user dir for storing user specific stuff
      ,  ehcOptCPP            ::  Bool              -- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude                     -- use & assume presence of prelude
                              ::  Bool
      ,  ehcOptPackageSearchFilter	 ::  [PackageSearchFilter]	-- description of what to expose from package database
      ,  ehcOptOutputDir      ::  Maybe String      -- where to put output, instead of same dir as input file
      ,  ehcOptKeepIntermediateFiles
                              ::  Bool              -- keep intermediate files
      ,  ehcOptPkg            ::  Maybe PkgOption	-- package building (etc) option
      ,  ehcOptCfgInstallRoot        ::  Maybe String      -- the directory where the installation resides; overrides ehcenvInstallRoot
      ,  ehcOptCfgInstallVariant     ::  Maybe String      -- the installation variant; overrides ehcenvVariant
%%]]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These are there for (temporary) backwards compatibility.

%%[(8 codegen grin) export(ehcOptFullProgAnalysis)
-- do full GRIN program analysis
ehcOptFullProgAnalysis :: EHCOpts -> Bool
ehcOptFullProgAnalysis = targetIsFullProgAnal . ehcOptTarget
%%]

%%[(8 codegen grin) export(ehcOptErrAboutBytecode)
-- report when Grin ByteCode errors occur
ehcOptErrAboutBytecode :: EHCOpts -> Bool
%%[[8
ehcOptErrAboutBytecode _ = False
%%][99
ehcOptErrAboutBytecode   = targetIsGrinBytecode . ehcOptTarget
%%]]
%%]

%%[(8 codegen grin) export(ehcOptEmitExecBytecode, ehcOptEmitBytecode)
-- generate bytecode
ehcOptEmitExecBytecode :: EHCOpts -> Bool
ehcOptEmitExecBytecode = targetIsGrinBytecode . ehcOptTarget

ehcOptEmitBytecode :: EHCOpts -> Bool
ehcOptEmitBytecode = ehcOptEmitExecBytecode
%%]

%%[(8 codegen grin) export(ehcOptEmitC)
-- generate C
ehcOptEmitC :: EHCOpts -> Bool
ehcOptEmitC = targetIsC . ehcOptTarget
%%]

%%[(8 codegen java) export(ehcOptEmitJava)
-- generate Java, as src text
ehcOptEmitJava :: EHCOpts -> Bool
ehcOptEmitJava o = ehcOptTarget o == Target_Interpreter_Core_Java
%%]

%%[(8 codegen grin) export(ehcOptEmitLLVM)
-- generate LLVM
ehcOptEmitLLVM :: EHCOpts -> Bool
ehcOptEmitLLVM = targetIsLLVM . ehcOptTarget
%%]

%%[(8 codegen clr) export(ehcOptEmitCLR)
-- generate CIL, as .il assembly file
ehcOptEmitCLR :: EHCOpts -> Bool
ehcOptEmitCLR = targetIsCLR . ehcOptTarget
%%]

%%[(8 codegen) export(ehcOptEmitCore)
-- generate Core
ehcOptEmitCore :: EHCOpts -> Bool
ehcOptEmitCore opts
  = ehcOptFullProgAnalysis opts || targetIsCore (ehcOptTarget opts)
%%]

%%[(8 codegen) export(ehcOptEmitTyCore,ehcOptTyCore)
-- generate TyCore
ehcOptEmitTyCore :: EHCOpts -> Bool
ehcOptEmitTyCore opts
  = {- ehcOptFullProgAnalysis opts || -} targetIsTyCore (ehcOptTarget opts)

ehcOptTyCore :: EHCOpts -> Bool
ehcOptTyCore opts = ehcOptEmitTyCore opts || isJust (ehcOptUseTyCore opts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.defaultEHCOpts export(defaultEHCOpts)
defaultEHCOpts
  = EHCOpts
      {  ehcOptAspects          =   "%%@{%{ASPECTS}%%}"
      ,  ehcOptShowHS           =   False
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
%%[[7_2
      ,  ehcOptUniqueness       =   True
%%]]
%%[[(8 codegen)
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimise         =   OptimiseNormal
      ,  ehcOptTarget           =   defaultTarget
      ,  ehcOptTargetFlavor     =   defaultTargetFlavor
      ,  ehcOptUseTyCore        =   Nothing
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile      =   False
      ,  ehcOptGenOwn           =   True
      ,  ehcOptGenRVS           =   False
      ,  ehcOptGenLink          =   False
      ,  ehcOptGenLocReg        =   False
      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace         =   False
      ,  ehcOptGenTrace2        =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptDumpGrinStages   =   False
      ,  ehcOptEarlyModMerge    =   False
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
      ,  ehcOptPkgDb          	=	emptyPackageDatabase
      ,  ehcOptLibPackages      =   []
      ,  ehcProgName            =   emptyFPath
      ,  ehcOptUserDir          =   ""
      ,  ehcOptCPP              =   False
      ,  ehcOptUseAssumePrelude =   True
      ,  ehcOptPackageSearchFilter
      							=	pkgSearchFilter PackageSearchFilter_ExposePkg Cfg.ehcAssumedPackages
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
%%% Options as passed on the command line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
ehcCmdLineOpts
  =  [  Option "h"  ["help"]             (NoArg oHelp)                        "print this help (then stop)"
     ,  Option ""   ["version"]          (NoArg oVersion)                     "print version info (then stop)"
%%[[99
     ,  Option ""   ["version-dotted"]   (NoArg oNumVersion)                  ("print version in \"x.y.z\" style (then stop)")
     ,  Option ""   ["version-asnumber"] (NoArg oVersionAsNumber)             ("print version in \"xyz\" style (then stop)")
     ,  Option ""   ["numeric-version"]  (NoArg oNumVersion)                  "see --version-dotted (to become obsolete)"
%%]]
%%[[8
     ,  Option "v"  ["verbose"]          (OptArg oVerbose "0|1|2|3|4")        (   "be verbose, 0=quiet, 4=debug, "
%%[[8
                                                                               ++ "default=2"
%%][100
                                                                               ++ "default=1"
%%]]
                                                                              )
%%]]
%%[[1
     ,  Option "t"  ["target"]           (OptArg oTarget "")                  "code generation not available"
%%][(8 codegen)
     ,  Option "t"  ["target"]           (ReqArg oTarget (showSupportedTargets'  "|"))  ("generate code for target, default=" ++ show defaultTarget)
     ,  Option ""   ["target-flavor"]    (ReqArg oTargetFlavor (showAllTargetFlavors' "|"))  ("generate code for target flavor, default=" ++ show defaultTargetFlavor)
%%]]
%%[[1
     ,  Option "p"  ["pretty"]           (OptArg oPretty "hs|eh|ast|-")       "show pretty printed source or EH abstract syntax tree, default=eh, -=off, (downstream only)"
     ,  Option "d"  ["debug"]            (NoArg oDebug)                       "show debug information"
     ,  Option ""   ["priv"]             (boolArg oPriv)                      "private flag, used during development of 2 impls of 1 feature"
%%][100
%%]]
%%[[(1 hmtyinfer)
     ,  Option ""   ["show-top-ty"]      (OptArg oShowTopTy "yes|no")         "show top ty, default=no"
%%][100
%%]]

%%[[1
     ,  Option ""   ["stopat"]
%%[[1
                                         (ReqArg oStopAt "0|1|2|3")           "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh"
%%][(8 codegen)
                                         (ReqArg oStopAt "0|1|2|3|4")         "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh, 4=core"
%%]]
%%][100
%%]]

%%[[7_2
     ,  Option ""   ["nounique"]         (NoArg oUnique)                      "do not compute uniqueness solution"
%%]]
%%[[(8 codegen)
     ,  Option "O"  ["optimise"]         (OptArg oOptimise "0|1|2")           "optimise, 0=none 1=normal 2=more, default=1"
%%]]
%%[[(8 codegen)
     ,  Option ""   ["code"]             (OptArg oCode "hs|eh|exe[c]|lexe[c]|bexe[c]|-")  "write code to file, default=bexe (will be obsolete and/or changed, use --target)"
     ,  Option ""   ["dump-core-stages"] (boolArg optDumpCoreStages)          "dump intermediate Core transformation stages (no)"
%%][100
%%]]
%%[[(8 codegen grin)
     ,  Option ""   ["time-compilation"] (NoArg oTimeCompile)                 "show grin compiler CPU usage for each compilation phase (only with -v2)"
     ,  Option ""   ["gen-casedefault"]  (boolArg optSetGenCaseDefault)       "trap wrong casedistinction in C (no)"
     ,  Option "g"  ["gen-own"]          (boolArg optSetGenOwn)               "use own stack, thus enabling tailcalls (yes)"
     ,  Option ""   ["gen-rvs"]          (boolArg optSetGenRVS)               "put return values on stack (no)"
     ,  Option ""   ["gen-link"]         (boolArg optSetGenLink)              "generate code for static link (no)"
     ,  Option ""   ["gen-locreg"]       (boolArg optSetGenLocReg)            "allocate locals in registers that are saved before calls (no)"
     ,  Option ""   ["gen-cmt"]          (boolArg optSetGenCmt)               "include comment about code in generated code"
     ,  Option ""   ["gen-debug"]        (boolArg optSetGenDebug)             "include debug info in generated code (yes)"
     ,  Option ""   ["gen-trace"]        (boolArg optSetGenTrace)             "trace functioncalls in C (no)"
     ,  Option ""   ["gen-trace-assign"] (boolArg optSetGenTrace2)            "trace assignments in C (no)"
     ,  Option ""   ["gen-rtsinfo"]      (ReqArg oRTSInfo "<nr>")             "flags for rts info dumping (default=0)"
     ,  Option ""   ["dump-grin-stages"] (boolArg optDumpGrinStages)          "dump intermediate Grin and Silly transformation stages (no)"
     ,  Option ""   ["early-mod-merge"]  (boolArg optEarlyModMerge)           "merge modules early, at Core stage (no)"
%%][100
%%]]
%%[[(8 codegen java)
%%]]
%%[[9
     -- ,  Option ""   ["chr-scoped"]       (ReqArg  oCHRScoped "0|1|2")         "scoped CHR gen: 0=inst, 1=super, 2=all (default=2)"
%%]]
%%[[20
     ,  Option ""   ["no-recomp"]        (NoArg oNoRecomp)                    "turn off recompilation check (force recompile)"
     ,  Option "c"  ["compile-only"]     (NoArg oCompileOnly)                 "compile only, do not link"
%%]]
%%[[20
     ,  Option ""   ["debug-stopat-hi-error"]
                                         (boolArg oStopAtHIError)             "debug: stop at .hi parse error (default=off)"
%%][100
%%]]
%%[[(20 codegen)
     ,  Option ""   ["debug-stopat-core-error"]
                                         (boolArg oStopAtCoreError)           "debug: stop at .core parse error (default=off)"
%%][100
%%]]
%%[[99
     ,  Option "i"  ["import-path"]      (ReqArg oUsrFileLocPath "path")       "search path for user files, path separators=';', appended to previous"
     ,  Option "L"  ["lib-search-path"]  (ReqArg oLibFileLocPath "path")       "search path for library files, see also --import-path"
     ,  Option ""   ["no-prelude"]       (NoArg oNoPrelude)                   "do not assume presence of Prelude"
     ,  Option ""   ["no-hi-check"] 	 (NoArg oNoHiCheck)                   "no check on .hi files not matching the compiler version"
     ,  Option ""   ["cpp"]              (NoArg oCPP)                         "preprocess source with CPP"
     ,  Option ""   ["limit-tysyn-expand"]
                                         (intArg oLimitTyBetaRed)             "type synonym expansion limit"
     -- 20071002: limiting the number of context reduction steps is not supported starting with the use of CHRs
     -- ,  Option ""   ["limit-ctxt-red"]   (intArg oLimitCtxtRed)               "context reduction steps limit"
     
     ,  Option ""   ["package"]          (ReqArg oExposePackage "package")    "see --pkg-expose"
     ,  Option ""   ["hide-all-packages"](NoArg oHideAllPackages)             "see --pkg-hide-all"
     ,  Option ""   ["odir"]             (ReqArg oOutputDir "dir")            "base directory for generated files. Implies --compile-only"
     ,  Option ""   ["keep-intermediate-files"] (NoArg oKeepIntermediateFiles) "keep intermediate files (default=off)"
%%]]
%%[[(99 hmtyinfer)
     ,  Option ""   ["deriv-tree"]       (OptArg oDerivTree ("f|i[,p=[{0,1,2,3,4,5}|<n>m]][,f=" ++ boolArgStr ++ "]"))
                                                                              "emit derivation tree on .lhs file; f=final, i=infer, default=f; p=paper size (0=a0,...; <n>m=2^<n> meter), dflt=2; f=show subsumption"
%%][100
%%]]
     ,  Option ""   ["meta-variant"]        (NoArg oVariant)                     "meta: print variant (then stop)"
     ,  Option ""   ["meta-target-default"] (NoArg oTargetDflt)                  "meta: print the default codegeneration target (then stop)"
     ,  Option ""   ["meta-targets"]        (NoArg oTargets)                     "meta: print list of supported codegeneration targets (then stop)"
%%[[99
     ,  Option ""   ["meta-export-env"]  	(OptArg oExportEnv "installdir[,variant]") "meta: export environmental info of installation (then stop) (will become obsolete soon)"
     ,  Option ""   ["meta-dir-env"]     	(NoArg oDirEnv)                      "meta: print directory holding environmental info of installation (then stop) (will become obsolete soon)"
     ,  Option ""   ["meta-pkgdir-system"]  (NoArg oMetaPkgdirSys) 				 "meta: print system package dir (then stop)"
     ,  Option ""   ["meta-pkgdir-user"]    (NoArg oMetaPkgdirUser) 			 "meta: print user package dir (then stop)"
     ,  Option ""   ["pkg-build"]        	(ReqArg oPkgBuild "package")         "pkg: build package from generated files. Implies --compile-only"
     ,  Option ""   ["pkg-expose"]       	(ReqArg oExposePackage "package")    "pkg: expose/use package"
     ,  Option ""   ["pkg-hide"]         	(ReqArg oHidePackage   "package")    "pkg: hide package"
     ,  Option ""   ["pkg-hide-all"]     	(NoArg oHideAllPackages)             "pkg: hide all (implicitly) assumed/used packages"
     ,  Option ""   ["pkg-searchpath"]      (ReqArg oPkgdirLocPath "path")       "pkg: additional package search directories, each dir must have subdir <pkg>/<variant>/<target>/<flavor> (this may change)"
     ,  Option ""   ["cfg-install-root"]    (ReqArg oCfgInstallRoot "dir")        "cfg: installation root (to be used only by wrapper script)"
     ,  Option ""   ["cfg-install-variant"] (ReqArg oCfgInstallVariant "variant") "cfg: installation variant (to be used only by wrapper script)"
%%]]
%%[[(8 codegen)
     ,  Option ""   ["tycore"]              (OptArg oUseTyCore "opt[,...]")      ("temporary/development: use typed core. opts: " ++ (concat $ intersperse " " $ Map.keys tycoreOptMp))
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
         oHelp           o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Help    }
         oVersion        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Version }
         oVariant        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_Variant }
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
         oUseTyCore ms   o =  case ms of
                                Just s -> o { ehcOptUseTyCore = Just opts2 }
                                       where opts1 = optOpts tycoreOptMp s
                                             opts2 = if TyCoreOpt_Unicode `elem` opts1 then ([TyCoreOpt_Sugar] ++ opts1) else opts1
                                _      -> o { ehcOptUseTyCore = Just [] }
%%]]
%%[[1
         oTarget        _ o =  o
%%][(8 codegen)
         oTarget        s o =  o { ehcOptTarget        = Map.findWithDefault defaultTarget        s supportedTargetMp }
         oTargetFlavor  s o =  o { ehcOptTargetFlavor  = Map.findWithDefault defaultTargetFlavor  s allTargetFlavorMp }
%%]]
%%[[1
         oTargets        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_Targets       }
         oTargetDflt     o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_TargetDefault  }
                                      
%%]]
%%[[8
         oCode       ms  o =  case ms of
                                Just "hs"    -> o { ehcOptEmitHS           = True   }
                                Just "eh"    -> o { ehcOptEmitEH           = True   }
%%[[(8 codegen)
                                Just "-"     -> o -- { ehcOptEmitCore         = False  }
                                Just "core"  -> o { ehcOptTarget           = Target_None_Core_None
                                                  }
                                Just "tycore"-> o { ehcOptTarget           = Target_None_TyCore_None
                                                  }
%%]]
%%[[(8 codegen java)
                                Just "java"  -> o { ehcOptTarget           = Target_Interpreter_Core_Java   }
%%]]
%%[[(8 codegen grin)
                                Just "grin"  -> o -- { ehcOptEmitGrin         = True   }
                                Just "bc"    -> o -- { ehcOptEmitBytecode     = True 
                                                  -- , ehcOptFullProgAnalysis = False
                                                  -- }
                                Just m | m `elem` ["bexe","bexec"]
                                             -> o { ehcOptTarget           = Target_Interpreter_Grin_C
                                                  }

                                Just "c"     -> o -- { ehcOptEmitC            = True
                                                  -- , ehcOptFullProgAnalysis = True
                                                  -- , ehcOptEmitExecBytecode = False
                                                  -- , ehcOptEmitBytecode     = False
                                                  -- , ehcOptErrAboutBytecode = False
                                                  -- }

                                Just m | m `elem` ["exe","exec"]
                                             -> o { ehcOptTarget           = Target_FullProgAnal_Grin_C
                                                  }

                                Just "llvm"  -> o -- { ehcOptEmitLLVM         = True
                                                  -- , ehcOptFullProgAnalysis = True
                                                  -- , ehcOptEmitExecBytecode = False
                                                  -- , ehcOptEmitBytecode     = False
                                                  -- , ehcOptErrAboutBytecode = False
                                                  -- }
                                Just m | m `elem` ["lexe", "lexec"]
                                             -> o { ehcOptTarget           = Target_FullProgAnal_Grin_LLVM
                                                  }                   
%%]]
%%[[(8 codegen clr)
                                Just "clr"   -> o { ehcOptTarget           = Target_FullProgAnal_Grin_CLR   }
%%]]
%%[[(99 hmtyinfer)
                                Just "dt"    -> o { ehcOptEmitDerivTree    = DerivTreeWay_Final   }
%%]]
                                _            -> o

%%[[(8888 codegen)
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
         oRTSInfo    s   o =  o { ehcOptGenRTSInfo     = read s       }
%%]]
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseMinimal     }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "3"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Just "4"    -> o { ehcOptVerbosity     = VerboseDebug       }
                                Nothing     -> o { ehcOptVerbosity     = succ (ehcOptVerbosity o)}
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
         oNoRecomp              o   = o { ehcOptCheckRecompile              = False    }
         oCompileOnly           o   = o { ehcOptDoLinking                   = False    }
%%]]
%%[[99
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
         oExportEnv          ms o   = o { ehcOptImmQuit                     = Just (ImmediateQuitOption_Meta_ExportEnv ms) }
         oDirEnv                o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_Meta_DirEnv }
         oMetaPkgdirSys         o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_Meta_Pkgdir_System }
         oMetaPkgdirUser        o   = o { ehcOptImmQuit                     = Just ImmediateQuitOption_Meta_Pkgdir_User }
         oExposePackage       s o   = o { ehcOptLibPackages                 = ehcOptLibPackages   o ++ [s]
                                        , ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ pkgSearchFilter PackageSearchFilter_ExposePkg [s]
                                        }
         oHidePackage         s o   = o { ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ pkgSearchFilter PackageSearchFilter_HidePkg [s]
                                        }
         oHideAllPackages       o   = o { ehcOptPackageSearchFilter         = ehcOptPackageSearchFilter o ++ [PackageSearchFilter_HideAll]
                                        -- , ehcOptHideAllPackages             = True
                                        }
         oOutputDir           s o   = o { ehcOptOutputDir                   = Just s
                                        , ehcOptDoLinking                   = False
                                        }
         -- oOutputPkgLibDir     s o   = o { ehcOptOutputPkgLibDir             = Just s }
         oKeepIntermediateFiles o   = o { ehcOptKeepIntermediateFiles       = True }
         oPkgBuild            s o   = o { ehcOptPkg                         = Just (PkgOption_Build s)
                                        , ehcOptDoLinking                   = False
                                        }
         oCfgInstallRoot      s o   = o { ehcOptCfgInstallRoot              = Just s }
         oCfgInstallVariant   s o   = o { ehcOptCfgInstallVariant           = Just s }
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
optSetGenTrace2      o b = o { ehcOptGenTrace2      = b }
optSetGenRTSInfo     o b = o { ehcOptGenRTSInfo     = b }
optSetGenCaseDefault o b = o { ehcOptGenCaseDefault = b }
optSetGenCmt         o b = o { ehcOptGenCmt         = b }
optSetGenOwn         o b = o { ehcOptGenOwn         = b }
optSetGenRVS         o b = o { ehcOptGenRVS         = b }
optSetGenLink        o b = o { ehcOptGenLink        = b }
optSetGenLocReg      o b = o { ehcOptGenLocReg      = b }
optSetGenDebug       o b = o { ehcOptGenDebug       = b }
optDumpGrinStages    o b = o { ehcOptDumpGrinStages = b {-, ehcOptEmitGrin = b -} }
optEarlyModMerge     o b = o { ehcOptEarlyModMerge  = b }
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
      -- , o "exec"            (ehcOptEmitExecC        opts)
      -- , o "bexec"           (ehcOptEmitExecBytecode opts)
      , show (ehcOptTarget opts)
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
fioBindNoSet _                = Set.empty

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
                        ,  fioDontBind       ::  TyVarIdS
%%]]
%%[[8
                        ,  fioExpandEqTyVar  ::  Bool                -- expand tyvars also when equal. Required for Sys F translation.
%%]]
%%[[9
                        ,  fioPredAsTy       ::  Bool                ,  fioAllowRPredElim       ::  Bool
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
                        ,  fioDontBind       =   Set.empty
%%]]
%%[[8
                        ,  fioExpandEqTyVar  =   False
%%]]
%%[[9
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
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
