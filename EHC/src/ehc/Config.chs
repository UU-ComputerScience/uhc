%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Config}
%%]

%%[1 import({%{EH}ConfigInstall}) export(module {%{EH}ConfigInstall})
%%]

%%[8 import(Data.Maybe,Data.List,UHC.Util.Utils,UHC.Util.FPath)
%%]

%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}Opts.CommandLine})
%%]

%%[8 import({%{EH}ConfigDefines}, {%{EH}Opts.Base}) export(module {%{EH}ConfigDefines})
%%]

%%[8 import({%{EH}EHC.Environment})
%%]

%%[5050 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(Version(..))
data Version
  = Version
      { verSvnRevision      :: !String
      , verMajor            :: !String
      , verMinor            :: !String
      , verMinorMinor       :: !String
      , verMinorMinorMinor  :: !String
      , verQuality          :: !String
      , verShort            :: !String
      , verMedium           :: !String
      , verFull             :: !String
      , verAsNumber         :: !String
      , verProg             :: !String
%%[[5050
      , verTimestamp        :: !String
      , verSig              :: !String
%%]]
      }
%%]

%%[1 export(version)
version :: Version
version
  = Version
      { verSvnRevision      = ehcSvnRevision
      , verMajor            = "1"
      , verMinor            = "1"
      , verMinorMinor       = "9"
      , verMinorMinorMinor  = "6"
      , verQuality          = "alpha"
      , verShort            = "1.1"
      , verMedium           = "1.1.9"
      , verFull             = "1.1.9.6"
      , verAsNumber         = "1010906"
      , verProg             = "ehc"
%%[[5050
      , verTimestamp        = Sig.timestamp
      , verSig              = Sig.sig
%%]]
      }
%%]

%%[1 export(verInfo)
verInfo :: Version -> String
verInfo v =
  verProg v ++ "-" ++ verFull v ++ ", revision " ++ verSvnRevision v
%%[[5050
  ++ ", timestamp " ++ verTimestamp v
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Install locations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(installRoot)
installRoot :: EHCOpts -> String
installRoot opts
%%[[8
  = envroot
%%][99
  = maybe envroot id (ehcOptCfgInstallRoot opts)
%%]]
  where envroot = ehcenvInstallRoot $ ehcOptEnvironment opts
%%]

%%[8 export(installVariant)
installVariant :: EHCOpts -> String
installVariant opts
%%[[8
  = envvariant
%%][99
  = maybe envvariant id (ehcOptCfgInstallVariant opts)
%%]]
  where envvariant = ehcenvVariant $ ehcOptEnvironment opts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File locations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkInstallFilePrefix)
mkInstallFilePrefix :: EHCOpts -> WhatInstallFile -> String -> String -> String
mkInstallFilePrefix opts whatfile variant pkg
  = mkDirbasedInstallPrefix (installRoot opts) whatfile variant (show $ ehcOptTarget opts) pkg
%%]

%%[(8 codegen) export(mkInstallBindirPrefix)
mkInstallBindirPrefix :: EHCOpts -> String -> String
mkInstallBindirPrefix opts variant
  = mkDirbasedInstallPrefix (installRoot opts) INST_BIN variant "" ""
%%]

%%[99 export(mkInstallPkgdirSystem)
mkInstallPkgdirSystem :: EHCOpts -> String
mkInstallPkgdirSystem opts
  = filePathUnPrefix $ mkInstallFilePrefix opts INST_LIB_PKG2 (installVariant opts) ""
%%]

%%[99 export(mkInstallPkgdirUser)
mkInstallPkgdirUser :: EHCOpts -> String
mkInstallPkgdirUser opts
  = filePathCoalesceSeparator $ filePathUnPrefix $ mkDirbasedInstallPrefix (ehcOptUserDir opts) INST_LIB_PKG2 "" (show $ ehcOptTarget opts) ""
%%]

%%[(8 codegen) export(mkInstalledRts)
-- | construct path for RTS
mkInstalledRts :: EHCOpts -> (String -> String -> String) -> WhatInstallFile -> String -> String -> String
mkInstalledRts opts mkLib how variant rts = mkLib (mkInstallFilePrefix opts how variant rts) rts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cmds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(shellCmdOverride)
shellCmdOverride'' :: (cmd -> Maybe FilePath) -> FilePath -> cmd -> FilePath
shellCmdOverride'' override base cmd = maybe base id $ override cmd

shellCmdOverride' :: Ord cmd => Map.Map cmd FilePath -> FilePath -> cmd -> FilePath
shellCmdOverride' override = shellCmdOverride'' (flip Map.lookup override)

shellCmdOverride :: EHCOpts -> FilePath -> PgmExec -> FilePath
shellCmdOverride opts = shellCmdOverride' (ehcOptPgmExecMp opts)
%%]

%%[8 export(shellCmdGcc)
shellCmdGcc :: String
shellCmdGcc = "/usr/bin/gcc"
%%]

%%[(8 llvm) export(shellCmdLLVMC)
shellCmdLLVMC :: EHCOpts -> String -> [String]
shellCmdLLVMC opts variant = [mkInstallBindirPrefix opts variant ++ "llvmc", "", "", "", "/usr/bin/gcc"]
%%]

%%[8888 export(shellCmdLLVM)
shellCmdLLVM :: String
shellCmdLLVM = "" ++ "/home/tibor_admin/Documents/thesis/uhc/EHC/bin/llvm-compilerdriver"
%%]

%%[(8 java || jazy) export(shellCmdJar)
shellCmdJar :: String
shellCmdJar = ""
%%]

%%[(8 javascript) export(shellCmdCat)
shellCmdCat :: String
shellCmdCat = "/bin/cat"
%%]

%%[99 export(shellCmdCpp)
shellCmdCpp :: String
shellCmdCpp = "/usr/bin/cpp"
%%]

%%[99 export(shellCmdAr)
shellCmdAr :: String
shellCmdAr = "/usr/bin/ar"
%%]

%%[99 export(shellCmdRanlib)
shellCmdRanlib :: String
shellCmdRanlib = "/usr/bin/ranlib -c"
%%]

%%[99 export(shellCmdLibtoolStatic)
shellCmdLibtoolStatic :: String
shellCmdLibtoolStatic = "no"
%%]

%%[99 export(shellCmdLibtoolStaticOpts)
shellCmdLibtoolStaticOpts :: String
shellCmdLibtoolStaticOpts = "-static -o"
%%]

%%[99 export(mkShellCmdLibtool)
mkShellCmdLibtool :: String -> [String] -> [[String]]
mkShellCmdLibtool archive files
  = if True -- "no" == "no"
    then [ [shellCmdAr,"-r","-s",archive] ++ files
         -- , [shellCmdRanlib,archive]
         ]
    else [ [shellCmdLibtoolStatic,shellCmdLibtoolStaticOpts,archive] ++ files
         ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Libraries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(libnamesGcc)
libnamesGcc :: EHCOpts -> [String]
libnamesGcc opts
  = [ ]
%%[[(8 codegen)
    ++ (if useBoehmGC opts
        then [ "gc" ]
        else []
       )
%%]]
%%[[97
    ++ (if mpLib == MPLib_GMP
        then [ "gmp" ]
        else []
       )
%%]]
%%]

%%[8 export(libnamesRts)
libnamesRts :: [String]
libnamesRts
  = [ prefixLib ++ "EH-RTS"]
%%]

%%[8 export(ehcGccOptsStatic,ehcGccOptsStatic')
ehcGccOptsStatic' :: CmdLineOpts
ehcGccOptsStatic' = fst $ parseCmdLineOpts Cmd_C ""

ehcGccOptsStatic :: [String]
ehcGccOptsStatic = map show ehcGccOptsStatic'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GCC options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
gccOpts :: String
gccOpts = " "

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GCC additional external libraries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(libnamesGccEhcExtraExternalLibs)
libnamesGccEhcExtraExternalLibs :: [String]
libnamesGccEhcExtraExternalLibs
  = words
      (  "pthread"
%%[[97
      ++ " m"
%%]]
      )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File suffixes, prefixes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
prefixLib :: String
prefixLib = ""
%%]

%%[8 export(mbSuffixExec,linkerSymbolPrefix)
suffixExec :: String
suffixExec = ""

mbSuffixExec :: Maybe String
mbSuffixExec
  = case suffixExec of
      ('.':s) -> Just s
      ""      -> Nothing
      s       -> Just s

linkerSymbolPrefix :: String
linkerSymbolPrefix = "_"

%%]

%%[8 export(suffixDotlessBinaryCoreRun, suffixDotlessInputOutputBinaryCoreRun, suffixDotlessOutputTextualCoreRun, suffixDotlessInputOutputTextualCoreRun)
-- | Suffix (dotless) for binary corerun files, to be read in as part of the compilation process
suffixDotlessBinaryCoreRun :: String
suffixDotlessBinaryCoreRun = "crr"

-- | Suffix (dotless) for binary dumped/output corerun files
suffixDotlessInputOutputBinaryCoreRun :: String
suffixDotlessInputOutputBinaryCoreRun = "bcrr"

-- | Suffix (dotless) for textually dumped/output corerun files
suffixDotlessOutputTextualCoreRun :: String
suffixDotlessOutputTextualCoreRun = "tcrr"

-- | Suffix (dotless) for textually read corerun files
suffixDotlessInputOutputTextualCoreRun :: String
suffixDotlessInputOutputTextualCoreRun = suffixDotlessOutputTextualCoreRun
%%]

%%[8 export(suffixDotlessBinaryCore, suffixDotlessInputOutputBinaryCore, suffixDotlessOutputTextualCore, suffixDotlessInputOutputTextualCore, suffixDotlessOutputTextualCoreAST)
-- | Suffix (dotless) for binary core files, to be read in as part of the compilation process
suffixDotlessBinaryCore :: String
suffixDotlessBinaryCore = "cr"

-- | Suffix (dotless) for binary dumped/output core files
suffixDotlessInputOutputBinaryCore :: String
suffixDotlessInputOutputBinaryCore = "bcr"

-- | Suffix (dotless) for textually dumped/output core files
suffixDotlessOutputTextualCore :: String
suffixDotlessOutputTextualCore = "tcr"

-- | Suffix (dotless) for textually read core files
suffixDotlessInputOutputTextualCore :: String
suffixDotlessInputOutputTextualCore = suffixDotlessOutputTextualCore

-- | Suffix (dotless) for textually dumped/output Core AST files
suffixDotlessOutputTextualCoreAST :: String
suffixDotlessOutputTextualCoreAST = "astcr"

%%]

%%[8 export(suffixDotlessOutputTextualEh, suffixDotlessOutputTextualEhAST)
-- | Suffix (dotless) for textually dumped/output EH files
suffixDotlessOutputTextualEh :: String
suffixDotlessOutputTextualEh = "teh"

-- | Suffix (dotless) for textually dumped/output EH AST files
suffixDotlessOutputTextualEhAST :: String
suffixDotlessOutputTextualEhAST = "asteh"
%%]

%%[(8 corerun) export(suffixDotlessInputOutputCoreRun)
-- | Suffix (dotless) for CoreRun files
suffixDotlessInputOutputCoreRun :: String
suffixDotlessInputOutputCoreRun = "rcr"
%%]

%%[(8 javascript) export(suffixJavaScriptLib)
suffixJavaScriptLib :: String
suffixJavaScriptLib = filter (/= '.') ".mjs"
%%]

%%[(8 codegen cmm) export(suffixCmmLib)
suffixCmmLib :: String
suffixCmmLib = filter (/= '.') ".cmm"
%%]
