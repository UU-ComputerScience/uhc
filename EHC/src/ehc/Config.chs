%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Config}
%%]

%%[1 import({%{EH}ConfigInstall}) export(module {%{EH}ConfigInstall})
%%]

%%[8 import(Data.Maybe,Data.List,EH.Util.Utils,EH.Util.FPath)
%%]

%%[8 import({%{EH}ConfigDefines}, {%{EH}Opts}) export(module {%{EH}ConfigDefines})
%%]

%%[8 import({%{EH}EHC.Environment})
%%]

%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
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
      , verQuality          :: !String
      , verShort            :: !String
      , verFull             :: !String
      , verAsNumber         :: !String
      , verProg             :: !String
%%[[50
      , verTimestamp        :: !String
      , verSig              :: !String
%%]
      }
%%]

%%[1 export(version)
version :: Version
version
  = Version
      { verSvnRevision      = ehcSvnRevision
      , verMajor            = "1"
      , verMinor            = "1"
      , verMinorMinor       = "4"
      , verQuality          = "alpha"
      , verShort            = "1.1"
      , verFull             = "1.1.4"
      , verAsNumber         = "114"
      , verProg             = "ehc"
%%[[50
      , verTimestamp        = Sig.timestamp
      , verSig              = Sig.sig
%%]
      }
%%]

%%[1 export(verInfo)
verInfo :: Version -> String
verInfo v = verProg v ++ "-" ++ verFull v ++ ", Revision " ++ verSvnRevision v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Install locations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(installRoot)
installRoot :: EHCOpts -> String
installRoot opts
%%[[8
  = envroot
%%][99
  = maybe envroot id (ehcOptCfgInstallRoot opts)
%%]]
  where envroot = ehcenvInstallRoot $ ehcOptEnvironment opts
%%]

%%[(8 codegen) export(installVariant)
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

%%[(8 codegen) export(mkInstallFilePrefix)
mkInstallFilePrefix :: EHCOpts -> WhatInstallFile -> String -> String -> String
mkInstallFilePrefix opts whatfile variant pkg
  = mkDirbasedInstallPrefix (installRoot opts) whatfile variant (show $ ehcOptTarget opts) pkg
%%]

%%[(8 codegen) export(mkInstallBindirPrefix)
mkInstallBindirPrefix :: EHCOpts -> String -> String
mkInstallBindirPrefix opts variant
  = mkDirbasedInstallPrefix (installRoot opts) INST_BIN variant "" ""
%%]

%%[(99 codegen) export(mkInstallPkgdirSystem)
mkInstallPkgdirSystem :: EHCOpts -> String
mkInstallPkgdirSystem opts
  = filePathUnPrefix $ mkInstallFilePrefix opts INST_LIB_PKG2 (installVariant opts) ""
%%]

%%[(99 codegen) export(mkInstallPkgdirUser)
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
shellCmdLLVM = "" ++ "/Users/alessandro/Documents/Uni/uhc/EHC/bin/llvm-compilerdriver"
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
shellCmdLibtoolStatic = "/usr/bin/libtool"
%%]

%%[99 export(shellCmdLibtoolStaticOpts)
shellCmdLibtoolStaticOpts :: String
shellCmdLibtoolStaticOpts = "-static -o"
%%]

%%[99 export(mkShellCmdLibtool)
mkShellCmdLibtool :: String -> [String] -> [[String]]
mkShellCmdLibtool archive files
  = if True -- "/usr/bin/libtool" == "no"
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
    ++ (if useBoehmGC opts
        then [ "gc" ]
        else []
       )
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

%%[8 export(ehcGccOptsStatic)
ehcGccOptsStatic :: [String]
ehcGccOptsStatic = [ "" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GCC options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
gccOpts :: String
gccOpts = ""

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GCC additional external libraries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(libnamesGccEhcExtraExternalLibs)
libnamesGccEhcExtraExternalLibs :: [String]
libnamesGccEhcExtraExternalLibs
  = words
      (  ""
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

%%[(8 javascript) export(suffixJavaScriptLib)
suffixJavaScriptLib :: String
suffixJavaScriptLib = filter (/= '.') ".mjs"
%%]

