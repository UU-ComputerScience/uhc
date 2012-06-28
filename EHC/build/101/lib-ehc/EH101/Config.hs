module EH101.Config
( module EH101.ConfigInstall
, Version (..)
, version
, verInfo
, module EH101.ConfigDefines
, installRoot
, installVariant
, mkInstallFilePrefix
, mkInstallBindirPrefix
, mkInstalledRts
, shellCmdGcc
, shellCmdCat
, libnamesGcc
, libnamesRts
, ehcGccOptsStatic
, libnamesGccEhcExtraExternalLibs
, mbSuffixExec, linkerSymbolPrefix
, suffixJavaScriptLib
, mkInstallPkgdirSystem
, mkInstallPkgdirUser
, shellCmdCpp
, shellCmdAr
, shellCmdRanlib
, shellCmdLibtoolStatic
, shellCmdLibtoolStaticOpts
, mkShellCmdLibtool )
where
import EH101.ConfigInstall
import Data.Maybe
import Data.List
import EH.Util.Utils
import EH.Util.FPath
import EH101.ConfigDefines
import EH101.Opts
import EH101.EHC.Environment
import qualified EH101.SourceCodeSig as Sig




{-# LINE 28 "src/ehc/Config.chs" #-}
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
      , verTimestamp        :: !String
      , verSig              :: !String
      }

{-# LINE 47 "src/ehc/Config.chs" #-}
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
      , verTimestamp        = Sig.timestamp
      , verSig              = Sig.sig
      }

{-# LINE 67 "src/ehc/Config.chs" #-}
verInfo :: Version -> String
verInfo v = verProg v ++ "-" ++ verFull v ++ ", Revision " ++ verSvnRevision v

{-# LINE 76 "src/ehc/Config.chs" #-}
installRoot :: EHCOpts -> String
installRoot opts
  = maybe envroot id (ehcOptCfgInstallRoot opts)
  where envroot = ehcenvInstallRoot $ ehcOptEnvironment opts

{-# LINE 87 "src/ehc/Config.chs" #-}
installVariant :: EHCOpts -> String
installVariant opts
  = maybe envvariant id (ehcOptCfgInstallVariant opts)
  where envvariant = ehcenvVariant $ ehcOptEnvironment opts

{-# LINE 102 "src/ehc/Config.chs" #-}
mkInstallFilePrefix :: EHCOpts -> WhatInstallFile -> String -> String -> String
mkInstallFilePrefix opts whatfile variant pkg
  = mkDirbasedInstallPrefix (installRoot opts) whatfile variant (show $ ehcOptTarget opts) pkg

{-# LINE 108 "src/ehc/Config.chs" #-}
mkInstallBindirPrefix :: EHCOpts -> String -> String
mkInstallBindirPrefix opts variant
  = mkDirbasedInstallPrefix (installRoot opts) INST_BIN variant "" ""

{-# LINE 114 "src/ehc/Config.chs" #-}
mkInstallPkgdirSystem :: EHCOpts -> String
mkInstallPkgdirSystem opts
  = filePathUnPrefix $ mkInstallFilePrefix opts INST_LIB_PKG2 (installVariant opts) ""

{-# LINE 120 "src/ehc/Config.chs" #-}
mkInstallPkgdirUser :: EHCOpts -> String
mkInstallPkgdirUser opts
  = filePathCoalesceSeparator $ filePathUnPrefix $ mkDirbasedInstallPrefix (ehcOptUserDir opts) INST_LIB_PKG2 "" (show $ ehcOptTarget opts) ""

{-# LINE 126 "src/ehc/Config.chs" #-}
-- | construct path for RTS
mkInstalledRts :: EHCOpts -> (String -> String -> String) -> WhatInstallFile -> String -> String -> String
mkInstalledRts opts mkLib how variant rts = mkLib (mkInstallFilePrefix opts how variant rts) rts

{-# LINE 136 "src/ehc/Config.chs" #-}
shellCmdGcc :: String
shellCmdGcc = "/usr/bin/gcc"

{-# LINE 156 "src/ehc/Config.chs" #-}
shellCmdCat :: String
shellCmdCat = "/bin/cat"

{-# LINE 161 "src/ehc/Config.chs" #-}
shellCmdCpp :: String
shellCmdCpp = "/usr/bin/cpp"

{-# LINE 166 "src/ehc/Config.chs" #-}
shellCmdAr :: String
shellCmdAr = "/usr/bin/ar"

{-# LINE 171 "src/ehc/Config.chs" #-}
shellCmdRanlib :: String
shellCmdRanlib = "/usr/bin/ranlib -c"

{-# LINE 176 "src/ehc/Config.chs" #-}
shellCmdLibtoolStatic :: String
shellCmdLibtoolStatic = "/usr/bin/libtool"

{-# LINE 181 "src/ehc/Config.chs" #-}
shellCmdLibtoolStaticOpts :: String
shellCmdLibtoolStaticOpts = "-static -o"

{-# LINE 186 "src/ehc/Config.chs" #-}
mkShellCmdLibtool :: String -> [String] -> [[String]]
mkShellCmdLibtool archive files
  = if True -- "/usr/bin/libtool" == "no"
    then [ [shellCmdAr,"-r","-s",archive] ++ files
         -- , [shellCmdRanlib,archive]
         ]
    else [ [shellCmdLibtoolStatic,shellCmdLibtoolStaticOpts,archive] ++ files
         ]

{-# LINE 201 "src/ehc/Config.chs" #-}
libnamesGcc :: EHCOpts -> [String]
libnamesGcc opts
  = [ ]
    ++ (if useBoehmGC opts
        then [ "gc" ]
        else []
       )
    ++ (if mpLib == MPLib_GMP
        then [ "gmp" ]
        else []
       )

{-# LINE 217 "src/ehc/Config.chs" #-}
libnamesRts :: [String]
libnamesRts
  = [ prefixLib ++ "EH-RTS"]

{-# LINE 223 "src/ehc/Config.chs" #-}
ehcGccOptsStatic :: [String]
ehcGccOptsStatic = [ "" ]

{-# LINE 241 "src/ehc/Config.chs" #-}
libnamesGccEhcExtraExternalLibs :: [String]
libnamesGccEhcExtraExternalLibs
  = words
      (  ""
      ++ " m"
      )

{-# LINE 256 "src/ehc/Config.chs" #-}
prefixLib :: String
prefixLib = ""

{-# LINE 261 "src/ehc/Config.chs" #-}
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


{-# LINE 277 "src/ehc/Config.chs" #-}
suffixJavaScriptLib :: String
suffixJavaScriptLib = filter (/= '.') ".mjs"

