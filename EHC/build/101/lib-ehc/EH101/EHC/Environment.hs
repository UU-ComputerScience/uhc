module EH101.EHC.Environment
( EHCEnvironment (..)
, ehcenvDir
, mkEhcenvKey )
where
import System.Environment
import System.Directory
import System.IO
import EH.Util.FPath

{-# LINE 27 "src/ehc/EHC/Environment.chs" #-}
data EHCEnvironment
  = EHCEnvironment
      { ehcenvVariant 			:: String
      , ehcenvInstallRoot		:: String
      }
      deriving (Show,Read)

{-# LINE 40 "src/ehc/EHC/Environment.chs" #-}
mkEhcenvKey :: String -> String -> String -> String
mkEhcenvKey version progName ehcDefaultVariant
  = progName
    ++ "-" ++ version
    ++ "-" ++ ehcDefaultVariant

{-# LINE 60 "src/ehc/EHC/Environment.chs" #-}
ehcenvDir :: String -> IO String
ehcenvDir progKey
  = do { d <- getAppUserDataDirectory progKey
       ; return d
       }

