%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC environmental info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environmental info for EHC to run.
Currently:
- install directory (where e.g. libraries reside)

20091210 AD: to become obsolete, because cmdline --cfg-XX parameters will take care of this from within a wrapper shell script.
%%]

%%[8 module {%{EH}EHC.Environment}
%%]

-- general imports
%%[8 import(System.Environment, System.Directory)
%%]
%%[8 import(IO,EH.Util.FPath)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCEnvironment(..))
data EHCEnvironment
  = EHCEnvironment
      { ehcenvVariant 			:: String
      , ehcenvInstallRoot		:: String
      }
      deriving (Show,Read)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The key used to access the proper environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(mkEhcenvKey)
mkEhcenvKey :: String -> String -> String -> String
mkEhcenvKey version progName ehcDefaultVariant
  = progName
    ++ "-" ++ version
    ++ "-" ++ ehcDefaultVariant
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Import from/export into application user directory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
ehcenvName :: String
ehcenvName = "environment"

mkEnvFPath :: String -> FPath
mkEnvFPath d = fpathSetDir d $ fpathFromStr ehcenvName
%%]

%%[8 export(ehcenvDir)
ehcenvDir :: String -> IO String
ehcenvDir progKey
  = do { d <- getAppUserDataDirectory progKey
       ; return d
       }
%%]

%%[8 export(importEHCEnvironment)
importEHCEnvironment :: String -> IO (Maybe EHCEnvironment)
importEHCEnvironment progKey
  = do { d <- ehcenvDir progKey
       ; let fpEnv = mkEnvFPath d
       ; envExists <- doesFileExist (fpathToStr fpEnv)
       ; if envExists
         then do { (fn,fh) <- openFPath fpEnv ReadMode False
                 ; txt <- hGetContents fh
                 ; let env = read txt
                 ; return (Just env)
                 }
         else return Nothing
       }
%%]

%%[8 export(exportEHCEnvironment)
exportEHCEnvironment :: String -> EHCEnvironment -> IO ()
exportEHCEnvironment progKey env
  = do { d <- ehcenvDir progKey
       ; let fpEnv = mkEnvFPath d
       ; createDirectoryIfMissing False d
       ; (fn,fh) <- openFPath fpEnv WriteMode False
       ; hPutStrLn fh (show env)
       ; hFlush fh
       ; hClose fh
       }
%%]


