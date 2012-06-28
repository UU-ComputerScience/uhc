module Paths_EH101 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/alessandro/Documents/Uni/uhc/EHC/install-for-build//bin"
libdir     = "/Users/alessandro/Documents/Uni/uhc/EHC/install-for-build//lib/EH101-1.1/ghc-7.4.1"
datadir    = "/Users/alessandro/Documents/Uni/uhc/EHC/install-for-build//share/EH101-1.1"
libexecdir = "/Users/alessandro/Documents/Uni/uhc/EHC/install-for-build//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "EH101_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "EH101_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "EH101_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "EH101_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
