{- ----------------------------------------------------------------------------------------
   what    : Create and remove directories
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory
import Control.Monad

root :: String
root = "filesForIOTesting"

cdir :: String
cdir = root  ++ "/" ++ "CreateDir"

rootn :: String
rootn = root ++ "/" ++ "CreateDirIfMissing" 

cdim :: String
cdim = rootn ++ "/" ++ "NewDir"

cdim' :: String
cdim' = rootn ++ "/" ++ "RenameDir"

notExist :: String 
notExist = "NotExist"

main :: IO ()
main = do
  createDirectory cdir
  doesDirectoryExist cdir >>= print
  createDirectoryIfMissing True cdim
  doesDirectoryExist cdim >>= print
  renameDirectory cdim cdim'
  doesDirectoryExist cdim' >>= print
  doesDirectoryExist cdim  >>= print
  doesDirectoryExist notExist >>= print
 
  removeDirectory cdir
  removeDirectoryRecursive rootn

