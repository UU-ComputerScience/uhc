{- ----------------------------------------------------------------------------------------
   what    : System.IO
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.IO
import System.Directory
import Debug.Trace

file :: FilePath
file = "filesForIOTesting/file2"

newFile :: FilePath
newFile = "filesForIOTesting/newFileName"

main :: IO ()
main =
  do c <- readFile file
     putStrLn c
     writeFile  newFile c
     appendFile newFile c
     c' <- readFile newFile
     print (c' == c ++ c)
     removeFile newFile

     h <- openFile file ReadMode
     hFileSize h   >>= print
     hIsOpen h     >>= print
     hIsClosed h   >>= print
     hIsReadable h >>= print
     hIsWritable h >>= print
     hIsSeekable h >>= print
     
     hClose h
     
     hIsOpen h     >>= print
     hIsClosed h   >>= print

