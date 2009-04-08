{- ----------------------------------------------------------------------------------------
   what    : exit program with error code, but no output (i.e. trace etc)
   expected: ok
---------------------------------------------------------------------------------------- -}

module SysExit1 where

import System.Exit

main :: IO ()
main
  = do exitFailure
