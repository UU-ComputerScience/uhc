{- ----------------------------------------------------------------------------------------
   what    : yield prog name + args
   expected: ok, result may vary on how program is invoked from testing environment
---------------------------------------------------------------------------------------- -}

module GetProgName where

import System.Environment

main :: IO ()
main
  = do p <- getProgName
       a <- getArgs
       putStrLn p
       putStrLn (show a)
