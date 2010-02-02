{- ----------------------------------------------------------------------------------------
   what    : yield environmental info
   expected: ok, result may vary on how program is invoked from testing environment
   platform: SHELL env variable
---------------------------------------------------------------------------------------- -}

module SysEnv1 where

import System.Environment

main :: IO ()
main
  = do p <- getProgName
       putStrLn p
       as <- getArgs
       putStrLn (show as)
       -- e <- getEnvironment
       -- mapM_ (\(k,v) -> putStrLn (k ++ ": " ++ v)) e
       e1 <- getEnv "SHELL"
       putStrLn e1
       withProgName "FOO"
         (do p <- getProgName
             putStrLn p)
       p2 <- getProgName
       putStrLn p2
