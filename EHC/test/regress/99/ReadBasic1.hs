{- ----------------------------------------------------------------------------------------
   what    : read of basic values
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

main :: IO ()
main
  = do putStrLn (show (read "34" :: Int))
       putStrLn (show (read "\"aap\"" :: String))
       putStrLn (show (read "'x'" :: Char))
