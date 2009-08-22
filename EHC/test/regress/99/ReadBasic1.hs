{- ----------------------------------------------------------------------------------------
   what    : read of basic values
   expected: ok
---------------------------------------------------------------------------------------- -}

module ReadBasic1 where

main :: IO ()
main
  = do putStrLn (show (read "34" :: Int))
       putStrLn (show (read "\"aap\"" :: String))
       putStrLn (show (read "'x'" :: Char))
       putStrLn (show (read "34.56" :: Float))
       putStrLn (show (read "34.56e2" :: Float))
       putStrLn (show (read "34.56" :: Double))
       putStrLn (show (read "34.56E2" :: Double))
       putStrLn (show (read "[34]" :: [Int]))
