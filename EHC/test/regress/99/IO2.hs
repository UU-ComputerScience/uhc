{- ----------------------------------------------------------------------------------------
   what    : IO
   expected: error, attempt to read beyond EOF
---------------------------------------------------------------------------------------- -}

module Main where

main :: IO ()
main
  = do h1 <- openFile "filesForIOTesting/empty" ReadMode
       l1 <- hGetLine h1
       l2 <- hGetLine h1
       hPutStrLn stdout l1
       hPutStrLn stdout l2
       hClose h1
