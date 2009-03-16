{- ----------------------------------------------------------------------------------------
   what    : deriving, for Read (and Show), on some datatype
   expected: ok
---------------------------------------------------------------------------------------- -}

module DerivingRead1 where

data T a = S | L Int | T a a | a :^: a
  deriving (Show,Read)

main :: IO ()
main
  = do putStrLn (show (read "S" :: T Int))
       putStrLn (show (read "L 5" :: T Int))
       putStrLn (show (read "T 2 3" :: T Int))
       putStrLn (show (read "5 :^: 4" :: T Int))
