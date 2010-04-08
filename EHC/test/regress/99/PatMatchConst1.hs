{- ----------------------------------------------------------------------------------------
   what    : bugfix triggered: multiple constant pattern matches
   expected: ok
---------------------------------------------------------------------------------------- -}

module PatMatchConst1 where

f :: String -> String
f ('a':'-':xs) = xs++xs
f ('a':c  :xs) = xs
f _            = "should not arrive here"

main :: IO ()
main
  = do putStrLn (f "abc")
       putStrLn (f "a-c")
