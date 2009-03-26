{- ----------------------------------------------------------------------------------------
-- what    : Numeric
-- expected: ok
---------------------------------------------------------------------------------------- -}

import Numeric

main :: IO ()
main =
  do putStrLn (showEFloat Nothing (3.4::Double) "")
     putStrLn (showEFloat (Just 5) (3.4::Double) "")
     putStrLn (showFFloat Nothing (3.4::Double) "")
     putStrLn (showFFloat (Just 5) (3.4::Double) "")
     putStrLn (showGFloat Nothing (3.4::Double) "")
     putStrLn (showGFloat (Just 5) (3.4::Double) "")
     putStrLn (showHex (5555::Int) "")
     putStrLn (showOct (5555::Int) "")
