{- ----------------------------------------------------------------------------------------
   what    : Enum class, for Double
   expected: ok
---------------------------------------------------------------------------------------- -}

module EnumDouble1 where

main :: IO ()
main
  = do putStrLn (show $ fromEnum (111::Double))
       putStrLn (show $ (toEnum (111::Int) :: Double))
       putStrLn (show $ [1::Double ..4])
       putStrLn (show $ [1::Double,3..200])
       putStrLn (show $ take 100 $ [1::Double,3..])
       putStrLn (show $ take 100 $ [1::Double ..])
