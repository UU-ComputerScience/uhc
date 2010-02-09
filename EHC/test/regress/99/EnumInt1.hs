{- ----------------------------------------------------------------------------------------
   what    : Enum class, for Int
   expected: ok
---------------------------------------------------------------------------------------- -}

module EnumInt1 where

main :: IO ()
main
  = do putStrLn (show $ fromEnum (111::Int))
       putStrLn (show $ (toEnum (111::Int) :: Int))
       putStrLn (show $ [1::Int ..4])
       putStrLn (show $ [1::Int,3..200])
       putStrLn (show $ take 100 $ [1::Int,3..])
       putStrLn (show $ take 100 $ [1::Int ..])
