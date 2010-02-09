{- ----------------------------------------------------------------------------------------
   what    : Enum class, for Integer
   expected: ok
---------------------------------------------------------------------------------------- -}

module EnumInteger1 where

main :: IO ()
main
  = do putStrLn (show $ fromEnum (111::Integer))
       putStrLn (show $ (toEnum (111::Int) :: Integer))
       putStrLn (show $ [1::Integer ..4])
       putStrLn (show $ [1::Integer,3..200])
       putStrLn (show $ take 100 $ [1::Integer,3..])
       putStrLn (show $ take 100 $ [1::Integer ..])
