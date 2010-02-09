{- ----------------------------------------------------------------------------------------
   what    : Enum class, for Float
   expected: ok
---------------------------------------------------------------------------------------- -}

module EnumFloat1 where

main :: IO ()
main
  = do putStrLn (show $ fromEnum (111::Float))
       putStrLn (show $ (toEnum (111::Int) :: Float))
       putStrLn (show $ [1::Float ..4])
       putStrLn (show $ [1::Float,3..200])
       putStrLn (show $ take 100 $ [1::Float,3..])
       putStrLn (show $ take 100 $ [1::Float ..])
