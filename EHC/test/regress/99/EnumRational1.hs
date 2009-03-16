{- ----------------------------------------------------------------------------------------
   what    : Enum class, for Rational
   expected: ok
---------------------------------------------------------------------------------------- -}

module EnumRational1 where

main :: IO ()
main
  = do putStrLn (show $ fromEnum (toRational (111::Integer)))
       putStrLn (show $ (toEnum (111::Int) :: Rational))
       putStrLn (show $ [toRational (1::Integer) .. toRational (4::Integer)])
       putStrLn (show $ [toRational (1::Integer),toRational (3::Integer) .. toRational (200::Integer)])
       putStrLn (show $ take 100 $ [toRational (1::Integer),toRational (3::Integer) ..])
       putStrLn (show $ take 100 $ [toRational (1::Integer) ..])

