{- ----------------------------------------------------------------------------------------
   what    : show of Integer values
   expected: ok
---------------------------------------------------------------------------------------- -}

module ShowInteger1 where

main
  = do putStrLn (show (0::Integer))
       putStrLn (show (1::Integer))
       putStrLn (show ((-1)::Integer))
       putStrLn (show (123456789012345678901234567890::Integer))
       putStrLn (show (-123456789012345678901234567890::Integer))

