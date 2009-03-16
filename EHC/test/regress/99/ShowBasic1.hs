{- ----------------------------------------------------------------------------------------
   what    : show of basic values
   expected: ok
---------------------------------------------------------------------------------------- -}

module ShowBasic1 where

main
  = do putStrLn (show 'a')
       putStrLn (show "aap")
       putStrLn (show "\n\a\b\t\r")
       putStrLn (show "\NAK\DEL")
       putStrLn (show "\x38\o70")
       putStrLn (show (111::Int))
       putStrLn (show True)
       putStrLn (show False)
       putStrLn (show stdout)

