{- ----------------------------------------------------------------------------------------
   what    : type synonym, in particular proper deep enough expansion,
             which did not occur at some time.
   expected: ok, no output, no errors
---------------------------------------------------------------------------------------- -}

module TySyn2 where

type  Shows x  =  x -> ShowS

showsEmpty                    :: ShowS
showsEmpty r                  =  r

showsConcat                   :: [ShowS] -> ShowS
showsConcat                   =  foldr (.) showsEmpty

showsString                   :: Shows String
showsString                   =  (++)

-- Onderstaande functie typecheckt niet in EHC v.1693
-- (in v.1667 deed-ie het nog wel)

showsStarSep                  :: String -> Shows x -> Shows [x]
showsStarSep s showsX []      =  showsEmpty
showsStarSep s showsX (x:xs)  =  showsX x
                              .  showsConcat [showString s . showsX x' | x' <- xs]


{-
-- Deze equivalente functie typecheckt *wel* in EHC:

showsStarSep2                 :: String -> Shows x -> Shows [x]
showsStarSep2 s showsX ys     =  case ys of
                                   [] -> showsEmpty
                                   (x:xs) -> showsX x . showsConcat [showString s . showsX x' | x' <- xs]
-}


main = return ()
