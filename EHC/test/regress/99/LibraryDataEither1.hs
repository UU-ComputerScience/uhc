{- ----------------------------------------------------------------------------------------
   what    : library Data.Either
   expected: ok
---------------------------------------------------------------------------------------- -}

module LibraryDataEither1 where

import Data.Either

main
  = do putStrLn ("lefts [Right 'a',Left 'b',Right 'c']: " ++ lefts [Right 'a',Left 'b',Right 'c'])
       putStrLn ("rights [Right 'a',Left 'b',Right 'c']: " ++ rights [Right 'a',Left 'b',Right 'c'])
       -- putStrLn ("partitionEithers [Right 'a',Left 'b',Right 'c']: " ++ show (partitionEithers [Right 'a',Left 'b',Right 'c']))
