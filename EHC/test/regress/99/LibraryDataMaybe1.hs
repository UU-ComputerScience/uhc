{- ----------------------------------------------------------------------------------------
   what    : library Data.Maybe
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import Data.Maybe

main
  = do putStrLn ("listToMaybe \"\": " ++ show (listToMaybe ""))
       putStrLn ("listToMaybe \"a\": " ++ show (listToMaybe "a"))
       putStrLn ("fromJust (Just \"a\"): " ++ fromJust (Just "a"))
       putStrLn ("maybeToList (Just 'a'): " ++ maybeToList (Just 'a'))
       putStrLn ("catMaybes [Just 'a',Nothing,Just 'b']: " ++ catMaybes [Just 'a',Nothing,Just 'b'])
