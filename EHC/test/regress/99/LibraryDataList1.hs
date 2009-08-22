{- ----------------------------------------------------------------------------------------
   what    : library Data.List
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import Data.List

s = "z1y2a3b4"

main
  = do putStrLn (s)
       putStrLn ("sort: " ++ sort s)
       putStrLn ("intersperse ',': " ++ intersperse ',' s)
       putStrLn ("maximum: " ++ show (maximum s))
       putStrLn ("minimum: " ++ show (minimum s))
       putStrLn ("delete 'a': " ++ delete 'a' s)
       putStrLn ("intersect \"abyz\": " ++ intersect "abyz" s)
       putStrLn ("findIndices 'a': " ++ show (findIndices (=='a') s))
       putStrLn ("isPrefixOf \"z1\": " ++ show (isPrefixOf "z1" s))
       putStrLn ("isSuffixOf \"b4\": " ++ show (isSuffixOf "b4" s))
       putStrLn ("isPrefixOf \"b4\": " ++ show (isPrefixOf "b4" s))
       putStrLn ("isSuffixOf \"z1\": " ++ show (isSuffixOf "z1" s))
       putStrLn ("findIndex 'a': " ++ show (findIndex (=='a') s))
       putStrLn ("elemIndex 'a': " ++ show (elemIndex 'a' s))
