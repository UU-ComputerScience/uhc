module Main where

main = do  s <- readFile "copyFile.hs"
           writeFile "copyOfCopyFile.hs" s
