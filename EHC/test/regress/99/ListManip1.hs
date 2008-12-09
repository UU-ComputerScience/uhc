{- ----------------------------------------------------------------------------------------
-- what    : basic list manipulation yielding strings
-- expected: all ok
---------------------------------------------------------------------------------------- -}

module ListManip1 where

main
  = do putStrLn "aap"
       putStrLn ['a', 'a', 'p']
       putStrLn (head "head" : "")
       putStrLn (tail "tail")
       putStrLn (init "init")
       putStrLn (map id "map")
       putStrLn (reverse "reverse")
       putStrLn (concatMap (\c -> c : c : "") "concatMap")
       putStrLn (foldr (\c r -> c : ' ' : r) "" "foldr")
       putStrLn (foldl (\r c -> c : ' ' : r) "" "foldl")
       putStrLn (concat $ scanr (\c r -> c : r) " " "scanr")
       putStrLn (concat $ scanl (\r c -> c : r) " " "scanl")
       putStrLn (takeWhile isLower "takeWhile")
       putStrLn (dropWhile isLower "dropWhile")
       putStrLn (fst $ span isAlpha "span break")
       putStrLn (snd $ span isAlpha "span break")
       putStrLn (fst $ break isSpace "break span")
       putStrLn (snd $ break isSpace "break span")
       putStrLn (filter isAlpha "f i l t e r")
       putStrLn ("noot" ++ " " ++ "mies")
       putStrLn (unwords ["unword1", "unword2", "unword3"])
       putStrLn (concat $ replicate 5 "replicate5 ")
       putStrLn (concat $ replicate (length "length") "length ")
       putStrLn (concat $ take 5 $ repeat "repeat5 ")
       putStrLn (concat $ zipWith (++) ["zip1", "zip2"] ["ZIP1", "ZIP2"])
       putStrLn (fst (splitAt 5 "splitat") ++ snd (splitAt 5 "splitat"))
       putStrLn (["!!1", "!!2", "!!3"] !! 1)
       putStrLn (take 26 $ iterate succ 'a')
       putStrLn (take 30 $ cycle "cycle ")

