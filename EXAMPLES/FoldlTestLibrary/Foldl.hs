module Foldl where
 
         
x1 = foldl (&&) True [True,False]

main = putStrLn (show x1)