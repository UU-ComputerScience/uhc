{- ----------------------------------------------------------------------------------------
   what    : local instance
   expected: ok
---------------------------------------------------------------------------------------- -}

module LocalInstance2 where

eqInt :: Int -> Int -> Bool
eqInt = (==)

instance dEqInt = Eq Int where
  (==) = eqInt

main :: IO ()
main
  = do let i = [4 :: Int, 3]
           j = [4 :: Int, 5]
       putStrLn (show (i == j))
       putStrLn (show (let instance dLocInstEqA = Eq a => Eq [a] where
                             (x:_) == (y:_) = x == y
                           instance Eq [Int] = dLocInstEqA dEqInt
                       in  i == j
                      ))
