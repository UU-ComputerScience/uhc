{- ----------------------------------------------------------------------------------------
   what    : local instance
   expected: ok
---------------------------------------------------------------------------------------- -}

module LocalInstance1 where

eqInt :: Int -> Int -> Bool
eqInt = (==)

main :: IO ()
main
  = do let i = 3 :: Int
           j = 5 :: Int
       putStrLn (show (i == j))
       putStrLn (show (let m = 2 :: Int
                           instance Eq Int where
                             x == y = eqInt (x `mod` m) (y `mod` m)
                       in  i == j
                      ))
