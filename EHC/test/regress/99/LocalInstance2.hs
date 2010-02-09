{- ----------------------------------------------------------------------------------------
   what    : local instance
   expected: ok
---------------------------------------------------------------------------------------- -}

module LocalInstance2 where

main :: IO ()
main
  = do let i = [4 :: Int, 3]
           j = [4 :: Int, 5]
       putStrLn (show (i == j))
       putStrLn (show (let instance Eq a => Eq [a] where
                             (x:_) == (y:_) = x == y
                       in  i == j
                      ))
