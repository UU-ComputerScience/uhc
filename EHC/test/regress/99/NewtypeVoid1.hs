{- ----------------------------------------------------------------------------------------
   what    : Infinite _|_ (bottom) type via newtype
   expected: ok
---------------------------------------------------------------------------------------- -}

module NewtypeVoid1 where

newtype Void = Void Void deriving Show

v1 = Void undefined
v2 = undefined :: Void

f (Void v) = 1::Int
v3 = f v1
v4 = f v2

main :: IO ()
  = do putStrLn (take 100 $ show v1)
       putStrLn (take 100 $ show v2)
       putStrLn (show v3)
       putStrLn (show v4)
