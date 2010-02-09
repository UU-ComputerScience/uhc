{- ----------------------------------------------------------------------------------------
   what    : existential type
   expected: ok
---------------------------------------------------------------------------------------- -}

module ExistentialType1 where

x1 :: exists a . a
x1 = 3 :: Int

x2 :: exists a . (a, a -> Int)
x2 = (3 :: Int, id)

xapp :: (exists b . (b,b -> a)) -> a
xapp (v,f) = f v

x2app = xapp x2

mkx :: Bool -> exists a . (a, a -> Int)
mkx b = if b then x2 else ('a',ord)

y1 = mkx True
y2 = mkx False

main :: IO ()
main
  = do putStrLn (show (xapp y1))
       putStrLn (show (xapp y2))
