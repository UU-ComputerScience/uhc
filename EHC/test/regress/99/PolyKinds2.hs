{- ----------------------------------------------------------------------------------------
   what    : poly kind inference
   expected: not ok, because of NoPolyKinds
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE NoPolyKinds #-}

module PolyKinds2 where
  
data Proxy t = Proxy

class Foo t where
  bar :: Proxy t -> Int

instance Foo Int  where bar _ = 0
instance Foo []   where bar _ = 0

main = return ()
