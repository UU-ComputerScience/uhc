let class A a where
      aa :: a -> Int
    instance A Int where
      aa = \x -> x
in
let f :: (# A a #) -> a -> a -> Int
in
let v = f 3 (# A Int :> (aa = \x -> x) #) 4
in  v
