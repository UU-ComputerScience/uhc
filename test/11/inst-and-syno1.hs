class C a where
  aa :: a -> a

data R a = R a
type RR = R Int

instance C Int where
  aa x = x

instance C a => C (R a) where
  aa (R x) = R (aa x)

v :: RR
v = aa (R 3)
