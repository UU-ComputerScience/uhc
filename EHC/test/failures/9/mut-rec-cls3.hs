-- mutual recursive classes: fail at runtime when commented alternative is used

class D a => C a where
  c :: a -> a

class C a => D a where
  d :: a -> a

instance D Int where
  d x = c x

instance C Int where
  c x = x
--  c x = d x

{-
data X a = X a | XX

instance C (X Int)
-}

v1 = d 3
main = v1
