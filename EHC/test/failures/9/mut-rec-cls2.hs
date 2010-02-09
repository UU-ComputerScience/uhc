-- mutual recursive classes: ok because superclass is not used

class D a => C a where
  c :: a -> a

class C a => D a where
  d :: a -> a

instance D Int where
  d x = x

instance C Int where
  c x = x

v1 = d 3
main = v1
