-- mutual referring classes

class C a where
  c :: D a => a -> a

class C a => D a where
  d :: a -> a
