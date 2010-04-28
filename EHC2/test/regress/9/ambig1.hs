class S a where
  s :: a -> Int

class R a where
  r :: Int -> a

f x = s (r x)
