let class X x where
      xx :: x -> x
    instance xxx = X Int where
      xx = \a -> a
    f1 :: (# X a #) -> a -> Int
    f2 :: (# r \ l #) -> (r | l :: a ) -> a
    f3 :: (# a = Int #) -> a -> Int
in  3
