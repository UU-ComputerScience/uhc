let class Fun a b | a -> b where
       first ::  a -> b

    instance Fun (a, b) a where
       first = \(x, y) -> x

 in first  (1, 2)
