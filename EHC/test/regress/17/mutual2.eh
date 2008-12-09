let

data A a = AA (B a) (a -> Int)
data B a = BB (a -> A a)

in 3
