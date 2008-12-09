let

data A a = AA (B a) a
data B a = BB (a -> A a)

in 3
