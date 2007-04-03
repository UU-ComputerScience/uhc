{-
The Reverse Polish Notation embedded
from: Techniques for Embedding Postfix Languages in Haskell
-}

module RPN where


quote            f = f ()
ldc st i         f = f (st, i)
add ((st, x), y) f = f (st, (x+y))
end ((), x)        = x
