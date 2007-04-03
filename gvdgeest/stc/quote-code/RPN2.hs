{-# OPTIONS -fglasgow-exts #-}
module RPN2
where
import CPS


quote'            = ()
ldc' st i         = (st, i)
add' ((st, x), y) = (st, x + y)
end' ((), x)      = x


quote      =  lift  quote'
ldc  st i  =  lift  (ldc' st i)
add  st    =  lift  (add' st)
end        =  end'

test = quote ldc 3 ldc 4 add end

