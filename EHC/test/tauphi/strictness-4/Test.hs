foreign import ccall "primAddInt" (+) :: @strict Int -> @strict Int -> Int

data Bool = False | True

foo :: Bool -> (@strict Int, @strict Int) -> Int
foo False _      = 0
foo True  (x, y) = x + y

main = foo True (3, 4)

