foreign import ccall "primAddInt" (+) :: @strict Int -> @strict Int -> Int

succ :: @strict Int -> Int
succ x = x + 1

main = succ 3

