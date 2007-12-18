data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int

replicate x = x : replicate x

length [] = 0
length (x:xs) = 1 + length xs

main = length (replicate 1) 

