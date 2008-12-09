{- Ehc only -}
data Bool = True | False

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primLtInt"  (<)  :: Int -> Int -> Bool 

fib n = if n < 2 
        then 1
        else fib (n-1) + fib (n-2)

main = fib 40 
