
data Bool = True
          | False

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)  :: Int -> Int -> Int
foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool 

fact n = fact_accum n 1

fact_accum n accum =
  if n == 0
  then accum
  else fact_accum2 (n - 1) (n * accum)

fact_accum2 n accum =
  if n == 0
  then accum
  else 1 + fact_accum ( n - 1 ) ( n + accum )

main = fact 5
