
data Bool = True
          | False

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)  :: Int -> Int -> Int
foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool 

fact n = fact_accum n 1

fact_accum n accum =
  if n == 0
  then accum
  else fact_accum (n - 1) (n * accum)

main = fact 10
