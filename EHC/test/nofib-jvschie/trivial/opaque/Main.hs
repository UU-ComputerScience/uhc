
data Integer 

foreign import ccall primIntToInteger :: Int -> Integer
foreign import ccall primIntegerToInt :: Integer -> Int

main = <PRINT_INT> (primIntegerToInt (primIntToInteger 37))
