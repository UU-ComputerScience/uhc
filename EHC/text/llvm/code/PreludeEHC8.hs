 
infixl 6  +, -
infix  4  ==

data Bool = False | True

foreign import ccall primSubInt   :: Int -> Int -> Int
foreign import ccall primEqInt    :: Int -> Int -> Bool
foreign import ccall primAddInt   :: Int -> Int -> Int

(+)   =  primAddInt
(-)   =  primSubInt
(==)  =  primEqInt
