module TestEqInt where

infixr 5  :
infix  4  ==, /=

data Bool    = False | True

not         :: Bool -> Bool
not True     = False
not False    = True

class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- default definitions
    x == y  =  not (x/=y)
    x /= y  =  not (x==y)

foreign import ccall primEqInt      :: Int -> Int -> Bool

instance Eq Int where 
   (==)  =  primEqInt

main = 37==42
