module TestEqList where


infixr 5  :
infix  4  ==, /=
infixr 3  &&
infixr 2  ||

data Bool  =  False | True

(&&) :: Bool -> Bool -> Bool
False && x  =  False
True  && x  =  x

(||) :: Bool -> Bool -> Bool
False || x  =  x
True  || x  =  True

not :: Bool -> Bool
not True   =  False
not False  =  True

class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- default definitions
    x == y  =  not (x/=y)
    x /= y  =  not (x==y)

foreign import ccall primEqInt      :: Int -> Int -> Bool

instance Eq Int where 
   (==)  =  primEqInt

instance Eq Bool where 
   True == True  =  True
   False == False = True
   _ == _ = False


data [] a = ''[]'' | a : [a]

instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False


main = [100] == [100] && [True]==[True] && True==False && 42==37
