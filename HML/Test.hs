-- test = let y = x
           -- x = 0
       -- in y
       
-- main = test

data List a = Cons a (List a)
            | Nil
            
-- data Bool = True | False
         
foreign import ccall "Prelude" (+) :: Int -> Int -> Int
foreign import ccall "Prelude" map :: forall a b.(a -> b) -> List a -> List b
foreign import ccall "Prelude" (.) :: forall a b c.(b -> c) -> (a -> b) -> a -> c         
         
id x = x  
       
test = ((+1) .)

-- id (x::forall a. a->a) = x
-- id (x::forall a. a->a) = x

-- consNil a = Nil

-- -- | Mutual recursive function is a problem, Need to think of a solution that fits within HML
-- -- foo = (bar :: Char)
-- foo = bar
-- bar = foo

-- const a b = a
-- const' a b c = a

-- toChar = \_ -> 'o'