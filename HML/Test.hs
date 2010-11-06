-- test = let y = x
           -- x = 0
       -- in y
       
-- main = test

-- data List a = Cons a (List a)
        -- | Nil
            
-- data Bool = True | False
         
-- foreign import ccall "Prelude" (+) :: Int -> Int -> Int
foreign import ccall "Prelude" map :: forall a b.(a -> b) -> List a -> List b
foreign import ccall "Prelude" id :: a -> a
-- foreign import ccall "Prelude" imap :: List a -> List a
-- foreign import ccall "Prelude" (.) :: forall a b c.(b -> c) -> (a -> b) -> a -> c         
-- foreign import ccall "Prelude" ($) :: forall a b.(a -> b) -> a -> b        

-- (f . g) x = f (g x)

-- app = map (id id) (Cons 1 Nil)
     
-- f $ x = f x
main = map id
-- test = map (id id)
-- foo = id id
-- id x = x    
-- intList = Cons 1 Nil
-- mList = Cons id Nil
       
-- test = imap intList

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