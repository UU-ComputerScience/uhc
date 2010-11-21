-- test = let y = x
           -- x = 0
       -- in y
       
-- main = test

data List a = Cons a (List a)
        | Nil
            
data Bool = True | False
         
foreign import ccall "Prelude" (+) :: Int -> Int -> Int
foreign import ccall "Prelude" map :: forall a b.(a -> b) -> List a -> List b
-- foreign import ccall "Prelude" id :: a -> a
foreign import ccall "Prelude" foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foreign import ccall "Prelude" zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
-- foreign import ccall "Prelude" imap :: List a -> List a
-- foreign import ccall "Prelude" (.) :: forall a b c.(b -> c) -> (a -> b) -> a -> c         
-- foreign import ccall "Prelude" ($) :: forall a b.(a -> b) -> a -> b        

(f . g) x = f (g x)
id x = x    
f $ x = f x

-- app = map (id id) (Cons 1 Nil)
     
-- main = ((+1). id)
-- main = test
-- main = (m . n)
-- main = \(f :: forall a. a -> Int) -> Cons (f 0) (Cons (f 'a') Nil)
-- app = \(f :: forall a. a -> Int) -> Cons (f 0) (Cons (f 'a') Nil)
-- fun x = 1
-- main = app id
-- main = ($)
main = map (id id)
-- main = id id
-- main f = Cons (f 0) (Cons (f 'a') Nil)
-- test = map (id id)
-- foo = id id
-- intList = Cons 1 Nil
-- mList = Cons id Nil
-- main = map map    
-- main = foldr map . \t -> zipWith map t Nil
-- m = foldr map
-- n =  \t -> zipWith map t Nil
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