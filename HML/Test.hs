test = let y = x
           x = 0
       in y
       
main = test

data List a = Cons a (List a)
            | Nil

-- id (x::forall a. a->a) = x
id x = x

consNil a = Nil

-- | Mutual recursive function is a problem, Need to think of a solution that fits within HML
-- foo = (bar :: Char)
foo = bar2
bar = foo

const a b = a
const' a b c = a

toChar = \_ -> 'o'