-- test = let y = x
           -- x = 0
       -- in y
       
-- main = test

data List a = Cons a (List a)
            | Nil

id x = Nil

-- | Mutual recursive function is a problem, Need to think of a solution that fits within HML
-- foo = bar
-- bar = foo

-- const a b c = a

-- toChar = \_ -> 'o'