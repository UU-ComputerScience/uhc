
-- Boolean type -------------------------------------------------------------

data Bool    = False | True

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
-- Lists --------------------------------------------------------------------

data [] a = ''[]'' | a : [a]


-- Standard Int types --------------------------------------------------

foreign import ccall primGtInt      :: Int -> Int -> Bool
foreign import ccall primAddInt       :: Int -> Int -> Int

-- Poor man's Eq and Num  (that is, all operators are only applicable to Int)

(+)   =  primAddInt
(>)   =  primGtInt

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

main = until (>5) (+1) 1
