-- %% inline test (prefix1) --

-- BEGIN
-- test prelude, variant 1, to be prefixed to source which request this
-- by means of '-- ## inline test (XXX) --' in first line

data PackedString
data [] a = ''[]'' | a : [a]
data Bool = False | True
data Ordering = LT | EQ | GT

-- type String = [Char]

foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]
foreign import ccall "primTraceStringExit" traceStringExit :: [Char] -> [Char]

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primDivInt" (/) :: Int -> Int -> Int
foreign import ccall "primMulInt" (*) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int
foreign import ccall "primCmpInt" compare :: Int -> Int -> Ordering

seq :: forall a . a -> forall b . b -> b
x `seq` y = letstrict x' = x in y

id x = x

error :: [Char] -> a
error s = traceStringExit s `seq` undefined
undefined :: forall a . a
undefined = error "undefined"
-- END

filter p (h:t) = if p h then h : filter p t else filter p t
filter _ []    = []

take n (h:t) = case n `compare` 0 of
                 GT -> h : take (n-1) t
                 _  -> []
take _ []    = []

length (h:t) = 1 + length t
length []    = 0

sum (h:t) = h + sum t
sum []    = 0

x == y = case x `compare` y of
           EQ -> True
           _  -> False

not x = if x then False else True

x /= y = not (x == y)

notMultiple x y = not ((y / x) * x == y)

sieve (h:t) = h : sieve (filter (notMultiple h) t)

main = length {- sum -} (take 1500 (sieve [2..]))

{-
main = take 50 (sieve [2..])
-}
enumFrom s = s : enumFrom (s+1)
