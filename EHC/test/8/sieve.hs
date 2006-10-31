-- %% inline test (prefix1) --

filter p (h:t) = if p h then h : filter p t else filter p t
filter _ []    = []

take n (h:t) = case n `compare` 0 of
                 GT -> h : take (n-1) t
                 _  -> []
take _ []    = []

length (h:t) = 1 + length t
length []    = 0

x == y = case x `compare` y of
           EQ -> True
           _  -> False

not x = if x then False else True

x /= y = not (x == y)

notMultiple x y = not ((y / x) * x == y)

sieve (h:t) = h : sieve (filter (notMultiple h) t)

main = length (take 50 (sieve [2..]))

enumFrom s = s : enumFrom (s+1)
