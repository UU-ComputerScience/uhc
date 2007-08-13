-- ## inline test (prefix1) --
-- name capture of 'x' in list comprehension.

f x inp = [(a,as) | (a:as) <- [inp], x==a ]

