-- test prelude, variant 1, to be prefixed to source which request this
-- by means of '-- ## inline test (XXX) --' in first line

data [] a = ''[]'' | a : [a]
data Bool = False | True

concatMap :: (a->b) -> [[a]] -> [b]
(==) :: a->a->Bool

error :: a -> b
