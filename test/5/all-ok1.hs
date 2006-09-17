-- data types, list, list comprehension

error :: forall a . [Char] -> a
concatMap :: (a -> [b]) -> [a] -> [b]
data ''[]'' a = a : [a]
              | ''[]''
data Bool = False | True
x :: [Int]
x = [3]
y = [ w | z <- x, f z, f v, let w = z ]
  where f :: forall a . a -> Bool
        f x = True
        v | True  = v3
          | False = v4
          where v3 = v4
                v4 = 4
f (a:as) = a
g [a,b] = b
data T = T Int
h (T i) = i
T j = T 4
-- s :: Int
s = "ss"
