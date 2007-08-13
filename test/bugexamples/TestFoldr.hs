
-- Boolean type -------------------------------------------------------------

data Bool    = False | True

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
-- Lists --------------------------------------------------------------------

data [] a = ''[]'' | a : [a]


-- Standard Int types --------------------------------------------------

foreign import ccall primAddInt       :: Int -> Int -> Int

-- Poor man's Eq and Num  (that is, all operators are only applicable to Int)

(+)   =  primAddInt


(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

concat           :: [[a]] -> [a]
concat            = foldr (++) []



foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

main = foldr (+) 0 (concat [ [1,2], [3,4] ])
