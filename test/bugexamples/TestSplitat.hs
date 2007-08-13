-- Compile with EHC8 or higher
-- The standard "splitAt" function
-- grinc   compiler compiles correctly
-- grinbc  interpreter crashes

-- Note that ToGrin generates strange code:
-- the thunk that is needed for handling the n>0 case is stored
-- before the test n<=0 is made.
-- A simpler example of the same situation is generated for the "demo" example.

-- This problem is not fatal for the grinc compiler.
-- It is not clear whether this causes the crash of grinbc


-- Essence of prelude -------------------------------------------------------------

data Bool    = False | True
data Ordering = LT | EQ | GT
data [] a = ''[]'' | a : [a]

foreign import ccall primAddInt       :: Int -> Int -> Int
foreign import ccall primSubInt       :: Int -> Int -> Int

foreign import ccall primLtInt      :: Int -> Int -> Bool
foreign import ccall primEqInt      :: Int -> Int -> Bool

(+)   =  primAddInt
(-)   =  primSubInt
(==)  =  primEqInt
(<)   =  primLtInt
x <= y  =  if x<y then True else x==y

fst (x,y) = x
snd (x,y) = y
undefined = undefined
hd []     = undefined
hd (x:xs) = x

------------------------------------------------------------------------------------

splitAt               :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([],xs)
splitAt _ []          = ([],[])
splitAt n (x:xs)      = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs

demo               :: [a] -> ([a], [a])
demo []          = ([],[])
demo xs          = (xs,xs)

main = hd (snd (splitAt 3 [1,2,3,4,5]))
