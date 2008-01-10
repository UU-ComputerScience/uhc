{- Ehc only -}
data PackedString
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]

error :: [Char] -> a
error s = undefined
undefined :: forall a . a
undefined = error "undefined"

data Bool = True | False
data [] a = ''[]'' | a : [a]

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primLtInt"  (<)  :: Int -> Int -> Bool 

zipWith :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _           = []

tail :: [a] -> [a]
tail (_:xs) = xs

(!!) :: [a] -> Int -> a
(!!) (x:xs) n = if n < 1
                then x
                else xs !! (n-1)

main = fibs !! 40  

{- GHC only -}
{-
main = putStr $ show $ fibs !! 160001 
-}
{- General -}
fibs = 1 : (1 : (zipWith (+) fibs (tail fibs)))

