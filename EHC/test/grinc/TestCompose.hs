module TestCompose where

data PackedString
foreign import ccall "primCStringToString" packedStringToString :: PackedString -> [Char]
error :: [Char] -> a
error s = undefined
undefined :: forall a . a
undefined = error "undefined"

data ''[]'' a = a : [a] | ''[]''

head             :: [a] -> a
head (x:_)        = x

tail             :: [a] -> [a]
tail (_:xs)       = xs

(.)            :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

main = (head . tail) [7,8,9]
