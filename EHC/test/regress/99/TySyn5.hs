{- ----------------------------------------------------------------------------------------
   what    : type synonym, in particular a simple renaming, ty eta reducible, where ty lambda args match tail of applied args in ty app of lambda body
   expected: ok, no output, no errors
---------------------------------------------------------------------------------------- -}

module TySyn5 where

type Parser a = GenParser Char () a
type CharParser st a = GenParser Char st a
newtype GenParser a b c = Parser (a, b, c)


instance Monad (GenParser a b) where
    return  = undefined
    (>>=)   = undefined

myFun :: Parser Integer
myFun = do a <- fun1
           char 'q' -- :: GenParser Char () Char
           fun2 a

fun1 :: Parser Integer
fun1 = undefined

fun2 :: Integer -> GenParser Char () Integer
fun2 i = undefined

char :: Char -> CharParser st Char
char = undefined

main = return ()
