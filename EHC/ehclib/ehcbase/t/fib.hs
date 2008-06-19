-- Prelude -----------------------


-- standard definitions

id :: a -> a
id x = x

undefined = undefined


-- Int

infixl 6  +, -

foreign import ccall primAddInt       :: Int -> Int -> Int
foreign import ccall primSubInt       :: Int -> Int -> Int
(+) = primAddInt
(-) = primSubInt

data Bool = False | True

foreign import ccall primLtInt      :: Int -> Int -> Bool
(<)     = primLtInt


data PackedString
foreign import ccall "primCStringToInteger" packedStringToInteger :: PackedString -> Integer


foreign import ccall primIntegerToInt :: Integer -> Int
fromInteger   = primIntegerToInt


-- Mini-IO

newtype IO a = IO (IOWorld -> IOResult a)

newtype IOResult a = IOResult a

data IOWorld

foreign import ccall primRawShow :: a -> a

primretIO :: IO ()
primretIO = IO (\_ -> IOResult ())

print       :: Int -> IO ()
print n   = letstrict n' = n
            in letstrict n'' = primRawShow n
               in primretIO


-- Wrapper around 'main', invoked as 'ehcRunMain main'

ehcRunMain :: IO a -> IO a
ehcRunMain m = m



-- Debugging primitives

data Oracle

foreign import ccall primDumpOracle      :: Int -- length
foreign import ccall primInitOracle      :: ()
foreign import ccall primOracleEnter     :: Oracle -> Oracle
foreign import ccall primOracleLeave     :: Oracle -> a -> a
foreign import ccall primOracleNewEntry  :: Oracle 

-- Fib ---------------------------



fib n = if n <2 then n else fib (n-1) + fib (n-2)

main = letstrict x = primInitOracle
       in print (fib 10)

