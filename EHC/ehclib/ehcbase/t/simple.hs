-- Prelude -----------------------

-- standard definitions

id :: a -> a
id x = x

undefined = undefined


-- Int


-- Mini-IO

newtype IO a = IO (IOWorld -> IOResult a)

newtype IOResult a = IOResult a

data IOWorld

foreign import ccall primRawShow :: a -> a

primretIO :: IO ()
primretIO = IO (\_ -> IOResult ())


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



main = --letstrict x = primInitOracle
       --in 
         primretIO

