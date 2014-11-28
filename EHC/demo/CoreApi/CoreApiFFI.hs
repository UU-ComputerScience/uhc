-- Small FFI for exporting IO primitives for the Core example program.
module CoreApiFFI where

import Prelude as P

-- Remove Monad class constraint

primBind :: IO a -> IO b -> IO b
primBind = (P.>>)

primReturn :: a -> IO a
primReturn = P.return

primPutStrLn :: String -> IO ()
primPutStrLn = P.putStrLn
