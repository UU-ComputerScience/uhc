-------------------------------------------------------------------------
-- Wrapper module around Data.Binary, providing additional functionality
-------------------------------------------------------------------------

module EH.Util.Binary
  ( module Data.Binary
  , module Data.Binary.Get
  , module Data.Binary.Put

  , hGetBinary
  , getBinaryFile
  , getBinaryFPath
  
  , hPutBinary
  , putBinaryFile
  , putBinaryFPath
  )
  where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put(runPut,putWord16be)
import Data.Binary.Get(runGet,getWord16be)
import IO
import System.IO(openBinaryFile)
import Control.Monad

import EH.Util.FPath

-------------------------------------------------------------------------
-- Decoding from ...
-------------------------------------------------------------------------

-- | Decode from Handle
hGetBinary :: Binary a => Handle -> IO a
hGetBinary h
  = liftM decode (L.hGetContents h)
    
-- | Decode from FilePath
getBinaryFile :: Binary a => FilePath -> IO a
getBinaryFile fn
  = do { h <- openBinaryFile fn ReadMode
       ; b <- hGetBinary h
       -- ; hClose h
       ; return b ;
       }

-- | Decode from FilePath
getBinaryFPath :: Binary a => FPath -> IO a
getBinaryFPath fp
  = getBinaryFile (fpathToStr fp)

-------------------------------------------------------------------------
-- Encoding to ...
-------------------------------------------------------------------------

-- | Encode to Handle
hPutBinary :: Binary a => Handle -> a -> IO ()
hPutBinary h pt
  = L.hPut h (encode pt)
    
-- | Encode to FilePath
putBinaryFile :: Binary a => FilePath -> a -> IO ()
putBinaryFile fn pt
  = do { h <- openBinaryFile fn WriteMode
       ; hPutBinary h pt
       ; hClose h
       }

-- | Encode to FPath, ensuring existence of path
putBinaryFPath :: Binary a => FPath -> a -> IO ()
putBinaryFPath fp pt
  = do { fpathEnsureExists fp
       ; putBinaryFile (fpathToStr fp) pt
       }

