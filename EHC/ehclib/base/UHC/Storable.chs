%%[99
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Storable
-- Copyright   :  (c) The FFI task force, 2000-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Helper functions for "Foreign.Storable"
--
-- Adapted for use in EHC
--
-----------------------------------------------------------------------------

-- #hide
module UHC.Storable
        ( readWideCharOffPtr  
        , readIntOffPtr       
        , readWordOffPtr      
        , readPtrOffPtr       
        , readFunPtrOffPtr    
        , readFloatOffPtr     
        , readDoubleOffPtr    
        , readStablePtrOffPtr 
        , readInt8OffPtr      
        , readInt16OffPtr     
        , readInt32OffPtr     
        , readInt64OffPtr     
        , readWord8OffPtr     
        , readWord16OffPtr    
        , readWord32OffPtr    
        , readWord64OffPtr    
        , writeWideCharOffPtr 
        , writeIntOffPtr      
        , writeWordOffPtr     
        , writePtrOffPtr      
        , writeFunPtrOffPtr   
        , writeFloatOffPtr    
        , writeDoubleOffPtr   
        , writeStablePtrOffPtr
        , writeInt8OffPtr     
        , writeInt16OffPtr    
        , writeInt32OffPtr    
        , writeInt64OffPtr    
        , writeWord8OffPtr    
        , writeWord16OffPtr   
        , writeWord32OffPtr   
        , writeWord64OffPtr   
        ) where

import UHC.Base
import UHC.StablePtr
import UHC.Types
import UHC.Ptr
-- import UHC.IOBase


foreign import prim "primReadWord8OffAddr" 		primReadWideCharOffAddr  	:: Addr -> Int -> Char
foreign import prim "primReadWordOffAddr" 		primReadIntOffAddr       	:: Addr -> Int -> Int
foreign import prim "primReadWordOffAddr" 		primReadWordOffAddr      	:: Addr -> Int -> Word
foreign import prim "primReadWordOffAddr" 		primReadAddrOffAddr      	:: Addr -> Int -> Addr
foreign import prim "primReadWordOffAddr" 		primReadStableAddrOffAddr	:: Addr -> Int -> Addr
foreign import prim "primReadFloatOffAddr" 		primReadFloatOffAddr     	:: Addr -> Int -> Float
foreign import prim "primReadDoubleOffAddr" 	primReadDoubleOffAddr    	:: Addr -> Int -> Double
foreign import prim "primReadWord8OffAddr" 		primReadInt8OffAddr      	:: Addr -> Int -> Int8
foreign import prim "primReadWord16OffAddr" 	primReadInt16OffAddr     	:: Addr -> Int -> Int16
foreign import prim "primReadWord32OffAddr" 	primReadInt32OffAddr     	:: Addr -> Int -> Int32
foreign import prim "primReadWord64OffAddr" 	primReadInt64OffAddr     	:: Addr -> Int -> Int64
foreign import prim "primReadWord8OffAddr" 		primReadWord8OffAddr     	:: Addr -> Int -> Word8
foreign import prim "primReadWord16OffAddr" 	primReadWord16OffAddr    	:: Addr -> Int -> Word16
foreign import prim "primReadWord32OffAddr" 	primReadWord32OffAddr    	:: Addr -> Int -> Word32
foreign import prim "primReadWord64OffAddr" 	primReadWord64OffAddr    	:: Addr -> Int -> Word64

readWideCharOffPtr  :: Ptr Char          -> Int -> IO Char
readIntOffPtr       :: Ptr Int           -> Int -> IO Int
readWordOffPtr      :: Ptr Word          -> Int -> IO Word
readPtrOffPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
readFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
readFloatOffPtr     :: Ptr Float         -> Int -> IO Float
readDoubleOffPtr    :: Ptr Double        -> Int -> IO Double
readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
readInt8OffPtr      :: Ptr Int8          -> Int -> IO Int8
readInt16OffPtr     :: Ptr Int16         -> Int -> IO Int16
readInt32OffPtr     :: Ptr Int32         -> Int -> IO Int32
readInt64OffPtr     :: Ptr Int64         -> Int -> IO Int64
readWord8OffPtr     :: Ptr Word8         -> Int -> IO Word8
readWord16OffPtr    :: Ptr Word16        -> Int -> IO Word16
readWord32OffPtr    :: Ptr Word32        -> Int -> IO Word32
readWord64OffPtr    :: Ptr Word64        -> Int -> IO Word64


readWideCharOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWideCharOffAddr a i  
readIntOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadIntOffAddr a i       
readWordOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWordOffAddr a i      
readPtrOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> Ptr (primReadAddrOffAddr a i)      
readFunPtrOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> FunPtr (primReadAddrOffAddr a i)      
readFloatOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadFloatOffAddr a i     
readDoubleOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadDoubleOffAddr a i    
readStablePtrOffPtr (Ptr a) i
  = ioFromPrim $ \_ -> StablePtr (primReadStableAddrOffAddr a i) 
readInt8OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadInt8OffAddr a i      
readWord8OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWord8OffAddr a i     
readInt16OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadInt16OffAddr a i     
readWord16OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWord16OffAddr a i    
readInt32OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadInt32OffAddr a i     
readWord32OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWord32OffAddr a i    
readInt64OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadInt64OffAddr a i     
readWord64OffPtr (Ptr a) i
  = ioFromPrim $ \_ -> primReadWord64OffAddr a i    
  

foreign import prim "primWriteWord8OffAddr" 	primWriteWideCharOffAddr  	:: Addr -> Int -> Char        -> ()
foreign import prim "primWriteWordOffAddr" 		primWriteIntOffAddr       	:: Addr -> Int -> Int         -> ()
foreign import prim "primWriteWordOffAddr" 		primWriteWordOffAddr      	:: Addr -> Int -> Word        -> ()
foreign import prim "primWriteWordOffAddr" 		primWriteAddrOffAddr      	:: Addr -> Int -> Addr        -> ()
foreign import prim "primWriteWordOffAddr" 		primWriteStableAddrOffAddr	:: Addr -> Int -> Addr        -> ()
foreign import prim "primWriteFloatOffAddr" 	primWriteFloatOffAddr     	:: Addr -> Int -> Float       -> ()
foreign import prim "primWriteDoubleOffAddr" 	primWriteDoubleOffAddr    	:: Addr -> Int -> Double      -> ()
foreign import prim "primWriteWord8OffAddr" 	primWriteInt8OffAddr      	:: Addr -> Int -> Int8        -> ()
foreign import prim "primWriteWord16OffAddr" 	primWriteInt16OffAddr     	:: Addr -> Int -> Int16       -> ()
foreign import prim "primWriteWord32OffAddr" 	primWriteInt32OffAddr     	:: Addr -> Int -> Int32       -> ()
foreign import prim "primWriteWord64OffAddr" 	primWriteInt64OffAddr     	:: Addr -> Int -> Int64       -> ()
foreign import prim "primWriteWord8OffAddr" 	primWriteWord8OffAddr     	:: Addr -> Int -> Word8       -> ()
foreign import prim "primWriteWord16OffAddr" 	primWriteWord16OffAddr    	:: Addr -> Int -> Word16      -> ()
foreign import prim "primWriteWord32OffAddr" 	primWriteWord32OffAddr    	:: Addr -> Int -> Word32      -> ()
foreign import prim "primWriteWord64OffAddr" 	primWriteWord64OffAddr    	:: Addr -> Int -> Word64      -> ()

writeWideCharOffPtr  :: Ptr Char          -> Int -> Char        -> IO ()
writeIntOffPtr       :: Ptr Int           -> Int -> Int         -> IO ()
writeWordOffPtr      :: Ptr Word          -> Int -> Word        -> IO ()
writePtrOffPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
writeFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
writeFloatOffPtr     :: Ptr Float         -> Int -> Float       -> IO ()
writeDoubleOffPtr    :: Ptr Double        -> Int -> Double      -> IO ()
writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
writeInt8OffPtr      :: Ptr Int8          -> Int -> Int8        -> IO ()
writeInt16OffPtr     :: Ptr Int16         -> Int -> Int16       -> IO ()
writeInt32OffPtr     :: Ptr Int32         -> Int -> Int32       -> IO ()
writeInt64OffPtr     :: Ptr Int64         -> Int -> Int64       -> IO ()
writeWord8OffPtr     :: Ptr Word8         -> Int -> Word8       -> IO ()
writeWord16OffPtr    :: Ptr Word16        -> Int -> Word16      -> IO ()
writeWord32OffPtr    :: Ptr Word32        -> Int -> Word32      -> IO ()
writeWord64OffPtr    :: Ptr Word64        -> Int -> Word64      -> IO ()


writeWideCharOffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWideCharOffAddr a i x  
writeIntOffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteIntOffAddr a i x       
writeWordOffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWordOffAddr a i x      
writePtrOffPtr (Ptr a) i (Ptr x)
  = ioFromPrim $ \_ -> primWriteAddrOffAddr a i x      
writeFunPtrOffPtr (Ptr a) i (FunPtr x)
  = ioFromPrim $ \_ -> primWriteAddrOffAddr a i x      
writeFloatOffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteFloatOffAddr a i x     
writeDoubleOffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteDoubleOffAddr a i x    
writeStablePtrOffPtr (Ptr a) i (StablePtr x)
  = ioFromPrim $ \_ -> primWriteStableAddrOffAddr a i x 
writeInt8OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteInt8OffAddr a i x      
writeWord8OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWord8OffAddr a i x     
writeInt16OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteInt16OffAddr a i x     
writeWord16OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWord16OffAddr a i x    
writeInt32OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteInt32OffAddr a i x     
writeWord32OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWord32OffAddr a i x    
writeInt64OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteInt64OffAddr a i x     
writeWord64OffPtr (Ptr a) i x
  = ioFromPrim $ \_ -> primWriteWord64OffAddr a i x    

%%]
