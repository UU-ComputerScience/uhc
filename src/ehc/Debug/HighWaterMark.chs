{-# OPTIONS -fglasgow-exts #-}
{- 
   megaBytesAllocated counts the number of megabytes currently
   allocated by the ghc runtime allocator.

   Note GHC (currently) does not free previously allocated mblocks.
   This means successive calls should never give decreasing values.

   Note from Simon Marlow:

   Note that this only counts memory allocated by the GHC storage manager;
   it doesn't include the data segments, malloc(), the C stack, or other
   mmap()'d stuff.  Be careful if your program is using any of these other
   allocation methods (perhaps via an external library through the FFI).
-}

%%[101 module {%{EH}Debug.HighWaterMark} import(Foreign.C.Types (CULong))
%%]

%%[101 export(megaBytesAllocated)
foreign import ccall "get_mblocks_allocated" 
                     getMBlocksAllocated :: IO CULong 

foreign import ccall "get_mblock_size" 
                     getMBlockSize:: IO Int

-- number of bytes in a megabyte
megaByteSize :: Integer
megaByteSize = 1024 * 1024

-- find out the maximum number of blocks currently allocated
-- multiply by the block size to find out the size in bytes
-- divide by the size of a megabyte
megaBytesAllocated :: IO Integer
megaBytesAllocated 
   = do blocks <- getMBlocksAllocated
        blockSize <- getMBlockSize
        let blocksInteger    = (fromIntegral blocks) :: Integer
        let blockSizeInteger = (fromIntegral blockSize) :: Integer
        let bytes            = blocksInteger * blockSizeInteger
        let megaBytes        = bytes `div` megaByteSize
        return megaBytes
%%]