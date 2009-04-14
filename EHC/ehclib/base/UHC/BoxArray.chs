%%[doesWhat doclatex
Interface to Array, in several varieties: mutable and immutable.
Offering the Array related primitives from GHC.Prim.

Content is garbage collected, holding boxed values only.
For arrays with non GC content see UHC.ByteArray.

All sizes and indices are in terms of machine words (that which holds a boxed value)
%%]

%%[99
module UHC.BoxArray
  ( BoxArray
  , MutableBoxArray
  
  , newArray
  , sameMutableArray
  , indexArray, readArray, writeArray
  
  , unsafeFreezeArray, unsafeThawArray
  )
  where

import UHC.Base

%%]

%%[99
data BoxArray x

newtype MutableBoxArray s x = MutableBoxArray (BoxArray x)
%%]

%%[99
-- allocate array, size in words, will only hold boxed values, init by x
-- NOTE: the 'x' may not be evaluated, the compiler knows about this!!
foreign import prim primNewArray :: Int -> x -> BoxArray x

-- index, write, compare array
foreign import prim "primIndexArray" 	indexArray 		:: BoxArray x -> Int -> x

-- NOTE: the 'x' may not be evaluated, the compiler knows about this!!
foreign import prim "primWriteArray"  	primWriteArray 	:: BoxArray x -> Int -> x -> ()

foreign import prim "primSameArray"  	primSameArray 	:: BoxArray x -> BoxArray x -> Bool
%%]

%%[99
newArray :: Int -> x -> State s -> ( State s, MutableBoxArray s x )
newArray i x s = letstrict a = primNewArray i x in (s, MutableBoxArray a)
   
sameMutableArray :: MutableBoxArray s x -> MutableBoxArray s x -> Bool
sameMutableArray (MutableBoxArray a) (MutableBoxArray b) = primSameArray a b
%%]

%%[99
readArray :: MutableBoxArray s x -> Int -> State s -> ( State s,x )
readArray (MutableBoxArray a) i s = letstrict x = indexArray a i in (s, x)

writeArray :: MutableBoxArray s x -> Int -> x -> State s -> State s
writeArray (MutableBoxArray a) i x s = letstrict _ = primWriteArray a i x in s
%%]

%%[99
unsafeFreezeArray :: MutableBoxArray s a -> State s -> ( State s, BoxArray a )
unsafeFreezeArray (MutableBoxArray a) s = letstrict _ = a in (s,a)

unsafeThawArray :: BoxArray a -> State s -> ( State s, MutableBoxArray s a )
unsafeThawArray a s = letstrict _ = a in (s, MutableBoxArray a)
%%]
