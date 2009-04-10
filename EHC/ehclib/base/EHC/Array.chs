%%[doesWhat doclatex
Interface to Array, in several varieties: mutable and immutable.
Offering the Array related primitives from GHC.Prim.

Content is garbage collected, holding boxed values only.
For arrays with non GC content see EHC.ByteArray.

All sizes and indices are in terms of machine words (that which holds a boxed value)
%%]

%%[99
module EHC.Array
  ( Array
  , MutableArray
  
  , readArray
  )
  where

import EHC.Prelude

%%]

%%[99
data Array x

newtype MutableArray s x = MutableArray (Array x)
%%]

%%[99
-- allocate array, size in words, will only hold boxed values, init by x
foreign import prim primNewArray :: Int -> x -> Array x

-- index, write, compare array
foreign import prim "primIndexArray" 	indexArray 		:: Array x -> Int -> x
foreign import prim "primWriteArray"  	primWriteArray 	:: Array x -> Int -> x -> ()
foreign import prim "primSameArray"  	primSameArray 	:: Array x -> Array x -> Bool
%%]

%%[99
readArray :: MutableArray s x -> Int -> State s -> ( State s,x )
readArray (MutableArray a) i s = let x = indexArray a i in (s, x)
%%]
