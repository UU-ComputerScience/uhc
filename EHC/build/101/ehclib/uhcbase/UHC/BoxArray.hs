{-# LANGUAGE NoImplicitPrelude #-}

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


data BoxArray x

newtype MutableBoxArray s x = MutableBoxArray (BoxArray x)

-- allocate array, size in words, will only hold boxed values, init by x
-- NOTE: the 'x' may not be evaluated, the compiler knows about this!!
foreign import prim primNewArray :: Int -> x -> BoxArray x

-- index, write, compare array
foreign import prim "primIndexArray" 	indexArray 		:: BoxArray x -> Int -> x

-- NOTE: the 'x' may not be evaluated, the compiler knows about this!!
foreign import prim "primWriteArray"  	primWriteArray 	:: BoxArray x -> Int -> x -> ()

foreign import prim "primSameArray"  	primSameArray 	:: BoxArray x -> BoxArray x -> Bool


newArray :: Int -> x -> State s -> ( State s, MutableBoxArray s x )
newArray i x s = let !a = primNewArray i x in (s, MutableBoxArray a)

sameMutableArray :: MutableBoxArray s x -> MutableBoxArray s x -> Bool
sameMutableArray (MutableBoxArray a) (MutableBoxArray b) = primSameArray a b

readArray :: MutableBoxArray s x -> Int -> State s -> ( State s,x )
readArray (MutableBoxArray a) i s = let !x = indexArray a i in (s, x)

writeArray :: MutableBoxArray s x -> Int -> x -> State s -> State s
writeArray (MutableBoxArray a) i x s = let !_ = primWriteArray a i x in s

unsafeFreezeArray :: MutableBoxArray s a -> State s -> ( State s, BoxArray a )
unsafeFreezeArray (MutableBoxArray a) s = let !_ = a in (s,a)

unsafeThawArray :: BoxArray a -> State s -> ( State s, MutableBoxArray s a )
unsafeThawArray a s = let !_ = a in (s, MutableBoxArray a)
