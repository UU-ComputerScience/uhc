module Array (
    module Ix,  -- export all of Ix for convenience
    Array, array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, ixmap
  ) where

import Ix
import Data.Array
