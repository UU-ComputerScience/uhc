module EH101.Base.Hashable
( Hash, Hashable (..) )
where
import EH.Util.Utils
import Data.HashTable
import Data.List
import Data.Int
import Data.Bits

{-# LINE 13 "src/ehc/Base/Hashable.chs" #-}
type Hash = Int32

class Hashable a where
  hash :: a -> Hash
  hashList :: [a] -> Hash
  hashList = foldr (\a h -> (h `rotateL` 4) `xor` hash a) 0

instance Hashable Int where
  hash = hashInt

instance Hashable String where
  hash = hashString


