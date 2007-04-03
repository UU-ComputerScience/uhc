

data Bit = Zero | One deriving Show

type Bin = [Bit]

class Binary a where
  showBin :: a -> Bin

data GRose f a = GRose a (f (GRose f a))

instance Binary Int where
  showBin i | i == 0    = [Zero]
            | otherwise = [One]

instance Binary a => Binary [a] where
  showBin [] = []
  showBin (x:xs) = showBin x ++ showBin xs

instance (Binary a, Binary (f (GRose f a))) => Binary (GRose f a) where
  showBin (GRose x ts) = showBin x ++ showBin ts
