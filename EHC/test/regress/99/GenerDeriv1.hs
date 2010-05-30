{- ----------------------------------------------------------------------------------------
   what    : generic deriving: kind * (i.e. using Representable0)
   expected: ok
---------------------------------------------------------------------------------------- -}

module GenerDeriv1 where

data Bit = OBit | IBit | BitsI Int | BitsC Int
  deriving Show

class Encode aT where
  encode :: aT -> [Bit]

class Encode' fT where
  encode' :: fT xT -> [Bit]

instance Encode' V1 where
  encode' _ = []

instance Encode' U1 where
  encode' _ = []

instance (Encode' fT) => Encode' (M1 iT cT fT) where
  encode' (M1 a) = encode' a

instance  (Encode' fT, Encode' gT) => Encode' (fT :+:  gT) where
  encode' (L1  a)     = OBit : encode' a
  encode' (R1  a)     = IBit : encode' a

instance  (Encode' fT, Encode' gT) => Encode' (fT :*:  gT) where
  encode' (a :*: b)  = encode' a ++ encode' b

instance (Encode fT) => Encode' (K1 iT fT) where
  encode' (K1 a) = encode a

instance Encode Int   where encode i = [BitsI i]
instance Encode Char  where encode c = [BitsC $ ord c]

encodeDefault  ::  (Representable0 aT repT, Encode' repT) 
               =>  repT xT -> aT -> [Bit]
encodeDefault rep x = encode' ((from0 x) `asTypeOf` rep)

{-# DERIVABLE Encode encode encodeDefault #-}

-- Representable0, parameterless
data D = D | E Int | F D D | G Char
  deriving Encode

-- Representable0, 1 type parameter
data A a = A a
  deriving Encode

main = do
  putStrLn (show (encode (F D (F (E 3) (G 'c')))))
  putStrLn (show (encode (A (3::Int))))

