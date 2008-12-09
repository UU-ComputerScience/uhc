{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Id.hs 228 2005-11-07 11:48:22Z uust $

module Lvm.Common.Id ( Id -- instance Eq, Show

          -- essential used in "asm" and "lvm"
          , stringFromId, idFromString, idFromStringEx
          , dummyId

          -- exotic: only used in the core compiler
          , freshIdFromId, getNameSpace, setNameSpace

          , NameSupply, newNameSupply
          , splitNameSupply, splitNameSupplies, freshId
          , mapWithSupply

          -- very exotic: only used internally for IdMap's that use IntMap
          , intFromId, idFromInt
          ) where

import Lvm.Common.Standard  (foldlStrict)
import qualified Lvm.Common.IntMap as IntMap

import Data.IORef( IORef, newIORef, readIORef, writeIORef )
import Lvm.Common.Special ( unsafePerformIO )

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
newtype Id        = Id Int

intFromId (Id i)  = i
idFromInt i       = Id i


----------------------------------------------------------------
-- Names: the symbol table
----------------------------------------------------------------
data Names        = Names Int (IntMap.IntMap [String])

namesRef :: IORef Names
namesRef
  = unsafePerformIO (newIORef emptyNames)

emptyNames :: Names
emptyNames
  = Names 0 (IntMap.empty)

idFromString :: String -> Id
idFromString s
  = idFromStringEx 0 s


idFromStringEx :: Enum a => a -> String -> Id
idFromStringEx ns name
  = unsafePerformIO $
    do{ names <- readIORef namesRef
      ; let (id,names') = insertName (fromEnum ns) name names
      ; writeIORef namesRef names'
      ; return id
      }

stringFromId :: Id -> String
stringFromId id@(Id i)
  | isUniq i  = "." ++ show (extractUniq i)
  | otherwise = unsafePerformIO $
                do{ names <- readIORef namesRef
                  ; case lookupId id names of
                      Nothing   -> error "Id.nameFromId: unknown id"
                      Just name -> return name
                  }


----------------------------------------------------------------
-- fresh identifiers without a nice name
-- but the advantage of a pure interface
----------------------------------------------------------------
newtype NameSupply   = NameSupply (IORef Int)

newNameSupply :: IO NameSupply
newNameSupply
  = do{ ref <- newIORef 0
      ; return (NameSupply ref)
      }

splitNameSupply :: NameSupply -> (NameSupply,NameSupply)
splitNameSupply supply
  = (supply,supply)

splitNameSupplies supply
  = repeat supply

freshIdFromId :: Id -> NameSupply -> (Id,NameSupply)
freshIdFromId id supply@(NameSupply ref)
  = unsafePerformIO (do{ i <- readIORef ref
                       ; writeIORef ref (i+1)
                       ; let name = stringFromId id ++ "." ++ show i
                             id'  = idFromString name
                             id'' = setNameSpace (getNameSpace id :: Int) id'
                       ; seq name $ seq id'' $ return (id'',supply)
                       })

freshId :: NameSupply -> (Id,NameSupply)
freshId supply@(NameSupply ref)
  = unsafePerformIO (do{ i <- readIORef ref
                       ; writeIORef ref (i+1)
                       ; return (Id (initUniq i), supply)
                       })

mapWithSupply :: (NameSupply -> a -> b) -> NameSupply -> [a] -> [b]
mapWithSupply f supply xs
  = zipWith f (splitNameSupplies supply) xs


----------------------------------------------------------------
-- Bit masks used within an Id
--
-- 0x | 0 0 0 0 | 0 0 0 0 |
--    |         |     F E |  sort (=namespace) of the id  (TODO: just 128 entries is too few)
--    |       F | F F     |  hash index in the hash table
--    | 7 F F   |         |  index in the list of id's in the leaves of the hash table
--    |         |       1 |  unique id (no name available)
--    | 7 F F F | F F     |  unique number of unique id
----------------------------------------------------------------
dummyId :: Id
dummyId           = Id (0x7FFFFFF1)

shiftSort, maxSort :: Int
shiftSort         = 0x00000002
maxSort           = 0x7F

maxHash, shiftHash :: Int
maxHash           = 0xFFF
shiftHash         = 0x00000100

shiftIdx, maxIdx :: Int
shiftIdx          = 0x00100000
maxIdx            = 0x7FF

shiftUniq,maxUniq,flagUniq :: Int
shiftUniq         = 0x00000100
maxUniq           = 0x007FFFFF
flagUniq          = 0x00000001

extractBits, clearBits, initBits :: Int -> Int -> Int -> Int
extractBits shift max i
  = (i `div` shift) `mod` (max+1)

clearBits shift max i
  = i - (extractBits shift max i * shift)

initBits shift v i
  = i + (shift * v)

extractSort = extractBits shiftSort maxSort
clearSort   = clearBits shiftSort maxSort
initSort    = initBits shiftSort

extractHash = extractBits shiftHash maxHash
initHash h  = initBits shiftHash h 0

extractIdx  = extractBits shiftIdx maxIdx
initIdx     = initBits shiftIdx

extractUniq = extractBits shiftUniq maxUniq
initUniq u  = initBits shiftUniq u 1

isUniq i    = odd i

----------------------------------------------------------------
-- The core of the symbol table: lookupId and insertName
----------------------------------------------------------------
instance Eq Id where
  (Id i1) == (Id i2)  = (i1 == i2)

instance Ord Id where
  -- i1      <= i2       = stringFromId i2 <= stringFromId i2
  compare (Id i1) (Id i2) = compare i1 i2


instance Show Id where
  show id   = "\"" ++ stringFromId id ++ "\""


getNameSpace :: Enum a => Id -> a
getNameSpace (Id i)
  = toEnum (extractSort i)

setNameSpace :: Enum a => a -> Id -> Id
setNameSpace sort (Id i)
  | s > maxSort   = error "Id.setIdSort: sort index out of range"
  | otherwise     = Id (initSort s (clearSort i))
  where
    s    = fromEnum sort


lookupId :: Id -> Names -> Maybe String
lookupId (Id i) (Names fresh map)
  = let idx = extractIdx i
        h   = extractHash i
    in  case IntMap.lookupM map h of
          Nothing -> Nothing
          Just xs -> Just (index idx xs)
  where
    index 0   (x:xx) = x
    index idx (x:xx) = index (idx-1) xx
    index idx []     = error "Id.lookupId: corrupted symbol table"


insertName sort name names
  = let (id,names') = insertName' name names
    in (setNameSpace sort id, names')

insertName' :: String -> Names -> (Id,Names)
insertName' name (Names fresh map)
  = let hname       = hash name
        h           = initHash hname
        (i,map1)    = IntMap.insertWithX insert h hname [name] map
        insert _ xs = let (idx,xs') = insertIdx name xs
                      in if (idx > maxIdx)
                          then error ("Id.insertName: too many names with the same hash value (" ++ show name ++ ")")
                          else (initIdx idx h, xs')
    in (Id i, Names fresh map1)


-- [insertIdx] returns the index of an element if it exists already, or
-- appends the element and returns its index.
insertIdx :: Eq a => a -> [a] -> (Int,[a])
insertIdx y xs
  = walk 0 xs
  where
    walk idx []         = (idx,[y])
    walk idx xs@(x:xx)  | x == y    = (idx,xs)
                        | otherwise = let (idx',xx') = walk (idx+1) xx
                                      in  (idx',x:xx')


----------------------------------------------------------------
-- Hashing
----------------------------------------------------------------
hash :: String -> Int
hash name
  = (hashx name `mod` prime) `mod` maxHash
  where
    prime = 32537 --65599   -- require: prime < maxHash


-- simple hash function that performs quite good in practice
hashx :: String -> Int
hashx name
  = foldlStrict gobble 0 name
  where
    gobble n c    = n*65599 + fromEnum c




-----------------------------------------------------------------------------
{-
import Bits
import Word

-- the [hashpjw] algorithm, see dragon book, section 7.6, page 436.
hashpjw :: String -> Int
hashpjw name
  = word32ToInt (foldlStrict gobble (intToWord32 0) name)
  where
    gobble n c    = let h     = (shiftL n 4) + intToWord32 (fromEnum c)
                        high  = shiftR h 24
                        g     = shiftL high 24
                    in if (g /= 0)
                        then xor (xor h high) g
                        else h
-}

{-
primes xs   = case xs of
                [] ->  []
                (x:xx) -> x:primes (filter ((/=0).(`mod`x)) xx)

-}

{-
,32537,32561,32563,32569,32573,32579,32587,32603,32609,32611,32621,32633,32647,3
2653,32687,32693,32707,32713,32717,32719,32749,32771,32779,32783,32789,32797,328
01,32803,32831,32833,32839,32843,32869,32887,32909,32911,32917,32933,32939,32941
,32957,32969,32971,32983,32987,32993,32999,33013,33023,33029,33037,33049,33053,3
3071,33073,33083,33091,33107,33113,33119,33149,33151,33161,33179,33181,33191,331
99,33203,33211,33223,33247,33287,33289,33301,33311,33317,33329,33331,33343,33347
,33349,33353,33359,33377,33391,33403,33409,33413,33427,33457,33461,33469,33479,3
3487,33493,33503,33521,33529,33533,33547,33563,33569,33577,33581,33587,33589,335
99,33601,33613,33617,33619,33623,33629,33637,33641,33647,33679,33703,33713,33721

,50891,50893,50909,50923,50929,50951,50957,50969,50971,50989,50993,51001,51031,5
1043,51047,51059,51061,51071,51109,51131,51133,51137,51151,51157,51169,51193,511
97,51199,51203,51217,51229,51239,51241,51257,51263,51283,51287,51307,51329,51341
,51343,51347,51349,51361,51383,51407,51413,51419,51421,51427,51431,51437,51439,5
1449,51461,51473,51479,51481,51487,51503,51511,51517,51521,51539,51551,51563,515
77,51581,51593,51599,51607,51613,51631,51637,51647,51659,51673,51679,51683,51691
,51713,51719,51721,51749,51767,51769,51787,51797,51803,51817,51827,51829,51839,5
1853,51859,51869,51871,51893,51899,51907,51913,51929,51941,51949,51971,51973,519
77,51991,52009,52021,52027
-}
