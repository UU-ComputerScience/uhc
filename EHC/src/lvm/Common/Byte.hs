{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Byte.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Common.Byte( Byte
           , Bytes  -- instance Show, Eq
           , nil, unit, cons, cat, cats, isNil
           , bytesLength
           , writeBytes
           , bytesFromList, listFromBytes

           , bytesFromString, stringFromBytes
           , bytesFromInt32
           , byteFromInt8

           , readByteList
           , int32FromByteList
           , stringFromByteList, bytesFromByteList
           ) where

import IO       ( IOMode(..) )
import Lvm.Common.Special  ( openBinary, writeBinaryChar, readBinary, closeBinary )
import Lvm.Common.Standard ( strict, foldlStrict )
import System   ( exitWith, ExitCode(..))

{----------------------------------------------------------------
  types
----------------------------------------------------------------}
type Byte   = Char

data Bytes  = Nil
            | Cons Byte   !Bytes    -- Byte is not strict since LvmWrite uses it lazily right now.
            | Cat  !Bytes !Bytes

instance Show Bytes where
  show bs     = show (listFromBytes bs)

instance Eq Bytes where
  bs1 == bs2  = (listFromBytes bs1) == (listFromBytes bs2)

{----------------------------------------------------------------
  conversion to bytes
----------------------------------------------------------------}
byteFromInt8 :: Int -> Byte
byteFromInt8 i
  = toEnum (mod i 256)
  
intFromByte :: Byte -> Int
intFromByte b
  = fromEnum b

bytesFromString :: String -> Bytes
bytesFromString s
  = bytesFromList s

stringFromBytes :: Bytes -> String
stringFromBytes bs
  = listFromBytes bs

bytesFromInt32 :: Int -> Bytes    -- 4 byte big-endian encoding
bytesFromInt32 i
  = let n0 = if (i < 0) then (max32+i+1) else i
        n1 = div n0 256
        n2 = div n1 256
        n3 = div n2 256
        xs = map byteFromInt8 [n3,n2,n1,n0]
    in bytesFromList xs

max32 :: Int
max32
  = 2^32-1


{----------------------------------------------------------------
  Byte lists
----------------------------------------------------------------}
isNil Nil         = True
isNil (Cons b bs) = False
isNil (Cat bs cs) = isNil bs && isNil cs

nil         = Nil
unit b      = Cons b Nil
cons b bs   = Cons b bs

cats bbs    = foldr Cat Nil bbs
cat bs cs   = case cs of
                Nil   -> bs
                other -> case bs of
                           Nil   -> cs
                           other -> Cat bs cs               

listFromBytes bs
  = loop [] bs
  where
    loop next bs
      = case bs of
          Nil       -> next
          Cons b bs -> b:loop next bs
          Cat bs cs -> loop (loop next cs) bs

bytesFromList bs
  = foldr Cons Nil bs


bytesLength :: Bytes -> Int
bytesLength bs
  = loop 0 bs
  where
    loop n bs
      = case bs of
          Nil       -> n
          Cons b bs -> strict loop (n+1) bs
          Cat bs cs -> loop (loop n cs) bs

writeBytes :: FilePath -> Bytes -> IO ()
writeBytes path bs
  = do{ h <- openBinary path WriteMode
      ; write h bs
      ; closeBinary h
      }
  where
    write h bs
      = case bs of
          Nil       -> return ()
          Cons b bs -> do{ writeBinaryChar h b; write h bs }
          Cat bs cs -> do{ write h bs; write h cs }


{----------------------------------------------------------------
  Byte lists
----------------------------------------------------------------}
int32FromByteList :: [Byte] -> (Int,[Byte])
int32FromByteList bs
  = case bs of
      (n3:n2:n1:n0:cs) -> let i = int32FromByte4 n3 n2 n1 n0 in seq i (i,cs)
      other            -> error "Byte.int32FromBytes: invalid byte stream"
                    
int32FromByte4 n0 n1 n2 n3
  = (intFromByte n0*16777216) + (intFromByte n1*65536) + (intFromByte n2*256) + intFromByte n3


stringFromByteList :: [Byte] -> String
stringFromByteList bs
  = bs

bytesFromByteList :: [Byte] -> Bytes
bytesFromByteList bs
  = bytesFromList bs

readByteList :: FilePath -> IO [Byte]
readByteList path 
  = do{ h <- openBinary path ReadMode
      ; xs <- readBinary h
      ; closeBinary h
      ; return xs
      } `catch` (\exception ->
            let message =  show exception ++ "\n\nUnable to read from file " ++ show path
            in do { putStrLn message; exitWith (ExitFailure 1) })
            