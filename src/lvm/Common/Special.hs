{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Special.hs 233 2006-01-04 10:03:27Z BastiaanHeeren $

{---------------------------------------------------------------
  Special definitions for the GHC system
---------------------------------------------------------------}
module Lvm.Common.Special( doesFileExist
              , openBinary, closeBinary, readBinary, writeBinaryChar
              , ST, STArray, runST, newSTArray, readSTArray, writeSTArray
              , unsafeCoerce, unsafePerformIO
              ) where

import Directory  ( doesFileExist )
import IO         ( Handle, hGetContents, hClose, hPutChar, IOMode(..) )
import System.IO  ( openBinaryFile )
import System.IO.Unsafe ( unsafePerformIO )
import GHC.Base         ( unsafeCoerce# )
import GHC.Arr          ( STArray, newSTArray, readSTArray, writeSTArray)
import Control.Monad.ST ( runST, ST ) 
               
unsafeCoerce :: a -> b
unsafeCoerce x
  = unsafeCoerce# x

openBinary :: FilePath -> IOMode -> IO Handle
openBinary
  = openBinaryFile

closeBinary :: Handle -> IO ()
closeBinary h
  = hClose h

readBinary :: Handle -> IO String
readBinary h
  = do{ xs <- hGetContents h
      ; seq (last xs) $ hClose h
      ; return xs
      }

writeBinaryChar :: Handle -> Char -> IO ()
writeBinaryChar h c
  = hPutChar h c
