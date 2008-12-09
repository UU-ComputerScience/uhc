{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Standard.hs 228 2005-11-07 11:48:22Z uust $

module Lvm.Common.Standard( trace, warning, assert
               , strict, seqList
               , foldlStrict, foldrStrict
               , Force, force
               , searchPath, searchPathMaybe, getLvmPath, splitPath
               , fst3, snd3, thd3
               , unsafeCoerce
               , raiseIO, raiseErr
               ) where

import List       (isPrefixOf)
import IO
import System     (getEnv)
import Lvm.Common.Special    (doesFileExist,unsafeCoerce,unsafePerformIO)

----------------------------------------------------------------
-- three tuples
----------------------------------------------------------------
fst3 (a,b,c)  = a
snd3 (a,b,c)  = b
thd3 (a,b,c)  = c


----------------------------------------------------------------
-- errors
----------------------------------------------------------------
raiseErr :: String -> a
raiseErr msg
  = error msg

raiseIO :: String -> IO a
raiseIO msg
  = ioError (userError msg)

----------------------------------------------------------------
-- debugging
----------------------------------------------------------------
trace s x     = seq (unsafePerformIO (hPutStrLn stderr s)) x
warning s x   = trace ("\nwarning: " ++ s) x

assert p msg x
  = if (p) then x else warning msg x

----------------------------------------------------------------
-- strictness
----------------------------------------------------------------
strict :: (a -> b) -> a -> b
strict f x    = f $! x

foldlStrict :: (b -> a -> b) -> b -> [a] -> b
foldlStrict f y xs
--  foldl f y xs
  = walk y xs
  where
    walk y []     = y
    walk y (x:xs) = strict walk (f y x) xs

foldrStrict :: (a -> b -> b) -> b -> [a] -> b
foldrStrict f y xs
  = walk xs
  where
    walk []     = y
    walk (x:xs) = f x $! (walk xs)

seqList :: [a] -> b -> b
seqList [] b     = b
seqList (x:xs) b = seqList xs b

class Force a where
  force :: a -> b -> b
  force a b   = seq a b

instance Force Bool
instance Force Char
instance Force Int
instance Force Integer
instance Force Double

instance Force a => Force [a] where
  force [] b      = b
  force (x:xs) b  = force x $ force xs b

instance (Force a,Force b) => Force (a,b) where
  force (x,y) b   = force x $ force y $ b


----------------------------------------------------------------
-- file searching
----------------------------------------------------------------
searchPath :: [String] -> String -> String -> IO String
searchPath path ext name = 
    fmap (maybe (fail ("could not find " ++ show nameext)) id)
         (searchPathMaybe path ext name)
  where
    nameext
      | isPrefixOf (reverse ext) (reverse name)  = name
      | otherwise  = name ++ ext
        
searchPathMaybe :: [String] -> String -> String -> IO (Maybe String)
searchPathMaybe  path ext name
  = walk (map makeFName path) -- was ("":path), but we don't want to look in the current directory by default
  where
    walk []         = return Nothing
    walk (fname:xs) = do{ exist <- doesFileExist fname
                        ; if exist
                           then return (Just fname)
                           else walk xs
                        }

    makeFName dir
      | null dir          = nameext
      | last dir == '/' ||
        last dir == '\\'  = dir ++ nameext
      | otherwise         = dir ++ "/" ++ nameext

    nameext
      | isPrefixOf (reverse ext) (reverse name)  = name
      | otherwise  = name ++ ext

getLvmPath :: IO [String]
getLvmPath
  = do{ xs <- getEnv "LVMPATH"
      ; return (splitPath xs)
      }
  `catch` \err -> return []

splitPath :: String -> [String]
splitPath xs
  = walk [] "" xs
  where
    walk ps p xs
      = case xs of
          []             -> if (null p)
                             then reverse ps
                             else reverse (reverse p:ps)
          (';':cs)       -> walk (reverse p:ps) "" cs
          (':':'\\':cs)  -> walk ps ("\\:" ++ p) cs
          (':':cs)       -> walk (reverse p:ps) "" cs
          (c:cs)         -> walk ps (c:p) cs
