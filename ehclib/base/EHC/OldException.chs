%%[doesWhat doclatex
Provide 'old' exception related stuff, for now, which is just the current one until the new one can be implemented.
The 'new' exception requires the combi of existentials + class predicates + datatype,
which is not yet supported by EHC.
%%]

%%[99
module EHC.OldException
  ( bracket, bracket_
  , throwIO
  )
  where

import EHC.Prelude
import EHC.IOBase

%%]

%%[99
-- | The 'bracket' function captures a common allocate, compute, deallocate
-- idiom in which the deallocation step must occur even in the case of an
-- error during computation. This is similar to try-catch-finally in Java.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket        :: forall a b c . IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- | A variant of 'bracket' where the middle computation doesn't want @x@.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket_@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket_        :: forall a b c . IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e
%%]

%%[99
throwIO  :: SomeException -> IO a
throwIO e = IO (\s -> throw e)
%%]
