{-# LANGUAGE NoImplicitPrelude #-}

module UHC.OldException
  ( bracket, bracket_

  , throwIO

  , throw

  , catchAny

  , block, unblock

  , assert

  , onException
  )
  where

import UHC.Base
import UHC.IOBase


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

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catchException

throwIO  :: SomeException -> IO a
throwIO e = IO (\s -> throw e)

block   :: IO a -> IO a
block    = id

unblock :: IO a -> IO a
unblock  = id

assert :: Bool -> a -> a
assert True  x = x
assert False _ = error "Assertion failed"

-- Performs the final action if there was an exception raised by the computation.
onException :: IO a -> IO b -> IO a
onException io what = catchAny io (\e -> do what
                                            throwIO e)


