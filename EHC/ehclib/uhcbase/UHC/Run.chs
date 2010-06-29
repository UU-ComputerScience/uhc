%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
{-# LANGUAGE NoImplicitPrelude, CPP #-}

module UHC.Run
  ( ehcRunMain
  )
  where

import UHC.Base
import UHC.IOBase
import UHC.OldException
import UHC.Handle
import UHC.StackTrace

#ifndef __UHC_TARGET_C__
import System.IO (hPutStrLn)
#endif
%%]

%%[99
#ifdef __UHC_TARGET_C__

-- Wrapper around 'main', invoked as 'ehcRunMain main'
ehcRunMain :: IO a -> IO a
ehcRunMain m = m

#else

foreign import prim primCallInfoKindIsVisible :: Int -> Bool

-- Wrapper around 'main', invoked as 'ehcRunMain main'
ehcRunMain :: IO a -> IO a
ehcRunMain m =
  catchTracedException (wrapCleanUp m)
    (\(exc,implTrace,explTrace) -> cleanUp >>
          case exc of
            ExitException ExitSuccess
              -> exitWithIntCode 0
            ExitException (ExitFailure code)
                | code == 0 -> exitWithIntCode 1
                | otherwise -> exitWithIntCode code
            _ -> do { hPutStrLn stderr ("Error: " ++ show exc)
                    ; if null explTrace
                      then if null implTrace
                           then return ()
                           else do { hPutStrLn stderr "Trace:"
                                   ; mapM_ (\(k,s) -> hPutStrLn stderr ("  " ++ {- show k ++ ": " ++ -} s)) $ filter (primCallInfoKindIsVisible . fst) $ reverse implTrace
                                   }
                      else do { hPutStrLn stderr "Explicit stack trace:"
                              ; mapM_ (\s -> hPutStrLn stderr s) explTrace
                              }
                    ; exitWithIntCode 1
                    }
    )

#endif

-- try to flush stdout/stderr, but don't worry if we fail
-- (these handles might have errors, and we don't want to go into
-- an infinite loop).
cleanUp :: IO ()
cleanUp = do
  hFlush stdout `catchAny` \_ -> return ()
  hFlush stderr `catchAny` \_ -> return ()

wrapCleanUp :: IO a -> IO a
wrapCleanUp m = do x <- m
                   cleanUp
                   return x
%%]
