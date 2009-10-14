%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module UHC.Run
  ( ehcRunMain
  )
  where

import UHC.Base
import UHC.IOBase
import UHC.OldException
import UHC.OldIO

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
  catchTracedException m
    (\(t,e) -> case e of
                 ExitException ExitSuccess
                   -> exitWithIntCode 0
                 ExitException (ExitFailure code)
                     | code == 0 -> exitWithIntCode 1
                     | otherwise -> exitWithIntCode code
                 _ -> do { hPutStrLn stderr ("Error: " ++ show e)
                         ; if null t
                           then return ()
                           else do { hPutStrLn stderr "Trace:"
                                   ; mapM_ (\(k,s) -> hPutStrLn stderr ("  " ++ {- show k ++ ": " ++ -} s)) $ filter (primCallInfoKindIsVisible . fst) $ reverse t
                                   }
                         ; exitWithIntCode 1
                         }
    )

#endif

%%]
