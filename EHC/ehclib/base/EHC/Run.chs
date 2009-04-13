%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module EHC.Run
  ( ehcRunMain
  )
  where

import EHC.Prelude
import EHC.IOBase
import EHC.OldException
import EHC.OldIO

%%]

%%[99
#ifdef __EHC_FULL_PROGRAM_ANALYSIS__

-- Wrapper around 'main', invoked as 'ehcRunMain main'
ehcRunMain :: IO a -> IO a
ehcRunMain m = m

#else

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
                                   ; mapM_ (\(k,s) -> hPutStrLn stderr ("  " ++ show k ++ ": " ++ s)) $ reverse t
                                   }
                         ; exitWithIntCode 1
                         }
    )

#endif

%%]
