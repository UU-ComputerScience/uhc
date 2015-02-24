%%[0 hs
{-# LANGUAGE GADTs #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Running of BuildFunction
%%]

%%[8 module {%{EH}EHC.BuildFunction.Run}
%%]

-- build function
%%[8 import ({%{EH}EHC.BuildFunction})
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileRun}, {%{EH}EHC.CompileUnit})
%%]

%%[8 import (UHC.Util.Lens)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(runBFun')
runBFun' :: EHCCompileRunner m => BFun' res -> EHCompilePhaseT m res
runBFun' bfun = do
    start

    res <- case bfun of
      -- The actual work
      EcuOf modNm -> do
           return undefined
           
      FPathSearchForFile suff fn -> do
           let fp    = mkTopLevelFPath suff fn
               modNm = mkHNm $ fpathBase fp
           return (fp, modNm)
      
      _ -> return undefined

    end
    return res
  where
    start = crStateInfo ^* crsiBState ^* bstateCallStack =$: (BFun bfun :)
    end = crStateInfo ^* crsiBState ^* bstateCallStack =$: tail

%%]

      {-
      -- Applicative part
      Pure res -> return res
      App  f a -> do
          f' <- runBFun' f
          a' <- runBFun' a
          return $ f' a'
      -}
      
