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
%%[8 import ({%{EH}EHC.CompileRun}, {%{EH}EHC.CompileUnit})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(runBFun)
runBFun :: EHCCompileRunner m => BFun res -> EHCompilePhaseT m res
runBFun bfun = do
    case bfun of
      -- Applicative part
      Pure res -> return res
      App  f a -> do
          f' <- runBFun f
          a' <- runBFun a
          return $ f' a'
      
      -- The actual work
      EcuOf modNm -> do
           return undefined
      _ -> return undefined
%%]

%%[8888 export(runBFunSq)
runBFunSq :: BFunSq res -> EHCompilePhase res
runBFunSq bfunsq = do
    case bfunsq of
      Pure res -> return res
      Lift bfn -> runBFun bfn
      App  f a -> do
          f' <- runBFunSq f
          a' <- runBFunSq a
          return $ f' a'
      _ -> return undefined
%%]

