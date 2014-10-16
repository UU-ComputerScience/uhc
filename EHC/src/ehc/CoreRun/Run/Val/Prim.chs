%%[0 hs
-- {-# LANGUAGE MagicHash #-}
-- {-# OPTIONS_GHC -O2 #-}
%%]

%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CoreRun Val impl: primitive bindings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val.Prim}
%%]

%%[(8 corerun) hs import({%{EH}CoreRun.Prim}, {%{EH}CoreRun.Run.Val}, {%{EH}CoreRun.Run}, {%{EH}CoreRun})
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.RWS.Strict, Control.Monad.Error)
%%]

%%[(8 corerun) hs import(qualified Data.Vector as V, qualified Data.Vector.Mutable as MV)
%%]

%%[(8888 corerun) hs import(GHC.Exts)
%%]

%%[(8 corerun) hs import(Data.IORef)
%%]

%%[(8888 corerun) hs import(Data.Primitive.MutVar)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Impl of primitives for Val runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(rvalPrim)
-- | Apply primitive to arguments
rvalPrim :: (RunSem RValCxt RValEnv m RVal) => RunPrim -> CRArray RVal -> RValT m RVal
rvalPrim pr as = do 
    case (pr, V.toList as) of
      -- Int arithmetic
      (RP_primAddInt, [RVal_Int i1, RVal_Int i2]) -> return $ RVal_Int $ i1 + i2
      (RP_primSubInt, [RVal_Int i1, RVal_Int i2]) -> return $ RVal_Int $ i1 - i2
      (RP_primMulInt, [RVal_Int i1, RVal_Int i2]) -> return $ RVal_Int $ i1 * i2
      (RP_primDivInt, [RVal_Int i1, RVal_Int i2]) -> return $ RVal_Int $ i1 `div` i2
      (RP_primEqInt, [RVal_Int i1, RVal_Int i2]) -> return $ mkBool $ i1 == i2
      
      -- Exception handling
      (RP_primCatchException, [x, hdl]) -> rsemEvl x -- err $ "Not impl: RP_primCatchException" -- TBD
      
      -- MutVar
      (RP_primNewMutVar, [x, s]) -> liftIO $ newIORef x >>= \mv -> return $ mkTuple [s, RVal_MutVar mv]
      (RP_primReadMutVar, [RVal_MutVar mv, s]) -> liftIO $ readIORef mv >>= \v -> return $ mkTuple [s, v]
      (RP_primWriteMutVar, [RVal_MutVar mv, v, s]) -> liftIO $ writeIORef mv v >> return s
      (RP_primSameMutVar, _) -> err $ "Not impl: RP_primSameMutVar" -- TBD
      
      -- Base
      (RP_primPackedStringToInteger, [RVal_PackedString x]) -> return $ RVal_Integer $ read x
%%]


  | RP_primNewMutVar
  | RP_primReadMutVar
  | RP_primWriteMutVar
  | RP_primSameMutVar
