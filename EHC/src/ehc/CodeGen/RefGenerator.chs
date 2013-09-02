%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generator of references
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}CodeGen.RefGenerator}
%%]

%%[(8 codegen) hs import({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]

%%[(8 codegen) hs import(Control.Monad, Control.Monad.State)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reference generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RefGenerator(refGen1),refGen)
type RefGenMonad ref = State Int ref

class RefGenerator ref where
  -- | Generate for 1 name
  refGen1M :: Int -> HsName -> RefGenMonad ref
  refGen1  :: Int -> Int -> HsName -> (ref, Int)
  
  -- defaults
  refGen1M dir nm = do 
    seed <- get
    let (r,seed') = refGen1 seed dir nm
    put seed'
    return r
  
  refGen1 seed dir nm = runState (refGen1M dir nm) seed

instance RefGenerator HsName where
  refGen1M _ = return

instance RefGenerator Int where
  refGen1 seed dir nm  = (seed, seed+dir)

instance RefGenerator Fld where
  refGen1 seed dir nm  = (Fld (Just nm) (Just seed), seed+dir)

-- | Generate for names, starting at a seed in a direction
refGenM :: RefGenerator ref => Int -> [HsName] -> RefGenMonad (AssocL HsName ref)
refGenM dir nmL
  = forM nmL $ \nm -> do
      r <- refGen1M dir nm
      return (nm,r)

refGen :: RefGenerator ref => Int -> Int -> [HsName] -> AssocL HsName ref
refGen seed dir nmL = evalState (refGenM dir nmL) seed
%%]

