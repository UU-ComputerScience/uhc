%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core infrastructure: builtin primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Prim}
%%]

%%[(8888 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8888 corerun) hs import({%{EH}CoreRun})
%%]

%%[(8 corerun) hs import(qualified Data.Map as Map)
%%]

%%[(8888 corerun) hs import(Data.Maybe, Data.Monoid)
%%]

%%[(8888 corerun) hs import(Control.Monad, Control.Monad.Error)
%%]

%%[(8888 corerun) hs import(Control.Monad.RWS.Strict)
%%]
%%[(8888 corerun) hs import(Control.Monad.State.Strict)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Enumeration of all primitives which should be taken care of by an implementation of running CoreRun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RunPrim(..))
-- | Primitives.
-- Assumption: name of primitive starts with 3 choosable char + exact name of prim
data RunPrim
  = 
    -- Int arithmetic
    RP_primAddInt
  | RP_primSubInt
  | RP_primMulInt
  | RP_primDivInt
  | RP_primEqInt
  
    -- UHC.IOBase: Exception handling
  | RP_primCatchException
  
    -- UHC.MutVar
  | RP_primNewMutVar
  | RP_primReadMutVar
  | RP_primWriteMutVar
  | RP_primSameMutVar
  
    -- UHC.Base
  | RP_primPackedStringToInteger

  deriving (Show, Eq, Ord, Enum, Bounded)
%%]

%%[(8 corerun) hs export(showRunPrim)
-- | Show prim without initial 3 chars
showRunPrim :: RunPrim -> String
showRunPrim p = drop 3 $ show p
%%]

%%[(8 corerun) hs export(allRunPrimMp)
allRunPrimMp :: Map.Map String RunPrim
allRunPrimMp = Map.fromList [ (showRunPrim p, p) | p <- [ minBound .. maxBound ] ]
%%]

