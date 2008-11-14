%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC common stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Used by all compiler driver code

%%[1 module {%{EH}EHC.Common}
%%]

-- general imports
%%[1 import(Data.List, Data.Char, Data.Maybe) export(module Data.Maybe, module Data.List, module Data.Char)
%%]
%%[1 import(Control.Monad.State, IO, System) export(module IO, module Control.Monad.State, module System)
%%]
%%[1 import(EH.Util.CompileRun, EH.Util.Pretty, EH.Util.FPath, EH.Util.Utils) export(module EH.Util.CompileRun, module EH.Util.Pretty, module EH.Util.FPath, module EH.Util.Utils)
%%]
%%[1 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Base.Opts}) export(module {%{EH}Base.Common}, module {%{EH}Base.Builtin}, module {%{EH}Base.Opts})
%%]
%%[1 import({%{EH}Error},{%{EH}Error.Pretty}) export(module {%{EH}Error},module {%{EH}Error.Pretty})
%%]

%%[8 import({%{EH}Gam}) export(module {%{EH}Gam})
%%]

%%[20 import(System.Time, System.Directory) export(module System.Time, module System.Directory)
%%]

%%[1
-- dummy, so module is not empty for initial variants, and exports will take effect
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The state HS compilation can be in

%%[8 export(HSState(..))
data HSState
  = HSStart
%%[[99
  | LHSStart
  | LHSOnlyImports
%%]]
  | HSAllSem
%%[[20
  | HSAllSemHI
  | HSOnlyImports
%%]]
  deriving (Show,Eq)
%%]

The state EH compilation can be in

%%[8 export(EHState(..))
data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)
%%]

The state any compilation can be in

%%[8 export(EHCompileUnitState(..))
data EHCompileUnitState
  = ECUSUnknown
  | ECUSHaskell !HSState
  | ECUSEh      !EHState
  | ECUSGrin
  | ECUSFail
  deriving (Show,Eq)
%%]

