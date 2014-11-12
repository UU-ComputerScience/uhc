%%[(8 core)

-- | Core Public API
--
-- Intended for constructing basic Core Programs.
-- This module does not offer any way to inspect the built Core Programs (on purpose), but the
-- EHXX.Core module does.
--
-- Invariants:
-- - Constructor applications (mkCon) always have to be fully saturated. (Should we handle this internally?)
-- - Haskell constructor names must be unambigous per module (mkHSCTag)
-- - TODO Tag ordering ?? What exactly are the invariants?
--
-- TODO Use AbstractCore instead of directly using the Constructors in the implementation

module %%@{%{EH}%%}Core.API
  (
  -- Opts
    EHCOpts
  , defaultEHCOpts
  -- * Core AST
  -- | The datatypes making up a Core program.
  , CModule
  , CImport
  , CDeclMeta
  , CDataCon
  , CExpr
  , CBind
  , CAlt

  -- Base.Common
  , CTag

  -- * Construction functions
  -- ** Constants
  , acoreUnit
  , acoreInt

  , acoreBuiltinInteger -- TODO acoreInt2 or acoreBuiltinInteger ?
  , acoreBuiltinString
  , acoreBuiltinError
  , acoreBuiltinUndefined

  -- ** Variables
  , acoreVar

  -- ** Let Bindings
  , acoreLetBase
  , acoreLet1Plain
  , acoreLet1Strict

  -- ** Case
  -- | Scrutinizes an expression and executes the appropriate alternative.
  -- The scrutinee of a case statement is required to be in WHNF (weak head normal form).
  , acoreCaseDflt
  
  -- ** Abstraction
  , acoreLam

  -- ** Application
  , acoreApp

  -- ** Datatypes
  , acoreTagTup

  -- * Utilities
  , makeMain
  )
  where

import qualified Data.Map as M

import %%@{%{EH}%%}AbstractCore
import %%@{%{EH}%%}Base.Common
import %%@{%{EH}%%}Base.HsName
import %%@{%{EH}%%}Core
import %%@{%{EH}%%}Opts


-- TODO how should we handle the type?
-- | Creates the unit expresssion.
acoreUnit :: EHCOpts -> CExpr
acoreUnit _ = acoreTup []

-- | Creates the main entry point, calling the given function when run. The created expression
-- should be used as-is inside the main module.
makeMain :: CExpr   -- ^ The expression to wrap inside the generated entry point.
    -> HsName       -- ^ The function containing the user code to call.
    -> CExpr
makeMain body main = mainEhc
  where mainEhc = acoreLet1Plain mainNm
            (acoreApp (acoreVar $ hsnMkModf ["UHC", "Run"] (hsnFromString "ehcRunMain") M.empty) [acoreVar main])
            (acoreVar mainNm)
        mainNm = hsnMkModf [] (hsnFromString "main") M.empty


%%]
