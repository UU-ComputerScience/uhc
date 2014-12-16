%%[(8 corerun)

-- | CoreRun Internal API
--
-- Intended for implementing stuff that has to know about internals (i.e. class instances)
--
-- See CoreRun.API
--

module %%@{%{EH}%%}CoreRun.API.Internal
  (
  -- * CoreRun AST
  -- | The datatypes making up a CoreRun program, with constructors
    Mod(..)
  , Exp(..)
  , SExp(..)
  , Alt(..)
  , Pat(..)

  -- * Utilities
  , crarrayFromList
  , crarrayToList
  
  -- * Re-export
  , module %%@{%{EH}%%}CoreRun.API
  )
  where

import %%@{%{EH}%%}CoreRun.API
import %%@{%{EH}%%}CoreRun as CR
import %%@{%{EH}%%}CoreRun.Run as CR


%%]
