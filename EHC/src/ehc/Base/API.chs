%%[1

-- | Base Public API (provisional, to be refactored)
--
module %%@{%{EH}%%}Base.API
  (
  -- * Opts
  -- | Options to the compiler.
    EHCOpts
  , defaultEHCOpts

  -- * Constructor Tags
  -- From Base.Common
  , CTag

  -- * Names
  , HsName

  -- * Misc
%%[[99
  , hsnEhcRunMain
%%]]
%%[[8
  , hsnMain
%%]]
  )
  where

import %%@{%{EH}%%}Base.Common
import %%@{%{EH}%%}Base.HsName
import %%@{%{EH}%%}Base.HsName.Builtin
import %%@{%{EH}%%}Opts

%%]
