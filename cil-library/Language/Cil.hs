-- |
-- A simple abstraction over the Common Intermediate Language (also known as
-- MSIL - Microsoft Intermediate Language).
-- Note that this library specific to the EHC .NET backend and isn't very
-- reusable (yet).
-- It only exposes a small subset of CIL.
--

module Language.Cil (
    module Language.Cil.Build
  , module Language.Cil.Pretty
  , module Language.Cil.Syntax
  ) where

import Language.Cil.Build
import Language.Cil.Pretty
import Language.Cil.Syntax

