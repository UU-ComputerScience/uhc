-- |
-- A simple abstraction over the Common Intermediate Language (also known as
-- MSIL - Microsoft Intermediate Language).
-- Note that this library specific to the EHC .NET backend and isn't very
-- reusable (yet).
-- It only exposes a small subset of CIL.
--

module Language.Cil (
    module Language.Cil.Analysis
  , module Language.Cil.Build
  , module Language.Cil.Parser
  , module Language.Cil.Pretty
  , module Language.Cil.Syntax
  , scanAssembly
  , parseAssembly
  ) where

import Control.Monad (liftM)
import UU.Scanner.Token (Token)

import Language.Cil.Analysis
import Language.Cil.Build
import Language.Cil.Parser
import Language.Cil.Pretty
import Language.Cil.Scanner
import Language.Cil.Syntax

scanAssembly :: FilePath -> IO [Token]
scanAssembly path = do
  src <- readFile path
  return $ scan path src

parseAssembly :: FilePath -> IO Assembly
parseAssembly path = liftM (parse pAss) (scanAssembly path)

