%%[(8 corerun)

-- | CoreRun Public API
--
-- Intended for constructing basic CoreRun Programs.
--
-- CoreRun is a simplified Core intended to be used for direct interpretation/execution.
-- For semantics, see TBD
--

module %%@{%{EH}%%}CoreRun.API
  (
  -- * CoreRun AST
  -- | The datatypes making up a CoreRun program.
    Mod
  , Exp, MbExp
  , SExp
  , Alt
  -- , Pat
  , Bind
  , RRef
  , Import
  , Meta
  , DataCon

  -- * Utilities
  , CRArray
  
  -- * Construction functions
  
  -- ** References
  , mkLocLevRef
  , mkLocDifRef
  , mkGlobRef
  , mkImpRef
  , mkExpRef
  , mkModRef
  
  -- ** Expressions
  , mkExp
  
  , mkVar, mkVar'
  , mkInt, mkInt'
  , mkChar, mkChar'
%%[[97
  , mkInteger, mkInteger'
%%]]
  , mkString, mkString'
  , mkDbg, mkDbg'
  
  , mkApp, mkApp'
  , mkTup, mkTup'
  , mkEval
  , mkTail
  , mkCase
  , mkLam, mkLam'
  , mkLet, mkLet'
  , mkFFI, mkFFI'
  
  -- ** Meta
  , mkMetaDataCon, mkMetaDataType
  
  -- ** Modules
  , mkMod, mkMod', mkModWithMetas, mkModWithImportsMetas
  , mkImport
  
  -- * Conversion
  , rrefToDif
  
  -- * Parsing
  , parseModFromString

  -- * Running
  , runCoreRunIO
  
  -- * Misc utils
  , printModule
  )
  where

import %%@{%{EH}%%}Base.API
import %%@{%{EH}%%}CoreRun as CR
import %%@{%{EH}%%}CoreRun.Pretty as CR
import %%@{%{EH}%%}CoreRun.Parser as CR
import %%@{%{EH}%%}CoreRun.Run as CR (runCoreRun, strMsg)
import %%@{%{EH}%%}CoreRun.Run.Val as CR
import %%@{%{EH}%%}CoreRun.Run.Val.RunExplStk as CR

import System.IO
import Control.Exception

import UHC.Util.Pretty


-- **************************************
-- Running
-- **************************************

-- | Run CoreRun in IO
-- TBD: fix dependence on whole program linked
runCoreRunIO
  :: EHCOpts		-- ^ options, e.g. for turning on tracing (if supported by runner)
     -> Mod			-- ^ the module to run
     -> IO (Either Err RVal)
runCoreRunIO opts mod = do
    catch
      (runCoreRun opts [] mod $ cmodRun opts mod)
      (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "runCoreRunIO: " ++ show e))

-- **************************************
-- Utilities (i.e. other stuff)
-- **************************************

-- | Pretty print 'Mod'
printModule :: EHCOpts -> Mod -> PP_Doc
printModule = ppMod'

%%]
