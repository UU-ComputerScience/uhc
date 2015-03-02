%%[0 hs
-- {-# LANGUAGE GADTs, TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction for dealing with general functionaly required for specific AST/formats/file types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Abstraction for dealing with AST formats
%%]

%%[8 module {%{EH}EHC.ASTHandler}
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.CompileRun})
%%]

%%[8 import (Data.Typeable, GHC.Generics)
%%]

%%[8 import (qualified Data.Map as Map)
%%]

-- parsing
%%[8 import(qualified UHC.Util.ScanUtils as ScanUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTHandler(..))
data ASTHandler ast
  = ASTHandler
      {
      --- * Meta
      
      --- | Meta info: name of ast
        _asthdlrName				:: !String

      --- * File
      
      --- | Construct FPath from module name, path, suffix
      , _asthdlrMkFPath				:: EHCOpts -> HsName -> FPath -> String -> FPath

      --- | Suffix info (not used yet)
      , _asthdlrSuffixMp			:: Map.Map ASTFileVariation String
      
      --- * Compile unit
      
      --- | Update EHCompileUnit
      , _asthdlrEcuStore			:: EcuUpdater ast
      
      --- * Output
      
      --- | Write to an ast to a file in the IO monad, return True if could be done
      , _asthdlrOutputIO			:: ASTFileVariation -> EHCOpts -> EHCompileUnit -> HsName -> FPath -> FilePath -> ast -> IO Bool
      
      --- * Input, parsing
      
      --- | Scanning parameterisation
      , _asthdlrParseScanOpts 		:: EHCOpts -> ScanUtils.ScanOpts
      
      --- | Input an ast
      , _asthdlrInput				:: forall m . EHCCompileRunner m => ASTFileVariation -> HsName -> EHCompilePhaseT m (Maybe ast)

      }
%%]

%%[8 export(emptyASTHandler)
emptyASTHandler :: ASTHandler ast
emptyASTHandler
  = ASTHandler
      { _asthdlrName 				= "Unknown AST"
      
      , _asthdlrMkFPath				= mkOutputFPath
      , _asthdlrSuffixMp 			= Map.empty
      
      , _asthdlrEcuStore			= const id

      , _asthdlrOutputIO 			= \_ _ _ _ _ _ _ -> return False

      , _asthdlrInput 				= \_ _ -> return Nothing
      , _asthdlrParseScanOpts		= const ScanUtils.defaultScanOpts
      }
%%]
