%%[0 hs
{-# LANGUAGE GADTs, TemplateHaskell #-}
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

%%[8888 import (Control.Applicative)
%%]

%%[8888 import (Data.Functor.Identity) export(module Data.Functor.Identity)
%%]

%%[8888 import (qualified Data.Map as Map, qualified Data.IntMap as IMap, Data.Maybe)
%%]

%%[8888 import (UHC.Util.Hashable)
%%]

%%[8 import (UHC.Util.Lens)
%%]

%%[8888 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[8 import (Data.Typeable, GHC.Generics)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTType
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTType(..))
-- | An 'Enum' listing all types of ast we can deal with
data ASTType
  = ASTType_Hs
  | ASTType_LitHs
  | ASTType_Core
  | ASTType_CoreRun
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTHandler(..))
data ASTHandler ast
  = ASTHandler
      { _asthdlrParseFromText		:: forall m . EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m (Maybe ast)
      }
%%]

%%[8888 export(asthdlrParseFromText)
mkLabel ''ASTHandler
%%]

%%[8 export(emptyASTHandler)
emptyASTHandler :: ASTHandler ast
emptyASTHandler
  = ASTHandler
      { _asthdlrParseFromText = \_ _ -> return Nothing
      }
%%]
