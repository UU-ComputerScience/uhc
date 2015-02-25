%%[0 hs
{-# LANGUAGE GADTs, TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction for dealing with general functionaly required for specific AST/formats/file types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Abstraction for dealing with AST formats
%%]

%%[8 module {%{EH}EHC.SourceHandler}
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
%%% SourceType
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(SourceType(..))
-- | An 'Enum' listing all types of files we can deal with
data SourceType
  = SourceType_Hs
  | SourceType_LitHs
  | SourceType_Core
  | SourceType_CoreRun
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SourceHandler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(SourceHandler(..))
data SourceHandler ast
  = SourceHandler
      { _shParseFromText		:: forall m . EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m (Maybe ast)
      }
%%]

%%[8888 export(shParseFromText)
mkLabel ''SourceHandler
%%]

%%[8 export(emptySourceHandler)
emptySourceHandler :: SourceHandler ast
emptySourceHandler
  = SourceHandler
      { _shParseFromText = \_ _ -> return Nothing
      }
%%]
