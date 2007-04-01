%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation of primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The prims table describes special handling for some primitives, independent of the backend (for now).
Module {%{GRIN}BuiltinPrims} describes similar information required in the frontend.
Ideally, these tables should be merged.

%%[96 module {%{EH}BuiltinPrims}
%%]

%%[96 import({%{EH}Base.HsName},{%{EH}Base.Common},{%{EH}Base.Builtin})
%%]

%%[96 import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
data Primitive
  = Prim    
      { primArgNeedEval     :: ![Bool]  		-- default: True
      }

lookupPrim :: {- Backend -> -} String -> Maybe Primitive
lookupPrim {- backend -} name =  Map.lookup name prims {- >>= Map.lookup backend -}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96
prims :: Map.Map String ({- Map.Map Backend -} Primitive)
prims
  = Map.fromList
      [ 
        ( "primCatchException", Prim [False,False] )
      , ( "primThrowException", Prim [False] )
      ]
%%]
