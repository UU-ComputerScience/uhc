%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin C primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{GRIN}Silly.BuiltinPrims}
%%]

%%[8 import(qualified Data.Map as Map, UU.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives with internal implementation expressed in C
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(SillyPrim(..),sillyPrims)
data SillyPrim
  = SillyPrim
      { sillyprimNrArgs        :: Int
      , sillyprimOp            :: [PP_Doc] -> PP_Doc
      }


infixOperator :: String -> [PP_Doc] -> PP_Doc
infixOperator op [arg1,arg2] = arg1  >#< op >#< arg2

compareOperator :: String -> [PP_Doc] -> PP_Doc
compareOperator op args = "(" >#< infixOperator op args >#< "?" >#< "CTrue" >#< ":" >#< "CFalse" >#< ")"

sillyPrims
  = Map.fromList
      [ ( "primAddInt", SillyPrim 2 (infixOperator "+") )
      , ( "primSubInt", SillyPrim 2 (infixOperator "-") )
      , ( "primMulInt", SillyPrim 2 (infixOperator "*") )
      , ( "primDivInt", SillyPrim 2 (infixOperator "/") )
      , ( "primModInt", SillyPrim 2 (infixOperator "%") )
      
      , ( "primGtInt",  SillyPrim 2 (compareOperator ">")  )
      , ( "primLtInt",  SillyPrim 2 (compareOperator ">")  )
      , ( "primGeInt",  SillyPrim 2 (compareOperator ">=") )
      , ( "primLeInt",  SillyPrim 2 (compareOperator "<=") )
      , ( "primEqInt",  SillyPrim 2 (compareOperator "==") )
      , ( "primNeInt",  SillyPrim 2 (compareOperator "!=") )
      
      ]

%%]

