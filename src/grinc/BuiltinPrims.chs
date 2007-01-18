%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation of primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The prims table describes alternate implementation, in particular how to inline.
Module {%{EH}BuiltinPrims} describes similar information required in the frontend.
Ideally, these tables should be merged.

%%[8 module {%{GRIN}BuiltinPrims}
%%]

%%[8 import({%{EH}Base.HsName},{%{EH}Base.Common},{%{EH}Base.Builtin},{%{GRIN}GrinByteCode})
%%]

%%[8 import(qualified Data.Map as Map, EH.Util.FastSeq, UU.Pretty)
%%]

%%[8 export(Backend(..), Primitive(..), lookupPrim)

-- Interface

data Primitive
  = GbPrim    
      { gbprimNrArgs        :: Int
      , gbprimMk            :: OptimCtxt -> NmEnv -> Int -> StackDepth -> [GrValIntro] -> GrValIntroAlt
      }
  | SillyPrim 
      { fromSillyPrim       :: [PP_Doc] -> PP_Doc
      }

lookupPrim :: Backend -> String -> Maybe Primitive
lookupPrim backend name =  Map.lookup name prims >>= Map.lookup backend


-- List of all primitives

prims :: Map.Map String (Map.Map Backend Primitive)
prims
  = Map.fromList
      [ 
        ( "primAddInt", Map.fromList[ (BackendSilly       	, SillyPrim (infixOperator "+")   		)
                                    , (BackendGrinByteCode	, GbPrim 2 (mkGbInsOp InsOp_TyOp_Add)	)
                                    ] )
      , ( "primSubInt", Map.fromList[ (BackendSilly			, SillyPrim (infixOperator "-")   		)
                                    , (BackendGrinByteCode	, GbPrim 2 (mkGbInsOp InsOp_TyOp_Sub)	)
                                    ] )
      , ( "primMulInt", Map.fromList[ (BackendSilly			, SillyPrim (infixOperator "*")   		)
                                    , (BackendGrinByteCode	, GbPrim 2 (mkGbInsOp InsOp_TyOp_Mul)	)
                                    ] )
      , ( "primDivInt", Map.fromList[ (BackendSilly			, SillyPrim (infixOperator "/")   		)
                                    , (BackendGrinByteCode	, GbPrim 2 (mkGbInsOp InsOp_TyOp_Div)	)
                                    ] )

      , ( "primGtInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator ">" )) ] )
      , ( "primLtInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "<" )) ] )
      , ( "primGeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator ">=")) ] )
      , ( "primLeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "<=")) ] )
      , ( "primEqInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "==")) ] )
      , ( "primNeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "!=")) ] )
      ]


-- Bytecode implementation

mkGbInsOp :: InsOp_TyOp -> OptimCtxt -> NmEnv -> Int -> StackDepth -> [GrValIntro] -> GrValIntroAlt
mkGbInsOp = mkGbOp InsOp_DataOp_IntWord      

mkGbOp :: InsOp_DataOp -> InsOp_TyOp -> OptimCtxt -> NmEnv -> Int -> StackDepth -> [GrValIntro] -> GrValIntroAlt
mkGbOp opndTy opTy optim env modNmConstInx stkDepth [a1,a2]
  = case gviLd' optim env modNmConstInx (stkDepth+inc1) a2 of
      GrValIntroAlt_OnTOS ins2 inc2 optimEffect
        -> GrValIntroAlt_OnTOS (ins1 :++: ins2 :++: oins) 1 optimEffect
        where oins = FSeqL [op opTy opndTy InsOp_LocODst_TOS InsOp_Deref_One InsOp_LocOSrc_TOS 0]
      GrValIntroAlt_Delay ins2 inc2 optimEffect _ (Load pins pinc ldsrc)
        -> GrValIntroAlt_OnTOS (ins1 :++: pins :++: oins) 1 optimEffect
        where oins = FSeqL [op opTy opndTy InsOp_LocODst_TOS deref src imm]
                   where (deref,src,imm)
                           = case ldsrc of
                               LoadSrc_TOS       -> (InsOp_Deref_One , InsOp_LocOSrc_TOS, 0)
                               LoadSrc_TOS_Rel o -> (InsOp_Deref_One , InsOp_LocOSrc_SP , toInteger $ nrWord2Byte o)
                               LoadSrc_Reg_Rel o -> (InsOp_Deref_One , InsOp_LocOSrc_Reg, toInteger $ nrWord2Byte o)
                               LoadSrc_Imm     c -> (InsOp_Deref_Zero, InsOp_LocOSrc_Imm, c)
                               LoadSrc_Imm_Int c -> (InsOp_Deref_Int , InsOp_LocOSrc_Imm, c)
  where g@(GrValIntroAlt_OnTOS ins1 inc1 _) = gviLd optim env modNmConstInx stkDepth a1
          
          

-- C implementation
          
infixOperator :: String -> [PP_Doc] -> PP_Doc
infixOperator op [arg1,arg2] = "((int)" >|< arg1 >|< ")" >#< op >#< "(int)(" >|< arg2 >|< ")"

compareOperator :: String -> [PP_Doc] -> PP_Doc
compareOperator op args = "((" >#< infixOperator op args >#< ")?" >#< "CTrue" >#< ":" >#< "CFalse" >#< ")"
--compareOperator op args@[arg1,arg2] = "((" >#< infixOperator op args >#< ")?" >#< "printf(\"%08x %08x groter\\n\", " >#< arg1 >#< "," >#< arg2 >#< "),((Pointer)global_True)[0]" >#< ":" >#< "printf(\"niet groter\\n\"),((Pointer)global_False)[0]" >#< ")"
%%]
