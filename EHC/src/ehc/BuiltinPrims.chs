%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation of primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The prims table describes special handling for some primitives, independent of the backend (for now).
Module {%{EH}BuiltinPrims} describes similar information required in the frontend.
Ideally, these tables should be merged.

%%[(8 codegen grin).moduleHeader module {%{EH}BuiltinPrims}
%%]

%%[(8 codegen grin) import({%{EH}Base.HsName},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Base.BasicAnnot},{%{EH}Base.Builtin},{%{EH}GrinByteCode})
%%]

%%[(8 codegen grin) import(qualified Data.Map as Map, qualified EH.Util.FastSeq as Seq, EH.Util.Pretty, EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(96 codegen grin) export(PrimitiveNeedsEval(..),lookupPrimNeedsEval)
data PrimitiveNeedsEval
  = Prim    
      { primArgNeedEval     :: ![Bool]  		-- default: True
      , primResNeedEval     :: ! Bool   		-- default: False
      }

lookupPrimNeedsEval :: {- Backend -> -} String -> Maybe PrimitiveNeedsEval
lookupPrimNeedsEval {- backend -} name =  Map.lookup name primsNeedsEval {- >>= Map.lookup backend -}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(96 codegen grin)
primsNeedsEval :: Map.Map String ({- Map.Map Backend -} PrimitiveNeedsEval)
primsNeedsEval
  = Map.fromList
      [ 
        ( "primCatchException"	, Prim [False,False]		False )
      , ( "primThrowException"	, Prim [False] 				False )
%%[[99
      , ( "primNewArray"		, Prim [True,False] 		False )
      , ( "primIndexArray"		, Prim [True,True] 			True  )
      , ( "primWriteArray"		, Prim [True,True,False] 	False )
      -- , ( "primReadMutVar"		, Prim [True,True]	 		True  )
%%]]
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation of primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The prims table describes alternate implementation, in particular how to inline.
Module {%{Base}Builtin} describes similar information required in the frontend.
Ideally, these tables should be merged.

%%[(8 codegen grin) export(Backend(..), Primitive(..), lookupPrim)

-- Interface

data Primitive
  = GbPrim    
      { gbprimNrArgs        :: !Int
      , gbprimMk            :: EHCOpts -> OptimCtxt -> NmEnv -> StackState -> GBState -> [GrValIntro] -> (GrValIntroAlt, GBState)
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
%%[[8
      , ( "primDivInt"		-- for now, to not break test examples using primDivInt
%%][97
      , ( "primQuotInt"
%%]]
                      , Map.fromList[ (BackendSilly			, SillyPrim (infixOperator "/")   		)
                                    , (BackendGrinByteCode	, GbPrim 2 (mkGbInsOp InsOp_TyOp_Quot)	)
                                    ] )

      , ( "primUnsafeId", Map.fromList[ (BackendGrinByteCode, GbPrim 1 (\opts o env d st [a] -> gviLd opts o env d st a)	) ] )

      , ( "primGtInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator ">" )) ] )
      , ( "primLtInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "<" )) ] )
      , ( "primGeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator ">=")) ] )
      , ( "primLeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "<=")) ] )
      , ( "primEqInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "==")) ] )
      , ( "primNeInt",  Map.fromList[ (BackendSilly,SillyPrim (compareOperator "!=")) ] )
      ]


-- Bytecode implementation

mkGbInsOp :: InsOp_TyOp -> EHCOpts -> OptimCtxt -> NmEnv -> StackState -> GBState -> [GrValIntro] -> (GrValIntroAlt, GBState)
mkGbInsOp = mkGbOp InsOp_DataOp_IntWord      

mkGbOp :: InsOp_DataOp -> InsOp_TyOp -> EHCOpts -> OptimCtxt -> NmEnv -> StackState -> GBState -> [GrValIntro] -> (GrValIntroAlt, GBState)
mkGbOp opndTy opTy opts optim env stState gbState [a1,a2]
  = case gviLd' opts optim env (stState `ststInc` inc1) gbState2 a2 of
      (GrValIntroAlt_OnTOS ins2 inc2 optimEffect _, gbState3)
        -> (GrValIntroAlt_OnTOS (ins1 Seq.:++: ins2 Seq.:++: oins) one optimEffect [grinBasicAnnotSize BasicAnnot_Dflt], gbState3)
        where oins = Seq.fromList [op opTy opndTy InsOp_LocODst_TOS InsOp_Deref_One InsOp_LocOSrc_TOS 0]
      (GrValIntroAlt_Delay ins2 inc2 optimEffect (_,Load pins pinc ldsrc postins,_,_), gbState3)
        -> (GrValIntroAlt_OnTOS (ins1 Seq.:++: pins Seq.:++: oins Seq.:++: postins) one optimEffect [grinBasicAnnotSize BasicAnnot_Dflt], gbState3)
        where oins = Seq.fromList [op opTy opndTy InsOp_LocODst_TOS deref src imm]
                   where (deref,src,imm)
                           = case ldsrc of
                               LoadSrc_TOS            -> (InsOp_Deref_One , InsOp_LocOSrc_TOS, 0)
                               LoadSrc_TOS_Rel o 1    -> (InsOp_Deref_One , InsOp_LocOSrc_SP , toInteger $ nrWord2Byte o)
                               LoadSrc_Reg_Rel o 1    -> (InsOp_Deref_One , InsOp_LocOSrc_Reg, toInteger $ nrWord2Byte o)
                               LoadSrc_Imm     c      -> (InsOp_Deref_Zero, InsOp_LocOSrc_Imm, c)
                               LoadSrc_Imm_Int c      -> (InsOp_Deref_Int , InsOp_LocOSrc_Imm, c)
                               _                      -> panic "BuiltinPrims.mkGbOp"
  where (g@(GrValIntroAlt_OnTOS ins1 inc1 _ _), gbState2) = gviLd opts optim env stState gbState a1
        one = ststFromDep 1
          
          

-- C implementation
          
infixOperator :: String -> [PP_Doc] -> PP_Doc
infixOperator op [arg1,arg2] = "((int)" >|< arg1 >|< ")" >#< op >#< "(int)(" >|< arg2 >|< ")"

compareOperator :: String -> [PP_Doc] -> PP_Doc
compareOperator op args = "((" >#< infixOperator op args >#< ")?" >#< "RTS_True" >#< ":" >#< "RTS_False" >#< ")"
--compareOperator op args@[arg1,arg2] = "((" >#< infixOperator op args >#< ")?" >#< "printf(\"%08x %08x groter\\n\", " >#< arg1 >#< "," >#< arg2 >#< "),((Pointer)global_True)[0]" >#< ":" >#< "printf(\"niet groter\\n\"),((Pointer)global_False)[0]" >#< ")"
%%]
