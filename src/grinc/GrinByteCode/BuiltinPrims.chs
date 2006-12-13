%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin GBM primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{GRIN}GrinByteCode.BuiltinPrims}
%%]

%%[8 import({%{EH}Base.HsName},{%{EH}Base.Builtin},{%{GRIN}GrinByteCode})
%%]

%%[8 import(qualified Data.Map as Map,EH.Util.FastSeq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives with internal implementation expressed as bytecodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(GbPrim(..),gbPrims)
data GbPrim
  = GbPrim
      { gbprimNrArgs        :: Int
      , gbprimMk            :: OptimCtxt -> NmEnv -> Int -> StackDepth -> [GrValIntro] -> GrValIntroAlt
      }

gbPrims
  = Map.fromList
      [ ( "primAddInt", GbPrim 2 (mkOp InsOp_TyOp_Add InsOp_DataOp_IntWord) )
      , ( "primSubInt", GbPrim 2 (mkOp InsOp_TyOp_Sub InsOp_DataOp_IntWord) )
      , ( "primMulInt", GbPrim 2 (mkOp InsOp_TyOp_Mul InsOp_DataOp_IntWord) )
      , ( "primDivInt", GbPrim 2 (mkOp InsOp_TyOp_Div InsOp_DataOp_IntWord) )
      ]
  where mkOp opTy opndTy optim env modNmConstInx stkDepth [a1,a2]
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
%%]

