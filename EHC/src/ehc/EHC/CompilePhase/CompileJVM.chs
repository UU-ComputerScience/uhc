%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

JVM compilation

%%[(8 codegen java) module {%{EH}EHC.CompilePhase.CompileJVM}
%%]

-- general imports
%%[(8 codegen java) import({%{EH}EHC.Common})
%%]
%%[(8 codegen java) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen java) import({%{EH}EHC.CompileRun})
%%]

%%[(8 codegen java) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 codegen java) import({%{EH}EHC.Environment})
%%]
%%[(8 codegen java) import({%{EH}Base.Target})
%%]

%%[(8 codegen jazy) import({%{EH}Core.ToJazy})
%%]
%%[(8 codegen java) import({%{EH}Base.Binary},{%{EH}JVMClass.ToBinary})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JVM compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen java jazy) export(cpCompileJazyJVM)
cpCompileJazyJVM :: HsName -> EHCompilePhase ()
cpCompileJazyJVM modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbCore          = ecuMbCore ecu
%%[[8
              fpCl c          = mkOutputFPath opts c (fpathSetBase (show c) fp) "class"		-- TBD: correct names
%%][20
              fpCl c          = mkOutputFPath opts c (maybe mFp (\d -> fpathPrependDir d mFp) (fpathMbDir fp)) "class"
                              where mFp = mkFPath c
%%]]
       ; when (isJust mbCore && targetIsJVM (ehcOptTarget opts))
              (do { let core  = fromJust mbCore
                        clss  = concatMap jvmclass2binary $ cmod2JazyJVMModule opts core
                  ; when (ehcOptVerbosity opts >= VerboseDebug)
                         (lift $ putStrLn (show modNm ++ " JVM classes: " ++ show (map fst clss)))
                  ; lift $
                    mapM_ (\(m,b) -> do { let fpModCl = fpCl m
                                        ; fpathEnsureExists fpModCl
                                        ; writeBinaryToFile (bytesToString b) fpModCl
                                        })
                          clss
                  }
              )
       }                 
%%]

