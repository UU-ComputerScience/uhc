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
              fpCl          = mkOutputFPath opts modNm fp "class"
       ; when (isJust mbCore && targetIsJVM (ehcOptTarget opts))
              (do { let core  = fromJust mbCore
                        cls   = cmod2JazyJVMModule core
                        bytes = jvmclass2binary cls
                  ; lift $ writeBinaryToFile (bytesToString bytes) fpCl
                  }
              )
       }                 
%%]

