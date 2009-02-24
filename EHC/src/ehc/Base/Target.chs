%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Base.Target}
%%]

%%[(8 codegen) import(qualified Data.Map as Map,Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Targets for code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Abstract naming convention of Target alternatives reflect what is done:

Target_<treatment>_<intermediate-lang>_<codegen-lang>

<treatment>
  : type of internal analysis done
    - FullProgAnal: full program analysis
    - Interpreter : enough for interpreting

<intermediate-lang>
  : the last intermediate language leading to final codegeneration
    - Grin
    - Core

<codegen-lang>
  : the language for which code is generated
    - C
    - LLVM
    - JVM
    - DOTNET
    - Jazy, Java lazy interpreter

Combinations are all hardcoded to make explicit that only particular combinations are allowed.
This may change later if/when combinations can be chosen independent/orthogonal.

%%[(8 codegen) export(Target(..))
data Target
  = Target_None								-- no codegen
  | Target_None_Core_None					-- only Core
%%[[(8 codegen jazy)
  | Target_Interpreter_Core_Jazy			-- java base on Core, using jazy library
%%]]
%%[[(8 codegen java)
  | Target_Interpreter_Core_Java			-- java base on Core, as src. Will be obsolete.
%%]]
%%[[(8 codegen grin)
  | Target_FullProgAnal_Grin_C				-- full program analysis on grin, generating C
  | Target_FullProgAnal_Grin_LLVM			-- full program analysis on grin, generating LLVM
  | Target_FullProgAnal_Grin_JVM			-- full program analysis on grin, generating for Java VM
  | Target_FullProgAnal_Grin_DOTNET			-- full program analysis on grin, generating for .net
  | Target_Interpreter_Grin_C				-- no full program analysis, grin interpreter, generating C
%%]]
  deriving (Eq,Ord)
%%]

Concrete naming convention.
Is derived from the abstract, attempting to keep each part of similar size (mostly 4 letters).

%%[(8 codegen)
instance Show Target where
  show Target_None							= "NONE"
  show Target_None_Core_None				= "core"
%%[[(8 codegen jazy)
  show Target_Interpreter_Core_Jazy			= "jazy"
%%]]
%%[[(8 codegen java)
  show Target_Interpreter_Core_Java			= "java"
%%]]
%%[[(8 codegen grin)
  show Target_FullProgAnal_Grin_C			= "C"
  show Target_FullProgAnal_Grin_LLVM		= "llvm"
  show Target_FullProgAnal_Grin_JVM			= "jvm"
  show Target_FullProgAnal_Grin_DOTNET		= "dotnet"
  show Target_Interpreter_Grin_C			= "bc"
%%]]
%%]

Default target

%%[(8 codegen) export(defaultTarget)
defaultTarget :: Target
%%[[(8 codegen grin)
defaultTarget = Target_Interpreter_Grin_C
%%][8
defaultTarget = Target_None
%%]]
%%]

Supported targets.

%%[(8 codegen) export(supportedTargetMp, showSupportedTargets', showSupportedTargets)
supportedTargetMp :: Map.Map String Target
supportedTargetMp
  = Map.fromList
      [ (show t, t)
      | t <- [ 
               Target_None_Core_None
%%[[(8 codegen jazy)
             , Target_Interpreter_Core_Jazy
%%]]
%%[[(8 codegen java)
             -- , Target_Interpreter_Core_Java
%%]]
%%[[(8 codegen grin)
             , Target_Interpreter_Grin_C
             , Target_FullProgAnal_Grin_C
%%]]
             ]
      ]

showSupportedTargets' :: String -> String
showSupportedTargets' sep
  = concat $ intersperse sep $ Map.keys supportedTargetMp

showSupportedTargets :: String
showSupportedTargets
  = showSupportedTargets' " "
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(targetIsFullProgAnal)
targetIsFullProgAnal :: Target -> Bool
targetIsFullProgAnal t
  = case t of
%%[[(8 codegen grin)
      Target_FullProgAnal_Grin_C 		-> True
      Target_FullProgAnal_Grin_LLVM 	-> True
      Target_FullProgAnal_Grin_JVM 		-> True
      Target_FullProgAnal_Grin_DOTNET 	-> True
%%]]
      _ 								-> False
%%]

%%[(8 codegen) export(targetIsGrin)
targetIsGrin :: Target -> Bool
targetIsGrin t
  = case t of
%%[[(8 codegen grin)
      _ 								-> True
%%][8
      _ 								-> False
%%]]
%%]

%%[(8 codegen) export(targetIsGrinBytecode)
targetIsGrinBytecode :: Target -> Bool
targetIsGrinBytecode t
  = case t of
%%[[(8 codegen grin)
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
%%]

%%[(8 codegen) export(targetIsC)
targetIsC :: Target -> Bool
targetIsC t
  = case t of
%%[[(8 codegen grin)
      Target_FullProgAnal_Grin_C 		-> True
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
%%]

%%[(8 codegen) export(targetAllowsOLinking)
targetAllowsOLinking :: Target -> Bool
targetAllowsOLinking t
  = case t of
%%[[(8 codegen grin)
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
%%]

%%[(8 codegen) export(targetIsCore)
targetIsCore :: Target -> Bool
targetIsCore t
  = case t of
      Target_None_Core_None				-> True
      _ 								-> False
%%]

%%[(8 codegen) export(targetIsJVM)
targetIsJVM :: Target -> Bool
targetIsJVM t
  = case t of
%%[[(8 codegen jazy)
      Target_Interpreter_Core_Jazy		-> True
%%]]
      _ 								-> False
%%]

%%[(8 codegen) export(targetIsLLVM)
targetIsLLVM :: Target -> Bool
targetIsLLVM t
  = case t of
%%[[(8 codegen grin)
      Target_FullProgAnal_Grin_LLVM 	-> True
%%]]
      _ 								-> False
%%]
