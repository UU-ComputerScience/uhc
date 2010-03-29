%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Base.Target}
%%]

%%[(8 codegen) import(qualified Data.Map as Map,Data.List)
%%]
%%[(8 codegen) import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[(20 codegen) import({%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[doesWhat doclatex
Abstract naming convention of Target alternatives reflect what is done:

\paragraph{Target}
Target_<treatment>_<intermediate-lang>_<codegen-lang>

\begin{itemize}
\item
\textbf{treatment}
  : type of internal analysis done
    \begin{itemize}
    \item FullProgAnal: full program analysis
    \item Interpreter : enough for interpreting
    \end{itemize}

\item
\textbf{intermediate-lang}
  : the last intermediate language leading to final codegeneration
    \begin{itemize}
    \item Grin
    \item Core
    \end{itemize}

\item
\textbf{codegen-lang}
  : the language for which code is generated
    \begin{itemize}
    \item C
    \item LLVM
    \item JVM
    \item CLR
    \item Jazy, Java lazy interpreter
    \end{itemize}
\end{itemize}

Combinations are all hardcoded to make explicit that only particular combinations are allowed.
This may change later if/when combinations can be chosen independent/orthogonal.

\paragraph{TargetFlavor}
Flavors of target are incompatible, that is cannot be used interchangedly.
The code is specific for a particular meta purpose, such as profiling and debugging.
Currently there are target flavors for:

\begin{itemize}
\item
\textbf{plain}
  : base flavor

\item
\textbf{debug}
  : includes debugging info, currently:
    \begin{itemize}
    \item stack trace
    \end{itemize}

\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Targets for code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(Target(..))
data Target
  = Target_None								-- no codegen
  | Target_None_Core_None					-- only Core
  | Target_None_TyCore_None					-- only TyCore
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
  | Target_Interpreter_Grin_C				-- no full program analysis, grin interpreter, generating C
%%]]
%%[[(8 codegen clr)
  | Target_FullProgAnal_Grin_CLR			-- full program analysis on grin, generating for Common Language Runtime (.NET / Mono)
%%]]
  deriving (Eq,Ord)
%%]

Concrete naming convention.
Is derived from the abstract, attempting to keep each part of similar size (mostly 4 letters).

%%[(8 codegen)
instance Show Target where
  show Target_None							= "NONE"
  show Target_None_Core_None				= "core"
  show Target_None_TyCore_None				= "tycore"
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
  show Target_Interpreter_Grin_C			= "bc"
%%]]
%%[[(8 codegen clr)
  show Target_FullProgAnal_Grin_CLR			= "clr"
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
(supportedTargetMp,allTargetInfoMp)
  = (Map.fromList ts, Map.fromList is)
  where (ts,is) = unzip
          [ ((show t, t),(t,i))
          | (t,i)
              <- []
                 -- ++ [ mk Target_None_Core_None [] ]
%%[[(8 codegen jazy)
                 ++ [ mk Target_Interpreter_Core_Jazy [FFIWay_Jazy] ]
%%]]
%%[[(8 codegen java)
                 -- ++ [ mk Target_Interpreter_Core_Java [] ]
%%]]
%%[[(8 codegen grin)
                 ++ [ mk Target_Interpreter_Grin_C [FFIWay_CCall]
                    , mk Target_FullProgAnal_Grin_C [FFIWay_CCall]
                    ]
%%]]
%%[[(8 codegen llvm)
                 ++ [ mk Target_FullProgAnal_Grin_LLVM [FFIWay_CCall] ]
%%]]
%%[[(8 codegen clr)
                 ++ [ mk Target_FullProgAnal_Grin_CLR [FFIWay_CCall] ]
%%]]
          ]
        mk t ffis = (t,TargetInfo (FFIWay_Prim : ffis)) 

showSupportedTargets' :: String -> String
showSupportedTargets'
  = showStringMapKeys supportedTargetMp

showSupportedTargets :: String
showSupportedTargets
  = showSupportedTargets' " "
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Target flavors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(TargetFlavor(..))
data TargetFlavor
  = TargetFlavor_Plain						-- no special stuff
  | TargetFlavor_Debug						-- debugging variant
  -- more: profiling, ....
  deriving (Eq,Ord,Enum)
%%]

%%[(8 codegen) export(defaultTargetFlavor)
defaultTargetFlavor :: TargetFlavor
defaultTargetFlavor = TargetFlavor_Plain
%%]

%%[(8 codegen)
instance Show TargetFlavor where
  show TargetFlavor_Plain				= "plain"
  show TargetFlavor_Debug				= "debug"
%%]

Supported target variants.

%%[(8 codegen) export(allTargetFlavorMp,showAllTargetFlavors',showAllTargetFlavors)
allTargetFlavorMp :: Map.Map String TargetFlavor
allTargetFlavorMp
  = Map.fromList ts
  where ts
          = [ (show t, t)
            | t <-
                  [ TargetFlavor_Plain
                  , TargetFlavor_Debug
                  ]
            ]

showAllTargetFlavors' :: String -> String
showAllTargetFlavors'
  = showStringMapKeys allTargetFlavorMp

showAllTargetFlavors :: String
showAllTargetFlavors
  = showAllTargetFlavors' " "
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
%%]]
%%[[(8 codegen clr)
      Target_FullProgAnal_Grin_CLR	 	-> True
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

%%[(8 codegen) export(targetAllowsJarLinking)
targetAllowsJarLinking :: Target -> Bool
targetAllowsJarLinking t
  = case t of
%%[[(8 jazy)
      Target_Interpreter_Core_Jazy		-> True
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

%%[(8 codegen) export(targetIsTyCore)
targetIsTyCore :: Target -> Bool
targetIsTyCore t
  = case t of
      Target_None_TyCore_None			-> True
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

%%[(8 codegen) export(targetIsCLR)
targetIsCLR :: Target -> Bool
targetIsCLR t
  = case t of
%%[[(8 codegen clr)
      Target_FullProgAnal_Grin_CLR	 	-> True
%%]]
      _ 								-> False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Possible FFI interface routes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FFIWay(..))
data FFIWay
  = FFIWay_Prim				-- as primitive
  | FFIWay_CCall			-- as C call
  | FFIWay_Jazy				-- as Java/Jazy
  | FFIWay_CLR				-- as CLR, just a placeholder
  deriving (Eq,Ord,Enum)

instance Show FFIWay where
  show FFIWay_Prim	= "prim"
  show FFIWay_CCall	= "ccall"
  show FFIWay_Jazy	= "jazy"
  show FFIWay_CLR	= "dotnet"

instance PP FFIWay where
  pp = pp . show
%%]

The default FFIWay for FFIWay_Prim for a target

%%[(8 codegen) export(ffiWayForPrim)
ffiWayForPrim :: Target -> Maybe FFIWay
%%[[(8 codegen jazy)
ffiWayForPrim Target_Interpreter_Core_Jazy			= Just FFIWay_Jazy
%%]]
%%[[(8 codegen clr)
ffiWayForPrim Target_FullProgAnal_Grin_CLR			= Just FFIWay_CLR
%%]]
%%[[(8 codegen llvm)
ffiWayForPrim Target_FullProgAnal_Grin_LLVM			= Just FFIWay_CCall
%%]]
ffiWayForPrim t | targetIsC t						= Just FFIWay_CCall
                | otherwise							= Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional info about targets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(TargetInfo(..),TargInfoMp)
data TargetInfo
  = TargetInfo
      { targiAllowedFFI		:: [FFIWay]
      }

type TargInfoMp = Map.Map Target TargetInfo
%%]

%%[(8 codegen) export(allTargetInfoMp,allFFIWays)
allTargetInfoMp :: TargInfoMp

allFFIWays :: [FFIWay]
allFFIWays = nub $ concatMap targiAllowedFFI $ Map.elems allTargetInfoMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
deriving instance Typeable FFIWay
deriving instance Data FFIWay

deriving instance Typeable TargetFlavor
deriving instance Data TargetFlavor
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
instance Binary FFIWay where
  put = putEnum8
  get = getEnum8

instance Serialize FFIWay where
  sput = sputPlain
  sget = sgetPlain

instance Binary TargetFlavor where
  put = putEnum8
  get = getEnum8

instance Serialize TargetFlavor where
  sput = sputPlain
  sget = sgetPlain

%%]


