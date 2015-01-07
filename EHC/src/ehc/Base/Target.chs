%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Target}
%%]

%%[8 import(qualified Data.Map as Map,Data.List)
%%]
%%[8 import(UHC.Util.Pretty,UHC.Util.Utils)
%%]
%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
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

%%[8 export(Target(..))
-- | All possible targets, even though they may not be configured (done in supportedTargetMp)
data Target
  = Target_None								-- no codegen
%%[[(8 codegen)
  | Target_None_Core_AsIs					-- only Core
  | Target_None_TyCore_None					-- only TyCore

  -- jazy
  | Target_Interpreter_Core_Jazy			-- java based on Core, using jazy library

  -- javascript
  | Target_Interpreter_Core_JavaScript		-- javascript based on Core
  | Target_Interpreter_GrinCmm_JavaScript	-- javascript based on Grin -> Cmm
%%[[(8888 codegen java)
  -- not used at all
  | Target_Interpreter_Core_Java			-- java based on Core, as src. Will be obsolete.
%%]]

  -- grin, wholeprogC
  | Target_FullProgAnal_Grin_C				-- full program analysis on grin, generating C

  -- grin, llvm, wholeprogC
  | Target_FullProgAnal_Grin_LLVM			-- full program analysis on grin, generating LLVM

  -- grin, jvm, wholeprogC
  | Target_FullProgAnal_Grin_JVM			-- full program analysis on grin, generating for Java VM

  -- grin
  | Target_Interpreter_Grin_C				-- no full program analysis, grin interpreter, generating C

  -- grin, clr, wholeprogC
  | Target_FullProgAnal_Grin_CLR			-- full program analysis on grin, generating for Common Language Runtime (.NET / Mono)
%%]]
  deriving ( Eq, Ord, Enum )
%%]

Concrete naming convention.
Is derived from the abstract, attempting to keep each part of similar size (mostly 4 letters).

%%[8
instance Show Target where
  show Target_None							= "NONE"
%%[[(8 codegen)
  show Target_None_Core_AsIs				= "cr"
  show Target_None_TyCore_None				= "tycore"
  show Target_Interpreter_Core_Jazy			= "jazy"
  show Target_Interpreter_Core_JavaScript	= "js"
  show Target_Interpreter_GrinCmm_JavaScript= "cmmjs"
%%[[(8888 java)
  show Target_Interpreter_Core_Java			= "java"
%%]]
  show Target_FullProgAnal_Grin_C			= "C"
  show Target_FullProgAnal_Grin_LLVM		= "llvm"
  show Target_FullProgAnal_Grin_JVM			= "jvm"
  show Target_Interpreter_Grin_C			= "bc"
  show Target_FullProgAnal_Grin_CLR			= "clr"
%%]]
%%]

Default target

%%[8 export(defaultTarget)
defaultTarget :: Target
%%[[(8 codegen grin)
defaultTarget = Target_Interpreter_Grin_C
%%][(103 corerun)
defaultTarget = Target_None_Core_AsIs
%%][8
defaultTarget = Target_None
%%]]
%%]

Supported targets.

%%[8 export(supportedTargetMp, showSupportedTargets', showSupportedTargets)
supportedTargetMp :: Map.Map String Target
(supportedTargetMp,allTargetInfoMp)
  = (Map.fromList ts, Map.fromList is)
  where (ts,is) = unzip
          [ ((show t, t),(t,i))
          | (t,i)
              <- [ mk Target_None [] ]
%%[[(8 corebackend)
                 ++ [ mk Target_None_Core_AsIs [] ]
%%]]
%%[[(8 jazy)
                 ++ [ mk Target_Interpreter_Core_Jazy [FFIWay_Jazy] ]
%%]]
%%[[(8 javascript)
                 ++ [ mk Target_Interpreter_Core_JavaScript [FFIWay_JavaScript] ]
%%]]
%%[[(8 cmm javascript)
                 -- ++ [ mk Target_Interpreter_GrinCmm_JavaScript [FFIWay_JavaScript] ]
%%]]
%%[[(8888 java)
                 -- ++ [ mk Target_Interpreter_Core_Java [] ]
%%]]
%%[[(8 grin)
                 ++ [ mk Target_Interpreter_Grin_C [FFIWay_CCall]
                    ]
%%]]
%%[[(8 grin wholeprogC)
                 ++ [ mk Target_FullProgAnal_Grin_C [FFIWay_CCall]
                    ]
%%]]
%%[[(8 llvm wholeprogC)
                 ++ [ mk Target_FullProgAnal_Grin_LLVM [FFIWay_CCall] ]
%%]]
%%[[(8 clr wholeprogC)
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

%%[8 export(TargetFlavor(..))
data TargetFlavor
  = TargetFlavor_Plain						-- no special stuff
  | TargetFlavor_Debug						-- debugging variant
  -- more: profiling, ....
  deriving (Eq,Ord,Enum)
%%]

%%[8 export(defaultTargetFlavor)
defaultTargetFlavor :: TargetFlavor
defaultTargetFlavor = TargetFlavor_Plain
%%]

%%[8
instance Show TargetFlavor where
  show TargetFlavor_Plain				= "plain"
  show TargetFlavor_Debug				= "debug"
%%]

Supported target variants.

%%[8 export(allTargetFlavorMp,showAllTargetFlavors',showAllTargetFlavors)
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

%%[(8 codegen) export(targetDoesHPTAnalysis)
targetDoesHPTAnalysis :: Target -> Bool
targetDoesHPTAnalysis t
  = case t of
%%[[(8 grin wholeprogC)
      Target_FullProgAnal_Grin_C 		-> True
      Target_FullProgAnal_Grin_JVM 		-> True
%%]]
%%[[(8 llvm wholeprogC)
      Target_FullProgAnal_Grin_LLVM 	-> True
%%]]
%%[[(8 clr wholeprogC)
      Target_FullProgAnal_Grin_CLR	 	-> True
%%]]
      _ 								-> False
%%]

%%[(8888 codegen) export(targetIsViaCmm)
targetIsViaCmm :: Target -> Bool
targetIsViaCmm t
  = case t of
%%[[(8 cmm)
      _ 								-> targetIsViaGrinCmmJavaScript t
%%][8
      _ 								-> False
%%]]
{-# INLINE targetIsViaCmm #-}
%%]

%%[(8888 codegen) export(targetIsViaGrin)
targetIsViaGrin :: Target -> Bool
targetIsViaGrin t
  = case t of
%%[[(8 grin)
      _ 								-> targetIsViaGrinCmmJavaScript t || targetIsGrinBytecode t || targetDoesHPTAnalysis t
%%][8
      _ 								-> False
%%]]
{-# INLINE targetIsViaGrin #-}
%%]

%%[(8 codegen) export(targetIsGrinBytecode)
targetIsGrinBytecode :: Target -> Bool
targetIsGrinBytecode t
  = case t of
%%[[(8 grin)
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
{-# INLINE targetIsGrinBytecode #-}
%%]

%%[(8 codegen) export(targetAllowsGrinNodePtrMix)
targetAllowsGrinNodePtrMix :: Target -> Bool
targetAllowsGrinNodePtrMix t
  = case t of
%%[[(8 grin)
      _		 							-> targetIsGrinBytecode t
%%][8
      _ 								-> False
%%]]
{-# INLINE targetAllowsGrinNodePtrMix #-}
%%]

%%[(8 codegen) export(targetIsC)
targetIsC :: Target -> Bool
targetIsC t
  = case t of
%%[[(8 grin)
%%[[(8 wholeprogAnal)
      Target_FullProgAnal_Grin_C 		-> True
%%]]
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
{-# INLINE targetIsC #-}
%%]

%%[(8 codegen) export(targetAllowsOLinking)
targetAllowsOLinking :: Target -> Bool
targetAllowsOLinking t
  = case t of
%%[[(8 grin)
      Target_Interpreter_Grin_C		 	-> True
%%]]
      _ 								-> False
{-# INLINE targetAllowsOLinking #-}
%%]

%%[(8 codegen) export(targetAllowsJarLinking)
targetAllowsJarLinking :: Target -> Bool
targetAllowsJarLinking t
  = case t of
%%[[(8 jazy)
      Target_Interpreter_Core_Jazy		-> True
%%]]
      _ 								-> False
{-# INLINE targetAllowsJarLinking #-}
%%]

%%[(8 codegen) export(targetIsCore)
targetIsCore :: Target -> Bool
targetIsCore t
  = case t of
      Target_None_Core_AsIs				-> True
      _ 								-> False
{-# INLINE targetIsCore #-}
%%]

%%[(8 codegen) export(targetIsTyCore)
targetIsTyCore :: Target -> Bool
targetIsTyCore t
  = case t of
      Target_None_TyCore_None			-> True
      _ 								-> False
{-# INLINE targetIsTyCore #-}
%%]

%%[(8 codegen) export(targetIsJVM)
targetIsJVM :: Target -> Bool
targetIsJVM t
  = case t of
%%[[(8 codegen jazy)
      Target_Interpreter_Core_Jazy		-> True
%%]]
      _ 								-> False
{-# INLINE targetIsJVM #-}
%%]

%%[(8 codegen) export(targetIsViaGrinCmmJavaScript)
targetIsViaGrinCmmJavaScript :: Target -> Bool
targetIsViaGrinCmmJavaScript t
  = case t of
%%[[(8 cmm javascript)
      Target_Interpreter_GrinCmm_JavaScript	-> True
%%]]
      _ 									-> False
{-# INLINE targetIsViaGrinCmmJavaScript #-}
%%]

%%[(8 codegen) export(targetIsViaCoreJavaScript)
targetIsViaCoreJavaScript :: Target -> Bool
targetIsViaCoreJavaScript t
  = case t of
%%[[(8 javascript)
      Target_Interpreter_Core_JavaScript	-> True
%%]]
      _ 									-> False
{-# INLINE targetIsViaCoreJavaScript #-}
%%]

%%[(8 codegen) export(targetIsJavaScript)
targetIsJavaScript :: Target -> Bool
targetIsJavaScript t
  = case t of
%%[[(8 javascript)
      _ 									-> targetIsViaCoreJavaScript t || targetIsViaGrinCmmJavaScript t
%%][8
      _ 									-> False
%%]]
{-# INLINE targetIsJavaScript #-}
%%]

%%[(8 codegen grin llvm wholeprogAnal wholeprogC) export(targetIsLLVM)
targetIsLLVM :: Target -> Bool
targetIsLLVM t
  = case t of
%%[[(8 codegen grin)
      Target_FullProgAnal_Grin_LLVM 	-> True
%%]]
      _ 								-> False
{-# INLINE targetIsLLVM #-}
%%]

%%[(8 codegen grin clr wholeprogC) export(targetIsCLR)
targetIsCLR :: Target -> Bool
targetIsCLR t
  = case t of
%%[[(8 codegen clr)
      Target_FullProgAnal_Grin_CLR	 	-> True
%%]]
      _ 								-> False
{-# INLINE targetIsCLR #-}
%%]

%%[(8 codegen) export(targetIsOnUnixAndOrC)
-- | target runs on (possibly emulated) UNIX / C environment? this should coincide with flag EHC_CFG_USE_UNIX_AND_C in src/ehc/variant.mk
targetIsOnUnixAndOrC :: Target -> Bool
targetIsOnUnixAndOrC t
  = targetIsC t || targetIsJVM t
{-# INLINE targetIsOnUnixAndOrC #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Possible FFI interface routes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FFIWay(..))
data FFIWay
  = FFIWay_Prim				-- as primitive
  | FFIWay_CCall			-- as C call
  | FFIWay_Jazy				-- as Java/Jazy
%%[[(8 codegen javascript)
  | FFIWay_JavaScript		-- JavaScript
%%]]
%%[[(8 codegen clr grin wholeprogC)
  | FFIWay_CLR				-- as CLR, just a placeholder
%%]]
  deriving (Eq,Ord,Enum)

instance Show FFIWay where
  show FFIWay_Prim			= "prim"
  show FFIWay_CCall			= "ccall"
  show FFIWay_Jazy			= "jazy"
%%[[(8 codegen javascript)
  show FFIWay_JavaScript	= "js"
%%]]
%%[[(8 codegen clr grin wholeprogC)
  show FFIWay_CLR			= "dotnet"
%%]]

instance PP FFIWay where
  pp = pp . show
%%]

The default FFIWay for FFIWay_Prim for a target

%%[8 export(ffiWayForPrim)
ffiWayForPrim :: Target -> Maybe FFIWay
%%[[(8 codegen jazy)
ffiWayForPrim Target_Interpreter_Core_Jazy			= Just FFIWay_Jazy
%%]]
%%[[(8 codegen javascript)
ffiWayForPrim Target_Interpreter_Core_JavaScript	= Just FFIWay_JavaScript
%%]]
%%[[(8 codegen cmm javascript)
ffiWayForPrim Target_Interpreter_GrinCmm_JavaScript	= Just FFIWay_JavaScript
%%]]
%%[[(8 codegen grin clr wholeprogC)
ffiWayForPrim Target_FullProgAnal_Grin_CLR			= Just FFIWay_CLR
%%]]
%%[[(8 codegen grin llvm wholeprogC)
ffiWayForPrim Target_FullProgAnal_Grin_LLVM			= Just FFIWay_CCall
%%]]
ffiWayForPrim t
%%[[(8 codegen)
  | targetIsC t			              				= Just FFIWay_CCall
%%]]
  | otherwise			              				= Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional info about targets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(TargetInfo(..),TargInfoMp)
data TargetInfo
  = TargetInfo
      { targiAllowedFFI		:: [FFIWay]
      }

type TargInfoMp = Map.Map Target TargetInfo
%%]

%%[8 export(allTargetInfoMp,allFFIWays)
allTargetInfoMp :: TargInfoMp

-- | All allowed platform dependent ways to do a FFI call, a primitive 'FFIWay_Prim' is always allowed even though there might be no backend for it.
-- This allows code still to compile when no target/backend is available.
allFFIWays :: [FFIWay]
allFFIWays = nub $ (FFIWay_Prim :) $ concatMap targiAllowedFFI $ Map.elems allTargetInfoMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Typeable Target
deriving instance Data Target

deriving instance Typeable FFIWay
deriving instance Data FFIWay

deriving instance Typeable TargetFlavor
deriving instance Data TargetFlavor
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Binary Target where
  put = putEnum8
  get = getEnum8

instance Serialize Target where
  sput = sputPlain
  sget = sgetPlain

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


