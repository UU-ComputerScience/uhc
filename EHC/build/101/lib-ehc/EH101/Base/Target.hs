module EH101.Base.Target
( Target (..)
, defaultTarget
, supportedTargetMp, showSupportedTargets', showSupportedTargets
, TargetFlavor (..)
, defaultTargetFlavor
, allTargetFlavorMp, showAllTargetFlavors', showAllTargetFlavors
, targetDoesHPTAnalysis
, targetIsGrinBytecode
, targetIsGrin
, targetIsC
, targetAllowsOLinking
, targetAllowsJarLinking
, targetIsCore
, targetIsTyCore
, targetIsJVM
, targetIsJavaScript
, targetIsOnUnixAndOrC
, FFIWay (..)
, ffiWayForPrim
, TargetInfo (..), TargInfoMp
, allTargetInfoMp, allFFIWays )
where
import qualified Data.Map as Map
import Data.List
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Binary
import EH101.Base.Serialize



{-# LINE 78 "src/ehc/Base/Target.chs" #-}
-- | All possible targets, even though they may not be configured (done in supportedTargetMp)
data Target
  = Target_None								-- no codegen
  | Target_None_Core_None					-- only Core
  | Target_None_TyCore_None					-- only TyCore

  -- jazy
  | Target_Interpreter_Core_Jazy			-- java based on Core, using jazy library

  -- javascript
  | Target_Interpreter_Core_JavaScript		-- javascript based on Core

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
  deriving ( Eq, Ord, Enum )

{-# LINE 115 "src/ehc/Base/Target.chs" #-}
instance Show Target where
  show Target_None							= "NONE"
  show Target_None_Core_None				= "core"
  show Target_None_TyCore_None				= "tycore"
  show Target_Interpreter_Core_Jazy			= "jazy"
  show Target_Interpreter_Core_JavaScript	= "js"
  show Target_FullProgAnal_Grin_C			= "C"
  show Target_FullProgAnal_Grin_LLVM		= "llvm"
  show Target_FullProgAnal_Grin_JVM			= "jvm"
  show Target_Interpreter_Grin_C			= "bc"
  show Target_FullProgAnal_Grin_CLR			= "clr"

{-# LINE 134 "src/ehc/Base/Target.chs" #-}
defaultTarget :: Target
defaultTarget = Target_Interpreter_Grin_C

{-# LINE 145 "src/ehc/Base/Target.chs" #-}
supportedTargetMp :: Map.Map String Target
(supportedTargetMp,allTargetInfoMp)
  = (Map.fromList ts, Map.fromList is)
  where (ts,is) = unzip
          [ ((show t, t),(t,i))
          | (t,i)
              <- []
                 -- ++ [ mk Target_None_Core_None [] ]
                 ++ [ mk Target_Interpreter_Core_JavaScript [FFIWay_JavaScript] ]
                 ++ [ mk Target_Interpreter_Grin_C [FFIWay_CCall]
                    ]
          ]
        mk t ffis = (t,TargetInfo (FFIWay_Prim : ffis))

showSupportedTargets' :: String -> String
showSupportedTargets'
  = showStringMapKeys supportedTargetMp

showSupportedTargets :: String
showSupportedTargets
  = showSupportedTargets' " "

{-# LINE 193 "src/ehc/Base/Target.chs" #-}
data TargetFlavor
  = TargetFlavor_Plain						-- no special stuff
  | TargetFlavor_Debug						-- debugging variant
  -- more: profiling, ....
  deriving (Eq,Ord,Enum)

{-# LINE 201 "src/ehc/Base/Target.chs" #-}
defaultTargetFlavor :: TargetFlavor
defaultTargetFlavor = TargetFlavor_Plain

{-# LINE 206 "src/ehc/Base/Target.chs" #-}
instance Show TargetFlavor where
  show TargetFlavor_Plain				= "plain"
  show TargetFlavor_Debug				= "debug"

{-# LINE 214 "src/ehc/Base/Target.chs" #-}
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

{-# LINE 239 "src/ehc/Base/Target.chs" #-}
targetDoesHPTAnalysis :: Target -> Bool
targetDoesHPTAnalysis t
  = case t of
      _ 								-> False

{-# LINE 256 "src/ehc/Base/Target.chs" #-}
targetIsGrinBytecode :: Target -> Bool
targetIsGrinBytecode t
  = case t of
      Target_Interpreter_Grin_C		 	-> True
      _ 								-> False

{-# LINE 266 "src/ehc/Base/Target.chs" #-}
targetIsGrin :: Target -> Bool
targetIsGrin t
  = case t of
      _ 								-> targetIsGrinBytecode t || targetDoesHPTAnalysis t

{-# LINE 277 "src/ehc/Base/Target.chs" #-}
targetIsC :: Target -> Bool
targetIsC t
  = case t of
      Target_Interpreter_Grin_C		 	-> True
      _ 								-> False

{-# LINE 290 "src/ehc/Base/Target.chs" #-}
targetAllowsOLinking :: Target -> Bool
targetAllowsOLinking t
  = case t of
      Target_Interpreter_Grin_C		 	-> True
      _ 								-> False

{-# LINE 300 "src/ehc/Base/Target.chs" #-}
targetAllowsJarLinking :: Target -> Bool
targetAllowsJarLinking t
  = case t of
      _ 								-> False

{-# LINE 310 "src/ehc/Base/Target.chs" #-}
targetIsCore :: Target -> Bool
targetIsCore t
  = case t of
      Target_None_Core_None				-> True
      _ 								-> False

{-# LINE 318 "src/ehc/Base/Target.chs" #-}
targetIsTyCore :: Target -> Bool
targetIsTyCore t
  = case t of
      Target_None_TyCore_None			-> True
      _ 								-> False

{-# LINE 326 "src/ehc/Base/Target.chs" #-}
targetIsJVM :: Target -> Bool
targetIsJVM t
  = case t of
      _ 								-> False

{-# LINE 336 "src/ehc/Base/Target.chs" #-}
targetIsJavaScript :: Target -> Bool
targetIsJavaScript t
  = case t of
      Target_Interpreter_Core_JavaScript	-> True
      _ 									-> False

{-# LINE 366 "src/ehc/Base/Target.chs" #-}
-- | target runs on (possibly emulated) UNIX / C environment? this should coincide with flag EHC_CFG_USE_UNIX_AND_C in src/ehc/variant.mk
targetIsOnUnixAndOrC :: Target -> Bool
targetIsOnUnixAndOrC t
  = targetIsC t || targetIsJVM t

{-# LINE 377 "src/ehc/Base/Target.chs" #-}
data FFIWay
  = FFIWay_Prim				-- as primitive
  | FFIWay_CCall			-- as C call
  | FFIWay_Jazy				-- as Java/Jazy
  | FFIWay_JavaScript		-- JavaScript
  deriving (Eq,Ord,Enum)

instance Show FFIWay where
  show FFIWay_Prim			= "prim"
  show FFIWay_CCall			= "ccall"
  show FFIWay_Jazy			= "jazy"
  show FFIWay_JavaScript	= "js"

instance PP FFIWay where
  pp = pp . show

{-# LINE 407 "src/ehc/Base/Target.chs" #-}
ffiWayForPrim :: Target -> Maybe FFIWay
ffiWayForPrim Target_Interpreter_Core_JavaScript	= Just FFIWay_JavaScript
ffiWayForPrim t | targetIsC t						= Just FFIWay_CCall
                | otherwise							= Nothing

{-# LINE 429 "src/ehc/Base/Target.chs" #-}
data TargetInfo
  = TargetInfo
      { targiAllowedFFI		:: [FFIWay]
      }

type TargInfoMp = Map.Map Target TargetInfo

{-# LINE 438 "src/ehc/Base/Target.chs" #-}
allTargetInfoMp :: TargInfoMp

allFFIWays :: [FFIWay]
allFFIWays = nub $ concatMap targiAllowedFFI $ Map.elems allTargetInfoMp

{-# LINE 449 "src/ehc/Base/Target.chs" #-}
deriving instance Typeable Target
deriving instance Data Target

deriving instance Typeable FFIWay
deriving instance Data FFIWay

deriving instance Typeable TargetFlavor
deriving instance Data TargetFlavor

{-# LINE 464 "src/ehc/Base/Target.chs" #-}
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


