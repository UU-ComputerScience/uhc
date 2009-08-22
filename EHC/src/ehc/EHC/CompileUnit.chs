%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile unit maintains info for one unit of compilation, a Haskell (HS) module, an EH file.

%%[8 module {%{EH}EHC.CompileUnit}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map)
%%]
%%[8 import({%{EH}EHC.Common})
%%]

-- Language syntax: HS, EH
%%[8 import(qualified {%{EH}HS} as HS, qualified {%{EH}EH} as EH)
%%]
-- Language syntax: Core, TyCore, Grin, ...
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
%%[(8 codegen) import(qualified {%{EH}TyCore} as C)
%%]
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinByteCode} as Bytecode)
%%]
%%[(8 jazy) hs import(qualified {%{EH}JVMClass} as Jvm)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[20 import(qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- module admin
%%[20 import({%{EH}Module})
%%]

-- timestamps
%%[20 import(System.Time, System.Directory)
%%]

-- Force evaluation for IO
%%[99 import({%{EH}Base.ForceEval})
%%]
%%[(99 codegen) import({%{EH}Core.Trf.ForceEval})
%%]
%%[(99 codegen grin) import({%{EH}GrinCode.Trf.ForceEval}, {%{EH}GrinByteCode.Trf.ForceEval})
%%]

-- debug
%%[99 import(EH.Util.Debug)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Inter module optimisation info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Intermodule optimisation info.
Currently only for Grin meant to be compiled to GrinByteCode.
Absence of this info should not prevent correct compilation.

%%[20 export(Optim(..),defaultOptim)
data Optim
  = Optim
%%[[(20 grin)
      { optimGrInlMp          :: Grin.GrInlMp        -- inlining map, from name to GrExpr (grin expressions)
      }
%%]]

defaultOptim :: Optim
defaultOptim
  = Optim
%%[[(20 grin)
      Map.empty
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation sequence nr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is not a necessity, just a gimmick because GHC has it :-).
Ok, it is useful to see how much is done.

%%[99 export(EHCCompileSeqNr(..))
data EHCCompileSeqNr
  = EHCCompileSeqNr
      { ecseqnrThis		:: !Int
      , ecseqnrTotal	:: !Int
      }
  deriving (Eq,Ord)

zeroEHCCompileSeqNr = EHCCompileSeqNr 0 0

instance Show EHCCompileSeqNr where
  show (EHCCompileSeqNr this total)
    = "[" ++ replicate (length tot - length ths) ' ' ++ ths ++ "/" ++ tot ++ "]"
    where tot = show total
          ths = show this
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileUnit(..))
data EHCompileUnit
  = EHCompileUnit
      { ecuSrcFilePath       :: !FPath
%%[[99
      , ecuMbCppFilePath     :: !(Maybe FPath)
%%]]
      , ecuFileLocation      :: !FileLoc
      , ecuGrpNm             :: !HsName
      , ecuModNm             :: !HsName
      , ecuMbHS              :: !(Maybe HS.AGItf)
      , ecuMbHSSem           :: !(Maybe HSSem.Syn_AGItf)
      , ecuMbEH              :: !(Maybe EH.AGItf)
      , ecuMbEHSem           :: !(Maybe EHSem.Syn_AGItf)
%%[[(8 codegen)
      , ecuMbCore            :: !(Maybe Core.CModule)
      , ecuMbCoreSem         :: !(Maybe Core2GrSem.Syn_CodeAGItf)
      , ecuMbTyCore          :: !(Maybe C.Module)
      -- , ecuMbTyCoreSem       :: !(Maybe Core2GrSem.Syn_CodeAGItf)
%%]]
%%[[(8 grin)
      , ecuMbGrin            :: !(Maybe Grin.GrModule)
      , ecuMbBytecode        :: !(Maybe Bytecode.Module)
      , ecuMbBytecodeSem     :: !(Maybe PP_Doc)
%%]]
%%[[(8 jazy)
      , ecuMbJVMClassL       :: !(Maybe (HsName,[Jvm.Class]))
%%]]
      , ecuState             :: !EHCompileUnitState
%%[[20
      , ecuHSDeclImpNmL      :: ![HsName]							-- imported modules as declared in src .hs
      , ecuHIDeclImpNmL      :: ![HsName]							-- imported modules as declared, either in .hs of .hi
      , ecuHIUsedImpNmL      :: ![HsName]							-- imported modules as actually used
      , ecuIsTopMod          :: !Bool								-- been specified on commandline
      , ecuHasMain           :: !Bool								-- has a def for 'main'?
      , ecuNeedsCompile      :: !Bool								-- (re)compilation from .hs needed?
      , ecuMbHSTime          :: !(Maybe ClockTime)
      , ecuMbHITime          :: !(Maybe ClockTime)
%%[[(8 codegen)
      , ecuMbCoreTime        :: !(Maybe ClockTime)
%%]]
%%[[(8 codegen grin)
      , ecuMbGrinTime        :: !(Maybe ClockTime)
%%]]
      , ecuMbHSSemMod        :: !(Maybe HSSemMod.Syn_AGItf)
      , ecuMod               :: !Mod
      , ecuMbPrevHI          :: !(Maybe HI.AGItf)
      , ecuMbPrevHISem       :: !(Maybe HISem.Syn_AGItf)
      , ecuMbOptim           :: !(Maybe Optim)
      , ecuHIInfo            :: !HI.HIInfo
      , ecuDirIsWritable     :: !Bool
%%]]
%%[[(99 codegen)
      , ecuGenCodeFiles      :: ![FPath]
      , ecuSeqNr      		 :: !EHCCompileSeqNr
%%]]
      }
%%]

%%[8 export(ecuFilePath)
ecuFilePath :: EHCompileUnit -> FPath
ecuFilePath ecu
%%[[8
  = ecuSrcFilePath ecu
%%][99
  = maybe (ecuSrcFilePath ecu) id (ecuMbCppFilePath ecu)
%%]]
%%]

%%[20 export(ecuIsMainMod)
ecuIsMainMod :: EHCompileUnit -> Bool
ecuIsMainMod e = ecuIsTopMod e && ecuHasMain e
%%]

%%[8 export(emptyECU)
emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuSrcFilePath       = emptyFPath
%%[[99
      , ecuMbCppFilePath     = Nothing
%%]]
      , ecuFileLocation      = emptyFileLoc
      , ecuGrpNm             = hsnUnknown
      , ecuModNm             = hsnUnknown
      , ecuMbHS              = Nothing
      , ecuMbHSSem           = Nothing
      , ecuMbEH              = Nothing
      , ecuMbEHSem           = Nothing
%%[[102
%%]]
%%[[(8 codegen)
      , ecuMbCore            = Nothing
      , ecuMbCoreSem         = Nothing
      , ecuMbTyCore          = Nothing
      -- , ecuMbTyCoreSem       = Nothing
%%]]
%%[[(8 grin)
      , ecuMbGrin            = Nothing
      , ecuMbBytecode        = Nothing
      , ecuMbBytecodeSem     = Nothing
%%]]
%%[[(8 jazy)
      , ecuMbJVMClassL       = Nothing
%%]]
      , ecuState             = ECUSUnknown
%%[[20
      , ecuHSDeclImpNmL      = []
      , ecuHIDeclImpNmL      = []
      , ecuHIUsedImpNmL      = []
      , ecuIsTopMod          = False
      , ecuHasMain           = False
      , ecuNeedsCompile      = True
      , ecuMbHSTime          = Nothing
      , ecuMbHITime          = Nothing
%%[[(20 codegen)
      , ecuMbCoreTime        = Nothing
%%]]
%%[[(20 codegen grin)
      , ecuMbGrinTime        = Nothing
%%]]
      , ecuMbHSSemMod        = Nothing
      , ecuMod               = emptyMod
      , ecuMbPrevHI          = Nothing
      , ecuMbPrevHISem       = Nothing
      , ecuMbOptim           = Nothing
      , ecuHIInfo            = HI.emptyHIInfo
      , ecuDirIsWritable     = False
%%]]
%%[[(99 codegen)
      , ecuGenCodeFiles      = []
      , ecuSeqNr			 = zeroEHCCompileSeqNr
%%]]
      }
%%]
      , ecuMbEHSem2          = Nothing

%%[20 export(ecuImpNmL)
ecuImpNmL :: EHCompileUnit -> [HsName]
ecuImpNmL ecu = (nub $ ecuHSDeclImpNmL ecu ++ ecuHIDeclImpNmL ecu ++ ecuHIUsedImpNmL ecu) \\ [ecuModNm ecu]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
instance CompileUnitState EHCompileUnitState where
  cusDefault      = ECUSEh EHStart
  cusUnk          = ECUSUnknown
  cusIsUnk        = (==ECUSUnknown)
%%]
%%[8.cusIsImpKnown
  cusIsImpKnown _ = True
%%]
%%[20 -8.cusIsImpKnown
  cusIsImpKnown s = case s of
                      ECUSHaskell HSOnlyImports  -> True
                      ECUSHaskell HIOnlyImports  -> True
%%[[99
                      ECUSHaskell LHSOnlyImports -> True
%%]]
                      ECUSHaskell HSAllSem       -> True
                      ECUSHaskell HIAllSem       -> True
                      _                          -> False
%%]

%%[8
instance FileLocatable EHCompileUnit FileLoc where
  fileLocation   = ecuFileLocation
  noFileLocation = emptyFileLoc
%%]

%%[8
instance CompileUnit EHCompileUnit HsName FileLoc EHCompileUnitState where
  cuDefault         = emptyECU
  cuFPath           = ecuFilePath
  cuLocation        = fileLocation
  cuKey             = ecuModNm
  cuState           = ecuState
  cuUpdFPath        = ecuStoreSrcFilePath
  cuUpdLocation     = ecuStoreFileLocation
  cuUpdState        = ecuStoreState
  cuUpdKey   nm u   = u {ecuModNm = nm}
%%[[8
  cuImports         = const []
%%][20
  cuImports         = ecuImpNmL
%%]]

instance CompileRunError Err () where
  crePPErrL                 = ppErrL
  creMkNotFoundErrL _ fp sp = [rngLift emptyRange Err_FileNotFound fp sp]
  creAreFatal               = errLIsFatal

instance CompileModName HsName where
  mkCMNm = hsnFromString

instance Show EHCompileUnit where
  show _ = "EHCompileUnit"

instance PP EHCompileUnit where
  pp ecu
    = ecuModNm ecu >|<
%%[[20
      ":" >#< ppCommas (ecuImpNmL ecu) >|<
%%]]
      "," >#< show (ecuState ecu)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storing into an EHCompileUnit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EcuUpdater,ecuStoreSrcFilePath,ecuStoreState,ecuStoreHS,ecuStoreEH,ecuStoreHSSem,ecuStoreEHSem)
type EcuUpdater a = a -> EHCompileUnit -> EHCompileUnit

ecuStoreSrcFilePath :: EcuUpdater FPath
ecuStoreSrcFilePath x ecu = ecu { ecuSrcFilePath = x }

ecuStoreFileLocation :: EcuUpdater FileLoc
ecuStoreFileLocation x ecu = ecu { ecuFileLocation = x }

ecuStoreState :: EcuUpdater EHCompileUnitState
ecuStoreState x ecu = ecu { ecuState = x }

ecuStoreHS :: EcuUpdater HS.AGItf
ecuStoreHS x ecu = ecu { ecuMbHS = Just x }

ecuStoreEH :: EcuUpdater EH.AGItf
ecuStoreEH x ecu = ecu { ecuMbEH = Just x }

ecuStoreHSSem :: EcuUpdater HSSem.Syn_AGItf
ecuStoreHSSem x ecu = ecu { ecuMbHSSem = Just x }

ecuStoreEHSem :: EcuUpdater EHSem.Syn_AGItf
ecuStoreEHSem x ecu = ecu { ecuMbEHSem = Just x }
%%]

%%[(8 codegen) export(ecuStoreCoreSem,ecuStoreCore)
ecuStoreCoreSem :: EcuUpdater Core2GrSem.Syn_CodeAGItf
ecuStoreCoreSem x ecu = ecu { ecuMbCoreSem = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
%%[[8
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }
%%][99
ecuStoreCore x ecu | forceEval x `seq` True = ecu { ecuMbCore = Just x }
%%]]
%%]

%%[(8 codegen) export(ecuStoreTyCore)
-- ecuStoreTyCoreSem :: EcuUpdater Core2GrSem.Syn_CodeAGItf
-- ecuStoreTyCoreSem x ecu = ecu { ecuMbCoreSem = Just x }

ecuStoreTyCore :: EcuUpdater C.Module
ecuStoreTyCore x ecu = ecu { ecuMbTyCore = Just x }
%%]

%%[(8 jazy) export(ecuStoreJVMClassL)
ecuStoreJVMClassL :: EcuUpdater (HsName,[Jvm.Class])
ecuStoreJVMClassL x ecu = ecu { ecuMbJVMClassL = Just x }
%%]

ecuStoreJVMClassFPathL :: EcuUpdater [FPath]
ecuStoreJVMClassFPathL x ecu = ecu { ecuMbJVMClassL = Just (Right x) }

%%[(8 grin) export(ecuStoreGrin,ecuStoreBytecode,ecuStoreBytecodeSem)
ecuStoreGrin :: EcuUpdater Grin.GrModule
%%[[8
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }
%%][99
ecuStoreGrin x ecu | forceEval x `seq` True = ecu { ecuMbGrin = Just x }
%%]]

ecuStoreBytecode :: EcuUpdater Bytecode.Module
%%[[8
ecuStoreBytecode x ecu = ecu { ecuMbBytecode = Just x }
%%][99
ecuStoreBytecode x ecu | forceEval x `seq` True = ecu { ecuMbBytecode = Just x }
%%]]

ecuStoreBytecodeSem :: EcuUpdater PP_Doc
ecuStoreBytecodeSem x ecu = ecu { ecuMbBytecodeSem = Just x }
%%]

%%[20 export(ecuStoreHSDeclImpL,ecuSetNeedsCompile,ecuStoreHIUsedImpL,ecuStoreHSTime,ecuStoreHITime,ecuStoreHSSemMod,ecuStoreHIDeclImpL,ecuStoreMod,ecuSetIsTopMod,ecuSetHasMain,ecuStorePrevHI,ecuStorePrevHISem,ecuStoreOptim,ecuStoreHIInfo)
ecuStoreHSTime :: EcuUpdater ClockTime
ecuStoreHSTime x ecu = ecu { ecuMbHSTime = Just x }

ecuStoreHITime :: EcuUpdater ClockTime
ecuStoreHITime x ecu = ecu { ecuMbHITime = Just x }

ecuStoreHSSemMod :: EcuUpdater HSSemMod.Syn_AGItf
ecuStoreHSSemMod x ecu = ecu { ecuMbHSSemMod = Just x }

ecuStoreHSDeclImpL :: EcuUpdater [HsName]
ecuStoreHSDeclImpL x ecu = ecu { ecuHSDeclImpNmL = x }

ecuStoreHIDeclImpL :: EcuUpdater [HsName]
ecuStoreHIDeclImpL x ecu = ecu { ecuHIDeclImpNmL = x }

ecuStoreHIUsedImpL :: EcuUpdater [HsName]
ecuStoreHIUsedImpL x ecu = ecu { ecuHIUsedImpNmL = x }

ecuStoreMod :: EcuUpdater Mod
ecuStoreMod x ecu = ecu { ecuMod = x }

ecuSetIsTopMod :: EcuUpdater Bool
ecuSetIsTopMod x ecu = ecu { ecuIsTopMod = x }

ecuSetHasMain :: EcuUpdater Bool
ecuSetHasMain x ecu = ecu { ecuHasMain = x }

ecuSetNeedsCompile :: EcuUpdater Bool
ecuSetNeedsCompile x ecu = ecu { ecuNeedsCompile = x }

ecuStorePrevHI :: EcuUpdater HI.AGItf
ecuStorePrevHI x ecu = ecu { ecuMbPrevHI = Just x }

ecuStorePrevHISem :: EcuUpdater HISem.Syn_AGItf
ecuStorePrevHISem x ecu = ecu { ecuMbPrevHISem = Just x }

ecuStoreOptim :: EcuUpdater Optim
ecuStoreOptim x ecu = ecu { ecuMbOptim = Just x }

ecuStoreHIInfo :: EcuUpdater HI.HIInfo
%%[[8
ecuStoreHIInfo x ecu = ecu { ecuHIInfo = x }
%%][99
ecuStoreHIInfo x ecu | forceEval x `seq` True = ecu { ecuHIInfo = x }
%%]]
%%]

%%[(20 codegen) export(ecuStoreCoreTime)
ecuStoreCoreTime :: EcuUpdater ClockTime
ecuStoreCoreTime x ecu = ecu { ecuMbCoreTime = Just x }
%%]

%%[(20 codegen grin) export(ecuStoreGrinTime)
ecuStoreGrinTime :: EcuUpdater ClockTime
ecuStoreGrinTime x ecu = ecu { ecuMbGrinTime = Just x }
%%]

%%[20 export(ecuStoreDirIsWritable)
ecuStoreDirIsWritable :: EcuUpdater Bool
ecuStoreDirIsWritable x ecu = ecu { ecuDirIsWritable = x }
%%]

%%[(99 codegen) export(ecuStoreGenCodeFiles,ecuStoreCppFilePath,ecuStoreSeqNr)
ecuStoreGenCodeFiles :: EcuUpdater [FPath]
ecuStoreGenCodeFiles x ecu = ecu { ecuGenCodeFiles = x }

ecuStoreSeqNr :: EcuUpdater EHCCompileSeqNr
ecuStoreSeqNr x ecu = ecu { ecuSeqNr = x }

ecuStoreCppFilePath :: EcuUpdater FPath
ecuStoreCppFilePath x ecu = ecu { ecuMbCppFilePath = Just x }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates on EHCompileUnit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 haddock
Is HS newer?
If no HS exists False is returned.
%%]

%%[20 export(ecuIsHSNewerThanHI)
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case (ecuMbHSTime ecu,ecuMbHITime ecu) of
      (Just ths,Just thi) -> ths `diffClockTimes` thi > noTimeDiff 
      (Nothing ,Just thi) -> False
      _                   -> True
%%]

%%[20 export(ecuIsValidHI)
ecuIsValidHI :: EHCompileUnit -> Bool
ecuIsValidHI ecu
  = case ecuMbPrevHISem ecu of
      Just s -> HISem.isValidVersion_Syn_AGItf s
      _      -> False
%%]

%%[20 haddock
Can HI be used instead of HS?
This is purely based on HI being of the right version and HS not newer.
The need for recompilation considers dependencies on imports as well.
%%]

%%[20 export(ecuCanUseHIInsteadOfHS)
ecuCanUseHIInsteadOfHS :: EHCompileUnit -> Bool
ecuCanUseHIInsteadOfHS ecu
  = ecuIsValidHI ecu && not (ecuIsHSNewerThanHI ecu)
%%]

