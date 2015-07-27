%%[0 hs
{-# LANGUAGE TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile unit maintains info for one unit of compilation, a Haskell (HS) module, an EH file.

%%[8 module {%{EH}EHC.CompileUnit}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]
%%[8 import({%{EH}EHC.Common})
%%]

%%[8 import(UHC.Util.Lens)
%%]

%%[8 import(Data.Typeable)
%%]

-- Language syntax: HS, EH
%%[8 import(qualified {%{EH}HS} as HS, qualified {%{EH}EH} as EH)
%%]
-- Language syntax: Core, TyCore, Grin, ...
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]
%%[(8 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinByteCode} as Bytecode)
%%]
%%[(8 jazy) hs import(qualified {%{EH}JVMClass} as Jvm)
%%]
%%[(8 javascript) hs import(qualified {%{EH}JavaScript} as JS)
%%]
%%[(8 codegen cmm) hs import(qualified {%{EH}Cmm} as Cmm)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 corerun core) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(8 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
-- Language semantics: CoreRun
%%[(8 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- module admin
%%[50 import({%{EH}Module.ImportExport}, {%{EH}CodeGen.ImportUsedModules})
%%]

-- timestamps
%%[50 import(UHC.Util.Time, System.Directory)
%%]

-- pragma, target
%%[99 hs import(qualified {%{EH}Base.Pragma} as Pragma, {%{EH}Base.Target})
%%]

-- debug
%%[99 import(UHC.Util.Debug)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Inter module optimisation info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Intermodule optimisation info.
Currently only for Grin meant to be compiled to GrinByteCode.
Absence of this info should not prevent correct compilation.

%%[50 export(Optim(..),defaultOptim)
data Optim
  = Optim
%%[[(50 grin)
      { optimGrInlMp          :: Grin.GrInlMp        -- inlining map, from name to GrExpr (grin expressions)
      }
%%]]

defaultOptim :: Optim
defaultOptim
  = Optim
%%[[(50 grin)
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
      { ecseqnrThis     :: !Int
      , ecseqnrTotal    :: !Int
      }
  deriving (Eq,Ord)

zeroEHCCompileSeqNr :: EHCCompileSeqNr
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
-- | Single compilation unit info, fields prefixed with _ have Lens access
data EHCompileUnit
  = EHCompileUnit
      { ecuSrcFilePath       :: !FPath
%%[[99
      , ecuMbCppFilePath     :: !(Maybe FPath)
%%]]
      , ecuFileLocation      :: !FileLoc
      , ecuGrpNm             :: !HsName
      , ecuModNm             :: !HsName
      , _ecuMbHS             :: !(Maybe HS.AGItf)
      , _ecuMbHSSem          :: !(Maybe HSSem.Syn_AGItf)
      , _ecuMbEH             :: !(Maybe EH.AGItf)
      , _ecuMbEHSem          :: !(Maybe EHSem.Syn_AGItf)
%%[[(8 codegen)
      , _ecuMbCore           :: !(Maybe Core.CModule)
%%]]
%%[[(8 codegen core)
      , _ecuMbCoreSem        :: !(Maybe Core2GrSem.Syn_CodeAGItf)
%%]]
%%[[(8 codegen corerun)
      , _ecuMbCore2CoreRunSem:: !(Maybe Core2CoreRunSem.Syn_CodeAGItf)
%%]]
%%[[(8 codegen corein)
      , _ecuMbCoreSemMod     :: !(Maybe Core2ChkSem.Syn_CodeAGItf)
%%]]
%%[[(8 corerun)
      , _ecuMbCoreRun        :: !(Maybe CoreRun.Mod)
%%]]
%%[[(8 codegen corerunin)
      , _ecuMbCoreRunSemMod  :: !(Maybe CoreRun2ChkSem.Syn_AGItf)
%%]]
%%[[(8 codegen tycore)
      , ecuMbTyCore          :: !(Maybe C.Module)
%%]]
%%[[(8 grin)
      , _ecuMbGrin           :: !(Maybe Grin.GrModule)
      , ecuMbBytecode        :: !(Maybe Bytecode.Module)
      , ecuMbBytecodeSem     :: !(Maybe PP_Doc)
%%]]
%%[[(8 cmm)
      , _ecuMbCmm            :: !(Maybe Cmm.Module)
%%]]
%%[[(8 jazy)
      , ecuMbJVMClassL       :: !(Maybe (HsName,[Jvm.Class]))
%%]]
%%[[(8 javascript)
      , _ecuMbJavaScript     :: !(Maybe JS.JavaScriptModule)
%%]]
      , ecuState             :: !EHCompileUnitState
      , _ecuASTType          :: !ASTType
      , _ecuASTFileContent   :: !ASTFileContent
      , _ecuASTFileUse       :: !ASTFileUse
%%[[50
      , ecuImportUsedModules :: !ImportUsedModules                  -- imported modules info
      , ecuIsTopMod          :: !Bool                               -- module has been specified for compilation on commandline
      , ecuHasMain           :: !Bool                               -- has a def for 'main'?
      , ecuNeedsCompile      :: !Bool                               -- (re)compilation from .hs needed?
      , _ecuMbSrcTime        :: !(Maybe ClockTime)                  -- timestamp of possibly absent source (hs, or other type) file
      , _ecuMbHIInfoTime     :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated hi file
%%[[(50 codegen)
      , _ecuMbCoreTime       :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated core file
%%]]
%%[[(50 corerun)
      , _ecuMbCoreRunTime    :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated corerun file
%%]]
%%[[(50 codegen grin)
      , _ecuMbGrinTime       :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated grin file
%%]]
      , _ecuMbHSSemMod       :: !(Maybe HSSemMod.Syn_AGItf)
      , ecuMod               :: !Mod                                -- import/export info of module
      , _ecuMbPrevHIInfo     :: !(Maybe HI.HIInfo)                  -- possible HI info of previous run
      , ecuMbOptim           :: !(Maybe Optim)
      , _ecuMbHIInfo         :: !(Maybe HI.HIInfo)                  -- HI info of module
      , _ecuDirIsWritable    :: !Bool                               -- can be written in dir of module?
      , _ecuMbPrevSearchInfo :: !(Maybe PrevSearchInfo)             -- file search info required for imported module search
%%]]
%%[[99
      , ecuMbOpts            :: (Maybe EHCOpts)                     -- possibly per module adaption of options (caused by pragmas)
      , ecuTarget            :: Target                              -- target for which we compile
      , ecuPragmas           :: !(Set.Set Pragma.Pragma)            -- pragmas of module
      , ecuUsedNames         :: ModEntRelFilterMp                   -- map holding actually used names, to later filter cache of imported hi's to be included in this module's hi
      , ecuSeqNr             :: !EHCCompileSeqNr                    -- sequence nr of sorted compilation
%%]]
%%[[(99 codegen)
      , ecuGenCodeFiles      :: ![FPath]                            -- generated code files
%%]]
      }
      deriving Typeable
%%]

%%[8
mkLabel ''EHCompileUnit
%%]

%%[8 export(ecuASTType, ecuASTFileContent, ecuASTFileUse)
%%]

%%[(8 core) export(ecuMbCore, ecuCore, ecuMbCoreSem, ecuCoreSem)
ecuCore = isoMb "ecuMbCore" ecuMbCore
ecuCoreSem = isoMb "ecuMbCoreSem" ecuMbCoreSem
%%]
%%[(8 grin) export(ecuMbGrin, ecuGrin)
ecuGrin = isoMb "ecuMbGrin" ecuMbGrin
%%]
%%[(8 javascript) export(ecuMbJavaScript, ecuJavaScript)
ecuJavaScript = isoMb "ecuMbJavaScript" ecuMbJavaScript
%%]
%%[(8 corerun) export(ecuMbCore2CoreRunSem, ecuMbCoreRun, ecuCoreRun)
ecuCoreRun = isoMb "ecuMbCoreRun" ecuMbCoreRun
%%]
%%[(8 corein) export(ecuMbCoreSemMod, ecuCoreSemMod)
ecuCoreSemMod = isoMb "ecuMbCoreSemMod" ecuMbCoreSemMod
%%]
%%[(8 corerunin) export(ecuMbCoreRunSemMod, ecuCoreRunSemMod)
ecuCoreRunSemMod = isoMb "ecuMbCoreRunSemMod" ecuMbCoreRunSemMod
%%]

%%[8 export(ecuMbHS, ecuHS, ecuMbHSSem, ecuHSSem, ecuMbEH, ecuEH, ecuMbEHSem, ecuEHSem)
ecuEH = isoMb "ecuMbEH" ecuMbEH
ecuEHSem = isoMb "ecuMbEHSem" ecuMbEHSem
ecuHS = isoMb "ecuMbHS" ecuMbHS
ecuHSSem = isoMb "ecuMbHSSem" ecuMbHSSem
%%]

%%[50 export(ecuMbHIInfo, ecuHIInfo, ecuMbPrevHIInfo, ecuPrevHIInfo, ecuMbHSSemMod, ecuHSSemMod, ecuMbSrcTime, ecuSrcTime, ecuMbHIInfoTime, ecuHIInfoTime)
ecuHIInfo = isoMbWithDefault HI.emptyHIInfo ecuMbHIInfo
ecuPrevHIInfo = isoMb "ecuMbPrevHIInfo" ecuMbPrevHIInfo
ecuHSSemMod = isoMb "ecuMbHSSemMod" ecuMbHSSemMod
ecuSrcTime = isoMb "ecuMbSrcTime" ecuMbSrcTime
ecuHIInfoTime = isoMb "ecuMbHIInfoTime" ecuMbHIInfoTime
%%]

%%[50 export(ecuDirIsWritable, ecuMbPrevSearchInfo, ecuPrevSearchInfo)
ecuPrevSearchInfo = isoMb "ecuMbPrevSearchInfo" ecuMbPrevSearchInfo
%%]

%%[(50 core) export(ecuMbCoreTime, ecuCoreTime)
ecuCoreTime = isoMb "ecuMbCoreTime" ecuMbCoreTime
%%]

%%[(50 grin) export(ecuMbGrinTime, ecuGrinTime)
ecuGrinTime = isoMb "ecuMbGrinTime" ecuMbGrinTime
%%]

%%[(50 corerun) export(ecuMbCoreRunTime, ecuCoreRunTime)
ecuCoreRunTime = isoMb "ecuMbCoreRunTime" ecuMbCoreRunTime
%%]

%%[50 export(ecuHSDeclImpNmS, ecuHIDeclImpNmS, ecuHIUsedImpNmS)
ecuHSDeclImpNmS = iumHSDeclModules . ecuImportUsedModules
ecuHIDeclImpNmS = iumHIDeclModules . ecuImportUsedModules
ecuHIUsedImpNmS = iumHIUsedModules . ecuImportUsedModules
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

%%[50 export(ecuIsMainMod)
ecuIsMainMod :: EHCompileUnit -> Bool
ecuIsMainMod e = ecuIsTopMod e && ecuHasMain e
%%]

%%[99 export(ecuAnHIInfo)
-- | give the current value HIInfo, or the previous one
ecuAnHIInfo :: EHCompileUnit -> HI.HIInfo
ecuAnHIInfo e
  = case _ecuMbPrevHIInfo e of
      Just pi | HI.hiiIsEmpty hii
        -> pi
      _ -> hii
  where hii = e ^. ecuHIInfo
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
      , _ecuMbHS             = Nothing
      , _ecuMbHSSem          = Nothing
      , _ecuMbEH             = Nothing
      , _ecuMbEHSem          = Nothing
%%[[102
%%]]
%%[[(8 codegen)
      , _ecuMbCore           = Nothing
%%]]
%%[[(8 codegen core)
      , _ecuMbCoreSem        = Nothing
%%]]
%%[[(8 codegen corerun)
      , _ecuMbCore2CoreRunSem= Nothing
%%]]
%%[[(8 codegen corein)
      , _ecuMbCoreSemMod     = Nothing
%%]]
%%[[(8 corerun)
      , _ecuMbCoreRun		 = Nothing
%%]]
%%[[(8 codegen corerunin)
      , _ecuMbCoreRunSemMod  = Nothing
%%]]
%%[[(8 codegen tycore)
      , ecuMbTyCore          = Nothing
%%]]
%%[[(8 grin)
      , _ecuMbGrin           = Nothing
      , ecuMbBytecode        = Nothing
      , ecuMbBytecodeSem     = Nothing
%%]]
%%[[(8 cmm)
      , _ecuMbCmm            = Nothing
%%]]
%%[[(8 jazy)
      , ecuMbJVMClassL       = Nothing
%%]]
%%[[(8 javascript)
      , _ecuMbJavaScript     = Nothing
%%]]
      , ecuState             = ECUS_Unknown
      , _ecuASTType          = ASTType_Unknown
      , _ecuASTFileContent   = ASTFileContent_Unknown
      , _ecuASTFileUse       = ASTFileUse_Unknown
%%[[50
      , ecuImportUsedModules = emptyImportUsedModules
      , ecuIsTopMod          = False
      , ecuHasMain           = False
      , ecuNeedsCompile      = True
      , _ecuMbSrcTime        = Nothing
      , _ecuMbHIInfoTime     = Nothing
%%[[(50 codegen)
      , _ecuMbCoreTime       = Nothing
%%]]
%%[[(50 corerun)
      , _ecuMbCoreRunTime	 = Nothing
%%]]
%%[[(50 codegen grin)
      , _ecuMbGrinTime       = Nothing
%%]]
      , _ecuMbHSSemMod       = Nothing
      , ecuMod               = emptyMod
      , _ecuMbPrevHIInfo     = Nothing
      , ecuMbOptim           = Nothing
      , _ecuMbHIInfo         = Nothing
      , _ecuDirIsWritable    = False
      , _ecuMbPrevSearchInfo = Nothing
%%]]
%%[[99
      , ecuMbOpts            = Nothing
      , ecuTarget            = defaultTarget
      , ecuPragmas           = Set.empty
      , ecuUsedNames         = Map.empty
      , ecuSeqNr             = zeroEHCCompileSeqNr
%%]]
%%[[(99 codegen)
      , ecuGenCodeFiles      = []
%%]]
      }
%%]

%%[50 export(ecuImpNmS,ecuImpNmL)
ecuImpNmS :: EHCompileUnit -> Set.Set HsName
ecuImpNmS ecu = -- (\v -> tr "XX" (pp $ Set.toList v) v) $
  Set.delete (ecuModNm ecu) $ Set.unions [ ecuHSDeclImpNmS ecu, ecuHIDeclImpNmS ecu, ecuHIUsedImpNmS ecu ] 

ecuImpNmL :: EHCompileUnit -> [HsName]
ecuImpNmL = Set.toList . ecuImpNmS -- ecu = (nub $ ecuHSDeclImpNmL ecu ++ ecuHIDeclImpNmL ecu ++ ecuHIUsedImpNmL ecu) \\ [ecuModNm ecu]
%%]

%%[50 export(ecuTransClosedUsedModMp, ecuTransClosedOrphanModS)
-- | The used modules, for linking, according to .hi info
ecuTransClosedUsedModMp :: EHCompileUnit -> HI.HIInfoUsedModMp
ecuTransClosedUsedModMp = HI.hiiTransClosedUsedModMp . ecuAnHIInfo

-- | The orphan modules, must be .hi read, according to .hi info
ecuTransClosedOrphanModS :: EHCompileUnit -> Set.Set HsName
ecuTransClosedOrphanModS = HI.hiiTransClosedOrphanModS . ecuAnHIInfo
%%]

%%[50 export(ecuIsOrphan)
-- | Is orphan, according to .hi info
ecuIsOrphan :: EHCompileUnit -> Bool
%%[[(50 hmtyinfer codegen)
ecuIsOrphan = isJust . HI.hiiMbOrphan . ecuAnHIInfo
%%][50
ecuIsOrphan = const False
%%]]
%%]

%%[5050 export(ecuIsFromCoreSrc)
-- | Is compilation from Core source
ecuIsFromCoreSrc :: EHCompileUnit -> Bool
ecuIsFromCoreSrc = ecuStateIsCore . ecuState
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
instance CompileUnitState FileSuffInitState where
  cusDefault      = (ECUS_Eh EHStart, ASTType_EH, ASTFileContent_Text, ASTFileUse_Src)
  cusUnk          = (ECUS_Unknown, ASTType_Unknown, ASTFileContent_Unknown, ASTFileUse_Unknown)
  cusIsUnk (ECUS_Unknown,_,_,_) = True
  cusIsUnk _                    = False
%%]
%%[8.cusIsImpKnown
  cusIsImpKnown _ = True
%%]
%%[50 -8.cusIsImpKnown
  cusIsImpKnown (s,_,_,_) = case s of
                      ECUS_Haskell HSOnlyImports  -> True
                      ECUS_Haskell HIOnlyImports  -> True
                      ECUS_Haskell HMOnlyMinimal  -> True
%%[[99
                      ECUS_Haskell LHSOnlyImports -> True
%%]]
                      ECUS_Haskell HSAllSem       -> True
                      ECUS_Haskell HIAllSem       -> True
%%[[(50 corein)
                      ECUS_Core    CROnlyImports  -> True
%%]]
%%[[(50 corerunin)
                      ECUS_CoreRun CRROnlyImports -> True
%%]]
                      _                           -> False
%%]

%%[8
instance FileLocatable EHCompileUnit FileLoc where
  fileLocation   = ecuFileLocation
  noFileLocation = emptyFileLoc
%%]

%%[8
instance CompileUnit EHCompileUnit HsName FileLoc FileSuffInitState where
  cuDefault             = emptyECU
  cuFPath               = ecuFilePath
  cuLocation            = fileLocation
  cuKey                 = ecuModNm
  cuState u             = (ecuState u, _ecuASTType u, _ecuASTFileContent u, _ecuASTFileUse u)
  cuUpdFPath            = ecuStoreSrcFilePath
  cuUpdLocation         = ecuStoreFileLocation
  cuUpdState (s,t,c,u)  = ecuStoreState s . (ecuASTType ^= t) . (ecuASTFileContent ^= c) . (ecuASTFileUse ^= u)
  cuUpdKey   nm u       = u {ecuModNm = nm}
%%[[8
  cuImports             = const []
%%][50
  cuImports             = ecuImpNmL
%%]]
%%[[(99 codegen)
  cuParticipation u     = if not (Set.null $ Set.filter (Pragma.pragmaIsExcludeTarget $ ecuTarget u) $ ecuPragmas u)
                          then [CompileParticipation_NoImport]
                          else []
%%][99
  cuParticipation u     = []
%%]]

-- instance FPathError Err

instance CompileRunError Err () where
  crePPErrL                      = ppErrL
  creMkNotFoundErrL _ fp sp sufs = [rngLift emptyRange Err_FileNotFound fp sp sufs]
  creAreFatal                    = errLIsFatal

instance CompileModName HsName where
  mkCMNm = hsnFromString

instance Show EHCompileUnit where
  show _ = "EHCompileUnit"

instance PP EHCompileUnit where
  pp ecu
    = ecuModNm ecu >|<
%%[[50
      ":" >#< ppBracketsCommas (ecuImpNmL ecu) >|<
%%]]
      "," >#< show (ecuState ecu)
%%]

%%[8 export(ecuFinalDestinationState)
-- | The final state to be reached
ecuFinalDestinationState :: EHCompileUnit -> EHCompileUnitState
ecuFinalDestinationState ecu = ecuStateFinalDestination upd $ ecuState ecu
  where 
%%[[50
        upd (ECUS_Haskell _)
          | ecuNeedsCompile ecu = ECUS_Haskell HSAllSem
          | otherwise           = ECUS_Haskell HIAllSem
%%]]
        upd s                   = s
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
ecuStoreHS x ecu = ecu { _ecuMbHS = Just x }

ecuStoreEH :: EcuUpdater EH.AGItf
ecuStoreEH x ecu = ecu { _ecuMbEH = Just x }

ecuStoreHSSem :: EcuUpdater HSSem.Syn_AGItf
ecuStoreHSSem x ecu = ecu { _ecuMbHSSem = Just x }

ecuStoreEHSem :: EcuUpdater EHSem.Syn_AGItf
ecuStoreEHSem x ecu = ecu { _ecuMbEHSem = Just x }
%%]

%%[(8 codegen corein) export(ecuStoreCoreSemMod)
ecuStoreCoreSemMod :: EcuUpdater Core2ChkSem.Syn_CodeAGItf
ecuStoreCoreSemMod x ecu = ecu { _ecuMbCoreSemMod = Just x }
%%]

%%[(8 codegen core) export(ecuStoreCoreSem)
ecuStoreCoreSem :: EcuUpdater Core2GrSem.Syn_CodeAGItf
ecuStoreCoreSem x ecu = ecu { _ecuMbCoreSem = Just x }
%%]

%%[(8 codegen) export(ecuStoreCore)
ecuStoreCore :: EcuUpdater Core.CModule
%%[[8
ecuStoreCore x ecu = ecu { _ecuMbCore = Just x }
%%][99
ecuStoreCore x ecu | x `seq` True = ecu { _ecuMbCore = Just x }
%%][9999
ecuStoreCore x ecu | forceEval x `seq` True = ecu { _ecuMbCore = Just x }
%%]]
%%]

%%[(8 corerun) export(ecuStoreCoreRun)
ecuStoreCoreRun :: EcuUpdater CoreRun.Mod
ecuStoreCoreRun x ecu | x `seq` True = ecu { _ecuMbCoreRun = Just x }
%%]

%%[(8 codegen corerunin) export(ecuStoreCoreRunSemMod)
ecuStoreCoreRunSemMod :: EcuUpdater CoreRun2ChkSem.Syn_AGItf
ecuStoreCoreRunSemMod x ecu = ecu { _ecuMbCoreRunSemMod = Just x }
%%]

%%[(8 codegen corerun) export(ecuStoreCore2CoreRunSem)
ecuStoreCore2CoreRunSem :: EcuUpdater Core2CoreRunSem.Syn_CodeAGItf
ecuStoreCore2CoreRunSem x ecu = ecu { _ecuMbCore2CoreRunSem = Just x }
%%]

%%[(8 codegen tycore) export(ecuStoreTyCore)
ecuStoreTyCore :: EcuUpdater C.Module
ecuStoreTyCore x ecu = ecu { ecuMbTyCore = Just x }
%%]

%%[(8 jazy) export(ecuStoreJVMClassL)
ecuStoreJVMClassL :: EcuUpdater (HsName,[Jvm.Class])
ecuStoreJVMClassL x ecu = ecu { ecuMbJVMClassL = Just x }
%%]

%%[(8 javascript) export(ecuStoreJavaScript)
ecuStoreJavaScript :: EcuUpdater (JS.JavaScriptModule)
ecuStoreJavaScript x ecu = ecu { _ecuMbJavaScript = Just x }
%%]

ecuStoreJVMClassFPathL :: EcuUpdater [FPath]
ecuStoreJVMClassFPathL x ecu = ecu { ecuMbJVMClassL = Just (Right x) }

%%[(8 grin) export(ecuStoreGrin,ecuStoreBytecode,ecuStoreBytecodeSem)
ecuStoreGrin :: EcuUpdater Grin.GrModule
%%[[8
ecuStoreGrin x ecu = ecu { _ecuMbGrin = Just x }
%%][99
ecuStoreGrin x ecu | x `seq` True = ecu { _ecuMbGrin = Just x }
%%][9999
ecuStoreGrin x ecu | forceEval x `seq` True = ecu { _ecuMbGrin = Just x }
%%]]

ecuStoreBytecode :: EcuUpdater Bytecode.Module
%%[[8
ecuStoreBytecode x ecu = ecu { ecuMbBytecode = Just x }
%%][99
ecuStoreBytecode x ecu | x `seq` True = ecu { ecuMbBytecode = Just x }
%%][9999
ecuStoreBytecode x ecu | forceEval x `seq` True = ecu { ecuMbBytecode = Just x }
%%]]

ecuStoreBytecodeSem :: EcuUpdater PP_Doc
ecuStoreBytecodeSem x ecu = ecu { ecuMbBytecodeSem = Just x }
%%]

%%[(8 codegen cmm) export(ecuStoreCmm)
ecuStoreCmm :: EcuUpdater Cmm.Module
ecuStoreCmm x ecu = ecu { _ecuMbCmm = Just x }
%%]

%%[50 export(ecuStoreHSDeclImpS,ecuSetNeedsCompile,ecuStoreHIUsedImpS,ecuStoreHIInfoTime,ecuStoreSrcTime,ecuStoreHSSemMod,ecuStoreIntrodModS,ecuStoreHIDeclImpS,ecuStoreMod,ecuSetIsTopMod,ecuSetHasMain,ecuStoreOptim,ecuStoreHIInfo,ecuStorePrevHIInfo)
ecuStoreSrcTime :: EcuUpdater ClockTime
ecuStoreSrcTime x ecu = ecu { _ecuMbSrcTime = Just x }

-- ecuStoreHITime :: EcuUpdater ClockTime
-- ecuStoreHITime x ecu = ecu { ecuMbHITime = Just x }

ecuStoreHIInfoTime :: EcuUpdater ClockTime
ecuStoreHIInfoTime x ecu = ecu { _ecuMbHIInfoTime = Just x }

ecuStoreHSSemMod :: EcuUpdater HSSemMod.Syn_AGItf
ecuStoreHSSemMod x ecu = ecu { _ecuMbHSSemMod = Just x }

ecuStoreHSDeclImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHSDeclImpS x ecu = ecu { ecuImportUsedModules = ium {iumHSDeclModules = x} }
  where ium = ecuImportUsedModules ecu

ecuStoreHIDeclImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHIDeclImpS x ecu = ecu { ecuImportUsedModules = ium {iumHIDeclModules = x} }
  where ium = ecuImportUsedModules ecu

ecuStoreHIUsedImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHIUsedImpS x ecu = ecu { ecuImportUsedModules = ium {iumHIUsedModules = x} }
  where ium = ecuImportUsedModules ecu

ecuStoreIntrodModS :: EcuUpdater (Set.Set HsName)
ecuStoreIntrodModS x ecu = ecu { ecuImportUsedModules = ium {iumIntrodModules = x} }
  where ium = ecuImportUsedModules ecu

ecuStoreMod :: EcuUpdater Mod
ecuStoreMod x ecu = ecu { ecuMod = x }

ecuSetIsTopMod :: EcuUpdater Bool
ecuSetIsTopMod x ecu = ecu { ecuIsTopMod = x }

ecuSetHasMain :: EcuUpdater Bool
ecuSetHasMain x ecu = ecu { ecuHasMain = x }

ecuSetNeedsCompile :: EcuUpdater Bool
ecuSetNeedsCompile x ecu = ecu { ecuNeedsCompile = x }

-- ecuStorePrevHI :: EcuUpdater HI.AGItf
-- ecuStorePrevHI x ecu = ecu { ecuMbPrevHI = Just x }

-- ecuStorePrevHISem :: EcuUpdater HISem.Syn_AGItf
-- ecuStorePrevHISem x ecu = ecu { ecuMbPrevHISem = Just x }

ecuStorePrevHIInfo :: EcuUpdater HI.HIInfo
ecuStorePrevHIInfo x ecu = ecu { _ecuMbPrevHIInfo = Just x }

ecuStoreOptim :: EcuUpdater Optim
ecuStoreOptim x ecu = ecu { ecuMbOptim = Just x }

ecuStoreHIInfo :: EcuUpdater HI.HIInfo
%%[[50
ecuStoreHIInfo x ecu = ecu { _ecuMbHIInfo = Just x }
%%][99
ecuStoreHIInfo x ecu | x `seq` True = ecu { _ecuMbHIInfo = Just x }
%%]]
%%]

%%[(50 codegen) export(ecuStoreCoreTime)
ecuStoreCoreTime :: EcuUpdater ClockTime
ecuStoreCoreTime x ecu = ecu { _ecuMbCoreTime = Just x }
%%]

%%[(50 corerun) export(ecuStoreCoreRunTime)
ecuStoreCoreRunTime :: EcuUpdater ClockTime
ecuStoreCoreRunTime x ecu = ecu { _ecuMbCoreRunTime = Just x }
%%]

%%[(50 codegen grin) export(ecuStoreGrinTime)
ecuStoreGrinTime :: EcuUpdater ClockTime
ecuStoreGrinTime x ecu = ecu { _ecuMbGrinTime = Just x }
%%]

%%[50 export(ecuStoreDirIsWritable)
ecuStoreDirIsWritable :: EcuUpdater Bool
ecuStoreDirIsWritable x ecu = ecu { _ecuDirIsWritable = x }
%%]

%%[99 export(ecuStoreOpts,ecuStorePragmas,ecuStoreUsedNames,ecuSetTarget)
ecuStoreOpts :: EcuUpdater EHCOpts
ecuStoreOpts x ecu = ecu { ecuMbOpts = Just x }

ecuSetTarget :: EcuUpdater Target
ecuSetTarget x ecu = ecu { ecuTarget = x }

ecuStorePragmas :: EcuUpdater (Set.Set Pragma.Pragma)
ecuStorePragmas x ecu = ecu { ecuPragmas = x }

ecuStoreUsedNames :: EcuUpdater ModEntRelFilterMp
ecuStoreUsedNames x ecu = ecu { ecuUsedNames = x }
%%]

%%[(99 codegen) export(ecuStoreGenCodeFiles)
ecuStoreGenCodeFiles :: EcuUpdater [FPath]
ecuStoreGenCodeFiles x ecu = ecu { ecuGenCodeFiles = x }
%%]

%%[99 export(ecuStoreCppFilePath,ecuStoreSeqNr)
ecuStoreSeqNr :: EcuUpdater EHCCompileSeqNr
ecuStoreSeqNr x ecu = ecu { ecuSeqNr = x }

ecuStoreCppFilePath :: EcuUpdater FPath
ecuStoreCppFilePath x ecu = ecu { ecuMbCppFilePath = Just x }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates on EHCompileUnit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ecuSrcHasSuffix)
-- | Has the source file the given extension? Given suffix is stripped from possible prefixed '.'.
ecuSrcHasSuffix :: String -> EHCompileUnit -> Bool
ecuSrcHasSuffix suff ecu
  = maybe False (==suff') $ fpathMbSuff $ ecuSrcFilePath ecu
  where suff' = case suff of {('.':s) -> s; _ -> suff}
%%]

%%[50 export(ecuIsHSNewerThanHI)
-- | Is HS newer?
--   If no HS exists False is returned.
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case (_ecuMbSrcTime ecu,_ecuMbHIInfoTime ecu) of
      (Just ths,Just thi) -> ths `diffClockTimes` thi > noTimeDiff 
      (Nothing ,Just thi) -> False
      _                   -> True
%%]

%%[5020 export(ecuIsValidHI)
ecuIsValidHI :: EHCompileUnit -> Bool
ecuIsValidHI ecu
  = case ecuMbPrevHISem ecu of
      Just s -> HISem.isValidVersion_Syn_AGItf s
      _      -> False
%%]

%%[50 export(ecuIsValidHIInfo)
ecuIsValidHIInfo :: EHCompileUnit -> Bool
ecuIsValidHIInfo ecu
  = case _ecuMbPrevHIInfo ecu of
      Just i -> HI.hiiValidity i == HI.HIValidity_Ok
      _      -> False
%%]

%%[50 export(ecuCanUseHIInsteadOfHS)
-- | Can HI be used instead of HS?
--   This is purely based on HI being of the right version and HS not newer.
--   The need for recompilation considers dependencies on imports as well.
ecuCanUseHIInsteadOfHS :: EHCompileUnit -> Bool
ecuCanUseHIInsteadOfHS ecu
  = ecuIsValidHIInfo ecu && not (ecuIsHSNewerThanHI ecu)
%%]

%%[50 export(ecuCanCompile)
-- | Compilation can actually be done?
ecuCanCompile :: EHCompileUnit -> Bool
ecuCanCompile ecu = isJust (_ecuMbSrcTime ecu) && _ecuDirIsWritable ecu
%%]
