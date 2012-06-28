module EH101.EHC.CompileUnit
( EHCompileUnit (..)
, ecuFilePath
, emptyECU
, EcuUpdater, ecuStoreSrcFilePath, ecuStoreState, ecuStoreHS, ecuStoreEH, ecuStoreHSSem, ecuStoreEHSem
, ecuStoreCoreSem, ecuStoreCore
, ecuStoreJavaScript
, ecuStoreGrin, ecuStoreBytecode, ecuStoreBytecodeSem
, Optim (..), defaultOptim
, ecuIsMainMod
, ecuImpNmS, ecuImpNmL
, ecuTransClosedUsedModMp, ecuTransClosedOrphanModS, ecuIsOrphan
, ecuStoreHSDeclImpS, ecuSetNeedsCompile, ecuStoreHIUsedImpS, ecuStoreHIInfoTime, ecuStoreHSTime, ecuStoreHSSemMod, ecuStoreHIDeclImpS, ecuStoreMod, ecuSetIsTopMod, ecuSetHasMain, ecuStoreOptim, ecuStoreHIInfo, ecuStorePrevHIInfo
, ecuStoreCoreTime
, ecuStoreGrinTime
, ecuStoreDirIsWritable
, ecuIsHSNewerThanHI
, ecuIsValidHIInfo
, ecuCanUseHIInsteadOfHS
, EHCCompileSeqNr (..)
, ecuAnHIInfo
, ecuStoreOpts, ecuStorePragmas, ecuStoreUsedNames, ecuSetTarget
, ecuStoreGenCodeFiles, ecuStoreCppFilePath, ecuStoreSeqNr )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.EHC.Common
import qualified EH101.HS as HS
import qualified EH101.EH as EH
import qualified EH101.Core as Core
import qualified EH101.GrinCode as Grin
import qualified EH101.GrinByteCode as Bytecode
import qualified EH101.JavaScript as JS
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.Core.ToGrin as Core2GrSem
import qualified EH101.HI as HI
import qualified EH101.HS.ModImpExp as HSSemMod
import EH101.Module
import System.Time
import System.Directory
import qualified EH101.Base.Pragma as Pragma
import EH101.Base.Target
import EH.Util.Debug









{-# LINE 77 "src/ehc/EHC/CompileUnit.chs" #-}
data Optim
  = Optim
      { optimGrInlMp          :: Grin.GrInlMp        -- inlining map, from name to GrExpr (grin expressions)
      }

defaultOptim :: Optim
defaultOptim
  = Optim
      Map.empty

{-# LINE 100 "src/ehc/EHC/CompileUnit.chs" #-}
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

{-# LINE 122 "src/ehc/EHC/CompileUnit.chs" #-}
data EHCompileUnit
  = EHCompileUnit
      { ecuSrcFilePath       :: !FPath
      , ecuMbCppFilePath     :: !(Maybe FPath)
      , ecuFileLocation      :: !FileLoc
      , ecuGrpNm             :: !HsName
      , ecuModNm             :: !HsName
      , ecuMbHS              :: !(Maybe HS.AGItf)
      , ecuMbHSSem           :: !(Maybe HSSem.Syn_AGItf)
      , ecuMbEH              :: !(Maybe EH.AGItf)
      , ecuMbEHSem           :: !(Maybe EHSem.Syn_AGItf)
      , ecuMbCore            :: !(Maybe Core.CModule)
      , ecuMbCoreSem         :: !(Maybe Core2GrSem.Syn_CodeAGItf)
      , ecuMbGrin            :: !(Maybe Grin.GrModule)
      , ecuMbBytecode        :: !(Maybe Bytecode.Module)
      , ecuMbBytecodeSem     :: !(Maybe PP_Doc)
      , ecuMbJavaScript      :: !(Maybe JS.JavaScriptModule)
      , ecuState             :: !EHCompileUnitState
      , ecuHSDeclImpNmS      :: !(Set.Set HsName)                   -- imported modules as declared in src .hs
      , ecuHIDeclImpNmS      :: !(Set.Set HsName)                   -- imported modules as declared, either in .hs or .hi
      , ecuHIUsedImpNmS      :: !(Set.Set HsName)                   -- imported modules as actually used
      , ecuIsTopMod          :: !Bool                               -- module has been specified for compilation on commandline
      , ecuHasMain           :: !Bool                               -- has a def for 'main'?
      , ecuNeedsCompile      :: !Bool                               -- (re)compilation from .hs needed?
      , ecuMbHSTime          :: !(Maybe ClockTime)                  -- timestamp of possibly absent hs file
      , ecuMbHIInfoTime      :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated hi file
      , ecuMbCoreTime        :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated core file
      , ecuMbGrinTime        :: !(Maybe ClockTime)                  -- timestamp of possibly previously generated grin file
      , ecuMbHSSemMod        :: !(Maybe HSSemMod.Syn_AGItf)
      , ecuMod               :: !Mod                                -- import/export info of module
      , ecuMbPrevHIInfo      :: !(Maybe HI.HIInfo)                  -- possible HI info of previous run
      , ecuMbOptim           :: !(Maybe Optim)
      , ecuHIInfo            :: !HI.HIInfo                          -- HI info of module
      , ecuDirIsWritable     :: !Bool                               -- can be written in dir of module?
      , ecuMbOpts            :: (Maybe EHCOpts)                     -- possibly per module adaption of options (caused by pragmas)
      , ecuTarget            :: Target                              -- target for which we compile
      , ecuPragmas           :: !(Set.Set Pragma.Pragma)            -- pragmas of module
      , ecuUsedNames         :: ModEntRelFilterMp                   -- map holding actually used names, to later filter cache of imported hi's to be included in this module's hi
      , ecuGenCodeFiles      :: ![FPath]                            -- generated code fiels
      , ecuSeqNr             :: !EHCCompileSeqNr                    -- sequence nr of sorted compilation
      }

{-# LINE 193 "src/ehc/EHC/CompileUnit.chs" #-}
ecuFilePath :: EHCompileUnit -> FPath
ecuFilePath ecu
  = maybe (ecuSrcFilePath ecu) id (ecuMbCppFilePath ecu)

{-# LINE 203 "src/ehc/EHC/CompileUnit.chs" #-}
ecuIsMainMod :: EHCompileUnit -> Bool
ecuIsMainMod e = ecuIsTopMod e && ecuHasMain e

{-# LINE 208 "src/ehc/EHC/CompileUnit.chs" #-}
-- | give the current value HIInfo, or the previous one
ecuAnHIInfo :: EHCompileUnit -> HI.HIInfo
ecuAnHIInfo e
  = case ecuMbPrevHIInfo e of
      Just pi | HI.hiiIsEmpty hii
        -> pi
      _ -> hii
  where hii = ecuHIInfo e

{-# LINE 219 "src/ehc/EHC/CompileUnit.chs" #-}
emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuSrcFilePath       = emptyFPath
      , ecuMbCppFilePath     = Nothing
      , ecuFileLocation      = emptyFileLoc
      , ecuGrpNm             = hsnUnknown
      , ecuModNm             = hsnUnknown
      , ecuMbHS              = Nothing
      , ecuMbHSSem           = Nothing
      , ecuMbEH              = Nothing
      , ecuMbEHSem           = Nothing
      , ecuMbCore            = Nothing
      , ecuMbCoreSem         = Nothing
      , ecuMbGrin            = Nothing
      , ecuMbBytecode        = Nothing
      , ecuMbBytecodeSem     = Nothing
      , ecuMbJavaScript      = Nothing
      , ecuState             = ECUSUnknown
      , ecuHSDeclImpNmS      = Set.empty
      , ecuHIDeclImpNmS      = Set.empty
      , ecuHIUsedImpNmS      = Set.empty
      , ecuIsTopMod          = False
      , ecuHasMain           = False
      , ecuNeedsCompile      = True
      , ecuMbHSTime          = Nothing
      , ecuMbHIInfoTime      = Nothing
      , ecuMbCoreTime        = Nothing
      , ecuMbGrinTime        = Nothing
      , ecuMbHSSemMod        = Nothing
      , ecuMod               = emptyMod
      , ecuMbPrevHIInfo      = Nothing
      , ecuMbOptim           = Nothing
      , ecuHIInfo            = HI.emptyHIInfo
      , ecuDirIsWritable     = False
      , ecuMbOpts            = Nothing
      , ecuTarget            = defaultTarget
      , ecuPragmas           = Set.empty
      , ecuUsedNames         = Map.empty
      , ecuGenCodeFiles      = []
      , ecuSeqNr             = zeroEHCCompileSeqNr
      }

{-# LINE 294 "src/ehc/EHC/CompileUnit.chs" #-}
ecuImpNmS :: EHCompileUnit -> Set.Set HsName
ecuImpNmS ecu = Set.delete (ecuModNm ecu) $ Set.unions [ ecuHSDeclImpNmS ecu, ecuHIDeclImpNmS ecu, ecuHIUsedImpNmS ecu ]

ecuImpNmL :: EHCompileUnit -> [HsName]
ecuImpNmL = Set.toList . ecuImpNmS -- ecu = (nub $ ecuHSDeclImpNmL ecu ++ ecuHIDeclImpNmL ecu ++ ecuHIUsedImpNmL ecu) \\ [ecuModNm ecu]

{-# LINE 302 "src/ehc/EHC/CompileUnit.chs" #-}
-- | The used modules, for linking, according to .hi info
ecuTransClosedUsedModMp :: EHCompileUnit -> HI.HIInfoUsedModMp
ecuTransClosedUsedModMp = HI.hiiTransClosedUsedModMp . ecuAnHIInfo

-- | The orphan modules, must be .hi read, according to .hi info
ecuTransClosedOrphanModS :: EHCompileUnit -> Set.Set HsName
ecuTransClosedOrphanModS = HI.hiiTransClosedOrphanModS . ecuAnHIInfo

-- | Is orphan, according to .hi info
ecuIsOrphan :: EHCompileUnit -> Bool
ecuIsOrphan = isJust . HI.hiiMbOrphan . ecuAnHIInfo

{-# LINE 320 "src/ehc/EHC/CompileUnit.chs" #-}
instance CompileUnitState EHCompileUnitState where
  cusDefault      = ECUSEh EHStart
  cusUnk          = ECUSUnknown
  cusIsUnk        = (==ECUSUnknown)
{-# LINE 329 "src/ehc/EHC/CompileUnit.chs" #-}
  cusIsImpKnown s = case s of
                      ECUSHaskell HSOnlyImports  -> True
                      ECUSHaskell HIOnlyImports  -> True
                      ECUSHaskell HMOnlyMinimal  -> True
                      ECUSHaskell LHSOnlyImports -> True
                      ECUSHaskell HSAllSem       -> True
                      ECUSHaskell HIAllSem       -> True
                      _                          -> False

{-# LINE 342 "src/ehc/EHC/CompileUnit.chs" #-}
instance FileLocatable EHCompileUnit FileLoc where
  fileLocation   = ecuFileLocation
  noFileLocation = emptyFileLoc

{-# LINE 348 "src/ehc/EHC/CompileUnit.chs" #-}
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
  cuImports         = ecuImpNmL
  cuParticipation u = if not (Set.null $ Set.filter (Pragma.pragmaIsExcludeTarget $ ecuTarget u) $ ecuPragmas u)
                      then [CompileParticipation_NoImport]
                      else []

instance FPathError Err

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
      ":" >#< ppBracketsCommas (ecuImpNmL ecu) >|<
      "," >#< show (ecuState ecu)

{-# LINE 396 "src/ehc/EHC/CompileUnit.chs" #-}
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

{-# LINE 421 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreCoreSem :: EcuUpdater Core2GrSem.Syn_CodeAGItf
ecuStoreCoreSem x ecu = ecu { ecuMbCoreSem = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
ecuStoreCore x ecu | x `seq` True = ecu { ecuMbCore = Just x }

{-# LINE 445 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreJavaScript :: EcuUpdater (JS.JavaScriptModule)
ecuStoreJavaScript x ecu = ecu { ecuMbJavaScript = Just x }

{-# LINE 453 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreGrin :: EcuUpdater Grin.GrModule
ecuStoreGrin x ecu | x `seq` True = ecu { ecuMbGrin = Just x }

ecuStoreBytecode :: EcuUpdater Bytecode.Module
ecuStoreBytecode x ecu | x `seq` True = ecu { ecuMbBytecode = Just x }

ecuStoreBytecodeSem :: EcuUpdater PP_Doc
ecuStoreBytecodeSem x ecu = ecu { ecuMbBytecodeSem = Just x }

{-# LINE 481 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreHSTime :: EcuUpdater ClockTime
ecuStoreHSTime x ecu = ecu { ecuMbHSTime = Just x }

-- ecuStoreHITime :: EcuUpdater ClockTime
-- ecuStoreHITime x ecu = ecu { ecuMbHITime = Just x }

ecuStoreHIInfoTime :: EcuUpdater ClockTime
ecuStoreHIInfoTime x ecu = ecu { ecuMbHIInfoTime = Just x }

ecuStoreHSSemMod :: EcuUpdater HSSemMod.Syn_AGItf
ecuStoreHSSemMod x ecu = ecu { ecuMbHSSemMod = Just x }

ecuStoreHSDeclImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHSDeclImpS x ecu = ecu { ecuHSDeclImpNmS = x }

ecuStoreHIDeclImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHIDeclImpS x ecu = ecu { ecuHIDeclImpNmS = x }

ecuStoreHIUsedImpS :: EcuUpdater (Set.Set HsName)
ecuStoreHIUsedImpS x ecu = ecu { ecuHIUsedImpNmS = x }

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
ecuStorePrevHIInfo x ecu = ecu { ecuMbPrevHIInfo = Just x }

ecuStoreOptim :: EcuUpdater Optim
ecuStoreOptim x ecu = ecu { ecuMbOptim = Just x }

ecuStoreHIInfo :: EcuUpdater HI.HIInfo
ecuStoreHIInfo x ecu | x `seq` True = ecu { ecuHIInfo = x }

{-# LINE 537 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreCoreTime :: EcuUpdater ClockTime
ecuStoreCoreTime x ecu = ecu { ecuMbCoreTime = Just x }

{-# LINE 542 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreGrinTime :: EcuUpdater ClockTime
ecuStoreGrinTime x ecu = ecu { ecuMbGrinTime = Just x }

{-# LINE 547 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreDirIsWritable :: EcuUpdater Bool
ecuStoreDirIsWritable x ecu = ecu { ecuDirIsWritable = x }

{-# LINE 552 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreOpts :: EcuUpdater EHCOpts
ecuStoreOpts x ecu = ecu { ecuMbOpts = Just x }

ecuSetTarget :: EcuUpdater Target
ecuSetTarget x ecu = ecu { ecuTarget = x }

ecuStorePragmas :: EcuUpdater (Set.Set Pragma.Pragma)
ecuStorePragmas x ecu = ecu { ecuPragmas = x }

ecuStoreUsedNames :: EcuUpdater ModEntRelFilterMp
ecuStoreUsedNames x ecu = ecu { ecuUsedNames = x }

{-# LINE 566 "src/ehc/EHC/CompileUnit.chs" #-}
ecuStoreGenCodeFiles :: EcuUpdater [FPath]
ecuStoreGenCodeFiles x ecu = ecu { ecuGenCodeFiles = x }

ecuStoreSeqNr :: EcuUpdater EHCCompileSeqNr
ecuStoreSeqNr x ecu = ecu { ecuSeqNr = x }

ecuStoreCppFilePath :: EcuUpdater FPath
ecuStoreCppFilePath x ecu = ecu { ecuMbCppFilePath = Just x }

{-# LINE 581 "src/ehc/EHC/CompileUnit.chs" #-}
{-|
Is HS newer?
If no HS exists False is returned.

-}
{-# LINE 586 "src/ehc/EHC/CompileUnit.chs" #-}
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case (ecuMbHSTime ecu,ecuMbHIInfoTime ecu) of
      (Just ths,Just thi) -> ths `diffClockTimes` thi > noTimeDiff
      (Nothing ,Just thi) -> False
      _                   -> True

{-# LINE 603 "src/ehc/EHC/CompileUnit.chs" #-}
ecuIsValidHIInfo :: EHCompileUnit -> Bool
ecuIsValidHIInfo ecu
  = case ecuMbPrevHIInfo ecu of
      Just i -> HI.hiiValidity i == HI.HIValidity_Ok
      _      -> False

{-# LINE 611 "src/ehc/EHC/CompileUnit.chs" #-}
{-|
Can HI be used instead of HS?
This is purely based on HI being of the right version and HS not newer.
The need for recompilation considers dependencies on imports as well.

-}
{-# LINE 617 "src/ehc/EHC/CompileUnit.chs" #-}
ecuCanUseHIInsteadOfHS :: EHCompileUnit -> Bool
ecuCanUseHIInsteadOfHS ecu
  = ecuIsValidHIInfo ecu && not (ecuIsHSNewerThanHI ecu)

