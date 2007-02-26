%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, Data.List, Control.Monad, System.Console.GetOpt, IO, EH.Util.Utils,UU.Pretty,EH.Util.PPUtils,{%{EH}Error.Pretty}, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Base.Builtin}, qualified {%{EH}Config} as Cfg, {%{EH}Scanner.Common}, {%{EH}Base.Opts})
%%]

%%[1 import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.Parser} as HSPrs, qualified {%{EH}HS.MainAG} as HSSem)
%%]

%%[8 import (EH.Util.CompileRun,{%{EH}Error},{%{EH}Gam},EH.Util.FPath,qualified Data.Map as Map,Data.Maybe,Data.List, EH.Util.ParseUtils, System)
%%]

%%[8 import ({%{EH}Core.ToJava},{%{EH}Core.Pretty})
%%]

%%[8 import ({%{EH}Core.Trf.RenUniq},{%{EH}Core.Trf.FullLazy},{%{EH}Core.Trf.InlineLetAlias},{%{EH}Core.Trf.LetUnrec},{%{EH}Core.Trf.LamGlobalAsArg},{%{EH}Core.Trf.LamFloatGlobal},{%{EH}Core.Trf.LamLift},{%{EH}Core.Trf.ConstProp},{%{EH}Core.Trf.EtaRed})
%%]

%%[8 import(qualified {%{EH}GrinCode} as Grin, {%{EH}GrinCode.Pretty}, qualified {%{EH}GrinCode.Parser} as GrinParser, {%{GRIN}GrinCode.ToGrinByteCode})
%%]

%%[8 import({%{GRIN}GrinCode.TrfLocal.UnusedMetaInfoElim},{%{GRIN}GrinCode.TrfLocal.UnusedNameElim},{%{GRIN}GrinCode.TrfLocal.AliasElim},{%{GRIN}GrinCode.TrfLocal.Unbox},{%{GRIN}GrinCode.TrfLocal.FlattenSeq},{%{GRIN}GrinCode.TrfLocal.EvalElim},{%{GRIN}GrinCode.TrfLocal.Inline})
%%]

%%[8 import ({%{GRIN}GrinByteCode.ToC}, qualified {%{GRIN}GrinByteCode} as GrinBC)
%%]

%%[8 import (qualified {%{GRIN}CompilerDriver} as GRINC, qualified {%{GRIN}GRINCCommon} as GRINCCommon)
%%]

%%[8 import (qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[8 import (qualified {%{EH}HS} as HS)
%%]
%%[8 import (qualified {%{EH}EH} as EH)
%%]
%%[8 import (qualified {%{EH}Core} as Core)
%%]
%%[8 import (Control.Monad.State)
%%]
%%[8 import (qualified EH.Util.ScanUtils as ScanUtils)
%%]

%%[9 import ({%{EH}Pred})
%%]

%%[12 import (qualified EH.Util.Rel as Rel, qualified EH.Util.FastSeq as Seq, qualified Data.Set as Set)
%%]

%%[12 import (System.Time, System.Directory)
%%]

%%[12 import (qualified {%{EH}HI.Parser} as HIPrs, {%{EH}Module}, qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]

%%[12 import (qualified {%{EH}Core.Parser} as CorePrs)
%%]

%%[12 import (qualified {%{EH}Pred} as Pr,qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  ehcOpts        = defaultEHCOpts
%%[[99
                                    {ehcProgName = progName}
%%]]
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldl (flip ($)) ehcOpts o
         ;  if ehcOptHelp opts
%%]
%%[1.main.ehcOptHelp
            then  putStrLn (usageInfo ("version: " ++ Cfg.verInfo Cfg.version ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
%%]
%%[8.main.ehcOptHelp -1.main.ehcOptHelp
            then  do  {  putStrLn (usageInfo ("version: " ++ Cfg.verInfo Cfg.version ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
                      }
%%]
%%[1.main.tl
            else  if ehcOptVersion opts
            then  putStrLn (Cfg.verInfo Cfg.version)
%%[[99
            else  if ehcOptShowNumVersion opts
            then  putStrLn (Cfg.verNumeric Cfg.version)
%%]]
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
mkParseErrInfoL :: (Eq s, Show s) => [Message s (Maybe Token)] -> ErrL
mkParseErrInfoL = map (\(Msg exp pos act) -> Err_Parse (show exp) (show act))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileGroup
  = EHCompileGroup
      { ecgNm                :: HsName
      , ecgModL              :: [HsName]
      }

emptyECG :: EHCompileGroup
emptyECG
  = EHCompileGroup
      { ecgNm                = hsnUnknown
      , ecgModL              = []
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Inter module optimisation info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Intermodule optimisation info.
Currently only for Grin meant to be compiled to GrinByteCode.
Absence of this info should not prevent correct compilation.

%%[12
data Optim
  = Optim
      { optimGrInlMp          :: Grin.GrInlMp        -- inlining map, from name to GrExpr (grin expressions)
      }

defaultOptim :: Optim
defaultOptim = Optim Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data HSState
  = HSStart
%%[[99
  | LHSStart
  | LHSOnlyImports
%%]]
  | HSAllSem
%%[[12
  | HSAllSemHI
  | HSOnlyImports
%%]]
  deriving (Show,Eq)
%%]

%%[8
data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)
%%]

%%[8
data EHCompileUnitState
  = ECUSUnknown
  | ECUSHaskell HSState
  | ECUSEh      EHState
  | ECUSGrin
  | ECUSFail
  deriving (Show,Eq)
%%]

%%[8
data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: FPath
      , ecuGrpNm             :: HsName
      , ecuModNm             :: HsName
      , ecuImpNmL            :: [HsName]
      , ecuMbHSSem           :: (Maybe HSSem.Syn_AGItf)
      , ecuMbEHSem           :: (Maybe EHSem.Syn_AGItf)
      , ecuMbCoreSem         :: (Maybe Core2GrSem.Syn_CodeAGItf)
      , ecuMbHS              :: (Maybe HS.AGItf)
      , ecuMbEH              :: (Maybe EH.AGItf)
      , ecuMbCore            :: (Maybe Core.CModule)
      , ecuMbGrin            :: (Maybe Grin.GrModule)
      , ecuMbGrinBC          :: (Maybe GrinBC.Module)
      , ecuState             :: EHCompileUnitState
%%[[12
      , ecuIsTopMod          :: Bool
      , ecuMbHSTime          :: (Maybe ClockTime)
      , ecuMbHITime          :: (Maybe ClockTime)
      , ecuMbCoreTime        :: (Maybe ClockTime)
      , ecuMbHSSemMod        :: (Maybe HSSemMod.Syn_AGItf)
      , ecuMod               :: Mod
      -- , ecuMbPrevCore        :: Maybe Core.CModule
      , ecuMbPrevHISem       :: (Maybe HISem.Syn_AGItf)
      , ecuMbPrevHI          :: (Maybe HI.AGItf)
      , ecuMbOptim           :: (Maybe Optim)
%%]
      }

%%]

%%[8
emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuGrpNm             = hsnUnknown
      , ecuModNm             = hsnUnknown
      , ecuImpNmL            = []
      , ecuMbHSSem           = Nothing
      , ecuMbEHSem           = Nothing
      , ecuMbCoreSem         = Nothing
      , ecuMbHS              = Nothing
      , ecuMbEH              = Nothing
      , ecuMbCore            = Nothing
      , ecuMbGrin            = Nothing
      , ecuMbGrinBC          = Nothing
      , ecuState             = ECUSUnknown
%%[[12
      , ecuIsTopMod          = False
      , ecuMbHSTime          = Nothing
      , ecuMbHITime          = Nothing
      , ecuMbCoreTime        = Nothing
      , ecuMbHSSemMod        = Nothing
      , ecuMod               = emptyMod
      -- , ecuMbPrevCore        = Nothing
      , ecuMbPrevHISem       = Nothing
      , ecuMbPrevHI          = Nothing
      , ecuMbOptim           = Nothing
%%]
      }
%%]

%%[8
instance CompileUnitState EHCompileUnitState where
  cusDefault      = ECUSEh EHStart
  cusUnk          = ECUSUnknown
  cusIsUnk        = (==ECUSUnknown)
%%]
%%[8.cusIsImpKnown
  cusIsImpKnown _ = True
%%]
%%[12 -8.cusIsImpKnown
  cusIsImpKnown s = case s of
                      ECUSHaskell HSOnlyImports  -> True
%%[[99
                      ECUSHaskell LHSOnlyImports -> True
%%]]
                      ECUSHaskell HSAllSem       -> True
                      ECUSHaskell HSAllSemHI     -> True
                      _                          -> False
%%]

%%[8
instance CompileUnit EHCompileUnit HsName EHCompileUnitState where
  cuDefault         = emptyECU
  cuFPath           = ecuFilePath
  cuKey             = ecuModNm
  cuState           = ecuState
  cuUpdFPath        = ecuStoreFilePath
  cuUpdState        = ecuStoreState
  cuUpdKey   nm u   = u {ecuModNm = nm}
  cuImports         = ecuImpNmL

instance CompileRunError Err () where
  crePPErrL                 = ppErrL
  creMkNotFoundErrL _ fp sp = [Err_FileNotFound fp sp]
  creAreFatal               = errLIsFatal

instance CompileModName HsName where
  mkCMNm = HNm

instance Show EHCompileUnit where
  show _ = "EHCompileUnit"

instance PP EHCompileUnit where
  pp ecu
    = ecuModNm ecu >|<
%%[[12
      ":" >#< ppCommas (ecuImpNmL ecu) >|<
%%]]
      "," >#< show (ecuState ecu)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Storing into an EHCompileUnit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type EcuUpdater a = a -> EHCompileUnit -> EHCompileUnit

ecuStoreFilePath :: EcuUpdater FPath
ecuStoreFilePath x ecu = ecu { ecuFilePath = x }

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

ecuStoreCoreSem :: EcuUpdater Core2GrSem.Syn_CodeAGItf
ecuStoreCoreSem x ecu = ecu { ecuMbCoreSem = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }

ecuStoreGrin :: EcuUpdater Grin.GrModule
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }

ecuStoreGrinBC :: EcuUpdater GrinBC.Module
ecuStoreGrinBC x ecu = ecu { ecuMbGrinBC = Just x }
%%]

%%[12
ecuStoreHSTime :: EcuUpdater ClockTime
ecuStoreHSTime x ecu = ecu { ecuMbHSTime = Just x }

ecuStoreHITime :: EcuUpdater ClockTime
ecuStoreHITime x ecu = ecu { ecuMbHITime = Just x }

ecuStoreCoreTime :: EcuUpdater ClockTime
ecuStoreCoreTime x ecu = ecu { ecuMbCoreTime = Just x }

ecuStoreHSSemMod :: EcuUpdater HSSemMod.Syn_AGItf
ecuStoreHSSemMod x ecu = ecu { ecuMbHSSemMod = Just x }

ecuStoreImpL :: EcuUpdater [HsName]
ecuStoreImpL x ecu = ecu { ecuImpNmL = x }

ecuStoreMod :: EcuUpdater Mod
ecuStoreMod x ecu = ecu { ecuMod = x }

ecuSetIsTopMod :: EcuUpdater Bool
ecuSetIsTopMod x ecu = ecu { ecuIsTopMod = x }

{-
ecuStorePrevCore :: EcuUpdater Core.CModule
ecuStorePrevCore x ecu = ecu { ecuMbPrevCore = Just x }
-}

ecuStorePrevHI :: EcuUpdater HI.AGItf
ecuStorePrevHI x ecu = ecu { ecuMbPrevHI = Just x }

ecuStorePrevHISem :: EcuUpdater HISem.Syn_AGItf
ecuStorePrevHISem x ecu = ecu { ecuMbPrevHISem = Just x }

ecuStoreOptim :: EcuUpdater Optim
ecuStoreOptim x ecu = ecu { ecuMbOptim = Just x }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: EHCOpts                              -- options
      , crsiHSInh       :: HSSem.Inh_AGItf                      -- current inh attrs for HS sem
      , crsiEHInh       :: EHSem.Inh_AGItf                      -- current inh attrs for EH sem
      , crsiCoreInh     :: Core2GrSem.Inh_CodeAGItf             -- current inh attrs for Core2Grin sem
      , crsiNextUID     :: UID                                  -- unique id, the next one
      , crsiHereUID     :: UID                                  -- unique id, the current one
%%[[12
      , crsiHIInh       :: HISem.Inh_AGItf                      -- current inh attrs for HI sem
      , crsiHSModInh    :: HSSemMod.Inh_AGItf                   -- current inh attrs for HS module analysis sem
      , crsiModMp       :: ModMp                                -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: Optim                                -- inter module optimisation info
      , crsiModOffMp    :: Core.HsName2OffsetMpMp               -- mapping of all modules + exp entries to offsets in module + exp tables
%%]
      }
%%]

%%[12
crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> Core.HsName2OffsetMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust "crsiExpNmOffMp" $ Map.lookup modNm $ crsiModMp crsi
%%]

%%[12
instance Show EHCompileRunStateInfo where
  show _ = "EHCompileRunStateInfo"

instance PP EHCompileRunStateInfo where
  pp i = "CRSI:" >#< ppModMp (crsiModMp i)
%%]

%%[8
instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()

type EHCompileRun     = CompileRun   HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = [(String,EHCompileUnitState)]

fileSuffMpHs :: FileSuffMp
fileSuffMpHs
  = [ ( "hs"  , ECUSHaskell HSStart )
%%[[99
    , ( "lhs" , ECUSHaskell LHSStart )
%%]]
    , ( "eh"  , ECUSEh EHStart )
    , ( "grin", ECUSGrin )
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpParseOffside :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> HsName -> EHCompilePhase ()
cpParseOffside parser scanOpts store description modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let (res,msgs) = parseOffsideToResMsgs parser tokens
            errs       = map mkPPErr msgs
      ; cpUpdCU modNm (store res)
      ; cpSetLimitErrsWhen 5 description errs
      }

cpParsePlain' :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> FPath -> HsName -> EHCompilePhase [Err]
cpParsePlain' parser scanOpts store fp modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath fp ReadMode
      ; tokens  <- lift $ scanHandle scanOpts fn fh
      ; let (res,msgs) = parseToResMsgs parser tokens
            errs       = map mkPPErr msgs
      ; when (null errs)
             (cpUpdCU modNm (store res))
      ; return errs
      }

cpParsePlain :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> FPath -> HsName -> EHCompilePhase ()
cpParsePlain parser scanOpts store description fp modNm
 = do { errs <- cpParsePlain' parser scanOpts store fp modNm
      ; cpSetLimitErrsWhen 5 description errs
      }

cpParseEH :: HsName -> EHCompilePhase ()
cpParseEH
  = cpParseOffside EHPrs.pAGItf ehScanOpts ecuStoreEH "Parse (EH syntax) of module"

cpParseGrin :: HsName -> EHCompilePhase ()
cpParseGrin modNm
  = do { cr <- get
       ; cpParsePlain GrinParser.pModule grinScanOpts ecuStoreGrin "Parse grin" (ecuFilePath (crCU modNm cr)) modNm
       }
%%]

%%[8.cpParseHs
cpParseHs :: HsName -> EHCompilePhase ()
cpParseHs = cpParseOffside HSPrs.pAGItf hsScanOpts ecuStoreHS "Parse (Haskell syntax) of module"
%%]

%%[99 -8.cpParseHs
cpParseHs :: Bool -> HsName -> EHCompilePhase ()
cpParseHs litmode
  = cpParseOffside HSPrs.pAGItf (hsScanOpts {ScanUtils.scoLitmode = litmode}) ecuStoreHS
                   ("Parse (" ++ (if litmode then "Literate " else "") ++ "Haskell syntax) of module")
%%]

%%[12
cpParseOffsideStopAtErr :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> HsName -> EHCompilePhase ()
cpParseOffsideStopAtErr parser scanOpts store modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let (res,_) = parseOffsideToResMsgsStopAtErr parser tokens
      ; cpUpdCU modNm (store res)
      }
%%]

%%[12.cpParseHsImport
cpParseHsImport :: HsName -> EHCompilePhase ()
cpParseHsImport = cpParseOffsideStopAtErr HSPrs.pAGItfImport hsScanOpts ecuStoreHS
%%]

%%[99 -12.cpParseHsImport
cpParseHsImport :: Bool -> HsName -> EHCompilePhase ()
cpParseHsImport litmode = cpParseOffsideStopAtErr HSPrs.pAGItfImport (hsScanOpts {ScanUtils.scoLitmode = litmode}) ecuStoreHS
%%]

%%[12
cpParseCore :: HsName -> EHCompilePhase ()
cpParseCore modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff "core" fp
       ; lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Parsing" Nothing modNm fpC
       ; cpParsePlain CorePrs.pCModule coreScanOpts ecuStoreCore "Parse Core (of previous compile) of module" fpC modNm
       }

cpParsePrevHI :: HsName -> EHCompilePhase ()
cpParsePrevHI modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpH     = fpathSetSuff "hi" fp
       ; lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Parsing" Nothing modNm fpH
       ; errs <- cpParsePlain' HIPrs.pAGItf hiScanOpts ecuStorePrevHI fpH modNm
       -- ; cpSetLimitErrsWhen 5 "Parse HI (of previous compile) of module" errs
       ; return ()
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile Run base info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
crBaseInfo' :: EHCompileRun -> (EHCompileRunStateInfo,EHCOpts)
crBaseInfo' cr
  = (crsi,opts)
  where crsi   = crStateInfo cr
        opts   = crsiOpts  crsi

crBaseInfo :: HsName -> EHCompileRun -> (EHCompileUnit,EHCompileRunStateInfo,EHCOpts,FPath)
crBaseInfo modNm cr
  = (ecu,crsi,opts,fp)
  where ecu         = crCU modNm cr
        (crsi,opts) = crBaseInfo' cr
        fp          = ecuFilePath ecu
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: computing semantics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
foldCore :: Core2GrSem.Inh_CodeAGItf -> EHCompileUnit -> EHCompileRunStateInfo -> Core.CModule -> Core2GrSem.Syn_CodeAGItf
foldCore coreInh ecu crsi core
 = Core2GrSem.wrap_CodeAGItf
     (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
     (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf            = crsiHereUID crsi
              , Core2GrSem.opts_Inh_CodeAGItf             = crsiOpts crsi
              })
%%]

%%[8
foldEH :: HSSem.Inh_AGItf -> EHSem.Inh_AGItf -> EHCompileUnit -> EHCompileRunStateInfo -> EH.AGItf -> EHSem.Syn_AGItf
foldEH _ ehInh ecu crsi eh
 = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                    (ehInh { EHSem.moduleNm_Inh_AGItf         = ecuModNm ecu
                           , EHSem.gUniq_Inh_AGItf            = crsiHereUID crsi
                           , EHSem.opts_Inh_AGItf             = crsiOpts crsi
%%[[12
                           , EHSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
%%]]
                           })
%%]

%%[8
foldHs :: HSSem.Inh_AGItf -> HsName -> EHCompileUnit -> EHCompileRunStateInfo -> HS.AGItf -> HSSem.Syn_AGItf
foldHs inh modNm ecu crsi hs
 = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
                    (inh { HSSem.opts_Inh_AGItf             = crsiOpts crsi
                         , HSSem.gUniq_Inh_AGItf            = crsiHereUID crsi
%%[[12
                         , HSSem.moduleNm_Inh_AGItf         = modNm
                         , HSSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
                         , HSSem.modInScope_Inh_AGItf       = inscps
                         , HSSem.modEntToOrig_Inh_AGItf     = exps
                         , HSSem.topInstanceNmL_Inh_AGItf   = modInstNmL (ecuMod ecu)
%%]]
                         })
%%[[12
 where mmi    = panicJust "foldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
       inscps = Rel.toDomMap $ mmiInscps $ mmi
       exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                             $ Rel.mapRng mentIdOcc $ mmiExps $ mmi
%%]]
%%]

%%[12
foldHsMod :: HSSemMod.Inh_AGItf -> HsName -> EHCompileUnit -> EHCompileRunStateInfo -> HS.AGItf -> HSSemMod.Syn_AGItf
foldHsMod inh modNm ecu crsi hs
 = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf hs)
                       (inh { HSSemMod.gUniq_Inh_AGItf        = crsiHereUID crsi
                            , HSSemMod.moduleNm_Inh_AGItf     = modNm
                            })
%%]

%%[12
foldHI :: HISem.Inh_AGItf -> EHCompileUnit -> EHCompileRunStateInfo -> HI.AGItf -> HISem.Syn_AGItf
foldHI inh ecu crsi hi
 = HISem.wrap_AGItf (HISem.sem_AGItf hi)
                    (inh { HISem.opts_Inh_AGItf             = crsiOpts crsi
                         })
%%]

%%[8
cpFoldCore :: HsName -> EHCompilePhase ()
cpFoldCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbCore   = ecuMbCore ecu
                 coreSem  = foldCore (crsiCoreInh crsi) ecu crsi (panicJust "cpFoldCore" mbCore)
         ;  when (isJust mbCore)
                 (cpUpdCU modNm (ecuStoreCoreSem coreSem))
         }

cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = foldEH (crsiHSInh crsi) (crsiEHInh crsi) ecu crsi (panicJust "cpFoldEH" mbEH)
         ;  when (isJust mbEH)
                 (cpUpdCU modNm (ecuStoreEHSem ehSem))
         }

cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHS   = ecuMbHS ecu
                 hsSem  = foldHs (crsiHSInh crsi) modNm ecu crsi (panicJust "cpFoldHs" mbHS)
         ;  when (isJust mbHS)
                 (cpUpdCU modNm (ecuStoreHSSem hsSem))
         }
%%]

%%[12
cpFoldHsMod :: HsName -> EHCompilePhase ()
cpFoldHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHS       = ecuMbHS ecu
                 hsSemMod   = foldHsMod (crsiHSModInh crsi) modNm ecu crsi (panicJust "cpFoldHsMod" mbHS)
         ;  when (isJust mbHS)
                 (cpUpdCU modNm (ecuStoreHSSemMod hsSemMod))
         }
%%]

%%[12
cpFoldHI :: HsName -> EHCompilePhase ()
cpFoldHI modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHI   = ecuMbPrevHI ecu
                 hiSem  = foldHI (crsiHIInh crsi) ecu crsi (panicJust "cpFoldHI" mbHI)
         ;  when (isJust mbHI && HISem.isValidVersion_Syn_AGItf hiSem)
                 (do { let mm     = crsiModMp crsi
                           mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                           mmi'   = mkModMpInfo (mmiInscps mmi) (HISem.exportRel_Syn_AGItf hiSem)
                     ; put (cr {crStateInfo = crsi {crsiModMp = Map.insert modNm mmi' mm}})
                     ; cpUpdCU modNm (ecuStorePrevHISem hiSem)
                     })
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: flowing info between module compiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
cpFlowHsSem1 :: HsName -> EHCompilePhase ()
cpFlowHsSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 hsSem  = panicJust "cpFlowHsSem1" $ ecuMbHSSem ecu
                 ehInh  = crsiEHInh crsi
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.idGam_Inh_AGItf      = HSSem.gathIdGam_Syn_AGItf                 hsSem `gamUnion` HSSem.idGam_Inh_AGItf     hsInh
                            , HSSem.fixityGam_Inh_AGItf  = HSSem.gathFixityGam_Syn_AGItf             hsSem `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                            }
                 ehInh' = ehInh
                            { EHSem.idQualGam_Inh_AGItf  = idGam2QualGam (HSSem.gathIdGam_Syn_AGItf hsSem) `gamUnion` EHSem.idQualGam_Inh_AGItf ehInh
%%[[95
                            -- , EHSem.fixityGam_Inh_AGItf  = HSSem.fixityGam_Inh_AGItf hsInh'
%%]]
                            }
                 opts'  = opts
                            { ehcOptBuiltinNames = mkEHBuiltinNames mk
                            }
%%[[12
                        where mk = idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh')
%%][99
                        where mk = if ehcOptUseAssumePrelude opts
                                   then \_ n -> n
                                   else \k n -> idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh') k (hsnQualified n)
%%]]
         ;  when (isJust (ecuMbHSSem ecu))
                 (put (cr {crStateInfo = crsi {crsiHSInh = hsInh', crsiEHInh = ehInh', crsiOpts = opts'}}))
         -- ;  lift $ putWidthPPLn 120 (ppGam $ EHSem.idQualGam_Inh_AGItf $ ehInh')
         }

cpFlowHsSem2 :: HsName -> EHCompilePhase ()
cpFlowHsSem2 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 hsSem  = panicJust "cpFlowHsSem2" $ ecuMbHSSem ecu
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = HSSem.gathFixityGam_Syn_AGItf hsSem `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                            }
         ;  when (isJust (ecuMbHSSem ecu))
                 (put (cr {crStateInfo = crsi {crsiHSInh = hsInh'}}))
         }
%%]

%%[12
cpFlowCore2GrSem :: HsName -> EHCompilePhase ()
cpFlowCore2GrSem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 ehSem    = panicJust "cpFlowCore2GrSem.ehSem" $ ecuMbEHSem ecu
                 ehInh    = crsiEHInh crsi
                 coreSem  = panicJust "cpFlowCore2GrSem.coreSem" $ ecuMbCoreSem ecu
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.arityMp_Inh_CodeAGItf   = Core2GrSem.gathArityMp_Syn_CodeAGItf coreSem `Map.union` Core2GrSem.arityMp_Inh_CodeAGItf coreInh
                              }
         ;  when (isJust (ecuMbCoreSem ecu))
                 (put (cr {crStateInfo = crsi {crsiCoreInh = coreInh'}}))
         }
%%]

%%[8
cpFlowEHSem1 :: HsName -> EHCompilePhase ()
cpFlowEHSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 ehSem    = panicJust "cpFlowEHSem1.ehSem" $ ecuMbEHSem ecu
                 ehInh    = crsiEHInh crsi
                 coreSem  = panicJust "cpFlowEHSem1.coreSem" $ ecuMbCoreSem ecu
                 coreInh  = crsiCoreInh crsi
%%[[12
                 ehInh'   = ehInh
                              { EHSem.dataGam_Inh_AGItf    = EHSem.gathDataGam_Syn_AGItf    ehSem `gamUnion` EHSem.dataGam_Inh_AGItf    ehInh
                              }
%%]]
                 coreInh' = coreInh
%%[[8
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf    ehSem
%%][12
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf        ehInh'
%%]]
                              }
         ;  when (isJust (ecuMbEHSem ecu))
                 (put (cr {crStateInfo = crsi { crsiCoreInh = coreInh'
%%[[12
                                              , crsiEHInh = ehInh'
%%]]
                                              }}))
         }
%%]

%%[12
cpFlowEHSem2 :: HsName -> EHCompilePhase ()
cpFlowEHSem2 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 ehSem  = panicJust "cpFlowEHSem2.ehSem" $ ecuMbEHSem ecu
                 ehInh  = crsiEHInh crsi
                 hsSem  = panicJust "cpFlowEHSem2.hsSem" $ ecuMbHSSem ecu
                 hsInh  = crsiHSInh crsi
                 ehInh' = ehInh
                            { EHSem.valGam_Inh_AGItf     = EHSem.gathValGam_Syn_AGItf     ehSem `gamUnion` EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = EHSem.gathTyGam_Syn_AGItf      ehSem `gamUnion` EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.kiGam_Inh_AGItf      = EHSem.gathKiGam_Syn_AGItf      ehSem `gamUnion` EHSem.kiGam_Inh_AGItf      ehInh
                            -- , EHSem.dataGam_Inh_AGItf    = EHSem.gathDataGam_Syn_AGItf    ehSem `gamUnion` EHSem.dataGam_Inh_AGItf    ehInh -- now in cpFlowEHSem1
                            , EHSem.prIntroGam_Inh_AGItf = EHSem.gathPrIntroGam_Syn_AGItf ehSem `gamUnion` EHSem.prIntroGam_Inh_AGItf ehInh
                            , EHSem.prElimTGam_Inh_AGItf = Pr.peTGamUnion basePrfCtxtId (EHSem.prfCtxtId_Inh_AGItf ehInh) (EHSem.gathPrElimTGam_Syn_AGItf ehSem) (EHSem.prElimTGam_Inh_AGItf ehInh)
                            }
         ;  when (isJust (ecuMbEHSem ecu))
                 (put (cr {crStateInfo = crsi {crsiEHInh = ehInh'}}))
         }
%%]

%%[12
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 ehInh  = crsiEHInh crsi
                 ehInh' = ehInh
                            { EHSem.valGam_Inh_AGItf     = HISem.valGam_Syn_AGItf     hiSem `gamUnion` EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = HISem.tyGam_Syn_AGItf      hiSem `gamUnion` EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.dataGam_Inh_AGItf    = HISem.dataGam_Syn_AGItf    hiSem `gamUnion` EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.prIntroGam_Inh_AGItf = HISem.prIntroGam_Syn_AGItf hiSem `gamUnion` EHSem.prIntroGam_Inh_AGItf ehInh
                            , EHSem.prElimTGam_Inh_AGItf = Pr.peTGamUnion basePrfCtxtId (EHSem.prfCtxtId_Inh_AGItf ehInh) (HISem.prElimTGam_Syn_AGItf hiSem) (EHSem.prElimTGam_Inh_AGItf ehInh)
                            }
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = HISem.fixityGam_Syn_AGItf hiSem `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = HISem.idGam_Syn_AGItf     hiSem `gamUnion` HSSem.idGam_Inh_AGItf     hsInh
                            }
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.arityMp_Inh_CodeAGItf   = HISem.arityMp_Syn_AGItf hiSem `Map.union` Core2GrSem.arityMp_Inh_CodeAGItf coreInh
                              }
                 optim    = crsiOptim crsi
                 optim'   = optim
                              { optimGrInlMp   = HISem.inlMp_Syn_AGItf hiSem `Map.union` optimGrInlMp optim
                              }
         ;  when (isJust (ecuMbPrevHISem ecu))
                 (do { put (cr {crStateInfo = crsi {crsiEHInh = ehInh', crsiHSInh = hsInh', crsiCoreInh = coreInh', crsiOptim = optim'}})
                     })
         }
%%]

%%[12
cpFlowOptim :: HsName -> EHCompilePhase ()
cpFlowOptim modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 optim  = crsiOptim crsi
                 moptim = panicJust "cpFlowOptim" $ ecuMbOptim ecu
                 optim' = optim
                            { optimGrInlMp = optimGrInlMp moptim `Map.union` optimGrInlMp optim
                            }
         ;  when (isJust (ecuMbOptim ecu))
                 (do { put (cr {crStateInfo = crsi {crsiOptim = optim'}})
                     })
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: get meta info, imports, time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpSetupMod
cpSetupMod :: HsName -> EHCompilePhase ()
cpSetupMod _ = return ()
%%]

%%[12 -8.cpSetupMod
cpSetupMod :: HsName -> EHCompilePhase ()
cpSetupMod modNm
  = cpSeq [cpGetModfTimes modNm,cpGetPrevHI modNm,cpFoldHI modNm]
%%]

%%[12
cpGetPrevHI :: HsName -> EHCompilePhase ()
cpGetPrevHI modNm
  = do { cr <- get
       ; let  ecu        = crCU modNm cr
       ; when (isJust (ecuMbHITime ecu))
              (cpParsePrevHI modNm)
       }
%%]

%%[12
cpGetPrevCore :: HsName -> EHCompilePhase ()
cpGetPrevCore modNm
  = do { cr <- get
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbCoreTime ecu) && isNothing (ecuMbCore ecu))
              (cpParseCore modNm)
       }
%%]

%%[12
cpGetHsImports :: HsName -> EHCompilePhase ()
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm
                  $ ecuStoreImpL
                  $ HSSemMod.modImpNmL_Syn_AGItf hsSemMod
                 )
         -- ; lift $ putWidthPPLn 120 (pp mod)
         }

cpGetHsMod :: HsName -> EHCompilePhase ()
cpGetHsMod modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsMod" mbHsSemMod
                 mod        = HSSemMod.mod_Syn_AGItf hsSemMod
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm (ecuStoreMod mod))
         -- ; lift $ putWidthPPLn 120 (pp mod)
         }

cpGetModfTimes :: HsName -> EHCompilePhase ()
cpGetModfTimes modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
         ;  tm ecuStoreHSTime   (                      ecuFilePath ecu)
         ;  tm ecuStoreHITime   (fpathSetSuff "hi"   $ ecuFilePath ecu)
         ;  tm ecuStoreCoreTime (fpathSetSuff "core" $ ecuFilePath ecu)
         }
  where tm store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when nExists
                      (do { t <- lift $ getModificationTime n
                          ; cpUpdCU modNm $ store t
                          })
               }
%%]

%%[12
crPartitionNewerOlderImports :: HsName -> EHCompileRun -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ ecuMbHITime ecu
        isNewer ecu'
            = t' `diffClockTimes` t > noTimeDiff 
            where t' = panicJust "crPartitionNewerOlderImports2" $ ecuMbHITime ecu'
%%]

%%[12
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case ecuMbHITime ecu of
      Just thi -> ths `diffClockTimes` thi > noTimeDiff 
               where ths = panicJust "ecuIsHSNewerThanHI" $ ecuMbHSTime ecu
      _        -> True
%%]

%%[12
ecuIsValidHI :: EHCompileUnit -> Bool
ecuIsValidHI ecu
  = case ecuMbPrevHISem ecu of
      Just s -> HISem.isValidVersion_Syn_AGItf s
      _      -> False
%%]

%%[12
crModNeedsCompile :: HsName -> EHCompileRun -> Bool
crModNeedsCompile modNm cr
  = ecuIsTopMod ecu
    || (not $ ehcOptCheckRecompile $ crsiOpts $ crStateInfo cr)
    || ecuIsHSNewerThanHI ecu
    || not (ecuIsValidHI ecu)
    || not (null newer)
  where ecu = crCU modNm cr
        (newer,_) = crPartitionNewerOlderImports modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile/checking actions: compile phase (fold + output)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
cpCheckMods' :: [Mod] -> EHCompilePhase ()
cpCheckMods' modL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             (mm,e) = modMpCombine modL (crsiModMp crsi)
       ; when (ehcOptVerbosity (crsiOpts crsi) >= VerboseDebug)
              (lift $ putWidthPPLn 120 (pp (head modL) >-< ppModMp mm)) -- debug
       ; put (cr {crStateInfo = crsi {crsiModMp = mm}})
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }

cpCheckMods :: [HsName] -> EHCompilePhase ()
cpCheckMods modNmL
  = do { cr <- get
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckMods' modL
       }
  where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }

cpUpdateModOffMp :: [HsName] -> EHCompilePhase ()
cpUpdateModOffMp modNmL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             offMp  = crsiModOffMp crsi
             offMp' = Map.fromList [ (m,(o,crsiExpNmOffMp m crsi)) | (m,o) <- zip modNmL [Map.size offMp ..] ] `Map.union` offMp
       ; put (cr {crStateInfo = crsi {crsiModOffMp = offMp'}})
       }
%%]

-- create dummy module info for .eh's
%%[12
cpGetCheckEhMod :: HsName -> EHCompilePhase ()
cpGetCheckEhMod modNm
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty []
       ; cpUpdCU modNm (ecuStoreMod mod)
       ; put (cr {crStateInfo = crsi {crsiModMp = Map.insert modNm emptyModMpInfo mm}})
       }
%%]

%%[8
cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbHsSem= ecuMbHSSem ecu
                 hsSem  = panicJust "cpTranslateHs2EH" mbHsSem
                 eh     = HSSem.eh_Syn_AGItf hsSem
                 errs   = HSSem.errL_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (do { cpUpdCU modNm (ecuStoreEH eh)
                     ; cpSetLimitErrsWhen 5 "Dependency/name analysis" errs
                     ; when (ehcOptEmitHS opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "hs2" fp)) (HSSem.pp_Syn_AGItf hsSem) 1000)
                     ; when (ehcOptShowHS opts)
                            (lift $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
                     })
         }
%%]

%%[8
cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 core   = EHSem.cmodule_Syn_AGItf ehSem
                 errs   = EHSem.allErrL_Syn_AGItf ehSem
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreCore core)
                     ; cpSetLimitErrsWhen 5 "Type checking" errs
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "eh2" fp)) (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
                     ; when (ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
                     }
                 )
         }
%%]

%%[8
cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCoreSem = ecuMbCoreSem ecu
                 coreSem   = panicJust "cpTranslateCore2Grin" mbCoreSem
                 grin      = Core2GrSem.grMod_Syn_CodeAGItf coreSem
         ;  when (isJust mbCoreSem && (ehcOptEmitGrin opts || ehcOptEmitGrinBC opts || ehcOptEmitLlc opts || ehcOptEmitLLVM opts))
                 (cpUpdCU modNm (ecuStoreGrin grin))
         }
%%]

%%[8
cpOptimizeGrin :: HsName -> EHCompilePhase ()
cpOptimizeGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpOptimizeGrin" mbGrin
                 optGrin   = ( grUnusedNameElim   

                             . grAliasElim   

                             . grEvalElim   

                             . grAliasElim   

                             . grFlattenSeq   

                             ) grin 

         ;  when (  ehcOptOptimise opts >= OptimiseNormal
                 && isJust mbGrin 
                 && (ehcOptEmitLlc opts || ehcOptEmitLLVM opts)
                 )
                 (cpUpdCU modNm (ecuStoreGrin optGrin))
         }
%%]

%%[8
cpTranslateGrin2ByteCode :: HsName -> EHCompilePhase ()
cpTranslateGrin2ByteCode modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 modNmLL= crCompileOrder cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin2ByteCode" mbGrin
%%[[12
                 expNmOffMp
                        = crsiExpNmOffMp modNm crsi
                 optim  = crsiOptim crsi
%%]]
%%[[8
                 grin'  = if ehcOptOptimise opts >= OptimiseNormal then trfOptim grin else trfNoOptim grin
%%][12
                 (grin',gathInlMp)
                        = if ehcOptOptimise opts >= OptimiseNormal then trfOptim grin else (trfNoOptim grin,Map.empty)
%%]]
                        where trfNoOptim g
                                = grFlattenSeq $ grUnbox GrinBC.tagUnbox $ grUnusedMetaInfoElim $ g
                              trfOptim g
%%[[8
                                = g3
                                where g2 = grInline g1
%%][12
                                = (g3,gathInlMp)
                                where (g2,gathInlMp) = grInline (Map.keysSet expNmOffMp) (optimGrInlMp optim) g1
%%]]
                                      g1 = evel $ grUnbox GrinBC.tagUnbox $ grUnusedMetaInfoElim $ g
                                      g3 = grUnusedNameElim $ evel $ g2
                                      evel = grAliasElim . grEvalElim . grAliasElim . grFlattenSeq
                 bc     = grinMod2ByteCodeMod opts
%%[[12
                            (if ecuIsTopMod ecu then [ m | (m,_) <- sortOn snd $ Map.toList $ Map.map fst $ crsiModOffMp crsi ] else [])
                            (crsiModOffMp crsi)
                            expNmOffMp
%%]]
                            $ grin'
         ;  when (isJust mbGrin && (ehcOptEmitGrinBC opts))
                 (do { cpUpdCU modNm
%%[[8
                         (ecuStoreGrinBC bc)
%%][12
                         (ecuStoreOptim (defaultOptim {optimGrInlMp = gathInlMp}). ecuStoreGrinBC bc)
%%]]
                     ; lift $ putPPFile (fpathToStr $ fpathSetSuff "grin-bc" $ fp) (ppGrModule grin') 400
                     })
         }
%%]

%%[8
cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin" mbGrin
         ;  when (isJust mbGrin && (ehcOptEmitLlc opts || ehcOptEmitLLVM opts))
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: C compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpSystem :: String -> EHCompilePhase ()
cpSystem cmd
  = do { exitCode <- lift $ system cmd
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }
%%]

%%[8
data GCC_CompileHow
  = GCC_CompileOnly
  | GCC_CompileExec

cpCompileWithGCC :: GCC_CompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = fpathSetSuff "c" fp
                 fpO fp = fpathSetSuff "o" fp
                 fpExec = maybe (fpathRemoveSuff fp) (\s -> fpathSetSuff s fp) Cfg.mbSuffixExec
                 (fpTarg,targOpt,linkOpts,linkLibOpt,dotOFilesOpt)
                        = case how of
                            GCC_CompileExec -> ( fpExec
                                               , [ Cfg.gccOpts, "-o", fpathToStr fpExec ]
                                               , Cfg.ehcGccOptsStatic
                                               , map ("-l" ++) Cfg.libnamesGcc
                                               , [ fpathToStr $ fpO fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
                                               )
                            GCC_CompileOnly -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [])
                                            where o = fpO fp
         ;  when (ehcOptEmitExec opts || ehcOptEmitExecBC opts)
                 (do { let compileC
                             = concat $ intersperse " "
                               $ (  [ Cfg.shellCmdGcc ]
                                 ++ [ "-L" ++ Cfg.fileprefixInplaceInstall ++ "%%@{%{VARIANT}%%}/lib"
                                    , "-L" ++ Cfg.fileprefixInplaceInstall ++ "lib"
                                    , "-I" ++ Cfg.fileprefixInplaceInstall ++ "%%@{%{VARIANT}%%}/include"
                                    , "-I" ++ Cfg.fileprefixInplaceInstall ++ "include"
                                    ]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
                                 ++ [ Cfg.fileprefixInplaceInstall ++ "%%@{%{VARIANT}%%}/include/mainSil.c"
                                    | ehcOptEmitExec opts
                                    ]
                                 ++ linkLibOpt
                                 )
                     ; when (ehcOptVerbosity opts >= VerboseALot)
                            (do { lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "GCC" Nothing modNm fpTarg
                                ; lift $ putStrLn compileC
                                })
                     ; cpSystem compileC
                     })
         }
%%]

%%[99
cpPreprocessWithCPP :: HsName -> EHCompilePhase ()
cpPreprocessWithCPP modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              fpCPP = fpathSetSuff (maybe "" (\s -> s ++ "-") (fpathMbSuff fp) ++ "cpp") fp
       ; when (ehcOptCPP opts)
              (do { let preCPP
                          = concat $ intersperse " "
                            $ (  [ Cfg.shellCmdCpp ]
                              ++ [ "-traditional-cpp", "-fno-show-column", "-P", "-D__EHC__" ]
                              ++ [ fpathToStr fp, fpathToStr fpCPP ]
                              )
                  ; when (ehcOptVerbosity opts >= VerboseALot)
                         (do { lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "CPP" Nothing modNm fp
                             ; lift $ putStrLn preCPP
                             })
                  ; cpSystem preCPP
                  ; cpUpdCU modNm (ecuStoreFilePath fpCPP)
                  })
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpCore1Trf :: HsName -> String -> EHCompilePhase ()
cpCore1Trf modNm trfNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 core   = panicJust "cpCore1Trf" mbCore
                 u1     = uidChild . crsiHereUID $ crsi
                 core2  = ( case trfNm of
                              "CER"     -> cmodTrfEtaRed
                              "CCP"     -> cmodTrfConstProp opts
                              "CRU"     -> cmodTrfRenUniq
                              "CLU"     -> cmodTrfLetUnrec
                              "CILA"    -> cmodTrfInlineLetAlias
%%[[12
                                             (Map.keysSet $ crsiExpNmOffMp modNm crsi)
%%]]
                              "CFL"     -> cmodTrfFullLazy u1
                              "CLGA"    -> cmodTrfLamGlobalAsArg
                              "CLFG"    -> cmodTrfLamFloatGlobal
                              -- "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Transforming" (lookup trfNm cmdLineTrfs) modNm fp
         ;  cpUpdCU modNm (ecuStoreCore core2)
         }

cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 tr     = ehcOptTrf opts
                 ps     = ( intersperse cpStepUID 
                          . map (cpCore1Trf modNm)
                          . filter (maybe True id . trfOptOverrides tr)
                          ) trfNmL
         ;  cpSeq ps
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: top level processing combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpProcessHs :: HsName -> EHCompilePhase ()
cpProcessHs modNm 
  = cpSeq [ cpFoldHs modNm
%%[[12
          , cpFlowHsSem1 modNm
%%]]
          , cpTranslateHs2EH modNm
          ]

cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm 
  = cpSeq [ cpFoldEH modNm
          , cpFlowEHSem1 modNm
          , cpTranslateEH2Core modNm
          ]

cpProcessCore1 :: HsName -> EHCompilePhase ()
cpProcessCore1 modNm 
  = cpSeq [ cpTransformCore
              modNm
              -- [ "CER", "CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU" ]
              [ "CER", "CCP", "CRU", "CLU", "CILA", "CFL", {- "CLL", -} "CLGA", "CLU", "CFL", "CLFG" {- , "CFL", "CLU" -} ]
          , cpOutputCore "core" modNm
          ]
          
cpProcessCore2 :: HsName -> EHCompilePhase ()
cpProcessCore2 modNm 
  = cpSeq [ cpFoldCore modNm
          ]
          
cpProcessCore3 :: HsName -> EHCompilePhase ()
cpProcessCore3 modNm 
  = cpSeq [ cpTranslateCore2Grin modNm
          ]
          
cpProcessGrin1 :: HsName -> EHCompilePhase ()
cpProcessGrin1 modNm 
  = cpSeq [ cpTranslateGrin2ByteCode modNm
          , cpOutputByteCodeAsC "c" modNm
          ]

cpProcessGrin2 :: HsName -> EHCompilePhase ()
cpProcessGrin2 modNm 
  = cpSeq [ cpOutputGrin "grin2" modNm
          ]
          
cpProcessGrin3 :: HsName -> EHCompilePhase ()
cpProcessGrin3 modNm 
  = cpSeq [ cpOptimizeGrin modNm
          , cpTranslateGrin modNm
          ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpOutputCore :: String -> HsName -> EHCompilePhase ()
cpOutputCore suff modNm
  =  do  {  cr <- get
         -- part 1: current .core
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpOutputCore" mbCore
                 (jBase,jPP) = cmodJavaSrc cMod
                 fpJ = fpathSetBase jBase fp                 
                 fpC = fpathSetSuff suff fp                
         ;  when (ehcOptEmitCore opts) 
                 (do { lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Emit Core" Nothing modNm fpC
                     ; lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (ppCModule opts cMod) 100
                     })
         -- ;  when (ehcOptEmitJava opts)
         --         (lift (putPPFile (fpathToStr (fpathSetSuff "java" fpJ)) jPP 100))

%%[[12
%%]]
         }
%%]
         -- part 2: previous .core
         ;  let  mbCore2= ecuMbPrevCore ecu
         ;  when (isJust mbCore2)
                 (lift $ putPPFile (fpathToStr (fpathSetSuff (suff ++ "-prev") fp)) (ppCModule (fromJust mbCore2)) 100)

%%[8
cpOutputGrin :: String -> HsName -> EHCompilePhase ()
cpOutputGrin suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpOutputGrin" mbGrin
                 grinPP = ppGrModule grin
                 fpG    = fpathSetSuff suff fp
         ;  when (ehcOptEmitGrin opts)
                 (do { lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Emit Grin" Nothing modNm fpG
                     ; lift $ putPPFile (fpathToStr fpG) grinPP 1000
                     })
         ;  when (ehcOptShowGrin opts)
                 (lift $ putPPLn grinPP)
         }
%%]

%%[8
cpOutputByteCodeAsC :: String -> HsName -> EHCompilePhase ()
cpOutputByteCodeAsC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrinBC = ecuMbGrinBC ecu
%%[[8
                 grinbcPP = gbmod2C opts $ panicJust "cpOutputByteCodeAsC" mbGrinBC
%%][12
                 grinbcPP = ppMod >-< (if ecuIsTopMod ecu then ppMain else empty)
                          where (ppMod,ppMain)
                                  = gbmod2C opts
                                    $ panicJust "cpOutputByteCodeAsC" mbGrinBC
%%]]
                 fpC      = fpathSetSuff suff fp
         ;  when (ehcOptEmitGrinBC opts)
                 (do { lift $ putCompileMsg VerboseALot (ehcOptVerbosity opts) "Emit ByteCode C" Nothing modNm fpC
                     ; lift $ putPPFile (fpathToStr fpC) grinbcPP 150
                     })
         }
%%]

%%[12
cpOutputHI :: String -> HsName -> EHCompilePhase ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         -- part 1: current .hi
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 hsSem  = panicJust "cpOutputHI.hsSem" $ ecuMbHSSem ecu
                 ehSem  = panicJust "cpOutputHI.ehSem" $ ecuMbEHSem ecu
                 coreSem= panicJust "cpOutputHI.coreSem" $ ecuMbCoreSem ecu
                 optim  = maybe defaultOptim id $ ecuMbOptim ecu
                 binds  = Seq.toList
                          $ HI.hiFromAllGams
                              (mmiExps $ panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi)
                              (HSSem.gathFixityGam_Syn_AGItf        hsSem)
                              (HSSem.gathIdGam_Syn_AGItf            hsSem)
                              (EHSem.gathValGam_Syn_AGItf           ehSem)
                              (EHSem.gathTyGam_Syn_AGItf            ehSem)
                              (EHSem.gathDataGam_Syn_AGItf          ehSem)
                              (EHSem.gathPrIntroGam_Syn_AGItf       ehSem)
                              (EHSem.gathPrElimTGam_Syn_AGItf       ehSem)
                              (if ehcOptFullProgGRIN opts
                               then Map.empty
                               else Core2GrSem.gathArityMp_Syn_CodeAGItf   coreSem
                              )
                              (optimGrInlMp                         optim)
                 hi     = HISem.wrap_AGItf
                            (HISem.sem_AGItf
                              (HI.AGItf_AGItf $ HI.Module_Module modNm
                                $ HI.Binding_Stamp (Cfg.verTimestamp Cfg.version) (Cfg.verSig Cfg.version) (Cfg.verMajor Cfg.version) (Cfg.verMinor Cfg.version) (Cfg.verQuality Cfg.version) (Cfg.verSvn Cfg.version) (optsDiscrRecompileRepr opts) 0
                                  : binds))
                            (crsiHIInh crsi)
         ;  when (isJust (ecuMbHSSem ecu) && isJust (ecuMbEHSem ecu))
                 (do { lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (HISem.pp_Syn_AGItf hi) 120
                     ; now <- lift $ getClockTime
                     ; cpUpdCU modNm $ ecuStoreHITime now
                     })

         -- part 2: previous .hi
         }

%%]
         ;  let  mbHI2  = ecuMbPrevHI ecu
                 hi2    = HISem.wrap_AGItf (HISem.sem_AGItf (fromJust mbHI2))
                            (HISem.Inh_AGItf)
         ;  when (isJust mbHI2)
                 (lift $ putPPFile (fpathToStr (fpathSetSuff (suff ++ "-prev") fp)) (HISem.pp_Syn_AGItf hi2) 120)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: step unique counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpStepUID :: EHCompilePhase ()
cpStepUID
  = do{ cr <- get
      ; let (n,h) = mkNewLevUID (crsiNextUID crsi)
            crsi = crStateInfo cr
      ; put (cr {crStateInfo = crsi {crsiNextUID = n, crsiHereUID = h}})
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: stop at phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpStopAt :: CompilePoint -> EHCompilePhase ()
cpStopAt atPhase
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; unless (atPhase < ehcStopAtPoint opts)
                cpSetStopAllSeq
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: compilation of module(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpCompileCU.sig
cpCompileCU :: HsName -> EHCompilePhase ()
cpCompileCU modNm
%%]
%%[12 -8.cpCompileCU.sig
cpCompileCU :: Maybe HSState -> HsName -> EHCompilePhase ()
cpCompileCU targHSState modNm
%%]
%%[8
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
             msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) m Nothing modNm fp)
             -- msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm fp)
%%[[8
       ; case (ecuState ecu,undefined) of
%%][12
       ; case (ecuState ecu,targHSState) of
%%]]
%%]
%%[12
           (ECUSHaskell HSStart,Just HSOnlyImports)
             -> do { msg "Imports of Haskell"
%%[[12
                   ; cuImportHS modNm
%%][99
                   ; cuImportHS False modNm
%%]]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSOnlyImports))
                   }
%%[[99
           (ECUSHaskell LHSStart,Just HSOnlyImports)
             -> do { msg "Imports of Literate Haskell"
                   ; cuImportHS True modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell LHSOnlyImports))
                   }
%%]]
           (ECUSHaskell HSOnlyImports,Just HSOnlyImports)
             -> return ()
%%[[99
           (ECUSHaskell LHSOnlyImports,Just HSOnlyImports)
             -> return ()
%%]]
           (ECUSHaskell HSOnlyImports,Just HSAllSem)
             -> do { msg "Compiling Haskell"
%%[[12
                   ; cuCompileHSAfterImport ecu opts modNm
%%][99
                   ; cuCompileHSAfterImport ecu opts False modNm
%%]]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
           (ECUSHaskell st,Just HSAllSemHI)
             |    st == HSOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> do { msg "Reading HI"
                   ; cpUpdateModOffMp [modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSemHI))
                   }
%%]]
%%[[99
           (ECUSHaskell LHSOnlyImports,Just HSAllSem)
             -> do { msg "Compiling Literate Haskell"
                   ; cuCompileHSAfterImport ecu opts True modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%]]
%%]
%%[8
           (ECUSHaskell HSStart,_)
             -> do { msg "Compiling Haskell"
                   ; cpSeq [ cpSetupMod modNm
%%[[8
                           , cpParseHs modNm
%%][99
                           , cpPreprocessWithCPP modNm
                           , cpParseHs False modNm
%%]]
                           , cpStopAt CompilePoint_Parse
                           , cpStepUID
                           , cpProcessHs modNm
                           , cpStopAt CompilePoint_AnalHS
                           , cpStepUID
                           , cpProcessEH modNm
                           , cpStopAt CompilePoint_AnalEH
                           , cpStepUID
                           , cpProcessCore1 modNm
                           , cpProcessCore2 modNm
                           , cpProcessCore3 modNm
                           , cpProcessGrin1 modNm
                           , cpProcessGrin2 modNm
                           , cpProcessGrin3 modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           ]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%[[12
           (_,Just HSOnlyImports)
             -> return ()
%%]]
           (ECUSEh EHStart,_)
             -> do { msg "Compiling EH"
                   ; cpSeq [ cpParseEH modNm
                           , cpStopAt CompilePoint_Parse
                           , cpStepUID
                           , cpProcessEH modNm
%%[[12
                           , cpGetCheckEhMod modNm
%%]]
                           , cpStopAt CompilePoint_AnalEH
                           , cpStepUID
                           , cpProcessCore1 modNm
                           , cpStopAt CompilePoint_Core
                           , cpProcessCore2 modNm
                           , cpProcessCore3 modNm
                           , cpProcessGrin1 modNm
                           , cpProcessGrin2 modNm
                           , cpProcessGrin3 modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           ]
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   }
           (ECUSGrin,_)
             -> do { msg "Compiling Grin"
                   ; cpSeq [ cpParseGrin modNm, cpProcessGrin1 modNm, cpProcessGrin2 modNm, cpProcessGrin3 modNm ]
                   }
           _ -> return ()
       }
%%]
%%[12
  where 
%%[[12
        cuImportHS modNm
%%][99
        cuImportHS litmode modNm
%%]]
          = cpSeq [ cpSetupMod modNm
%%[[12
                  , cpParseHsImport modNm
%%][99
                  , cpPreprocessWithCPP modNm
                  , cpParseHsImport litmode modNm
%%]]
                  , cpStepUID
                  , cpFoldHsMod modNm
                  , cpGetHsImports modNm
                  ]
%%[[12
        cuCompileHSAfterImport ecu opts modNm
%%][99
        cuCompileHSAfterImport ecu opts litmode modNm
%%]]
          = cpSeq [ 
%%[[12
                    cpParseHs modNm
%%][99
                    cpParseHs litmode modNm
%%]]
                  , cpStopAt CompilePoint_Parse
                  , cpStepUID, cpFoldHsMod modNm, cpGetHsMod modNm, cpCheckMods [modNm], cpUpdateModOffMp [modNm], cpProcessHs modNm
                  , cpStopAt CompilePoint_AnalHS
                  , cpStepUID, cpProcessEH modNm
                  , cpStopAt CompilePoint_AnalEH
                  , cpStepUID, cpProcessCore1 modNm
                  , cpStopAt CompilePoint_Core
                  , cpSeq (if not (ehcOptFullProgGRIN opts)
                           then [cpProcessCore2 modNm, cpProcessCore3 modNm, cpProcessGrin1 modNm, cpProcessGrin2 modNm]
                                ++ (if ecuIsTopMod ecu then [] else [cpCompileWithGCC GCC_CompileOnly [] modNm])
                           else []
                          )
                  , cpOutputHI "hi" modNm
                  ]
%%]

%%[8
%%]
crCompileCG :: Maybe HSState -> [HsName] -> EHCompileRun -> IO EHCompileRun
crCompileCG targHSState modNmL cr
  = do { let grpNm = HNm $ concat $ intersperse "-" $ map show $ modNmL
             crsi  = crStateInfo cr
             cr2   = cr {crStateInfo = crsi {crsiGrpMp = Map.insert grpNm (emptyECG {ecgNm = grpNm, ecgModL = modNmL}) (crsiGrpMp crsi)}}
             crSetNm = crSeq $ map (\n -> crUpdCU n (\ecu -> return (ecu {ecuGrpNm = grpNm}))) modNmL
       ; crSetNm cr2
       }

%%[12
cpCompileOrderedCUs :: EHCompilePhase ()
cpCompileOrderedCUs
 = do { cr <- get
      ; let modNmLL = crCompileOrder cr
            modNmL = map head modNmLL
            (_,opts) = crBaseInfo' cr
            Just (mm@(mImpL,mMain)) = initlast modNmL
      ; cpSeq [anal modNmL, biggrin opts modNmL mm, gcc mm]
      }
  where anal ms
          = cpSeq (merge (map comp ms) (map flow ms))
        merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs
        comp m
          = do { cr <- get
               ; let targ = if crModNeedsCompile m cr then HSAllSem else HSAllSemHI
               ; cpSeq [cpCompileCU (Just targ) m]
               }
        flow m
          = do { cr <- get
               ; case {- (\v -> trp "XX" (m >#< show v) v) $ -} ecuState $ crCU m cr of
                   ECUSHaskell HSAllSem   -> flowSem
                   ECUSHaskell HSAllSemHI -> cpFlowHISem m
                   _                      -> return ()
               }
          where flowSem = cpSeq [cpFlowHsSem2 m,cpFlowEHSem2 m,cpFlowCore2GrSem m,cpFlowOptim m]
        core mL
          = cpSeq [cpGetPrevCore m | m <- mL]
        biggrin opts mL (mImpL,mMain)
          = if ehcOptFullProgGRIN opts
            then cpSeq [core mL, oneBigCore, cpProcessCore2 mMain, cpProcessCore3 mMain, cpProcessGrin2 mMain, cpProcessGrin3 mMain]
            else return ()
          where oneBigCore
                  = do { cr <- get
                       ; cpUpdCU mMain (ecuStoreCore (Core.cModMerge [ panicJust "cpCompileOrderedCUs.oneBigCore" $ ecuMbCore $ crCU m cr | m <- mL ]))
                       }
        gcc (mImpL,mMain)
          = cpSeq [cpCompileWithGCC GCC_CompileExec mImpL mMain]
        
%%]

%%[12
%%]
cpCheckImpExp :: EHCompilePhase ()
cpCheckImpExp
  = do { cr <- get
       ; let modLL  = [ [ addBuiltin $ ecuMod $ crCU n cr | n <- nn ] | nn <- crCompileOrder cr ]
                    where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }
             crsi   = crStateInfo cr
             (modMp,errs)
                    = foldl (\(mp,es) ms -> let (m,e) = ms `modMpCombine` mp in (m,es++e)) (Map.empty,[]) modLL
       -- ; lift $ putWidthPPLn 120 (ppModMp modMp) -- debug
       ; put (cr {crStateInfo = crsi {crsiModMp = modMp}})
       ; cpSetLimitErrsWhen 5 "Module analysis" errs
       }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;  when
              (ehcStopAtPoint opts >= CompilePoint_Parse)
              (do { tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
                  ; resd <-
                      if isHS
                      then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; if ehcStopAtPoint opts >= CompilePoint_AnalHS
                                then do { let res   = HSSem.sem_AGItf resd
                                              wrRes = HSSem.wrap_AGItf res
                                                        (HSSem.Inh_AGItf
                                                          { HSSem.opts_Inh_AGItf = opts
                                                          })
                                        ; putStrLn (disp (ppErrL $ HSSem.errL_Syn_AGItf $ wrRes) 1000 "")
                                        ; when (ehcOptShowHS opts)
                                               (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
                                        ; return (HSSem.eh_Syn_AGItf wrRes)
                                        }
                                else return undefined
                              }
                      else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; return resd
                              }
                  ; when
                      (ehcStopAtPoint opts >= CompilePoint_AnalEH)
                      (do { let res   = EHSem.sem_AGItf resd
                                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
                          ; when (ehcOptShowEH opts)
                                 (putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 ""))
                          ; when (ehcOptShowAst opts)
                                 (putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 ""))
                          ; when (ehcOptShowTopTyPP opts)
                                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
%%[[7_2
                          ; when (not (null filename) && ehcOptUniqueness opts)
                                 (writeFile (filename ++ ".html") (EHSem.ppHTML_Syn_AGItf wrRes))
%%]]
                          })
                  })
         }
%%]

%%[7_2.doCompile
%%]
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;  tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
         ;  resd <-
              if isHS
              then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; let res   = HSSem.sem_AGItf resd
                            wrRes = HSSem.wrap_AGItf res (HSSem.Inh_AGItf {HSSem.opts_Inh_AGItf = opts})
                      ; return (HSSem.eh_Syn_AGItf wrRes)
                      }
              else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; return resd
                      }
         ;  let res   = EHSem.sem_AGItf resd
                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
         ;  when (ehcOptShowTopTyPP opts)
                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
         ;  when (not (null filename) && ehcOptUniqueness opts)
                 (writeFile (filename ++ ".html") (EHSem.ppHTML_Syn_AGItf wrRes))
         }

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "hs" fn
             topModNm       = mkHNm (fpathBase fp)
             searchPath     = mkInitSearchPath fp ++ ehcOptSearchPath opts
             opts2          = opts { ehcOptSearchPath = searchPath }
             hsInh          = HSSem.Inh_AGItf { HSSem.opts_Inh_AGItf            = opts2
                                              , HSSem.idGam_Inh_AGItf           = HSSem.tyGam2IdDefOccGam initTyGam
                                                                                    `gamUnion` HSSem.kiGam2IdDefOccGam initKiGam
%%[[9
                                                                                    `gamUnion` HSSem.pigiGam2IdDefOccGam initPIGIGam
%%]]
                                              , HSSem.gUniq_Inh_AGItf           = uidStart
%%[[12
                                              , HSSem.isTopMod_Inh_AGItf        = False
                                              , HSSem.moduleNm_Inh_AGItf        = hsnUnknown
                                              , HSSem.modInScope_Inh_AGItf      = Map.empty
                                              , HSSem.modEntToOrig_Inh_AGItf    = Map.empty
                                              , HSSem.fixityGam_Inh_AGItf       = emptyGam
                                              , HSSem.topInstanceNmL_Inh_AGItf  = []
%%]]
                                              }
             ehInh          = EHSem.Inh_AGItf { EHSem.moduleNm_Inh_AGItf        = mkHNm (fpathBase fp)
                                              , EHSem.gUniq_Inh_AGItf           = uidStart
                                              , EHSem.opts_Inh_AGItf            = opts2
%%[[12
                                              , EHSem.isTopMod_Inh_AGItf        = False
                                              , EHSem.valGam_Inh_AGItf          = emptyGam
                                              , EHSem.dataGam_Inh_AGItf         = emptyGam
                                              , EHSem.tyGam_Inh_AGItf           = initTyGam
                                              , EHSem.kiGam_Inh_AGItf           = initKiGam
                                              , EHSem.prIntroGam_Inh_AGItf      = initPIGIGam
                                              , EHSem.prElimTGam_Inh_AGItf      = emptyTGam basePrfCtxtId
                                              , EHSem.prfCtxtId_Inh_AGItf       = basePrfCtxtId
                                              , EHSem.idQualGam_Inh_AGItf       = emptyGam
%%]]
%%[[95
                                              -- , EHSem.fixityGam_Inh_AGItf       = emptyGam
%%]]
                                              }
             coreInh        = Core2GrSem.Inh_CodeAGItf
                                              { Core2GrSem.gUniq_Inh_CodeAGItf           = uidStart
                                              , Core2GrSem.dataGam_Inh_CodeAGItf         = emptyGam
                                              , Core2GrSem.opts_Inh_CodeAGItf            = opts2
%%[[12
                                              , Core2GrSem.arityMp_Inh_CodeAGItf         = Map.empty
%%]]
                                              }
%%[[12
             hsModInh       = HSSemMod.Inh_AGItf { HSSemMod.gUniq_Inh_AGItf       = uidStart
                                                 , HSSemMod.moduleNm_Inh_AGItf    = hsnUnknown
                                                 , HSSemMod.opts_Inh_AGItf        = opts2
                                                 }
             hiInh          = HISem.Inh_AGItf { HISem.opts_Inh_AGItf            = opts2
                                              }
%%]]
             initialState   = mkEmptyCompileRun
                                topModNm
                                (EHCompileRunStateInfo opts2 hsInh ehInh coreInh uidStart uidStart
%%[[12
                                 hiInh hsModInh Map.empty Map.empty defaultOptim Map.empty
%%]]
                                )
%%[[8
             comp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    ; when (isJust mbFoundFp)
                           (cpCompileCU nm)
                    }
%%][12
             imp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    -- ; lift $ putStrLn $ show nm ++ ": " ++ show mbFp ++ ": " ++ show mbFoundFp
                    ; when (isJust mbFp)
                           (cpUpdCU nm (ecuSetIsTopMod True))
                    ; when (isJust mbFoundFp)
                           (cpCompileCU (Just HSOnlyImports) nm)
                    }
%%]]
       -- ; putStrLn $ show searchPath
%%[[8
       ; _ <- runStateT (cpSeq [ comp (Just fp) topModNm
                               ]) initialState
%%][12
       ; _ <- runStateT (cpSeq [ imp (Just fp) topModNm
                               , cpImportGather (imp Nothing) topModNm
                               , cpCheckMods' [modBuiltin]
                               , cpCompileOrderedCUs
                               ]) initialState
%%]]
       ; return ()
       }

%%]

