%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, Data.List, Control.Monad, System.Console.GetOpt, IO, EH.Util.Pretty,{%{EH}Error.Pretty}, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Base.Builtin}, qualified {%{EH}Config} as Cfg, {%{EH}Scanner.Common}, {%{EH}Base.Opts})
%%]

%%[1 import(qualified EH.Util.FastSeq as Seq,EH.Util.Utils)
%%]

%%[1 import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.Parser} as HSPrs, qualified {%{EH}HS.MainAG} as HSSem)
%%]

%%[8 import (EH.Util.CompileRun,{%{EH}Error},{%{EH}Gam},EH.Util.FPath,qualified Data.Map as Map,Data.Maybe,Data.List, EH.Util.ParseUtils, System)
%%]

%%[8 import ({%{EH}Core.ToJava},{%{EH}Core.Pretty})
%%]

%%[8 import ({%{EH}Core.Trf.RenUniq},{%{EH}Core.Trf.FullLazy},{%{EH}Core.Trf.InlineLetAlias},{%{EH}Core.Trf.LetUnrec},{%{EH}Core.Trf.LamGlobalAsArg},{%{EH}Core.Trf.CAFGlobalAsArg},{%{EH}Core.Trf.FloatToGlobal},{%{EH}Core.Trf.ConstProp},{%{EH}Core.Trf.EtaRed},{%{EH}Core.Trf.ElimTrivApp})
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

%%[20 import (qualified EH.Util.Rel as Rel, qualified Data.Set as Set)
%%]

%%[20 import (System.Time, System.Directory)
%%]

%%[20 import (qualified {%{EH}HI.Parser} as HIPrs, {%{EH}Module}, qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]

%%[20 import (qualified {%{EH}Core.Parser} as CorePrs)
%%]

%%[20 import (qualified {%{EH}Pred} as Pr,qualified {%{EH}HS.ModImpExp} as HSSemMod,{%{EH}Pred.ToCHR},{%{EH}CHR.Solve})
%%]

%%[99 import({%{EH}Base.ForceEval},{%{EH}Ty.Trf.ForceEval},{%{EH}GrinCode.Trf.ForceEval},{%{EH}Core.Trf.ForceEval},{%{GRIN}GrinByteCode.Trf.ForceEval})
%%]

%%[99 import ({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}Pred.CHR},{%{EH}Pred.CommonCHR})
%%]

%%[101 import ({%{EH}Core.Trf.Strip},{%{EH}Debug.HighWaterMark})
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

%%[20
data Optim
  = Optim
      { optimGrInlMp          :: Grin.GrInlMp        -- inlining map, from name to GrExpr (grin expressions)
      }

defaultOptim :: Optim
defaultOptim = Optim Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data HSState
  = HSStart
%%[[99
  | LHSStart
  | LHSOnlyImports
%%]]
  | HSAllSem
%%[[20
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
  | ECUSHaskell !HSState
  | ECUSEh      !EHState
  | ECUSGrin
  | ECUSFail
  deriving (Show,Eq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: !FPath
      , ecuGrpNm             :: !HsName
      , ecuModNm             :: !HsName
      , ecuImpNmL            :: ![HsName]
      , ecuMbHS              :: !(Maybe HS.AGItf)
      , ecuMbHSSem           :: !(Maybe HSSem.Syn_AGItf)
      , ecuMbEH              :: !(Maybe EH.AGItf)
      , ecuMbEHSem           :: !(Maybe EHSem.Syn_AGItf)
%%[[101
%%]]
      , ecuMbCore            :: !(Maybe Core.CModule)
      , ecuMbCoreSem         :: !(Maybe Core2GrSem.Syn_CodeAGItf)
      , ecuMbGrin            :: !(Maybe Grin.GrModule)
      , ecuMbGrinBC          :: !(Maybe GrinBC.Module)
      , ecuMbGrinBCSem       :: !(Maybe PP_Doc)
      , ecuState             :: !EHCompileUnitState
%%[[20
      , ecuIsTopMod          :: !Bool
      , ecuMbHSTime          :: !(Maybe ClockTime)
      , ecuMbHITime          :: !(Maybe ClockTime)
      , ecuMbCoreTime        :: !(Maybe ClockTime)
      , ecuMbHSSemMod        :: !(Maybe HSSemMod.Syn_AGItf)
      , ecuMod               :: !Mod
      , ecuMbPrevHI          :: !(Maybe HI.AGItf)
      , ecuMbPrevHISem       :: !(Maybe HISem.Syn_AGItf)
      , ecuMbOptim           :: !(Maybe Optim)
      , ecuHIInfo            :: !HI.HIInfo
%%]]
      }
%%]
      , ecuMbEHSem2          :: !(Maybe EHSem.Syn_AGItf)

%%[8
emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuGrpNm             = hsnUnknown
      , ecuModNm             = hsnUnknown
      , ecuImpNmL            = []
      , ecuMbHS              = Nothing
      , ecuMbHSSem           = Nothing
      , ecuMbEH              = Nothing
      , ecuMbEHSem           = Nothing
%%[[101
%%]]
      , ecuMbCore            = Nothing
      , ecuMbCoreSem         = Nothing
      , ecuMbGrin            = Nothing
      , ecuMbGrinBC          = Nothing
      , ecuMbGrinBCSem       = Nothing
      , ecuState             = ECUSUnknown
%%[[20
      , ecuIsTopMod          = False
      , ecuMbHSTime          = Nothing
      , ecuMbHITime          = Nothing
      , ecuMbCoreTime        = Nothing
      , ecuMbHSSemMod        = Nothing
      , ecuMod               = emptyMod
      , ecuMbPrevHI          = Nothing
      , ecuMbPrevHISem       = Nothing
      , ecuMbOptim           = Nothing
      , ecuHIInfo            = HI.emptyHIInfo
%%]]
      }
%%]
      , ecuMbEHSem2          = Nothing

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
%%[[8
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }
%%][99
ecuStoreCore x ecu | forceEval x `seq` True = ecu { ecuMbCore = Just x }
%%]]

ecuStoreGrin :: EcuUpdater Grin.GrModule
%%[[8
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }
%%][99
ecuStoreGrin x ecu | forceEval x `seq` True = ecu { ecuMbGrin = Just x }
%%]]

ecuStoreGrinBC :: EcuUpdater GrinBC.Module
%%[[8
ecuStoreGrinBC x ecu = ecu { ecuMbGrinBC = Just x }
%%][99
ecuStoreGrinBC x ecu | forceEval x `seq` True = ecu { ecuMbGrinBC = Just x }
%%]]

ecuStoreGrinBCSem :: EcuUpdater PP_Doc
ecuStoreGrinBCSem x ecu = ecu { ecuMbGrinBCSem = Just x }
%%]
ecuStoreCore x ecu | x `seq` True = ecu { ecuMbCore = Just x }
ecuStoreGrin x ecu | x `seq` True = ecu { ecuMbGrin = Just x }
ecuStoreGrinBC x ecu | x `seq` True = ecu { ecuMbGrinBC = Just x }

%%[20
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

ecuStoreHIInfo :: EcuUpdater HI.HIInfo
%%[[8
ecuStoreHIInfo x ecu = ecu { ecuHIInfo = x }
%%][99
ecuStoreHIInfo x ecu | forceEval x `seq` True = ecu { ecuHIInfo = x }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: !EHCOpts                             -- options
      , crsiHSInh       :: !HSSem.Inh_AGItf                     -- current inh attrs for HS sem
      , crsiEHInh       :: !EHSem.Inh_AGItf                     -- current inh attrs for EH sem
      , crsiCoreInh     :: !Core2GrSem.Inh_CodeAGItf            -- current inh attrs for Core2Grin sem
      , crsiNextUID     :: !UID                                 -- unique id, the next one
      , crsiHereUID     :: !UID                                 -- unique id, the current one
%%[[20
      , crsiHIInh       :: !HISem.Inh_AGItf                     -- current inh attrs for HI sem
      , crsiHSModInh    :: !HSSemMod.Inh_AGItf                  -- current inh attrs for HS module analysis sem
      , crsiModMp       :: !ModMp                               -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: !Optim                               -- inter module optimisation info
      , crsiModOffMp    :: !Core.HsName2OffsetMpMp              -- mapping of all modules + exp entries to offsets in module + exp tables
%%]
      }
%%]

%%[20
crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> Core.HsName2OffsetMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust "crsiExpNmOffMp" $ Map.lookup modNm $ crsiModMp crsi
%%]

%%[20
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
%%% Show sizes, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[101
showSizeCore :: Core.CModule -> String
showSizeCore x = fevShow "Core" x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpMsg :: HsName -> Verbosity -> String -> EHCompilePhase ()
cpMsg modNm v m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm v m Nothing fp
       }

cpMsg' :: HsName -> Verbosity -> String -> Maybe String -> FPath -> EHCompilePhase ()
cpMsg' modNm v m mbInfo fp
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; lift $ putCompileMsg v (ehcOptVerbosity opts) m mbInfo modNm fp
       -- ; lift $ putStrLn "XX"
       ; cpMemUsage
       }
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
            errs       = map (rngLift emptyRange mkPPErr) msgs
      ; cpUpdCU modNm (store res)
      ; cpSetLimitErrsWhen 5 description errs
      }

cpParsePlain' :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> FPath -> HsName -> EHCompilePhase [Err]
cpParsePlain' parser scanOpts store fp modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath fp ReadMode
      ; tokens  <- lift $ scanHandle scanOpts fn fh
      ; let (res,msgs) = parseToResMsgs parser tokens
            errs       = map (rngLift emptyRange mkPPErr) msgs
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

%%[20
cpParseOffsideStopAtErr :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> HsName -> EHCompilePhase ()
cpParseOffsideStopAtErr parser scanOpts store modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let (res,_) = parseOffsideToResMsgsStopAtErr parser tokens
      ; cpUpdCU modNm (store res)
      }
%%]

%%[20.cpParseHsImport
cpParseHsImport :: HsName -> EHCompilePhase ()
cpParseHsImport = cpParseOffsideStopAtErr HSPrs.pAGItfImport hsScanOpts ecuStoreHS
%%]

%%[99 -20.cpParseHsImport
cpParseHsImport :: Bool -> HsName -> EHCompilePhase ()
cpParseHsImport litmode = cpParseOffsideStopAtErr HSPrs.pAGItfImport (hsScanOpts {ScanUtils.scoLitmode = litmode}) ecuStoreHS
%%]

%%[20
cpParseCore :: HsName -> EHCompilePhase ()
cpParseCore modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff "core" fp
       ; cpMsg' modNm VerboseALot "Parsing" Nothing fpC
       ; cpParsePlain CorePrs.pCModule coreScanOpts ecuStoreCore "Parse Core (of previous compile) of module" fpC modNm
       }

cpParsePrevHI :: HsName -> EHCompilePhase ()
cpParsePrevHI modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpH     = fpathSetSuff "hi" fp
       ; cpMsg' modNm VerboseALot "Parsing" Nothing fpH
       ; errs <- cpParsePlain' HIPrs.pAGItf hiScanOpts ecuStorePrevHI fpH modNm
       ; when (ehcDebugStopAtHIError opts) $ cpSetLimitErrsWhen 5 "Parse HI (of previous compile) of module" errs
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
cpFoldCore :: HsName -> EHCompilePhase ()
cpFoldCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbCore   = ecuMbCore ecu
                 coreInh  = crsiCoreInh crsi
                 coreSem  = Core2GrSem.wrap_CodeAGItf
                              (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf $ panicJust "cpFoldCore" mbCore))
                              (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf            = crsiHereUID crsi
                                       , Core2GrSem.opts_Inh_CodeAGItf             = crsiOpts crsi
                                       })
         ;  when (isJust mbCore)
                 (cpUpdCU modNm $! ecuStoreCoreSem coreSem)
         }

cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf $ panicJust "cpFoldEH" mbEH)
                                           ((crsiEHInh crsi)
                                                  { EHSem.moduleNm_Inh_AGItf         = ecuModNm ecu
                                                  , EHSem.gUniq_Inh_AGItf            = crsiHereUID crsi
                                                  , EHSem.opts_Inh_AGItf             = opts
%%[[20
                                                  , EHSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
%%]]
                                                  })
         ;  when (isJust mbEH)
                 (cpUpdCU modNm $! ecuStoreEHSem $! ehSem)
         }
%%]

%%[8
cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHS   = ecuMbHS ecu
                 inh    = crsiHSInh crsi
                 hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf $ panicJust "cpFoldHs" mbHS)
                                           (inh { HSSem.opts_Inh_AGItf             = crsiOpts crsi
                                                , HSSem.gUniq_Inh_AGItf            = crsiHereUID crsi
%%[[20
                                                , HSSem.moduleNm_Inh_AGItf         = modNm
                                                , HSSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
                                                , HSSem.modInScope_Inh_AGItf       = inscps
                                                , HSSem.modEntToOrig_Inh_AGItf     = exps
                                                , HSSem.topInstanceNmL_Inh_AGItf   = modInstNmL (ecuMod ecu)
%%]]
                                                })
%%[[20
                        where mmi    = panicJust "cpFoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                              inscps = Rel.toDomMap $ mmiInscps $ mmi
                              exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                                                    $ Rel.mapRng mentIdOcc $ mmiExps $ mmi
%%]]
         ;  when (isJust mbHS)
                 (cpUpdCU modNm $! ecuStoreHSSem $! hsSem)
         }
%%]

%%[20
cpFoldHsMod :: HsName -> EHCompilePhase ()
cpFoldHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHS       = ecuMbHS ecu
                 inh        = crsiHSModInh crsi
                 hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf $ panicJust "cpFoldHsMod" mbHS)
                                                  (inh { HSSemMod.gUniq_Inh_AGItf        = crsiHereUID crsi
                                                       , HSSemMod.moduleNm_Inh_AGItf     = modNm
                                                       })
         ;  when (isJust mbHS)
                 (cpUpdCU modNm $! ecuStoreHSSemMod $! hsSemMod)
         }
%%]

%%[20
cpFoldHI :: HsName -> EHCompilePhase ()
cpFoldHI modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHI   = ecuMbPrevHI ecu
                 inh    = crsiHIInh crsi
                 hiSem  = HISem.wrap_AGItf (HISem.sem_AGItf $ panicJust "cpFoldHI" mbHI)
                                           (inh { HISem.opts_Inh_AGItf             = crsiOpts crsi
                                                })
         ;  when (isJust mbHI && HISem.isValidVersion_Syn_AGItf hiSem)
                 (do { let mm     = crsiModMp crsi
                           mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                           mmi'   = mkModMpInfo (mmiInscps mmi) (HISem.exportRel_Syn_AGItf hiSem) (HISem.exportHideRel_Syn_AGItf hiSem)
                     ; put (cr {crStateInfo = crsi {crsiModMp = Map.insert modNm mmi' mm}})
                     ; cpUpdCU modNm (ecuStorePrevHISem hiSem)
                     })
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional processing before flowing into next whatever: in particular, force evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20.prepFlow
prepFlow :: a -> a
prepFlow = id

gamUnionFlow :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnionFlow = gamUnion
%%]

%%[99 -20.prepFlow
prepFlow :: ForceEval a => a -> a
prepFlow = forceEval

gamUnionFlow :: (Ord k, ForceEval (Gam k v)) => Gam k v -> Gam k v -> Gam k v
gamUnionFlow g1 g2 | forceEval g1 `seq` True = gamUnion g1 g2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: flowing info between module compiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
cpFlowHsSem1 :: HsName -> EHCompilePhase ()
cpFlowHsSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 hsSem  = panicJust "cpFlowHsSem1" $ ecuMbHSSem ecu
                 ehInh  = crsiEHInh crsi
                 hsInh  = crsiHSInh crsi
                 hii    = ecuHIInfo ecu
                 ig     = prepFlow $! HSSem.gathIdGam_Syn_AGItf hsSem
                 fg     = prepFlow $! HSSem.gathFixityGam_Syn_AGItf hsSem
                 hsInh' = hsInh
                            { HSSem.idGam_Inh_AGItf      = ig `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            , HSSem.fixityGam_Inh_AGItf  = fg `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            }
                 ehInh' = ehInh
                            { EHSem.idQualGam_Inh_AGItf  = idGam2QualGam ig `gamUnionFlow` EHSem.idQualGam_Inh_AGItf ehInh
                            }
                 hii'   = hii
                            { HI.hiiFixityGam       = fg
                            , HI.hiiIdDefOccGam = ig
                            }
                 opts'  = opts
                            { ehcOptBuiltinNames = mkEHBuiltinNames mk
                            }
%%[[20
                        where mk = idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh')
%%][99
                        where mk = if ehcOptUseAssumePrelude opts
                                   then \_ n -> n
                                   else \k n -> idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh') k (hsnQualified n)
%%]]
         ;  when (isJust (ecuMbHSSem ecu))
                 (do { put (cr {crStateInfo = crsi {crsiHSInh = hsInh', crsiEHInh = ehInh', crsiOpts = opts'}})
                     ; cpUpdCU modNm $! ecuStoreHIInfo hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowHsSem1")
                     })
         -- ;  lift $ putWidthPPLn 120 (ppGam $ EHSem.idQualGam_Inh_AGItf $ ehInh')
         }

%%]

%%[8
cpFlowEHSem1 :: HsName -> EHCompilePhase ()
cpFlowEHSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 ehSem    = panicJust "cpFlowEHSem1.ehSem" $ ecuMbEHSem ecu
                 ehInh    = crsiEHInh crsi
                 coreSem  = panicJust "cpFlowEHSem1.coreSem" $ ecuMbCoreSem ecu
                 coreInh  = crsiCoreInh crsi
%%[[20
                 dg       = prepFlow $! EHSem.gathDataGam_Syn_AGItf    ehSem
                 vg       = prepFlow $! EHSem.gathValGam_Syn_AGItf     ehSem
                 tg       = prepFlow $! EHSem.gathTyGam_Syn_AGItf      ehSem
                 tkg      = prepFlow $! EHSem.gathTyKiGam_Syn_AGItf    ehSem
                 kg       = prepFlow $! EHSem.gathKiGam_Syn_AGItf      ehSem
                 pg       = prepFlow $! EHSem.gathClGam_Syn_AGItf      ehSem
                 cs       = prepFlow $! EHSem.gathChrStore_Syn_AGItf   ehSem
%%]]
%%[[20
                 hii      = ecuHIInfo ecu
                 ehInh'   = ehInh
                              { EHSem.dataGam_Inh_AGItf    = dg  `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                              , EHSem.valGam_Inh_AGItf     = vg  `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                              , EHSem.tyGam_Inh_AGItf      = tg  `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                              , EHSem.tyKiGam_Inh_AGItf    = tkg `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                              , EHSem.kiGam_Inh_AGItf      = kg  `gamUnionFlow`  EHSem.kiGam_Inh_AGItf      ehInh
                              , EHSem.clGam_Inh_AGItf      = pg  `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                              , EHSem.chrStore_Inh_AGItf   = cs  `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                              }
                 hii'     = hii
                              { HI.hiiValGam        = vg
                              , HI.hiiTyGam     	= tg
                              , HI.hiiTyKiGam     	= tkg
                              , HI.hiiDataGam       = dg
                              , HI.hiiClGam         = pg
                              , HI.hiiCHRStore      = cs
                              }
%%]]
                 coreInh' = coreInh
%%[[8
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf    ehSem
%%][20
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf        ehInh'
%%]]
                              }
         ;  when (isJust (ecuMbEHSem ecu))
                 (do { put (cr {crStateInfo = crsi { crsiCoreInh = coreInh'
%%[[20
                                                   , crsiEHInh = ehInh'
%%]]
                                                   }})
%%[[20
                     ; cpUpdCU modNm $! ecuStoreHIInfo hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowEHSem1")
%%]]
%%[[101
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (do { lift $ putStrLn $ fevShow "gathDataGam" dg
                                ; lift $ putStrLn $ fevShow "gathValGam" vg
                                ; lift $ putStrLn $ fevShow "gathTyGam" tg
                                ; lift $ putStrLn $ fevShow "gathTyKiGam" tkg
                                ; lift $ putStrLn $ fevShow "gathKiGam" kg
                                ; lift $ putStrLn $ fevShow "gathClGam" pg
                                ; lift $ putStrLn $ fevShow "gathChrStore" cs
                                ; lift $ putStrLn $ fevShow "cmodule" $ EHSem.cmodule_Syn_AGItf   ehSem
                                })
%%]]
                     })
         }
%%]
                 dg       = emptyGam
                 vg       = emptyGam
                 tg       = emptyGam
                 tkg      = emptyGam
                 kg       = emptyGam
                 pg       = emptyGam
                 cs       = emptyCHRStore

%%[20
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 ehInh  = crsiEHInh crsi
                 ehInh' = ehInh
                            { EHSem.valGam_Inh_AGItf     = (prepFlow $! HISem.valGam_Syn_AGItf     hiSem) `gamUnionFlow` EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = (prepFlow $! HISem.tyGam_Syn_AGItf      hiSem) `gamUnionFlow` EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.tyKiGam_Inh_AGItf    = (prepFlow $! HISem.tyKiGam_Syn_AGItf    hiSem) `gamUnionFlow` EHSem.tyKiGam_Inh_AGItf    ehInh
                            , EHSem.dataGam_Inh_AGItf    = (prepFlow $! HISem.dataGam_Syn_AGItf    hiSem) `gamUnionFlow` EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.clGam_Inh_AGItf      = (prepFlow $! HISem.clGam_Syn_AGItf      hiSem) `gamUnionFlow` EHSem.clGam_Inh_AGItf      ehInh
                            , EHSem.chrStore_Inh_AGItf   = (prepFlow $! HISem.chrStore_Syn_AGItf   hiSem) `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                            }
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = (prepFlow $! HISem.fixityGam_Syn_AGItf hiSem) `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = (prepFlow $! HISem.idGam_Syn_AGItf     hiSem) `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            }
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.arityMp_Inh_CodeAGItf   = (prepFlow $! HISem.arityMp_Syn_AGItf hiSem) `Map.union` Core2GrSem.arityMp_Inh_CodeAGItf coreInh
                              }
                 optim    = crsiOptim crsi
                 optim'   = optim
                              { optimGrInlMp   = (prepFlow $! HISem.inlMp_Syn_AGItf hiSem) `Map.union` optimGrInlMp optim
                              }
         ;  when (isJust (ecuMbPrevHISem ecu))
                 (do { put (cr {crStateInfo = crsi {crsiEHInh = ehInh', crsiHSInh = hsInh', crsiCoreInh = coreInh', crsiOptim = optim'}})
                     })
         }
%%]

%%[20
cpFlowCoreSem :: HsName -> EHCompilePhase ()
cpFlowCoreSem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 coreSem  = panicJust "cpFlowCoreSem.coreSem" $ ecuMbCoreSem ecu
                 coreInh  = crsiCoreInh crsi
                 hii      = ecuHIInfo ecu
                 am       = prepFlow $! Core2GrSem.gathArityMp_Syn_CodeAGItf coreSem
                 coreInh' = coreInh
                              { Core2GrSem.arityMp_Inh_CodeAGItf   = am `Map.union` Core2GrSem.arityMp_Inh_CodeAGItf coreInh
                              }
                 hii'     = hii
                              { HI.hiiCArityMp  = if ehcOptFullProgGRIN opts then Map.empty else am
                              }
         ;  when (isJust (ecuMbCoreSem ecu))
                 (do { put (cr {crStateInfo = crsi {crsiCoreInh = coreInh'}})
                     ; cpUpdCU modNm $! ecuStoreHIInfo $! prepFlow hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowCoreSem")
                     })
         }
%%]

%%[20
cpFlowOptim :: HsName -> EHCompilePhase ()
cpFlowOptim modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 optim  = crsiOptim crsi
                 moptim = panicJust "cpFlowOptim" $ ecuMbOptim ecu
                 hii    = ecuHIInfo ecu
                 gm     = prepFlow $! optimGrInlMp moptim
                 optim' = optim
                            { optimGrInlMp = gm `Map.union` optimGrInlMp optim
                            }
                 hii'   = hii
                            { HI.hiiGrInlMp = gm
                            }
         ;  when (isJust (ecuMbOptim ecu))
                 (do { put (cr {crStateInfo = crsi {crsiOptim = optim'}})
                     ; cpUpdCU modNm $! ecuStoreHIInfo $! prepFlow hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowOptim")
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

%%[20 -8.cpSetupMod
cpSetupMod :: HsName -> EHCompilePhase ()
cpSetupMod modNm
  = cpSeq [cpGetModfTimes modNm,cpGetPrevHI modNm,cpFoldHI modNm]
%%]

%%[20
cpGetPrevHI :: HsName -> EHCompilePhase ()
cpGetPrevHI modNm
  = do { cr <- get
       ; let  ecu        = crCU modNm cr
       ; when (isJust (ecuMbHITime ecu))
              (cpParsePrevHI modNm)
       }
%%]

%%[20
cpGetPrevCore :: HsName -> EHCompilePhase ()
cpGetPrevCore modNm
  = do { cr <- get
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbCoreTime ecu) && isNothing (ecuMbCore ecu))
              (cpParseCore modNm)
       }
%%]

%%[20
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

%%[20
crPartitionNewerOlderImports :: HsName -> EHCompileRun -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ ecuMbHITime ecu
        isNewer ecu'
            = t' `diffClockTimes` t > noTimeDiff 
            where t' = panicJust "crPartitionNewerOlderImports2" $ ecuMbHITime ecu'
%%]

%%[20
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case ecuMbHITime ecu of
      Just thi -> ths `diffClockTimes` thi > noTimeDiff 
               where ths = panicJust "ecuIsHSNewerThanHI" $ ecuMbHSTime ecu
      _        -> True
%%]

%%[20
ecuIsValidHI :: EHCompileUnit -> Bool
ecuIsValidHI ecu
  = case ecuMbPrevHISem ecu of
      Just s -> HISem.isValidVersion_Syn_AGItf s
      _      -> False
%%]

%%[20
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

%%[20
cpCheckMods' :: [Mod] -> EHCompilePhase ()
cpCheckMods' modL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             (mm,e) = modMpCombine modL (crsiModMp crsi)
%%[[20
       ; when (ehcOptVerbosity (crsiOpts crsi) >= VerboseDebug)
              (lift $ putWidthPPLn 120 (pp (head modL) >-< ppModMp mm)) -- debug
%%][99
%%]]
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
%%[20
cpGetCheckEhMod :: HsName -> EHCompilePhase ()
cpGetCheckEhMod modNm
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty Rel.empty []
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
                 errs   = Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
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
%%[[8
                 errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
%%][101
                 -- core   = Core.CModule_Mod modNm (Core.CExpr_Int 1) []
                 errs   = []
%%]]
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreCore core)
                     ; cpSetLimitErrsWhen 5 "Type checking" errs
%%[[8
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "eh2" fp)) (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%][101
%%]]
%%[[8
                     ; when (ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
                     ; when (ecuIsTopMod ecu && ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
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
         ;  when (isJust mbCoreSem && (ehcOptEmitGrin opts || ehcOptEmitGrinBC opts || ehcOptFullProgGRIN opts))
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
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
%%[[20
                 expNmOffMp
                        = crsiExpNmOffMp modNm crsi
                 optim  = crsiOptim crsi
%%]]
                 (bc,errs)
                        = grinMod2ByteCodeMod opts
%%[[20
                            (if ecuIsTopMod ecu then [ m | (m,_) <- sortOn snd $ Map.toList $ Map.map fst $ crsiModOffMp crsi ] else [])
                            (crsiModOffMp crsi)
                            expNmOffMp
%%]]
                            $ grin
         ;  when (isJust mbGrin && (ehcOptEmitGrinBC opts))
                 (do { cpUpdCU modNm $! ecuStoreGrinBC bc
                     ; lift $ putPPFile (fpathToStr $ fpathSetSuff "grin-bc" $ fp) (ppGrModule grin) 400
                     })
         ;  when (ehcOptErrAboutGrinBC opts)
                 (cpSetLimitErrsWhen 5 "Grin to ByteCode" errs)
         }
%%]

%%[8
cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin" mbGrin
         ;  when (isJust mbGrin && (ehcOptFullProgGRIN opts))
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }
%%]

%%[8
cpTranslateByteCode :: HsName -> EHCompilePhase ()
cpTranslateByteCode modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbGrinBC = ecuMbGrinBC ecu
%%[[8
                 grinbcPP = gbmod2C opts $ panicJust "cpTranslateByteCode" mbGrinBC
%%][20
                 grinbcPP = vlist ([ppMod] ++ (if ecuIsTopMod ecu then [ppMain] else []))
                          where (ppMod,ppMain)
                                  = gbmod2C opts $ panicJust "cpTranslateByteCode" mbGrinBC
%%]]
         ;  when (ehcOptEmitGrinBC opts && isJust mbGrinBC)
                 (do { cpUpdCU modNm $! ecuStoreGrinBCSem grinbcPP
                     })
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
                 && (ehcOptFullProgGRIN opts)
                 )
                 (cpUpdCU modNm $! ecuStoreGrin $! optGrin)
         }
%%]

%%[8
cpMsgGrinTrf :: HsName -> String -> EHCompilePhase ()
cpMsgGrinTrf modNm m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Local GRIN optim" (Just m) fp
       }

cpFromGrinTrf :: HsName -> (Grin.GrModule -> Grin.GrModule) -> String -> EHCompilePhase ()
cpFromGrinTrf modNm trf m
  = do { cr <- get
       ; let (ecu,_,_,fp) = crBaseInfo modNm cr
       ; cpMsgGrinTrf modNm m
       ; cpUpdCU modNm $ ecuStoreGrin $ trf $ fromJust $ ecuMbGrin ecu
       }
%%]

%%[8
cpOptimiseGrinLocal :: HsName -> EHCompilePhase ()
cpOptimiseGrinLocal modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 optGrin= if ehcOptOptimise opts >= OptimiseNormal
                          then if ehcOptFullProgGRIN opts
                               then mk [ fl, ale, eve, ale, nme ]
                               else (  mk [ mte, unb ]
                                    ++ evel
%%[[8
                                    ++ mk [ ( grInline  , "inline" ) ]
%%][20
                                    ++ [ do { cr <- get
                                            ; let (ecu,crsi,_,_) = crBaseInfo modNm cr
                                                  expNmOffMp
                                                         = crsiExpNmOffMp modNm crsi
                                                  optim  = crsiOptim crsi
                                                  (g,gathInlMp) = grInline (Map.keysSet expNmOffMp) (optimGrInlMp optim) $ fromJust $ ecuMbGrin ecu
                                            ; cpMsgGrinTrf modNm "inline"
                                            ; cpUpdCU modNm (ecuStoreOptim (defaultOptim {optimGrInlMp = gathInlMp}) . ecuStoreGrin g)
                                            } ]
%%]]
                                    ++ evel
                                    ++ mk [ nme ]
                                    )
                          else if ehcOptFullProgGRIN opts
                               then []
                               else mk [ mte, unb, fl ]
                        where mk   = map (\(trf,msg) -> cpFromGrinTrf modNm trf msg)
                              evel = mk [ fl, ale, eve, ale ]
                              fl   = ( grFlattenSeq, "flatten" )
                              ale  = ( grAliasElim , "alias elim" )
                              nme  = ( grUnusedNameElim , "unused name elim" )
                              eve  = ( grEvalElim  , "eval elim" )
                              mte  = ( grUnusedMetaInfoElim  , "meta info elim" )
                              unb  = ( grUnbox GrinBC.tagUnbox  , "unbox" )
         ;  when (isJust $ ecuMbGrin ecu)
                 (cpSeq optGrin)
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
                            (do { cpMsg' modNm VerboseALot "GCC" Nothing fpTarg
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
                         (do { cpMsg modNm VerboseALot "CPP"
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
                 u1     = uidChild $ crsiHereUID $ crsi
                 core2  = ( case trfNm of
                              "CER"     -> cmodTrfEtaRed
                              "CETA"    -> cmodTrfElimTrivApp opts
                              "CCP"     -> cmodTrfConstProp opts
                              "CRU"     -> cmodTrfRenUniq
                              "CLU"     -> cmodTrfLetUnrec
                              "CILA"    -> cmodTrfInlineLetAlias
%%[[20
                                             (Map.keysSet $ crsiExpNmOffMp modNm crsi)
%%]]
                              "CFL"     -> cmodTrfFullLazy u1
                              "CLGA"    -> cmodTrfLamGlobalAsArg
                              "CCGA"    -> cmodTrfCAFGlobalAsArg
                              "CLFG"    -> cmodTrfFloatToGlobal
%%[[101
                              "CS"      -> cmodTrfStrip
%%]]
                              -- "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  cpMsg' modNm VerboseALot "Transforming" (lookup trfNm cmdLineTrfs) fp
%%[[101
%%]]
         ;  cpUpdCU modNm $! ecuStoreCore $! core2
         }
%%]
         ;  cpMsg  modNm VerboseDebug ("Core sz1: " ++ showSizeCore core)
         ;  cpMsg  modNm VerboseDebug ("Core sz2: " ++ showSizeCore core2)

%%[8
cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 tr     = ehcOptTrf opts
                 ps     = intersperse cpStepUID 
                          $ map (cpCore1Trf modNm)
                          $ filter (maybe True id . trfOptOverrides tr)
                          $ trfNmL
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
%%[[20
          , cpFlowHsSem1 modNm
%%]]
          , cpTranslateHs2EH modNm
%%[[99
          , cpCleanupHS modNm
%%]]
          ]

cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm
  = cpSeq [ cpFoldEH modNm
%%[[99
          , cpCleanupFoldEH modNm
%%]]
          , cpFlowEHSem1 modNm
          , cpTranslateEH2Core modNm
%%[[99
          , cpCleanupEH modNm
%%]]
          ]

cpProcessCoreBasic :: HsName -> EHCompilePhase ()
cpProcessCoreBasic modNm 
  = cpSeq [ cpTransformCore
              modNm
                (
%%[[101
                  -- [ "CS" ] ++
%%]]
                  [ "CER", "CRU", "CLU", "CILA", "CETA", "CCP", "CILA", "CETA", "CFL", "CLGA", "CCGA", "CLU", "CFL", "CLFG" ]
                )
          , cpOutputCore "core" modNm
          ]
          
cpProcessCoreRest :: HsName -> EHCompilePhase ()
cpProcessCoreRest modNm 
  = cpSeq [ cpFoldCore modNm
%%[[20
          , cpFlowCoreSem modNm
%%]]
          , cpTranslateCore2Grin modNm
%%[[99
          , cpCleanupCore modNm
%%]]
          ]
          
%%]

%%[8
cpProcessGrinAll' :: HsName -> EHCompilePhase ()
cpProcessGrinAll' modNm 
  = cpSeq [ cpOutputGrin "grin2" modNm
          , cpOptimiseGrinLocal modNm
          , cpTranslateGrin2ByteCode modNm
          -- , cpOptimizeGrin modNm
          , cpOutputGrin "grin3" modNm
          , cpTranslateGrin modNm
          ]
%%]

%%[20
cpProcessGrinModOnly' :: HsName -> EHCompilePhase ()
cpProcessGrinModOnly' modNm 
  = cpSeq [ cpOutputGrin "grin2" modNm
          , cpOptimiseGrinLocal modNm
          , cpTranslateGrin2ByteCode modNm
%%[[20
          , cpFlowOptim modNm
%%]]
%%[[99
          , cpCleanupGrin modNm
%%]]
          ]

cpProcessGrinFullProg' :: HsName -> EHCompilePhase ()
cpProcessGrinFullProg' modNm 
  = cpSeq [ cpOutputGrin "grin2" modNm
          -- , cpOptimizeGrin modNm
          , cpOptimiseGrinLocal modNm
          , cpOutputGrin "grin3" modNm
          , cpTranslateGrin modNm
          ]
%%]

%%[8
cpProcessGrinBC :: HsName -> EHCompilePhase ()
cpProcessGrinBC modNm 
  = cpSeq [ cpTranslateByteCode modNm
%%[[99
          , cpCleanupFoldGrinBC modNm
%%]]
          , cpOutputByteCodeC "c" modNm
%%[[99
          , cpCleanupGrinBC modNm
%%]]
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
                 (do { cpMsg modNm VerboseALot "Emit Core"
                     ; lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (ppCModule opts cMod) 100
                     })
         ;  when (ehcOptEmitJava opts)
                 (lift (putPPFile (fpathToStr (fpathSetSuff "java" fpJ)) jPP 100))
         }
%%]

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
                 (do { cpMsg modNm VerboseALot "Emit Grin"
                     ; lift $ putPPFile (fpathToStr fpG) grinPP 1000
                     })
         ;  when (ehcOptShowGrin opts)
                 (lift $ putPPLn grinPP)
         }
%%]

%%[8
cpOutputByteCodeC :: String -> HsName -> EHCompilePhase ()
cpOutputByteCodeC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,fp) = crBaseInfo modNm cr
                 mbPP     = ecuMbGrinBCSem ecu
                 fpC      = fpathSetSuff suff fp
         ;  when (ehcOptEmitGrinBC opts && isJust mbPP)
                 (do { cpMsg' modNm VerboseALot "Emit ByteCode C" Nothing fpC
                     ; lift $ putPPFile (fpathToStr fpC) (fromJust mbPP) 150
                     })
         }
%%]

%%[20
cpOutputHI :: String -> HsName -> EHCompilePhase ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mmi    = panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 binds  = Seq.toList $ HI.hiFromHIInfo
                          $ ((ecuHIInfo ecu)
                               { HI.hiiExps       = mmiExps       mmi
                               , HI.hiiHiddenExps = mmiHiddenExps mmi
                               })
                 hi     = HISem.wrap_AGItf
                            (HISem.sem_AGItf
                              (HI.AGItf_AGItf $ HI.Module_Module modNm
                                $ HI.Binding_Stamp (Cfg.verTimestamp Cfg.version) (Cfg.verSig Cfg.version) (Cfg.verMajor Cfg.version) (Cfg.verMinor Cfg.version) (Cfg.verQuality Cfg.version) (Cfg.verSvn Cfg.version) (optsDiscrRecompileRepr opts) 0
                                  : binds))
                            (crsiHIInh crsi)
         ;  cpMsg modNm VerboseALot "Emit HI"
         ;  lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (HISem.pp_Syn_AGItf hi) 120
         ;  now <- lift $ getClockTime
         ;  cpUpdCU modNm $ ecuStoreHITime now
         }

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% XXX periments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
infixr 2 >$>

(>$>) :: CompileRunError e p => CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
this >$> next = this >-> next

cpLift :: CompilePhase n u i e () -> CompilePhase n u i e ()
cpLift = id
%%]
infixr 2 >$>

(>$>) :: CompileRunError e p => (CompilePhase n u i e () -> CompilePhase n u i e ()) -> CompilePhase n u i e () -> CompilePhase n u i e ()
this >$> next = this next

cpLift :: CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
cpLift this next
  = do { _ <- this
       ; next
       }

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
%%% Compile actions: cleanup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
cpCleanupHSMod :: HsName -> EHCompilePhase ()
cpCleanupHSMod modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHSSemMod     	  = Nothing
               }
      )

cpCleanupHS :: HsName -> EHCompilePhase ()
cpCleanupHS modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHS              = Nothing
               , ecuMbHSSem           = Nothing
               }
      )

cpCleanupFoldEH :: HsName -> EHCompilePhase ()
cpCleanupFoldEH modNm 
  = cpUpdCU modNm
      (\e -> e { ecuMbEH              = Nothing
               }
      )

cpCleanupEH :: HsName -> EHCompilePhase ()
cpCleanupEH modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbEHSem           = Nothing
               }
      )

cpCleanupCore :: HsName -> EHCompilePhase ()
cpCleanupCore modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbCore            = Nothing
               , ecuMbCoreSem         = Nothing
               }
      )

cpCleanupGrin :: HsName -> EHCompilePhase ()
cpCleanupGrin modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbGrin            = Nothing
               }
      )

cpCleanupFoldGrinBC :: HsName -> EHCompilePhase ()
cpCleanupFoldGrinBC modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbGrinBC          = Nothing
               }
      )

cpCleanupGrinBC :: HsName -> EHCompilePhase ()
cpCleanupGrinBC modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbGrinBCSem       = Nothing
               }
      )

cpCleanupCU :: HsName -> EHCompilePhase ()
cpCleanupCU modNm
  = do { cpCleanupGrin modNm
       ; cpUpdCU modNm
           (\e -> e { ecuHIInfo            = HI.emptyHIInfo
                    , ecuMbOptim           = Nothing
                    }
           )
       }

cpCleanupFlow :: HsName -> EHCompilePhase ()
cpCleanupFlow modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHSSemMod        = Nothing
               , ecuMbPrevHI          = Nothing
               , ecuMbPrevHISem       = Nothing
               }
      )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: debug info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpMemUsage :: EHCompilePhase ()
cpMemUsage
%%[[8
  = return ()
%%][101
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
       ; size <- lift $ megaBytesAllocated
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (lift $ putStrLn ("Mem: " ++ show size ++ "M"))
       }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: compilation of module(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpCompileCU.sig
cpCompileCU :: HsName -> EHCompilePhase ()
cpCompileCU modNm
%%]
%%[20 -8.cpCompileCU.sig
cpCompileCU :: Maybe HSState -> HsName -> EHCompilePhase ()
cpCompileCU targHSState modNm
%%]
%%[8
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
%%[[8
       ; case (ecuState ecu,undefined) of
%%][20
       ; case (ecuState ecu,targHSState) of
%%]]
%%]
%%[20
           (ECUSHaskell HSStart,Just HSOnlyImports)
             -> do { cpMsg modNm VerboseNormal "Imports of Haskell"
%%[[20
                   ; cuImportHS modNm
%%][99
                   ; cuImportHS False modNm
%%]]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSOnlyImports))
                   }
%%[[99
           (ECUSHaskell LHSStart,Just HSOnlyImports)
             -> do { cpMsg modNm VerboseNormal "Imports of Literate Haskell"
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
             -> cpSeq [ cpMsg modNm VerboseNormal "Compiling Haskell"
%%[[20
                      , cuCompileHSAfterImport (ecuIsTopMod ecu) opts modNm
%%][99
                      , cuCompileHSAfterImport (ecuIsTopMod ecu) opts False modNm
%%]]
                      , cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                      ]
           (ECUSHaskell st,Just HSAllSemHI)
             |    st == HSOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> do { cpMsg modNm VerboseNormal "Reading HI"
                   ; cpUpdateModOffMp [modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSemHI))
                   }
%%]]
%%[[99
           (ECUSHaskell LHSOnlyImports,Just HSAllSem)
             -> do { cpMsg modNm VerboseNormal "Compiling Literate Haskell"
                   ; cuCompileHSAfterImport (ecuIsTopMod ecu) opts True modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%]]
%%]
%%[8
           (ECUSHaskell HSStart,_)
             -> do { cpMsg modNm VerboseNormal "Compiling Haskell"
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
                           , cpProcessCoreBasic modNm
                           , cpProcessCoreRest modNm
                           , cpProcessGrinAll' modNm
                           , cpProcessGrinBC modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           ]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%[[20
           (_,Just HSOnlyImports)
             -> return ()
%%]]
           (ECUSEh EHStart,_)
             -> do { cpMsg modNm VerboseNormal "Compiling EH"
                   ; cpSeq [ cpParseEH modNm
                           , cpStopAt CompilePoint_Parse
                           , cpStepUID
                           , cpProcessEH modNm
%%[[20
                           , cpGetCheckEhMod modNm
%%]]
                           , cpStopAt CompilePoint_AnalEH
                           , cpStepUID
                           , cpProcessCoreBasic modNm
                           , cpStopAt CompilePoint_Core
                           , cpProcessCoreRest modNm
                           , cpProcessGrinAll' modNm
                           , cpProcessGrinBC modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           ]
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   }
           (ECUSGrin,_)
             -> do { cpMsg modNm VerboseNormal "Compiling Grin"
                   ; cpSeq [ cpParseGrin modNm, cpProcessGrinAll' modNm, cpProcessGrinBC modNm ]
                   }
           _ -> return ()
       }
%%]
%%[20
  where
%%[[20
        cuImportHS modNm
%%][99
        cuImportHS litmode modNm
%%]]
          = cpSeq [ cpSetupMod modNm
%%[[20
                  , cpParseHsImport modNm
%%][99
                  , cpPreprocessWithCPP modNm
                  , cpParseHsImport litmode modNm
%%]]
                  , cpStepUID
                  , cpFoldHsMod modNm
                  , cpGetHsImports modNm
                  ]
%%[[20
        cuCompileHSAfterImport isTopMod opts modNm
%%][99
        cuCompileHSAfterImport isTopMod opts litmode modNm
%%]]
          = cpSeq [ p1 modNm, p2 modNm, p3 modNm, p4 modNm, p5 modNm
                  , cpOutputHI "hi" modNm
%%[[99
                  , cpCleanupCU modNm
%%]]
                  ]
          where p1 modNm
                  = cpSeq [
%%[[20
                            cpParseHs modNm
%%][99
                            cpParseHs litmode modNm
%%]]
                          , cpMsg modNm VerboseALot "Parsing done"
                          , cpStopAt CompilePoint_Parse
                          ]
                p2 modNm
                  = cpSeq [ cpStepUID, cpFoldHsMod modNm, cpGetHsMod modNm
%%[[99
                          , cpCleanupHSMod modNm
%%]]
                          , cpCheckMods [modNm], cpUpdateModOffMp [modNm], cpProcessHs modNm
                          , cpMsg modNm VerboseALot "Name+dependency analysis done"
                          , cpStopAt CompilePoint_AnalHS
                          ]
                p3 modNm
                  = cpSeq [ cpStepUID, cpProcessEH modNm
                          , cpMsg modNm VerboseALot "Type analysis done"
                          , cpStopAt CompilePoint_AnalEH
                          ]
                p4 modNm
                  = cpSeq [ cpStepUID, cpProcessCoreBasic modNm
						  , cpMsg modNm VerboseALot "Core (basic) done"
						  , cpStopAt CompilePoint_Core
                          ]
                p5 modNm
                  = cpSeq [ cpSeq (if not (ehcOptFullProgGRIN opts)
                                   then [cpProcessCoreRest modNm, cpProcessGrinModOnly' modNm, cpProcessGrinBC modNm]
                                        ++ (if isTopMod then [] else [cpCompileWithGCC GCC_CompileOnly [] modNm])
                                   else []
                                  )
                          , cpMsg modNm VerboseALot "Core+Grin done"
                          ]
%%]

%%[8
%%]
crCompileCG :: Maybe HSState -> [HsName] -> EHCompileRun -> IO EHCompileRun
crCompileCG targHSState modNmL cr
  = do { let grpNm = hsnFromString $ concat $ intersperse "-" $ map show $ modNmL
             crsi  = crStateInfo cr
             cr2   = cr {crStateInfo = crsi {crsiGrpMp = Map.insert grpNm (emptyECG {ecgNm = grpNm, ecgModL = modNmL}) (crsiGrpMp crsi)}}
             crSetNm = crSeq $ map (\n -> crUpdCU n (\ecu -> return (ecu {ecuGrpNm = grpNm}))) modNmL
       ; crSetNm cr2
       }

%%[20
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
                   ECUSHaskell HSAllSem   -> return ()
                   ECUSHaskell HSAllSemHI -> cpFlowHISem m
                   _                      -> return ()
%%[[99
               ; cpCleanupFlow m
%%]]
               }
        core mL
          = cpSeq [cpGetPrevCore m | m <- mL]
        biggrin opts mL (mImpL,mMain)
          = if ehcOptFullProgGRIN opts
            then cpSeq [core mL, oneBigCore, cpProcessCoreRest mMain, cpProcessGrinFullProg' mMain]
            else return ()
          where oneBigCore
                  = do { cr <- get
                       ; cpUpdCU mMain (ecuStoreCore (Core.cModMerge [ panicJust "cpCompileOrderedCUs.oneBigCore" $ ecuMbCore $ crCU m cr | m <- mL ]))
                       }
        gcc (mImpL,mMain)
          = cpSeq [cpCompileWithGCC GCC_CompileExec mImpL mMain]
        
%%]

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
                                        ; putStrLn (disp (ppErrL $ Seq.toList $ HSSem.errSq_Syn_AGItf $ wrRes) 1000 "")
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
                                                                                    `gamUnion` HSSem.clGam2IdDefOccGam initClGam
%%]]
                                              , HSSem.gUniq_Inh_AGItf           = uidStart
%%[[20
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
%%[[20
                                              , EHSem.isTopMod_Inh_AGItf        = False
                                              , EHSem.valGam_Inh_AGItf          = emptyGam
                                              , EHSem.dataGam_Inh_AGItf         = emptyGam
                                              , EHSem.tyGam_Inh_AGItf           = initTyGam
                                              , EHSem.tyKiGam_Inh_AGItf         = initTyKiGam
                                              , EHSem.kiGam_Inh_AGItf           = initKiGam
                                              , EHSem.clGam_Inh_AGItf           = initClGam
                                              , EHSem.chrStore_Inh_AGItf        = initScopedPredStore
                                              , EHSem.idQualGam_Inh_AGItf       = emptyGam
%%]]
                                              }
             coreInh        = Core2GrSem.Inh_CodeAGItf
                                              { Core2GrSem.gUniq_Inh_CodeAGItf           = uidStart
                                              , Core2GrSem.dataGam_Inh_CodeAGItf         = emptyGam
                                              , Core2GrSem.opts_Inh_CodeAGItf            = opts2
%%[[20
                                              , Core2GrSem.arityMp_Inh_CodeAGItf         = Map.empty
%%]]
                                              }
%%[[20
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
%%[[20
                                 hiInh hsModInh Map.empty Map.empty defaultOptim Map.empty
%%]]
                                )
%%[[8
             comp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    ; when (isJust mbFoundFp)
                           (cpCompileCU nm)
                    }
%%][20
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
%%][20
       ; _ <- runStateT (cpSeq [ imp (Just fp) topModNm
                               , cpImportGather (imp Nothing) topModNm
                               , cpCheckMods' [modBuiltin]
                               , cpCompileOrderedCUs
                               ]) initialState
%%]]
       ; return ()
       }

%%]

