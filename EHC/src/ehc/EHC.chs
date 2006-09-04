%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, Data.List, Control.Monad, System.Console.GetOpt, IO, EH.Util.Utils,UU.Pretty,EH.Util.PPUtils,{%{EH}Error.Pretty}, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Base.Opts})
%%]

%%[1 import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.Parser} as HSPrs, qualified {%{EH}HS.MainAG} as HSSem)
%%]

%%[8 import (EH.Util.CompileRun,{%{EH}Error},{%{EH}Gam},EH.Util.FPath,qualified Data.Map as Map,Data.Maybe,Data.List, EH.Util.ParseUtils)
%%]

%%[8 import ({%{EH}Core.Java},{%{EH}Core.Grin},{%{EH}Core.Pretty})
%%]

%%[8 import ({%{EH}Core.Trf.RenUniq},{%{EH}Core.Trf.FullLazy},{%{EH}Core.Trf.InlineLetAlias},{%{EH}Core.Trf.LetUnrec},{%{EH}Core.Trf.LamLift},{%{EH}Core.Trf.ConstProp},{%{EH}Core.Trf.EtaRed})
%%]

%%[8 import ({%{EH}GrinCode.Pretty})
%%]

%%[8 import (qualified {%{GRIN}CompilerDriver} as GRINC, qualified {%{GRIN}GRINCCommon} as GRINCCommon)
%%]

%%[8 import (qualified {%{EH}HS} as HS)
%%]
%%[8 import (qualified {%{EH}EH} as EH)
%%]
%%[8 import (qualified {%{EH}Core} as Core)
%%]
%%[8 import (qualified {%{EH}GrinCode} as Grin)
%%]
%%[8 import (qualified {%{GRIN}CmmCode} as Cmm)
%%]
%%[8 import (Control.Monad.State)
%%]
%%[8 import (qualified EH.Util.ScanUtils as ScanUtils)
%%]
%%[8 import(qualified {%{EH}GrinCode.Parser} as GrinParser)
%%]

%%[12 import (qualified EH.Util.Rel as Rel)
%%]

%%[12 import (System.Time, System.Directory)
%%]

%%[12 import (qualified {%{EH}HI.Parser} as HIPrs, {%{EH}Module}, qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]

%%[12 import (qualified {%{EH}Core.Parser} as CorePrs)
%%]

%%[12 import (qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Version of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
versionSvn      = "$Id$"
versionMajor    = "0"
versionMinor    = "1"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ehc"
versionInfo     = versionProg ++ versionDist ++ ", " ++ versionSvn
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
%%]
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldl (flip ($)) ehcOpts o
         ;  if ehcOptHelp opts
%%]
%%[1.main.ehcOptHelp
            then  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
%%]
%%[8.main.ehcOptHelp -1.main.ehcOptHelp
            then  do  {  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
                      }
%%]
%%[1.main.tl
            else  if ehcOptVersion opts
            then  putStrLn versionDist
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
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data HaskellState
  = HSStart
  | HSAllSem
%%[[12
  | HSOnlyImports
%%]
  deriving (Show,Eq)

data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)

data EHCompileUnitState
  = ECUSUnknown | ECUSHaskell HaskellState | ECUSEh EHState | ECUSGrin | ECUSFail
  deriving (Show,Eq)

data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: FPath
      , ecuGrpNm             :: HsName
      , ecuModNm             :: HsName
%%[[12
      , ecuIsTopMod          :: Bool
%%]
      , ecuImpNmL            :: [HsName]
      , ecuMbHSSem           :: Maybe HSSem.Syn_AGItf
      , ecuMbEHSem           :: Maybe EHSem.Syn_AGItf
      , ecuMbHS              :: Maybe HS.AGItf
      , ecuMbEH              :: Maybe EH.AGItf
      , ecuMbCore            :: Maybe Core.CModule
      , ecuMbGrin            :: Maybe Grin.GrModule
      , ecuState             :: EHCompileUnitState
%%[[12
      , ecuMbHSTime          :: Maybe ClockTime
      , ecuMbHITime          :: Maybe ClockTime
      , ecuMbCoreTime        :: Maybe ClockTime
      , ecuMbHSSemMod        :: Maybe HSSemMod.Syn_AGItf
      , ecuMod               :: Mod
      , ecuMbPrevCore        :: Maybe Core.CModule
      , ecuMbPrevHI          :: Maybe HI.AGItf
%%]
      }

type EcuUpdater a = a -> EHCompileUnit -> EHCompileUnit

ecuStoreHS :: EcuUpdater HS.AGItf
ecuStoreHS x ecu = ecu { ecuMbHS = Just x }

ecuStoreEH :: EcuUpdater EH.AGItf
ecuStoreEH x ecu = ecu { ecuMbEH = Just x }

ecuStoreHSSem :: EcuUpdater HSSem.Syn_AGItf
ecuStoreHSSem x ecu = ecu { ecuMbHSSem = Just x }

ecuStoreEHSem :: EcuUpdater EHSem.Syn_AGItf
ecuStoreEHSem x ecu = ecu { ecuMbEHSem = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }

ecuStoreGrin :: EcuUpdater Grin.GrModule
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }
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

ecuStoreMod :: EcuUpdater Mod
ecuStoreMod x ecu = ecu { ecuMod = x, ecuImpNmL = map mimpSource $ modImpL $ x }

ecuSetIsTopMod :: EcuUpdater Bool
ecuSetIsTopMod x ecu = ecu { ecuIsTopMod = x }

ecuStorePrevCore :: EcuUpdater Core.CModule
ecuStorePrevCore x ecu = ecu { ecuMbPrevCore = Just x }

ecuStorePrevHI :: EcuUpdater HI.AGItf
ecuStorePrevHI x ecu = ecu { ecuMbPrevHI = Just x }

%%]

%%[8
ecuStoreState :: EcuUpdater EHCompileUnitState
ecuStoreState x ecu = ecu { ecuState = x }

emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuGrpNm             = hsnUnknown
      , ecuModNm             = hsnUnknown
%%[[12
      , ecuIsTopMod          = False
%%]
      , ecuImpNmL            = []
      , ecuMbHSSem           = Nothing
      , ecuMbEHSem           = Nothing
      , ecuMbHS              = Nothing
      , ecuMbEH              = Nothing
      , ecuMbCore            = Nothing
      , ecuMbGrin            = Nothing
      , ecuState             = ECUSUnknown
%%[[12
      , ecuMbHSTime          = Nothing
      , ecuMbHITime          = Nothing
      , ecuMbCoreTime        = Nothing
      , ecuMbHSSemMod        = Nothing
      , ecuMod               = emptyMod
      , ecuMbPrevCore        = Nothing
      , ecuMbPrevHI          = Nothing
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
                      ECUSHaskell HSOnlyImports -> True
                      ECUSHaskell HSAllSem      -> True
                      _                         -> False
%%]

%%[8
instance CompileUnit EHCompileUnit HsName EHCompileUnitState where
  cuDefault         = emptyECU
  cuFPath           = ecuFilePath
  cuKey             = ecuModNm
  cuState           = ecuState
  cuUpdFPath fp u   = u {ecuFilePath = fp}
  cuUpdState st u   = u {ecuState = st}
  cuUpdKey   nm u   = u {ecuModNm = nm}
  cuImports         = ecuImpNmL

instance CompileRunError Err () where
  crePPErrL                 = ppErrL
  creMkNotFoundErrL _ fp sp = [Err_FileNotFound fp sp]
  creAreFatal               = const True

instance CompileModName HsName where
  mkCMNm = HNm

instance Show EHCompileUnit where
  show _ = "EHCompileUnit"

instance PP EHCompileUnit where
  pp ecu
    = ecuModNm ecu >|<
%%[[12
      ":" >#< ppCommas (ecuImpNmL ecu) >|<
%%]
      "," >#< show (ecuState ecu)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: EHCOpts
      , crsiHSInh       :: HSSem.Inh_AGItf
      , crsiEHInh       :: EHSem.Inh_AGItf
      , crsiNextUID     :: UID
      , crsiHereUID     :: UID
%%[[12
      , crsiHSModInh    :: HSSemMod.Inh_AGItf
      , crsiModMp       :: ModMp
      , crsiGrpMp       :: Map.Map HsName EHCompileGroup
%%]
      }
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
type FileSuffMp = Map.Map String EHCompileUnitState

fileSuffMpHs :: FileSuffMp
fileSuffMpHs = Map.fromList [ ( "hs", ECUSHaskell HSStart ), ( "eh", ECUSEh EHStart ), ( "grin", ECUSGrin ) ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
-- should move to ParseUtils
parsePlain :: (Symbol s, InputState inp s pos) 
      => AnaParser inp Pair s pos a 
      -> inp 
      -- -> Steps (Pair a (Pair inp ())) s pos
      -> Steps (a, inp) s pos
parsePlain p inp
  = val fromPair (parse p inp)
  where fromPair (Pair x (Pair y _)) = (x,y)

%%[8
cpParseOffside :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> HsName -> EHCompilePhase ()
cpParseOffside parser scanOpts store description modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let steps = parseOffside parser tokens
      ; cpUpdCU modNm (store (fst (evalSteps steps)))
      ; cpSetLimitErrsWhen 5 description (map mkPPErr (getMsgs steps))
      }

cpParsePlain :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> FPath -> HsName -> EHCompilePhase ()
cpParsePlain parser scanOpts store description fp modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath fp ReadMode
      ; tokens  <- lift $ scanHandle scanOpts fn fh
      ; let steps = parsePlain parser tokens
      ; cpUpdCU modNm (store (fst (evalSteps steps)))
      ; cpSetLimitErrsWhen 5 description (map mkPPErr (getMsgs steps))
      }

cpParseHs :: HsName -> EHCompilePhase ()
cpParseHs = cpParseOffside HSPrs.pAGItf hsScanOpts ecuStoreHS "Parse (Haskell syntax) of module"

cpParseEH :: HsName -> EHCompilePhase ()
cpParseEH = cpParseOffside EHPrs.pAGItf ehScanOpts ecuStoreEH "Parse (EH syntax) of module"

cpParseGrin :: HsName -> EHCompilePhase ()
cpParseGrin modNm
  = do { cr <- get
       ; cpParsePlain GrinParser.pModule grinScanOpts ecuStoreGrin "Parse grin" (ecuFilePath (crCU modNm cr)) modNm
       }

{-
cpParseGrin :: HsName -> EHCompilePhase ()
cpParseGrin modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ scanHandle GrinParser.scanOpts fn fh
      ; gr      <- lift $ parseIO GrinParser.pModule tokens
      ; cpUpdCU modNm (ecuStoreGrin gr)
      }
-}
%%]

%%[12
cpParsePrevCore :: HsName -> EHCompilePhase ()
cpParsePrevCore modNm
  = do { cr <- get
       ; cpParsePlain CorePrs.pCModule coreScanOpts ecuStorePrevCore "Parse Core (of previous compile) of module" (fpathSetSuff "core" $ ecuFilePath (crCU modNm cr)) modNm
       }

cpParsePrevHI :: HsName -> EHCompilePhase ()
cpParsePrevHI modNm
  = do { cr <- get
       ; cpParsePlain HIPrs.pAGItf hiScanOpts ecuStorePrevHI "Parse HI (of previous compile) of module" (fpathSetSuff "hi" $ ecuFilePath (crCU modNm cr)) modNm
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: computing semantics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
foldEH :: EHSem.Inh_AGItf -> FPath -> UID -> EHCOpts -> EH.AGItf -> EHSem.Syn_AGItf
foldEH inh fp uid opts eh
 = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                    (inh { EHSem.baseName_Inh_AGItf = mkHNm (fpathBase fp)
                         , EHSem.gUniq_Inh_AGItf    = uid
                         , EHSem.opts_Inh_AGItf     = opts
                         })

foldHs :: HSSem.Inh_AGItf -> HsName -> EHCompileUnit -> EHCompileRunStateInfo -> HS.AGItf -> HSSem.Syn_AGItf
foldHs inh modNm ecu crsi hs
 = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
                    (inh { HSSem.opts_Inh_AGItf         = crsiOpts crsi
                         , HSSem.gUniq_Inh_AGItf        = crsiHereUID crsi
%%[[12
                         , HSSem.moduleNm_Inh_AGItf     = modNm
                         , HSSem.isTopMod_Inh_AGItf     = ecuIsTopMod ecu
                         , HSSem.modInScope_Inh_AGItf   = inscps
                         , HSSem.modEntToOrig_Inh_AGItf = exps
%%]
                         })
%%[[12
 where inscps = Rel.toDomMap $ mmiInscps $ mmi
       exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                             $ Rel.mapRng mentIdOcc $ mmiExps $ mmi
       mmi    = panicJust "foldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
%%]
%%]

%%[12
foldHsMod :: HSSemMod.Inh_AGItf -> HsName -> EHCompileUnit -> EHCompileRunStateInfo -> HS.AGItf -> HSSemMod.Syn_AGItf
foldHsMod inh modNm ecu crsi hs
 = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf hs)
                       (inh { HSSemMod.gUniq_Inh_AGItf        = crsiHereUID crsi
                            , HSSemMod.moduleNm_Inh_AGItf     = modNm
                            })
%%]

%%[8
cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = foldEH (crsiEHInh crsi) (ecuFilePath ecu) (crsiHereUID crsi) (crsiOpts crsi) (panicJust "cpFoldEH" mbEH)
         ;  when (isJust mbEH)
                 (cpUpdCU modNm (ecuStoreEHSem ehSem))
         }

cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
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
         ;  let  ecu        = crCU modNm cr
                 crsi       = crStateInfo cr
                 mbHS       = ecuMbHS ecu
                 hsSemMod   = foldHsMod (crsiHSModInh crsi) modNm ecu crsi (panicJust "cpFoldHsMod" mbHS)
         ;  when (isJust mbHS)
                 (cpUpdCU modNm (ecuStoreHSSemMod hsSemMod))
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: flowing info between module compiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
cpFlowHsSem :: HsName -> EHCompilePhase ()
cpFlowHsSem modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 hsSem  = fromJust (ecuMbHSSem ecu)
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf = HSSem.gathFixityGam_Syn_AGItf hsSem `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf     = HSSem.gathIdGam_Syn_AGItf     hsSem `gamUnion` HSSem.idGam_Inh_AGItf     hsInh
                            }
         ;  when (isJust (ecuMbHSSem ecu))
                 (put (cr {crStateInfo = crsi {crsiHSInh = hsInh'}}))
         }

cpFlowEHSem :: HsName -> EHCompilePhase ()
cpFlowEHSem modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 ehSem  = fromJust (ecuMbEHSem ecu)
                 ehInh  = crsiEHInh crsi
                 ehInh' = ehInh
                            { EHSem.valGam_Inh_AGItf  = EHSem.gathValGam_Syn_AGItf  ehSem `gamUnion` EHSem.valGam_Inh_AGItf  ehInh
                            , EHSem.tyGam_Inh_AGItf   = EHSem.gathTyGam_Syn_AGItf   ehSem `gamUnion` EHSem.tyGam_Inh_AGItf   ehInh
                            , EHSem.kiGam_Inh_AGItf   = EHSem.gathKiGam_Syn_AGItf   ehSem `gamUnion` EHSem.kiGam_Inh_AGItf   ehInh
                            , EHSem.dataGam_Inh_AGItf = EHSem.gathDataGam_Syn_AGItf ehSem `gamUnion` EHSem.dataGam_Inh_AGItf ehInh
                            , EHSem.prIntroGam_Inh_AGItf = EHSem.gathPrIntroGam_Syn_AGItf ehSem `gamUnion` EHSem.prIntroGam_Inh_AGItf ehInh
                            , EHSem.prElimTGam_Inh_AGItf = tgamAddGam (EHSem.gathPrfCtxtId_Syn_AGItf ehSem) (EHSem.prfCtxtId_Inh_AGItf ehInh) (EHSem.gathPrElimTGam_Syn_AGItf ehSem) (EHSem.prElimTGam_Inh_AGItf ehInh)
                            }
         ;  when (isJust (ecuMbEHSem ecu))
                 (put (cr {crStateInfo = crsi {crsiEHInh = ehInh'}}))
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
  = cpSeq [cpGetModfTimes modNm,cpGetPrevCompile modNm]
%%]

%%[12
cpGetPrevCompile :: HsName -> EHCompilePhase ()
cpGetPrevCompile modNm
  = do { cr <- get
       ; let  ecu        = crCU modNm cr
       ; when (isJust $ ecuMbHITime ecu)
              (cpParsePrevHI modNm)
       ; when (isJust $ ecuMbCoreTime ecu)
              (cpParsePrevCore modNm)
       }
%%]

%%[12
cpGetHsImports :: HsName -> EHCompilePhase ()
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: compile phase (fold + output)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
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

cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 mbEHSem= ecuMbEHSem ecu
                 fp     = ecuFilePath ecu
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

cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpTranslateCore2Grin" mbCore
                 u1     = uidChild . crsiHereUID $ crsi
                 grin   = cmodGrin u1 cMod
         ;  when (isJust mbCore && (ehcOptEmitGrin opts || ehcOptEmitCmm opts || ehcOptEmitLlc opts))
                 (cpUpdCU modNm (ecuStoreGrin grin))
         }

cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin" mbGrin
         ;  when (isJust mbGrin && (ehcOptEmitCmm opts || ehcOptEmitLlc opts))
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpCore1Trf :: HsName -> String -> EHCompilePhase ()
cpCore1Trf modNm trfNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 mbCore = ecuMbCore ecu
                 core   = panicJust "cpCore1Trf" mbCore
                 u1     = uidChild . crsiHereUID $ crsi
                 core2  = ( case trfNm of
                              "CER"     -> cmodTrfEtaRed
                              "CCP"     -> cmodTrfConstProp
                              "CRU"     -> cmodTrfRenUniq
                              "CLU"     -> cmodTrfLetUnrec
                              "CILA"    -> cmodTrfInlineLetAlias
                              "CFL"     -> cmodTrfFullLazy u1
                              "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  lift (putCompileMsg VerboseALot (ehcOptVerbosity . crsiOpts $ crsi) "Transforming" (lookup trfNm cmdLineTrfs) modNm (ecuFilePath ecu))
         ;  cpUpdCU modNm (ecuStoreCore core2)
         }

cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
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
          , cpTranslateHs2EH modNm
          , cpProcessEH modNm
          ]

cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm 
  = cpSeq [ cpStepUID
          , cpFoldEH modNm
%%[[12
          , cpOutputHI modNm
%%]
          , cpTranslateEH2Core modNm
          , cpProcessCore modNm
          ]

cpProcessCore :: HsName -> EHCompilePhase ()
cpProcessCore modNm 
  = cpSeq [ cpStepUID
          , cpTransformCore modNm ["CER", "CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU"]
          , cpOutputCore "core" modNm
          , cpTranslateCore2Grin modNm
          , cpProcessGrin modNm
          ]
          
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = cpSeq [ cpOutputGrin "grin2" modNm
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
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 fp     = ecuFilePath ecu
                 mbCore = ecuMbCore ecu
                 opts   = crsiOpts crsi
                 cMod   = panicJust "cpOutputCore" mbCore
                 (jBase,jPP) = cmodJavaSrc cMod
                 jFP = fpathSetBase jBase fp                 
         ;  when (ehcOptEmitCore opts) 
                 (lift (putPPFile (fpathToStr (fpathSetSuff suff fp)) (ppCModule cMod) 100))
         ;  when (ehcOptEmitJava opts)
                 (lift (putPPFile (fpathToStr (fpathSetSuff "java" jFP)) jPP 100))
         }

cpOutputGrin :: String -> HsName -> EHCompilePhase ()
cpOutputGrin suff modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 fp     = ecuFilePath ecu
                 mbGrin = ecuMbGrin ecu
                 opts   = crsiOpts crsi
                 grin   = panicJust "cpOutputGrin" mbGrin
                 grinPP = ppGrModule grin
         ;  when (ehcOptEmitGrin opts)
                 (lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) grinPP 1000)
         ;  when (ehcOptShowGrin opts)
                 (lift $ putPPLn grinPP)
         }
%%]

%%[12
cpOutputHI :: HsName -> EHCompilePhase ()
cpOutputHI modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 fp     = ecuFilePath ecu
                 hsSem  = fromJust (ecuMbHSSem ecu)
                 ehSem  = fromJust (ecuMbEHSem ecu)
                 binds  = HI.hiFromAllGams
                            (HSSem.gathFixityGam_Syn_AGItf  hsSem)
                            (EHSem.gathValGam_Syn_AGItf     ehSem)
                            (EHSem.gathTyGam_Syn_AGItf      ehSem)
                            (EHSem.gathDataGam_Syn_AGItf    ehSem)
                            (EHSem.gathPrIntroGam_Syn_AGItf ehSem)
                            (EHSem.gathPrElimTGam_Syn_AGItf ehSem,EHSem.gathPrfCtxtId_Syn_AGItf ehSem)
                 hi     = HISem.wrap_AGItf (HISem.sem_AGItf (HI.AGItf_AGItf $ HI.Module_Module modNm $ HI.Binding_Stamp "stamp" 0 : binds))
                            (HISem.Inh_AGItf)
         ;  lift (putPPFile (fpathToStr (fpathSetSuff "hi" fp)) (HISem.pp_Syn_AGItf hi) 4000)
         }

%%]

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
%%% Compile actions: compilation of module(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpCompileCU.sig
cpCompileCU :: HsName -> EHCompilePhase ()
cpCompileCU modNm
%%]
%%[12 -8.cpCompileCU.sig
cpCompileCU :: Maybe HaskellState -> HsName -> EHCompilePhase ()
cpCompileCU targHSState modNm
%%]
%%[8
  = do { cr <- get
       ; let ecu   = crCU modNm cr
             opts  = crsiOpts (crStateInfo cr)
             msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) m Nothing modNm (ecuFilePath ecu))
             -- msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm (ecuFilePath ecu))
%%]
%%[8.cpCompileCU.case
       ; case (ecuState ecu,undefined) of
%%]
%%[12 -8.cpCompileCU.case
       ; case (ecuState ecu,targHSState) of
%%]
%%[12
           (ECUSHaskell HSStart,Just HSOnlyImports)
             -> do { cpSeq [cpSetupMod modNm,cpParseHs modNm, cpStepUID, cpFoldHsMod modNm, cpGetHsImports modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSOnlyImports))
                   }
           (ECUSHaskell HSOnlyImports,Just HSOnlyImports)
             -> return ()
           (ECUSHaskell HSOnlyImports,_)
             -> do { msg "Compiling Haskell"
                   ; cpProcessHs modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%]
%%[8
           (ECUSHaskell HSStart,_)
             -> do { msg "Compiling Haskell"
                   ; cpSeq [cpSetupMod modNm,cpParseHs modNm, cpStepUID, cpProcessHs modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
%%[[12
           (_,Just HSOnlyImports)
             -> return ()
%%]
           (ECUSEh EHStart,_)
             -> do { msg "Compiling EH"
                   ; cpSeq [cpParseEH modNm, cpProcessEH modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   }
           (ECUSGrin,_)
             -> do { msg "Compiling Grin"
                   ; cpSeq [cpParseGrin modNm, cpProcessGrin modNm]
                   }
           _ -> return ()
       }
%%]

%%[8
%%]
crCompileCG :: Maybe HaskellState -> [HsName] -> EHCompileRun -> IO EHCompileRun
crCompileCG targHSState modNmL cr
  = do { let grpNm = HNm $ concat $ intersperse "-" $ map show $ modNmL
             crsi  = crStateInfo cr
             cr2   = cr {crStateInfo = crsi {crsiGrpMp = Map.insert grpNm (emptyECG {ecgNm = grpNm, ecgModL = modNmL}) (crsiGrpMp crsi)}}
             crSetNm = crSeq $ map (\n -> crUpdCU n (\ecu -> return (ecu {ecuGrpNm = grpNm}))) modNmL
       ; crSetNm cr2
       }

%%[8.cpCompileOrderedCUs
cpCompileOrderedCUs :: EHCompilePhase ()
cpCompileOrderedCUs
 = do { modNmLL <- gets crCompileOrder
      ; cpSeq [ cpCompileCU m | (m:_) <- modNmLL ]
      }
%%]

%%[12 -8.cpCompileOrderedCUs
cpCompileOrderedCUs :: EHCompilePhase ()
cpCompileOrderedCUs
 = do { modNmLL <- gets crCompileOrder
      ; let modNmL = map head modNmLL
      ; cpSeq (merge [ cpCompileCU Nothing m | m <- modNmL ]
                     [ cpSeq [cpFlowHsSem m,cpFlowEHSem m] | m <- take (length modNmL - 1) modNmL ]
              )
      }
  where merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs
%%]

%%[12
cpCheckImpExp :: EHCompilePhase ()
cpCheckImpExp
  = do { cr <- get
       ; let modLL  = [[modBuiltin]] ++ [ [ addBuiltin $ ecuMod $ crCU n cr | n <- nn ] | nn <- crCompileOrder cr ]
                    where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }
             crsi   = crStateInfo cr
             (modMp,errs)
                    = foldl (\(mp,es) ms -> let (m,e) = ms `modMpCombine` mp in (m,es++e)) (Map.empty,[]) modLL
       -- ; lift $ putWidthPPLn 120 (ppModMp modMp) -- debug
       ; put (cr {crStateInfo = crsi {crsiModMp = modMp}})
       ; cpSetLimitErrsWhen 5 "Module analysis" errs
       }
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
         ;  tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
         ;  resd <-
              if isHS
              then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; let res   = HSSem.sem_AGItf resd
                            wrRes = HSSem.wrap_AGItf res
                                      (HSSem.Inh_AGItf
                                        { HSSem.opts_Inh_AGItf = opts
                                        -- , HSSem.idGam_Inh_AGItf = emptyGam
                                        })
                      ; putStrLn (disp (ppErrL $ HSSem.errL_Syn_AGItf $ wrRes) 1000 "")
                      ; when (ehcOptShowHS opts)
                             (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
                      ; return (HSSem.eh_Syn_AGItf wrRes)
                      }
              else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; return resd
                      }
         ;  let res   = EHSem.sem_AGItf resd
                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
         ;  when (ehcOptShowEH opts)
                 (putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 ""))
         ;  when (ehcOptShowAst opts)
                 (putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 ""))
         ;  when (ehcOptShowTopTyPP opts)
                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
         }
%%]

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "hs" fn
             topModNm       = mkHNm (fpathBase fp)
             searchPath     = ehcOptSearchPath opts ++ mkInitSearchPath fp
             opts2          = opts { ehcOptSearchPath = searchPath }
             hsInh          = HSSem.Inh_AGItf { HSSem.opts_Inh_AGItf        = opts2
                                              , HSSem.idGam_Inh_AGItf       = HSSem.tyGam2IdDefOccGam initTyGam
                                                                                `gamUnion` HSSem.kiGam2IdDefOccGam initKiGam
                                              , HSSem.gUniq_Inh_AGItf       = uidStart
%%[[12
                                              , HSSem.isTopMod_Inh_AGItf    = False
                                              , HSSem.moduleNm_Inh_AGItf    = hsnUnknown
                                              , HSSem.modInScope_Inh_AGItf  = Map.empty
                                              , HSSem.modEntToOrig_Inh_AGItf= Map.empty
                                              , HSSem.fixityGam_Inh_AGItf   = emptyGam
%%]
                                              }
             ehInh          = EHSem.Inh_AGItf { EHSem.baseName_Inh_AGItf    = mkHNm (fpathBase fp)
                                              , EHSem.gUniq_Inh_AGItf       = uidStart
                                              , EHSem.opts_Inh_AGItf        = opts2
%%[[12
                                              , EHSem.valGam_Inh_AGItf      = emptyGam
                                              , EHSem.dataGam_Inh_AGItf     = emptyGam
                                              , EHSem.tyGam_Inh_AGItf       = initTyGam
                                              , EHSem.kiGam_Inh_AGItf       = initKiGam
                                              , EHSem.prIntroGam_Inh_AGItf  = emptyGam
                                              , EHSem.prElimTGam_Inh_AGItf  = emptyTGam uidStart
                                              , EHSem.prfCtxtId_Inh_AGItf   = uidStart
%%]
                                              }
%%[[12
             hsModInh       = HSSemMod.Inh_AGItf { HSSemMod.gUniq_Inh_AGItf       = uidStart
                                                 , HSSemMod.moduleNm_Inh_AGItf    = hsnUnknown
                                                 }
%%]
             initialState   = mkEmptyCompileRun
                                topModNm
                                (EHCompileRunStateInfo opts2 hsInh ehInh uidStart uidStart
%%[[12
                                 hsModInh Map.empty Map.empty
%%]
                                )
             imp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
%%[[12
                    ; when (isJust mbFp)
                           (cpUpdCU nm (ecuSetIsTopMod True))
%%]
                    ; when (isJust mbFoundFp)
                           (cpCompileCU
%%[[12
                              (Just HSOnlyImports)
%%]
                              nm)
                    }
       ; _ <- runStateT (cpSeq [ imp (Just fp) topModNm
                               , cpImportGather (imp Nothing) topModNm
%%[[12
                               , cpCheckImpExp
%%]
                               , cpCompileOrderedCUs
                               ]) initialState
       ; return ()
       }

%%]

