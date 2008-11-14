%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main
%%]

%%[1 import(System, System.Console.GetOpt)
%%]
%%[1 import(Data.Char, Data.List)
%%]
%%[1 import(qualified EH.Util.FastSeq as Seq)
%%]
%%[1 import(qualified {%{EH}Config} as Cfg)
%%]
%%[1 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- parsing later put in {%{EH}EHC.CompilePhase.Parsers}
%%[1.scannercommon import({%{EH}Scanner.Common})
%%]
%%[1.parsinglib import(UU.Parsing, UU.Parsing.Offside)
%%]
%%[1.parse.EHPrs.HSPrs import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}HS.Parser} as HSPrs)
%%]

-- compiler driver modules
%%[1 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileGroup})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[8 -(1.scannercommon 1.parsinglib 1.parse.EHPrs.HSPrs) import({%{EH}EHC.CompilePhase.Parsers})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Output})
%%]

-- general imports
%%[8 import(qualified Debug.Trace)
%%]
%%[8 import(qualified Data.Map as Map)
%%]
%%[8 import({%{EH}Gam})
%%]
-- Language syntax: HS, EH
%%[8 import(qualified {%{EH}HS} as HS, qualified {%{EH}EH} as EH)
%%]
-- Language syntax: Core, Grin, ...
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinByteCode} as Bytecode)
%%]
-- Core output
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf.RenUniq}, {%{EH}Core.Trf.FullLazy}, {%{EH}Core.Trf.InlineLetAlias}, {%{EH}Core.Trf.LetUnrec})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LamGlobalAsArg}, {%{EH}Core.Trf.CAFGlobalAsArg}, {%{EH}Core.Trf.FloatToGlobal}, {%{EH}Core.Trf.ConstProp})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EtaRed}, {%{EH}Core.Trf.ElimTrivApp})
%%]
%%[(9 codegen) import({%{EH}Core.Trf.LiftDictFields})
%%]
%%[(8_2 codegen) import({%{EH}Core.Trf.PrettyVarNames})
%%]
-- GRIN
%%[(8 codegen grin) import(qualified {%{EH}GRINCCommon} as GRINCCommon)
%%]
-- Grin output
%%[(8 codegen grin) import({%{EH}GrinCode.ToGrinByteCode})
%%]
-- Grin transformations
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.UnusedMetaInfoElim}, {%{EH}GrinCode.Trf.UnusedNameElim}, {%{EH}GrinCode.Trf.AliasElim}, {%{EH}GrinCode.Trf.MayLiveUnboxed})
%%]
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.ConstPropagation}, {%{EH}GrinCode.Trf.FlattenSeq}, {%{EH}GrinCode.Trf.EvalElim}, {%{EH}GrinCode.Trf.Inline})
%%]
%%[(8_2 codegen grin) hs import({%{EH}GrinCode.Trf.PrettyVarNames})
%%]
-- Bytecode output
%%[(8 codegen grin) import({%{EH}GrinByteCode.ToC})
%%]
-- Alternative backends
%%[(8 codegen grin) import(qualified {%{EH}EHC.GrinCompilerDriver} as GRINC)
%%]


%%[9 import(qualified {%{EH}Pred} as Pr)
%%]


%%[20 import(qualified Data.Set as Set)
%%]
%%[20 import(qualified EH.Util.Rel as Rel)
%%]
%%[20 import({%{EH}Module})
%%]
-- HI syntax and semantics
%%[20 import(qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- CHR solver
%%[(20 hmtyinfer) import({%{EH}Pred.ToCHR}, {%{EH}CHR.Solve})
%%]


-- Force evaluation for IO
%%[99 import({%{EH}Base.ForceEval})
%%]
-- CHR solver
%%[(99 hmtyinfer) import({%{EH}CHR}, {%{EH}CHR.Constraint}, {%{EH}Pred.CHR}, {%{EH}Pred.CommonCHR})
%%]

-- Misc
%%[(102 codegen) import({%{EH}Core.Trf.Strip})
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
                                    { ehcProgName      = p
                                    , ehcOptUseInplace = fpathBase p == Cfg.verProg Cfg.version
                                    }
                                where p = mkFPath progName
%%]]
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldl (flip ($)) ehcOpts o
         ;  if ehcOptHelp opts
%%[[1
            then  putStrLn (usageInfo ("version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                       ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:"
                                      ) ehcCmdLineOpts)
%%][8
            then  do  {  putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                             ++ "\n\nUsage: " ++ progName
                                             ++ " [options] [file[.eh|.hs]]\n\noptions:"
                                             )
                                             ehcCmdLineOpts)
%%[[(8 codegen)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
%%]]
                      }
%%]]
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
%%[[(8 grin)
    , ( "grin", ECUSGrin )
%%]]
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show sizes, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(102 codegen)
showSizeCore :: Core.CModule -> String
showSizeCore x = fevShow "Core" x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: computing semantics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
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
%%]

%%[8
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
%%[[(8 codegen)
                 coreSem  = panicJust "cpFlowEHSem1.coreSem" $ ecuMbCoreSem ecu
                 coreInh  = crsiCoreInh crsi
%%]]
%%[[(20 hmtyinfer)
                 dg       = prepFlow $! EHSem.gathDataGam_Syn_AGItf    ehSem
                 vg       = prepFlow $! EHSem.gathValGam_Syn_AGItf     ehSem
                 tg       = prepFlow $! EHSem.gathTyGam_Syn_AGItf      ehSem
                 tkg      = prepFlow $! EHSem.gathTyKiGam_Syn_AGItf    ehSem
                 pg       = prepFlow $! EHSem.gathPolGam_Syn_AGItf     ehSem
                 kg       = prepFlow $! EHSem.gathKiGam_Syn_AGItf      ehSem
                 clg      = prepFlow $! EHSem.gathClGam_Syn_AGItf      ehSem
                 cs       = prepFlow $! EHSem.gathChrStore_Syn_AGItf   ehSem
%%]]
%%[[20
                 hii      = ecuHIInfo ecu
                 ehInh'   = ehInh
%%[[(20 hmtyinfer)
                              { EHSem.dataGam_Inh_AGItf    = dg  `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                              , EHSem.valGam_Inh_AGItf     = vg  `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                              , EHSem.tyGam_Inh_AGItf      = tg  `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                              , EHSem.tyKiGam_Inh_AGItf    = tkg `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                              , EHSem.polGam_Inh_AGItf     = pg  `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                              , EHSem.kiGam_Inh_AGItf      = kg  `gamUnionFlow`  EHSem.kiGam_Inh_AGItf      ehInh
                              , EHSem.clGam_Inh_AGItf      = clg `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                              , EHSem.chrStore_Inh_AGItf   = cs  `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                              }
%%]]
                 hii'     = hii
%%[[(20 hmtyinfer)
                              { HI.hiiValGam        = vg
                              , HI.hiiTyGam     	= tg
                              , HI.hiiTyKiGam     	= tkg
                              , HI.hiiPolGam     	= pg
                              , HI.hiiDataGam       = dg
                              , HI.hiiClGam         = clg
                              , HI.hiiCHRStore      = cs
                              }
%%]]
%%]]
%%[[(8 codegen)
                 coreInh' = coreInh
%%[[8
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf    ehSem
%%][20
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf        ehInh'
%%]]
                              }
%%]]
         ;  when (isJust (ecuMbEHSem ecu))
                 (do { put (cr {crStateInfo = crsi
%%[[(8 codegen)
                                   { crsiCoreInh = coreInh' }
%%][(20 codegen)
                                   { crsiCoreInh = coreInh', crsiEHInh = ehInh' }
%%][20
                                   { crsiEHInh = ehInh' }
%%]]
                               })
%%[[20
                     ; cpUpdCU modNm $! ecuStoreHIInfo hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowEHSem1")
%%]]
%%[[102
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (do { lift $ putStrLn $ fevShow "gathDataGam" dg
                                ; lift $ putStrLn $ fevShow "gathValGam" vg
                                ; lift $ putStrLn $ fevShow "gathTyGam" tg
                                ; lift $ putStrLn $ fevShow "gathTyKiGam" tkg
                                ; lift $ putStrLn $ fevShow "gathPolGam" pg
                                ; lift $ putStrLn $ fevShow "gathKiGam" kg
                                ; lift $ putStrLn $ fevShow "gathClGam" clg
                                ; lift $ putStrLn $ fevShow "gathChrStore" cs
                                ; lift $ putStrLn $ fevShow "cmodule" $ EHSem.cmodule_Syn_AGItf   ehSem
                                })
%%]]
                     })
         }
%%]

%%[20
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 ehInh  = crsiEHInh crsi
%%[[20
                 ehInh' = ehInh
%%[[(20 hmtyinfer)
                            { EHSem.valGam_Inh_AGItf     = (prepFlow $! HISem.valGam_Syn_AGItf     hiSem) `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = (prepFlow $! HISem.tyGam_Syn_AGItf      hiSem) `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.tyKiGam_Inh_AGItf    = (prepFlow $! HISem.tyKiGam_Syn_AGItf    hiSem) `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                            , EHSem.polGam_Inh_AGItf     = (prepFlow $! HISem.polGam_Syn_AGItf     hiSem) `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                            , EHSem.dataGam_Inh_AGItf    = (prepFlow $! HISem.dataGam_Syn_AGItf    hiSem) `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.clGam_Inh_AGItf      = (prepFlow $! HISem.clGam_Syn_AGItf      hiSem) `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                            , EHSem.chrStore_Inh_AGItf   = (prepFlow $! HISem.chrStore_Syn_AGItf   hiSem) `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                            }
%%]]
%%]]
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = (prepFlow $! HISem.fixityGam_Syn_AGItf hiSem) `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = (prepFlow $! HISem.idGam_Syn_AGItf     hiSem) `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            }
%%[[(20 codegen)
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.arityMp_Inh_CodeAGItf   = (prepFlow $! HISem.arityMp_Syn_AGItf hiSem) `Map.union` Core2GrSem.arityMp_Inh_CodeAGItf coreInh
                              }
%%]]
                 optim    = crsiOptim crsi
                 optim'   = optim
%%[[(20 codegen grin)
                              { optimGrInlMp   = (prepFlow $! HISem.inlMp_Syn_AGItf hiSem) `Map.union` optimGrInlMp optim
                              }
%%]]
         ;  when (isJust (ecuMbPrevHISem ecu))
                 (do { put (cr {crStateInfo = crsi { crsiEHInh = ehInh'
                                                   , crsiHSInh = hsInh'
%%[[(20 codegen)
                                                   , crsiCoreInh = coreInh'
%%]]
                                                   , crsiOptim = optim'}})
                     })
         }
%%]

%%[(20 codegen)
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
%%[[(20 codegen grin)
                              { HI.hiiCArityMp  = if ehcOptFullProgAnalysis opts then Map.empty else am
                              }
%%]]
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
%%[[(20 codgen grin)
                 gm     = prepFlow $! optimGrInlMp moptim
%%]]
                 optim' = optim
%%[[(20 codgen grin)
                            { optimGrInlMp = gm `Map.union` optimGrInlMp optim
                            }
%%]]
                 hii'   = hii
%%[[(20 codgen grin)
                            { HI.hiiGrInlMp = gm
                            }
%%]]
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
  = cpSeq [cpGetMetaInfo modNm,cpGetPrevHI modNm,cpFoldHI modNm]
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

%%[(20 codegen)
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

cpGetMetaInfo :: HsName -> EHCompilePhase ()
cpGetMetaInfo modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
         ;  tm ecuStoreHSTime        (                      ecuFilePath ecu)
         ;  tm ecuStoreHITime        (fpathSetSuff "hi"   $ ecuFilePath ecu)
%%[[(20 codegen)
         ;  tm ecuStoreCoreTime      (fpathSetSuff "core" $ ecuFilePath ecu)
%%]]
%%[[101
         ;  wr ecuStoreDirIsWritable (                      ecuFilePath ecu)
%%]]
         }
  where tm store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when nExists
                      (do { t <- lift $ getModificationTime n
                          ; cpUpdCU modNm $ store t
                          })
               }
%%[[101
        wr store fp
          = do { pm <- lift $ getPermissions (maybe "." id $ fpathMbDir fp)
               -- ; lift $ putStrLn (fpathToStr fp ++ " writ " ++ show (writable pm))
               ; cpUpdCU modNm $ store (writable pm)
               }
%%]]
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

%%[101
crModCanCompile :: HsName -> EHCompileRun -> Bool
crModCanCompile modNm cr
  = ecuDirIsWritable ecu
  where ecu = crCU modNm cr
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
%%]

%%[(20 codegen grin)
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
cpTranslateEH2Output :: HsName -> EHCompilePhase ()
cpTranslateEH2Output modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Output" mbEHSem
%%[[(8 hmtyinfer)
                 about  = "EH analyses: Type checking"
%%][8
                 about  = "EH analyses"
%%]]
%%[[8
                 errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
%%][102
                 -- core   = Core.CModule_Mod modNm (Core.CExpr_Int 1) []
                 errs   = []
%%]]
         ;  when (isJust mbEHSem)
                 (do { cpSetLimitErrsWhen 5 about errs
%%[[8
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "eh2" fp)) (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%][102
%%]]
%%[[8
                     ; when (ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
                     ; when (ecuIsTopMod ecu && ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
%%[[(99 hmtyinfer)
                     ; when (ecuIsTopMod ecu && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "lhs" fp)) (EHSem.dt_Syn_AGItf ehSem) 1000)
%%][100
%%]]
                     }
                 )
         }
%%]

%%[(8 codegen)
cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 core   = EHSem.cmodule_Syn_AGItf ehSem
%%[[8
%%][102
                 -- core   = Core.CModule_Mod modNm (Core.CExpr_Int 1) []
%%]]
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreCore core)
                     }
                 )
         }
%%]

%%[(8 codegen grin)
cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCoreSem = ecuMbCoreSem ecu
                 coreSem   = panicJust "cpTranslateCore2Grin" mbCoreSem
                 grin      = Core2GrSem.grMod_Syn_CodeAGItf coreSem
         ;  when (isJust mbCoreSem && (ehcOptEmitGrin opts || ehcOptEmitBytecode opts || ehcOptFullProgAnalysis opts))
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
         }
%%]

%%[(8 codegen grin)
cpTranslateGrin2Bytecode :: HsName -> EHCompilePhase ()
cpTranslateGrin2Bytecode modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 modNmLL= crCompileOrder cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin2Bytecode" mbGrin
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
%%[[20
         -- ;  lift $ putStrLn (show (crsiModOffMp crsi))
%%]]

         ;  when (isJust mbGrin && (ehcOptEmitBytecode opts))
                 (cpUpdCU modNm $! ecuStoreBytecode bc)
         ;  when (ehcOptErrAboutBytecode opts)
                 (cpSetLimitErrsWhen 5 "Grin to ByteCode" errs)
         }
%%]

%%[(8 codegen grin)
cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin" mbGrin
         ;  when (isJust mbGrin && (ehcOptFullProgAnalysis opts))
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }
%%]

%%[(8 codegen grin)
cpTranslateByteCode :: HsName -> EHCompilePhase ()
cpTranslateByteCode modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbBytecode = ecuMbBytecode ecu
%%[[8
                 grinbcPP = gbmod2C opts $ panicJust "cpTranslateByteCode" mbBytecode
%%][20
                 grinbcPP = vlist ([ppMod] ++ (if ecuIsTopMod ecu then [ppMain] else []))
                          where (ppMod,ppMain)
                                  = gbmod2C opts $ panicJust "cpTranslateByteCode" mbBytecode
%%]]
         ;  when (ehcOptEmitBytecode opts && isJust mbBytecode)
                 (do { cpUpdCU modNm $! ecuStoreBytecodeSem grinbcPP
                     })
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin)
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

%%[(8 codegen grin)
cpTransformGrin :: HsName -> EHCompilePhase ()
cpTransformGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 forBytecode = not (ehcOptFullProgAnalysis opts)
                 optimizing  = ehcOptOptimise opts >= OptimiseNormal
         
                 trafos  =     (if forBytecode               then mk [mte,unb]               else [])
                           ++  (if optimizing                then mk evel                    else mk [flt])
                           ++  (if forBytecode && optimizing then inline ++ mk (evel++[cpr]) else [])
                           ++  (if optimizing                then mk [nme]                   else [])

                   where mk   = map (\(trf,msg) -> (cpFromGrinTrf modNm trf msg,msg))
                         inl  = ( grInline True                  , "inline"           )
                         flt  = ( grFlattenSeq                   , "flatten"          )
                         ale  = ( grAliasElim                    , "alias elim"       )
                         nme  = ( grUnusedNameElim               , "unused name elim" )
                         eve  = ( grEvalElim                     , "eval elim"        )
                         mte  = ( grUnusedMetaInfoElim           , "meta info elim"   )
                         cpr  = ( grConstPropagation             , "const prop"       )
                         unb  = ( grMayLiveUnboxed 
                                   Bytecode.tagAllowsUnboxedLife , "unbox"            )
%%[[8_2
                         frm  = ( grPrettyNames                  , "rename uniform"   ) 
%%]]
%%[[8
                         evel = [ flt, ale, eve, ale ]
%%][8_2
                         evel = [ flt, ale, frm, eve, ale ]
%%]]
%%[[8                              
                         inline = mk [inl]
%%][20                                
                         inline = [ ( do { cr <- get
                                         ; let (ecu,crsi,_,_) = crBaseInfo modNm cr
                                               expNmOffMp     = crsiExpNmOffMp modNm crsi
                                               optim          = crsiOptim crsi
                                               (g,gathInlMp)  = grInline True (Map.keysSet expNmOffMp) (optimGrInlMp optim) $ fromJust $ ecuMbGrin ecu
                                         ; cpMsgGrinTrf modNm "inline"
                                         ; cpUpdCU modNm (ecuStoreOptim (defaultOptim {optimGrInlMp = gathInlMp}) . ecuStoreGrin g)
                                         }
                                    , "inline" 
                                    ) 
                                  ]
%%]]                              
                              
                 optGrinNormal = map fst trafos
                 optGrinDump   = out 0 "from core" : concat [ [o,out n nm] | (n,(o,nm)) <- zip [1..] trafos ]
                        where out n nm = cpOutputGrin ("-0" ++ show (10+n) ++ "-" ++ filter isAlpha nm) modNm
         ;  when (isJust $ ecuMbGrin ecu)
                 (cpSeq (if ehcOptDumpGrinStages opts then optGrinDump else optGrinNormal))
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

%%[(8 codegen grin)
cpCompileWithLLVM :: HsName -> EHCompilePhase()
cpCompileWithLLVM modNm
  = do { cr <- get
       ; let  (_,_,opts,fp) = crBaseInfo modNm cr
              fpLL          = fpathSetSuff "ll" fp
              fpExec        = maybe (fpathRemoveSuff fp) (\s -> fpathSetSuff s fp) 
                                    Cfg.mbSuffixExec
              libs          = map (\lib -> "-l " ++ lib) $
                              [ Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/prim.o"
                              , Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/llvm-gc.o"
                              , Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/timing.o"
                              , Cfg.selectFileprefixInstall opts
                                ++ "lib/libgc.a"
                              ]
              inputOpts     = [ fpathToStr fpLL ]
              outputOpts    = ["-o " ++ fpathToStr fpExec]
       ; when ( ehcOptEmitExecLLVM opts )
         (  do { let compileLL 
                       = concat $ intersperse " "
                         $  [ Cfg.shellCmdLLVM ]
                         ++ libs
                         ++ outputOpts
                         ++ inputOpts
               ; when (ehcOptVerbosity opts >= VerboseALot)
                 (  do { cpMsg' modNm VerboseALot "LLVM" Nothing fpExec
                       ; lift $ putStrLn compileLL
                       }
                 )
               ; cpSystem compileLL
               }
         ) 
       }                 
%%]

%%[(8 codegen)
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
                                               -- , map ("-l" ++) (Cfg.libnamesGccPerVariant ++ Cfg.libnamesGcc)
                                               , map (\l -> Cfg.selectFileprefixInstall opts ++ perVariantSuffix ++ l ++ ".a") Cfg.libnamesGccPerVariant
                                                 ++ map (\l -> Cfg.selectFileprefixInstall opts ++ "lib/lib" ++ l ++ ".a") Cfg.libnamesGcc
                                                 ++ map ("-l" ++) Cfg.libnamesGccEhcExtraExternalLibs
                                               , if   ehcOptEmitExecC opts
                                                 then [ ]
                                                 else [ fpathToStr $ fpO fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
                                               )
%%[[8
                                            where perVariantSuffix = "%%@{%{VARIANT}%%}/lib/lib"
%%][101
                                            where perVariantSuffix = "lib/lib"
%%]]
                            GCC_CompileOnly -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [])
                                            where o = fpO fp
         ;  when (ehcOptEmitExecC opts || ehcOptEmitExecBytecode opts)
                 (do { let compileC
                             = concat $ intersperse " "
                               $ (  [ Cfg.shellCmdGcc ]
%%[[8
                                 ++ [ "-I" ++ Cfg.selectFileprefixInstall opts ++ "%%@{%{VARIANT}%%}/include" ]
%%][101
%%]]
                                 ++ [ "-I" ++ Cfg.selectFileprefixInstall opts ++ "include" ]
                                 ++ [ "-I" ++ Cfg.selectFileprefixInstall opts ++ "include/gc" ]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
                                 ++ [ Cfg.selectFileprefixInstall opts ++ "%%@{%{VARIANT}%%}/include/mainSil.c"
                                    | ehcOptEmitExecC opts
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
       ; when (  ehcOptCPP opts
              || modNm == hsnModIntlPrelude		-- 20080211, AD: builtin hack to preprocess EHC.Prelude with cpp, for now, to avoid implementation of pragmas
              )
              (do { let preCPP
                          = concat $ intersperse " "
                            $ (  [ Cfg.shellCmdCpp ]
                              ++ [ "-traditional-cpp", "-fno-show-column", "-P", "-D__EHC__" ]
%%[[(99 codegen grin)
                              ++ (if ehcOptFullProgAnalysis opts then ["-D__FULL_PROGRAM_ANALYSIS__"] else [])
%%]]
                              ++ [ fpathToStr fp, fpathToStr fpCPP ]
                              )
                  ; when (ehcOptVerbosity opts >= VerboseALot)
                         (do { cpMsg modNm VerboseALot "CPP"
                             ; lift $ putStrLn preCPP
                             })
%%[[99
                  ; cpSystem preCPP
%%][101
                  ; when (crModCanCompile modNm cr)
                         (cpSystem preCPP)
%%]]
                  ; cpUpdCU modNm (ecuStoreFilePath fpCPP)
                  })
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
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
%%[[9
                              "CLDF"    -> if   ehcOptFullProgAnalysis opts
                                           then cmodTrfLiftDictFields
                                           else id
%%]]
%%[[8_2
                              "CPRNM"   -> cmodTrfPrettyNames
%%]]
%%[[102
                              "CS"      -> cmodTrfStrip
%%]]
                              -- "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  cpMsg' modNm VerboseALot "Transforming" (lookup trfNm cmdLineTrfs) fp
%%[[102
%%]]
         ;  cpUpdCU modNm $! ecuStoreCore $! core2
         }
%%]
         ;  cpMsg  modNm VerboseDebug ("Core sz1: " ++ showSizeCore core)
         ;  cpMsg  modNm VerboseDebug ("Core sz2: " ++ showSizeCore core2)

%%[(8 codegen)
cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 tr     = ehcOptTrf opts
                 ps     = dmp 0 "fromeh"
                          : (concat
                            $ intersperse [cpStepUID]
                            $ zipWith trf [1..]
                            $ filter (maybe True id . trfOptOverrides tr)
                            $ trfNmL
                            )
                 trf n t= [cpCore1Trf modNm t,dmp n t]
                 dmp n t= when (ehcOptDumpCoreStages opts) (cpOutputCore ("core-" ++ show n ++ "-" ++ t) modNm)
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
          , cpTranslateEH2Output modNm
%%[[(8 codegen)
          , cpTranslateEH2Core modNm
%%]]
%%[[99
          , cpCleanupEH modNm
%%]]
          ]
%%]

%%[(8 codegen)
cpProcessCoreBasic :: HsName -> EHCompilePhase ()
cpProcessCoreBasic modNm 
  = cpSeq [ cpTransformCore
              modNm
                (
%%[[102
                  -- [ "CS" ] ++
%%]]
                  [ "CER", "CRU", "CLU", "CILA", "CETA", "CCP", "CILA", "CETA"
                  , "CFL", "CLGA", "CCGA", "CLU", "CFL", {- "CLGA", -} "CLFG"    
%%[[9                  
                  ,  "CLDF"
%%]
%%[[8_2           
                  , "CPRNM"
%%]]
                  ]
                )
          , cpOutputCore "core" modNm
%%[[(8 java)
          , cpOutputJava "java" modNm
%%]]
          ]
%%]

%%[(8 codegen)
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

%%[(8 codegen grin)
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = cpSeq [ cpOutputGrin "-000-initial" modNm
          , cpTransformGrin modNm
          , cpOutputGrin "-099-final" modNm
          , cpTranslateGrin2Bytecode modNm
          , cpTranslateGrin modNm
          ]
%%]

%%[(8 codegen grin)
cpProcessBytecode :: HsName -> EHCompilePhase ()
cpProcessBytecode modNm 
  = cpSeq [ cpTranslateByteCode modNm
%%[[99
          , cpCleanupFoldBytecode modNm
%%]]
          , cpOutputByteCodeC "c" modNm
%%[[99
          , cpCleanupBytecode modNm
%%]]
          ]

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
%%]

%%[(99 codegen)
cpCleanupCore :: HsName -> EHCompilePhase ()
cpCleanupCore modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbCore            = Nothing
               , ecuMbCoreSem         = Nothing
               }
      )
%%]

%%[(99 codegen grin)
cpCleanupGrin :: HsName -> EHCompilePhase ()
cpCleanupGrin modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbGrin            = Nothing
               }
      )

cpCleanupFoldBytecode :: HsName -> EHCompilePhase ()
cpCleanupFoldBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecode          = Nothing
               }
      )

cpCleanupBytecode :: HsName -> EHCompilePhase ()
cpCleanupBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecodeSem       = Nothing
               }
      )
%%]

%%[99
cpCleanupCU :: HsName -> EHCompilePhase ()
cpCleanupCU modNm
  = do { cpUpdCU modNm
           (\e -> e { ecuHIInfo            = HI.emptyHIInfo
                    , ecuMbOptim           = Nothing
                    }
           )
%%[[(99 codegen grin)
       ; cpCleanupGrin modNm
%%]]
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
%%[[(20 codegen grin)
                   ; cpUpdateModOffMp [modNm]
%%]]
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
%%[[(8 codegen)
                           , cpProcessCoreBasic modNm
                           , cpProcessCoreRest modNm
%%]]
%%[[(8 codegen grin)
                           , cpProcessGrin modNm
                           , cpProcessBytecode modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           , cpCompileWithLLVM modNm
%%]]
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
%%[[(8 codegen)
                           , cpProcessCoreBasic modNm
                           , cpStopAt CompilePoint_Core
                           , cpProcessCoreRest modNm
%%]]
%%[[(8 codegen grin)
                           , cpProcessGrin modNm
                           , cpProcessBytecode modNm
                           , cpCompileWithGCC GCC_CompileExec [] modNm
                           , cpCompileWithLLVM modNm
%%]]
                           ]
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   }
%%[[(8 codegen grin)
           (ECUSGrin,_)
             -> do { cpMsg modNm VerboseNormal "Compiling Grin"
                   ; cpSeq [ cpParseGrin modNm
                           , cpProcessGrin modNm
                           , cpProcessBytecode modNm 
                           ]
                   }
%%]]
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
          = cpSeq [ p1 modNm, p2 modNm, p3 modNm, p4 modNm
%%[[(20 codegen grin)
                  , p5 modNm
%%]]
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
                          , cpCheckMods [modNm]
%%[[(20 codegen grin)
                          , cpUpdateModOffMp [modNm]
%%]]
                          , cpProcessHs modNm
                          , cpMsg modNm VerboseALot "Name+dependency analysis done"
                          , cpStopAt CompilePoint_AnalHS
                          ]
                p3 modNm
                  = cpSeq [ cpStepUID, cpProcessEH modNm
                          , cpMsg modNm VerboseALot "Type analysis done"
                          , cpStopAt CompilePoint_AnalEH
                          ]
                p4 modNm
                  = cpSeq [ cpStepUID
%%[[(20 codegen)
                          , cpProcessCoreBasic modNm
						  , cpMsg modNm VerboseALot "Core (basic) done"
						  , cpStopAt CompilePoint_Core
%%]]
                          ]
%%[[(20 codegen grin)
                p5 modNm
                  = cpSeq [ cpSeq (if not (ehcOptFullProgAnalysis opts)
                                   then [ cpProcessCoreRest modNm
                                        , cpProcessGrin modNm
%%[[20
                                        , cpFlowOptim modNm
%%]]
%%[[99
                                        , cpCleanupGrin modNm
%%]]
                                        , cpProcessBytecode modNm
                                        ]
                                        ++ (if isTopMod then [] else [cpCompileWithGCC GCC_CompileOnly [] modNm])
                                   else []
                                  )
                          , cpMsg modNm VerboseALot "Core+Grin done"
                          ]
%%]]
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
      ; cpSeq [ anal modNmL
%%[[(20 codegen grin)
              , biggrin opts modNmL mm, gcc mm, llvm mm
%%]]
              ]
      }
  where anal ms
          = cpSeq (merge (map comp ms) (map flow ms))
        merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs
        comp m
          = do { cr <- get
               ; let targ = if crModNeedsCompile m cr
%%[[101
                               && crModCanCompile m cr
%%]]
                            then HSAllSem
                            else HSAllSemHI
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
%%[[(20 codegen grin)
        core mL
          = cpSeq [cpGetPrevCore m | m <- mL]
        biggrin opts mL (mImpL,mMain)
          = if ehcOptFullProgAnalysis opts
            then cpSeq [ core mL
                       , oneBigCore
                       , cpProcessCoreRest mMain
                       , cpProcessGrin mMain
                       ]
            else return ()
          where oneBigCore
                  = do { cr <- get
                       ; cpUpdCU mMain (ecuStoreCore (Core.cModMerge [ panicJust "cpCompileOrderedCUs.oneBigCore" $ ecuMbCore $ crCU m cr | m <- mL ]))
                       }
        gcc (mImpL,mMain)
          = cpSeq [cpCompileWithGCC GCC_CompileExec mImpL mMain]
        llvm (_,mMain)
          = cpSeq [cpCompileWithLLVM mMain]
%%]]        
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
%%[[(1 hmtyinfer)
                          ; when (ehcOptShowTopTyPP opts)
                                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
%%]]
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
             searchPath     = mkInitSearchPath fp
                              ++ ehcOptSearchPath opts
%%[[101
                              ++ (if ehcOptUseInplace opts then [] else [Cfg.fileprefixInstall ++ "ehclib/ehcbase"])
%%]]
             opts2          = opts { ehcOptSearchPath = searchPath }
             hsInh          = HSSem.Inh_AGItf { HSSem.opts_Inh_AGItf            = opts2
                                              , HSSem.idGam_Inh_AGItf           = HSSem.tyGam2IdDefOccGam initTyGam
                                                                                    `gamUnion` HSSem.kiGam2IdDefOccGam initKiGam
%%[[9
                                                                                    `gamUnion` HSSem.clGam2IdDefOccGam Pr.initClGam
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
                                              , EHSem.idQualGam_Inh_AGItf       = emptyGam
%%]]
%%[[(20 hmtyinfer)
                                              , EHSem.valGam_Inh_AGItf          = emptyGam
                                              , EHSem.dataGam_Inh_AGItf         = emptyGam
                                              , EHSem.tyGam_Inh_AGItf           = initTyGam
                                              , EHSem.tyKiGam_Inh_AGItf         = initTyKiGam
                                              , EHSem.polGam_Inh_AGItf          = initPolGam
                                              , EHSem.kiGam_Inh_AGItf           = initKiGam
                                              , EHSem.clGam_Inh_AGItf           = Pr.initClGam
                                              , EHSem.chrStore_Inh_AGItf        = initScopedPredStore
%%]]
                                              }
%%[[(8 codegen)
             coreInh        = Core2GrSem.Inh_CodeAGItf
                                              { Core2GrSem.gUniq_Inh_CodeAGItf           = uidStart
                                              , Core2GrSem.dataGam_Inh_CodeAGItf         = emptyGam
                                              , Core2GrSem.opts_Inh_CodeAGItf            = opts2
%%[[20
                                              , Core2GrSem.arityMp_Inh_CodeAGItf         = Map.empty
%%]]
                                              }
%%]]
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
                                (EHCompileRunStateInfo opts2 hsInh ehInh
%%[[(8 codegen)
                                                       coreInh
%%]]
                                                       uidStart uidStart
%%[[20
                                                       hiInh hsModInh Map.empty Map.empty defaultOptim
%%]]
%%[[(20 codegen)
                                                       Map.empty
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

