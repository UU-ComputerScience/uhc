%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

XXX

%%[8 module {%{EH}EHC.CompilePhase.FlowBetweenPhase}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem)
%%]

-- HS semantics
%%[8 import(qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- Core semantics
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(20 codegen) import({%{EH}Core.UsedModNms})
%%]

-- HI syntax and semantics
%%[20 import(qualified {%{EH}HI} as HI)
%%]

-- CHR solver
%%[(20 hmtyinfer) import({%{EH}CHR.Solve}(chrStoreUnion))
%%]

-- Force evaluation for IO
%%[99 import({%{EH}Base.ForceEval})
%%]

-- for debug
%%[20 hs import({%{EH}Base.Debug},EH.Util.Pretty)
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

%%[20 export(cpFlowHsSem1)
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
                            { HI.hiiFixityGam            = fg
                            , HI.hiiIdDefAssocL          = HI.hiiIdDefOccGamToAssocL ig
                            , HI.hiiHIDeclImpModL        = ecuHIDeclImpNmL ecu
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
                 (do { cpUpdSI (\crsi -> crsi {crsiHSInh = hsInh', crsiEHInh = ehInh', crsiOpts = opts'})
                     ; cpUpdCU modNm $! ecuStoreHIInfo hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowHsSem1")
                     })
         -- ;  lift $ putWidthPPLn 120 (ppGam $ EHSem.idQualGam_Inh_AGItf $ ehInh')
         }

%%]

%%[8 export(cpFlowEHSem1)
cpFlowEHSem1 :: HsName -> EHCompilePhase ()
cpFlowEHSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 ehSem    = panicJust "cpFlowEHSem1.ehSem" $ ecuMbEHSem ecu
                 ehInh    = crsiEHInh crsi
%%[[(8 codegen)
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
                              , HI.hiiCHRStoreL     = HI.hiiScopedPredStoreToList cs
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
                 (do { cpUpdSI
                               (\crsi -> crsi
%%[[(8 codegen)
                                   { crsiCoreInh = coreInh' }
%%][(20 codegen)
                                   { crsiCoreInh = coreInh', crsiEHInh = ehInh' }
%%][20
                                   { crsiEHInh = ehInh' }
%%]]
                               )
%%[[20
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     )
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

%%[20 export(cpFlowHISem)
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 -- hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 hiInfo = panicJust "cpFlowHISem.hiInfo" $ ecuMbPrevHIInfo ecu
                 ehInh  = crsiEHInh crsi
%%[[20
                 ehInh' = ehInh
%%[[(20 hmtyinfer)
                            { EHSem.valGam_Inh_AGItf     = (HI.hiiValGam     hiInfo) `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = (HI.hiiTyGam      hiInfo) `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.tyKiGam_Inh_AGItf    = (HI.hiiTyKiGam    hiInfo) `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                            , EHSem.polGam_Inh_AGItf     = (HI.hiiPolGam     hiInfo) `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                            , EHSem.dataGam_Inh_AGItf    = (HI.hiiDataGam    hiInfo) `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.clGam_Inh_AGItf      = (HI.hiiClGam      hiInfo) `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                            , EHSem.chrStore_Inh_AGItf   = (HI.hiiCHRStore   hiInfo) `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                            }
%%]]
%%]]
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = (HI.hiiFixityGam    hiInfo) `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = (HI.hiiIdDefOccGam  hiInfo) `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            }
%%[[(20 codegen)
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = (HI.hiiLamMp hiInfo) `Map.union` Core2GrSem.lamMp_Inh_CodeAGItf coreInh
                              }
%%]]
                 optim    = crsiOptim crsi
                 optim'   = optim
%%[[(20 codegen grin)
                              { optimGrInlMp   = (HI.hiiGrInlMp hiInfo) `Map.union` optimGrInlMp optim
                              }
%%]]
         ;  when (isJust (ecuMbPrevHIInfo ecu))
                 (do { cpUpdSI (\crsi -> crsi { crsiEHInh = ehInh'
                                              , crsiHSInh = {- tr "cpFlowHISem.crsiHSInh" (pp $ HSSem.idGam_Inh_AGItf hsInh') $ -} hsInh'
%%[[(20 codegen)
                                              , crsiCoreInh = coreInh'
%%]]
                                              , crsiOptim = optim'
                                              })
                     })
         }
%%]

%%[(20 codegen) export(cpFlowCoreSem)
cpFlowCoreSem :: HsName -> EHCompilePhase ()
cpFlowCoreSem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 coreSem  = panicJust "cpFlowCoreSem.coreSem" $ ecuMbCoreSem ecu
                 core     = panicJust "cpFlowCoreSem.core"    $ ecuMbCore    ecu
                 usedImpL = Set.toList $ cmodUsedModNms core
                 coreInh  = crsiCoreInh crsi
                 hii      = ecuHIInfo ecu
                 am       = prepFlow $! Core2GrSem.gathLamMp_Syn_CodeAGItf coreSem
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = am `Map.union` Core2GrSem.lamMp_Inh_CodeAGItf coreInh
                              }
                 hii'     = hii
                              { HI.hiiHIUsedImpModL = usedImpL
%%[[(20 codegen grin)
                              , HI.hiiLamMp         = am
%%]]
                              }
         ;  when (isJust (ecuMbCoreSem ecu))
                 (do { cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh'})
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     . ecuStoreHIUsedImpL usedImpL
                                     )
                     })
         }
%%]

%%[(20 codegen) export(cpFlowHILamMp)
cpFlowHILamMp :: HsName -> EHCompilePhase ()
cpFlowHILamMp modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
              coreInh  = crsiCoreInh crsi
              hii      = ecuHIInfo ecu

         -- put back result: call info map (lambda arity, ...), overwriting previous entries
       ; cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh {Core2GrSem.lamMp_Inh_CodeAGItf = HI.hiiLamMp hii `Map.union` Core2GrSem.lamMp_Inh_CodeAGItf coreInh}})
       }
%%]

%%[20 export(cpFlowOptim)
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
                 (do { cpUpdSI (\crsi -> crsi {crsiOptim = optim'})
                     ; cpUpdCU modNm $! ecuStoreHIInfo $! prepFlow hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowOptim")
                     })
         }
%%]


