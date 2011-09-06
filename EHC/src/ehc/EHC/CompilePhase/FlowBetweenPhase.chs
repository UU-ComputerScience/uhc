%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

XXX

%%[8 module {%{EH}EHC.CompilePhase.FlowBetweenPhase}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]
%%[92 import(qualified EH.Util.FastSeq as Seq)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- module related
%%[50 import({%{EH}Module})
%%]
%%[92 import({%{EH}EHC.CompilePhase.Module(cpUpdHiddenExports)})
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
%%[(50 codegen) import({%{EH}Core.UsedModNms})
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- CHR solver
%%[(50 hmtyinfer) import({%{EH}CHR.Solve}(chrStoreUnion))
%%]

-- Force evaluation for IO
%%[9999 import({%{EH}Base.ForceEval})
%%]

-- Misc info: LamInfo/LamMp
%%[(8 codegen) hs import({%{EH}LamInfo})
%%]

-- for debug
%%[50 hs import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional processing before flowing into next whatever: in particular, force evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50.prepFlow
prepFlow :: a -> a
prepFlow x | x `seq` True = x
-- prepFlow = id

gamUnionFlow :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnionFlow = gamUnion
%%]

%%[9999 -50.prepFlow
prepFlow :: ForceEval a => a -> a
prepFlow = forceEval

gamUnionFlow :: (Ord k, ForceEval (Gam k v)) => Gam k v -> Gam k v -> Gam k v
gamUnionFlow g1 g2 | forceEval g1 `seq` True = gamUnion g1 g2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: flowing info between module compiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpFlowHsSem1)
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
                            -- , HI.hiiIdDefHIIdGam         = HI.hiiIdDefOccGamToHIIdGam ig
                            , HI.hiiHIDeclImpModL        = ecuHIDeclImpNmL ecu
                            }
                 opts'  = opts
                            { ehcOptBuiltinNames = mkEHBuiltinNames mk
                            }
%%[[50
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
%%[[(50 hmtyinfer)
                 dg       = prepFlow $! EHSem.gathDataGam_Syn_AGItf    ehSem
                 vg       = prepFlow $! EHSem.gathValGam_Syn_AGItf     ehSem
                 tg       = prepFlow $! EHSem.gathTyGam_Syn_AGItf      ehSem
                 tkg      = prepFlow $! EHSem.gathTyKiGam_Syn_AGItf    ehSem
                 pg       = prepFlow $! EHSem.gathPolGam_Syn_AGItf     ehSem
                 kg       = prepFlow $! EHSem.gathKiGam_Syn_AGItf      ehSem
                 clg      = prepFlow $! EHSem.gathClGam_Syn_AGItf      ehSem
                 dfg      = prepFlow $! EHSem.gathClDfGam_Syn_AGItf    ehSem
                 cs       = prepFlow $! EHSem.gathChrStore_Syn_AGItf   ehSem
%%]]
%%[[(50 hmtyinfer)
                 lm       = prepFlow $! EHSem.gathLamMp_Syn_AGItf      ehSem
%%]]
%%[[50
                 mmi      = panicJust "cpFlowEHSem1.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 hii      = ecuHIInfo ecu
                 mentrelFilterMp
                          = mentrelFilterMpUnions [ EHSem.gathMentrelFilterMp_Syn_AGItf ehSem, mentrelToFilterMp' False [modNm] (mmiExps mmi) ]
                 usedImpL = Set.toList $ mentrelFilterMpModuleNames mentrelFilterMp
                 ehInh'   = ehInh
%%[[(50 hmtyinfer)
                              { EHSem.dataGam_Inh_AGItf    = dg  `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                              , EHSem.valGam_Inh_AGItf     = vg  `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                              , EHSem.tyGam_Inh_AGItf      = tg  `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                              , EHSem.tyKiGam_Inh_AGItf    = tkg `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                              , EHSem.polGam_Inh_AGItf     = pg  `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                              , EHSem.kiGam_Inh_AGItf      = kg  `gamUnionFlow`  EHSem.kiGam_Inh_AGItf      ehInh
                              , EHSem.clGam_Inh_AGItf      = clg `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                              , EHSem.clDfGam_Inh_AGItf    = dfg `gamUnionFlow`  EHSem.clDfGam_Inh_AGItf    ehInh
                              , EHSem.chrStore_Inh_AGItf   = cs  `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                              }
%%]]
                 hii'     = hii
                              { -- 20100717 AD: redundant because later extracted from Core because of inlining etc, TBD
                                HI.hiiHIUsedImpModL = usedImpL
%%[[(50 hmtyinfer)
                              , HI.hiiValGam        = vg
                              , HI.hiiTyGam     	= tg
                              , HI.hiiTyKiGam     	= tkg
                              , HI.hiiPolGam     	= pg
                              , HI.hiiDataGam       = dg
                              , HI.hiiClGam         = clg
                              , HI.hiiClDfGam       = dfg
                              , HI.hiiCHRStore      = {- HI.hiiScopedPredStoreToList -} cs
                              -- , HI.hiiLamMp         = lm
%%]]
                              }
%%]]
%%[[(8 codegen)
                 coreInh' = coreInh
%%[[8
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf ehSem
                              , Core2GrSem.lamMp_Inh_CodeAGItf   = EHSem.gathLamMp_Syn_AGItf   ehSem
%%][50
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf     ehInh'
                              , Core2GrSem.lamMp_Inh_CodeAGItf   = lm `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh		-- assumption: no duplicates, otherwise merging as done later has to be done
%%]]
                              }
%%]]
         ;  when (isJust (ecuMbEHSem ecu))
                 (do { cpUpdSI
                               (\crsi -> crsi
%%[[(8 codegen)
                                   { crsiCoreInh = coreInh' }
%%][(50 codegen)
                                   { crsiCoreInh = coreInh', crsiEHInh = ehInh' }
%%][50
                                   { crsiEHInh = ehInh' }
%%]]
                               )
%%[[50
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     . ecuStoreHIUsedImpL usedImpL
%%[[99
                                     . ecuStoreUsedNames mentrelFilterMp
%%]]
                                     )
%%]]
%%[[92
                     -- put back additional hidden exports
                     ; cpUpdHiddenExports modNm $ Seq.toList $ EHSem.gathHiddenExports_Syn_AGItf ehSem
%%]]
                     })
         }
%%]

%%[50 export(cpFlowHISem)
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 -- hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 hiInfo = panicJust "cpFlowHISem.hiInfo" $ ecuMbPrevHIInfo ecu
                 ehInh  = crsiEHInh crsi
%%[[50
                 ehInh' = ehInh
%%[[(50 hmtyinfer)
                            { EHSem.valGam_Inh_AGItf     = (HI.hiiValGam     hiInfo) `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = (HI.hiiTyGam      hiInfo) `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.tyKiGam_Inh_AGItf    = (HI.hiiTyKiGam    hiInfo) `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                            , EHSem.polGam_Inh_AGItf     = (HI.hiiPolGam     hiInfo) `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                            , EHSem.dataGam_Inh_AGItf    = (HI.hiiDataGam    hiInfo) `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.clGam_Inh_AGItf      = (HI.hiiClGam      hiInfo) `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                            , EHSem.clDfGam_Inh_AGItf    = (HI.hiiClDfGam    hiInfo) `gamUnionFlow`  EHSem.clDfGam_Inh_AGItf    ehInh
                            , EHSem.chrStore_Inh_AGItf   = (HI.hiiCHRStore   hiInfo) `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                            }
%%]]
%%]]
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = (HI.hiiFixityGam    hiInfo) `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = (HI.hiiIdDefOccGam  hiInfo) `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            }
%%[[(50 codegen)
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = (HI.hiiLamMp hiInfo) `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh
                              }
%%]]
                 optim    = crsiOptim crsi
                 optim'   = optim
%%[[(50 codegen grin)
                              { optimGrInlMp   = (HI.hiiGrInlMp hiInfo) `Map.union` optimGrInlMp optim
                              }
%%]]
         ;  when (isJust (ecuMbPrevHIInfo ecu))
                 (do { cpUpdSI (\crsi -> crsi { crsiEHInh = ehInh'
                                              , crsiHSInh = {- tr "cpFlowHISem.crsiHSInh" (pp $ HSSem.idGam_Inh_AGItf hsInh') $ -} hsInh'
%%[[(50 codegen)
                                              , crsiCoreInh = coreInh'
%%]]
                                              , crsiOptim = optim'
                                              })
                     })
         }
%%]

%%[(50 codegen) export(cpFlowCoreSem)
cpFlowCoreSem :: HsName -> EHCompilePhase ()
cpFlowCoreSem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 coreSem  = panicJust "cpFlowCoreSem.coreSem" $ ecuMbCoreSem ecu
                 core     = panicJust "cpFlowCoreSem.core"    $ ecuMbCore    ecu
                 
                 -- 20100717 AD: required here because of inlining etc, TBD
                 usedImpL = Set.toList $ cmodUsedModNms core
                 
                 coreInh  = crsiCoreInh crsi
                 hii      = ecuHIInfo ecu
                 am       = prepFlow $! Core2GrSem.gathLamMp_Syn_CodeAGItf coreSem
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = am `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh	-- assumption: old info can be overridden, otherwise merge should be done here
                              }
                 hii'     = hii
%%[[(50 codegen grin)
                              { -- 20100717 AD: required here because of inlining etc, TBD
                                {- -} HI.hiiHIUsedImpModL = usedImpL
                              , {- -} HI.hiiLamMp         = am
                              }
%%]]
         ;  when (isJust (ecuMbCoreSem ecu))
                 (do { cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh'})
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     -- 
                                     -- 20100717 AD: required here because of inlining etc, TBD
                                     . ecuStoreHIUsedImpL usedImpL
                                     )
                     })
         }
%%]

%%[(50 codegen) export(cpFlowHILamMp)
cpFlowHILamMp :: HsName -> EHCompilePhase ()
cpFlowHILamMp modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
              coreInh  = crsiCoreInh crsi
              hii      = ecuHIInfo ecu

         -- put back result: call info map (lambda arity, ...), overwriting previous entries
       ; cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh {Core2GrSem.lamMp_Inh_CodeAGItf = HI.hiiLamMp hii `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh}})
       }
%%]

%%[50 export(cpFlowOptim)
cpFlowOptim :: HsName -> EHCompilePhase ()
cpFlowOptim modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 optim  = crsiOptim crsi
                 moptim = panicJust "cpFlowOptim" $ ecuMbOptim ecu
                 hii    = ecuHIInfo ecu
%%[[(50 codgen grin)
                 gm     = prepFlow $! optimGrInlMp moptim
%%]]
                 optim' = optim
%%[[(50 codgen grin)
                            { optimGrInlMp = gm `Map.union` optimGrInlMp optim
                            }
%%]]
                 hii'   = hii
%%[[(50 codgen grin)
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


