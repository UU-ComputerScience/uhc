%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile output generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Output generation, on stdout or file

%%[8 module {%{EH}EHC.CompilePhase.Output}
%%]

-- general imports
%%[8 import(qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[50 import(UHC.Util.Time, System.Directory)
%%]

%%[8 import(Control.Monad.State)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

-- EH semantics
%%[99 import(qualified {%{EH}EH.MainAG} as EHSem)
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
-- TBD: this depends on grin gen, but should also be available for Core, so in a CoreXXXSem
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- Core output
%%[(8 codegen) import({%{EH}Core} as Core,{%{EH}Core.Pretty})
%%]
-- TyCore output
%%[(8 codegen tycore) import({%{EH}TyCore},{%{EH}TyCore.Pretty})
%%]
-- Grin input and output
%%[(8 codegen grin) import({%{EH}GrinCode} as Grin,{%{EH}GrinCode.Pretty})
%%]
-- Java output
%%[(8888 codegen java) import({%{EH}Core.ToJava})
%%]
-- JavaScript output
%%[(8 javascript) import({%{EH}Core},{%{EH}JavaScript.Pretty})
%%]
-- Cmm output
%%[(8 codegen cmm) import({%{EH}Cmm} as Cmm,{%{EH}Cmm.ToC}(cmmMod2C), {%{EH}Cmm.Pretty})
%%]

-- serialization
%%[50 import(qualified UHC.Util.Binary as Bin, UHC.Util.Serialize)
%%]
-- for debugging only
%%[50 import({%{EH}Gam})
%%]

-- module admin
%%[50 import({%{EH}Module})
%%]

-- gam related utils
%%[99 import({%{EH}Gam.Utils})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: abstract writing of output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- | Abstraction for writing a module to output with variation in suffices
cpOutputSomeModules
  ::    (EHCOpts -> FPath -> FilePath -> mod -> IO ())
     -> (EHCOpts -> HsName -> FPath -> String -> FPath)
     -> (Int -> String -> String)
     -> String
     -> HsName
     -> [(String,mod)]
     -> EHCompilePhase ()
cpOutputSomeModules write mkfp mknmsuff suff modNm mods = do
    cr <- get
    let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
    lift $ do
      forM_ (zip [1..] mods) $ \(nr,(nmsuff,mod)) -> do
        let fpC     = mkfp opts modNm fp (suff ++ mknmsuff nr nmsuff) -- for now nmsuff after suff, but should be inside name
            fnC     = fpathToStr fpC
        fpathEnsureExists fpC
        write opts fpC fnC mod
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: specific output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) export(cpOutputTyCoreModule,cpOutputTyCore)
cpOutputTyCoreModule :: Bool -> String -> String -> HsName -> Module -> EHCompilePhase ()
cpOutputTyCoreModule binary nmsuff suff modNm tyMod
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC = mkOutputFPath opts modNm fp (suff ++ nmsuff) -- for now nmsuff after suff, but should be inside name
                 fnC    = fpathToStr fpC
%%[[8
         ;  lift $ putPPFPath fpC (ppModule opts tyMod) 100
%%][50
         ;  lift (if binary
                  then do { fpathEnsureExists fpC		-- should be in FPath equivalent of putSerializeFile
                          ; putSerializeFile fnC tyMod
                          }
                  else putPPFPath fpC (ppModule opts tyMod) 100
                 )
%%]]
         }

cpOutputTyCore :: String -> HsName -> EHCompilePhase ()
cpOutputTyCore suff modNm
  =  do  {  cr <- get
         -- part 1: current .tycore
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbTyCore = ecuMbTyCore ecu
                 cMod   = panicJust "cpOutputTyCore" mbTyCore
                 fpC = mkOutputFPath opts modNm fp suff
         ;  cpMsg modNm VerboseALot "Emit TyCore"
         ;  lift $ putPPFPath fpC (ppModule opts cMod) 100
         }
%%]

%%[(8 codegen) export(cpOutputCoreModules)
cpOutputCoreModules
  :: Bool
     -> (Int -> String -> String)
     -> String -> HsName
     -> [(String,CModule)]
     -> EHCompilePhase ()
cpOutputCoreModules binary mknmsuff suff modNm cMods
  = cpOutputSomeModules write mkOutputFPath mknmsuff suff modNm cMods
  where write opts fpC fnC cMod = do
%%[[50
          if binary
            then putSerializeFile fnC cMod
            else
%%]]
                 putPPFPath fpC (ppCModule opts cMod) 100
%%]

%%[(8 codegen) export(cpOutputCore)
cpOutputCore :: Bool -> String -> String -> HsName -> EHCompilePhase ()
cpOutputCore binary nmsuff suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,_,_) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpOutputCore" mbCore
         ;  cpMsg modNm VerboseALot "Emit Core"
         ;  cpOutputCoreModules binary (\_ nm -> nm) suff modNm [(nmsuff,cMod)]
         }
%%]

%%[(8 grin) export(cpOutputGrinModules)
cpOutputGrinModules
  :: Bool
     -> (Int -> String -> String)
     -> String -> HsName
     -> [(String,GrModule)]
     -> EHCompilePhase ()
cpOutputGrinModules binary mknmsuff suff modNm cMods
  = cpOutputSomeModules write mkOutputFPath mknmsuff suff modNm cMods
  where write opts fpC fnC gMod = do
%%[[50
          if binary
            then putSerializeFile fnC gMod
            else
%%]]
                 putPPFPath fpC (ppGrModule gMod) 100
%%]

%%[(8 codegen grin) export(cpOutputGrin)
cpOutputGrin :: Bool -> String -> HsName -> EHCompilePhase ()
cpOutputGrin binary suff modNm
  =  do  { cr <- get
         ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                mbGrin = ecuMbGrin ecu
                grin   = panicJust "cpOutputGrin" mbGrin
         ; cpMsg modNm VerboseALot "Emit Grin"
         ; cpOutputGrinModules binary (\_ nm -> nm) "grin" modNm [(suff,grin)]
         }
%%]

%%[(8 cmm) export(cpOutputCmmModules)
cpOutputCmmModules
  :: Bool
     -> (Int -> String -> String)
     -> String -> HsName
     -> [(String,Cmm.Module)]
     -> EHCompilePhase ()
cpOutputCmmModules _ mknmsuff suff modNm cMods
  = cpOutputSomeModules write mkOutputFPath mknmsuff suff modNm cMods
  where write opts fpC fnC cmmMod = do
          putPPFPath fpC (ppCmmModule cmmMod) 100
%%]

%%[(8 cmm) export(cpOutputCmm)
cpOutputCmm :: Bool -> String -> HsName -> EHCompilePhase ()
cpOutputCmm binary suff modNm
  =  do  { cr <- get
         ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                mbCmm  = ecuMbCmm ecu
                cmm    = panicJust "cpOutputCmm" mbCmm
         ; cpMsg modNm VerboseALot "Emit Cmm"
         ; cpOutputCmmModules binary (\_ nm -> nm) "cmm" modNm [(suff,cmm)]
         }
%%]

%%[(8 codegen grin) export(cpOutputByteCodeC)
cpOutputByteCodeC :: String -> HsName -> EHCompilePhase ()
cpOutputByteCodeC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,fp) = crBaseInfo modNm cr
                 bc       = panicJust "cpOutputByteCodeC bytecode" $ ecuMbBytecodeSem ecu
                 fpC      = mkOutputFPath opts modNm fp suff
%%[[(8 cmm)
                 cmm      = panicJust "cpOutputByteCodeC cmm" $ ecuMbCmm ecu
                 fpCmm    = mkOutputFPath opts modNm fp (suff ++ "-cmm")
%%]]
         ;  cpMsg' modNm VerboseALot "Emit ByteCode C" Nothing fpC     -- '
%%[[99
         ;  cpRegisterFilesToRm [fpC]
%%]]
         ;  lift $ putPPFPath fpC bc 150
%%[[(8 cmm)
         -- 20111220: temporary, until Cmm is main path
         ;  when (ehcOptPriv opts)
                 (do { let cmmPP = cmmMod2C opts cmm
                     ; lift $ putPPFPath fpCmm cmmPP 150
                     })
%%]]
         }
%%]

%%[50 export(cpOutputHI)
cpOutputHI :: String -> HsName -> EHCompilePhase ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mmi    = panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 hii1   = ecuHIInfo ecu
                 impNmS = ecuImpNmS ecu
                 hii2   = hii1 { HI.hiiValidity             = HI.HIValidity_Ok
                               , HI.hiiModuleNm             = modNm
                               , HI.hiiExps                 = mmiExps       mmi
                               , HI.hiiHiddenExps           = mmiHiddenExps mmi
                               , HI.hiiHasMain              = ecuHasMain ecu
                               , HI.hiiTarget               = ehcOptTarget opts
                               , HI.hiiTargetFlavor         = ehcOptTargetFlavor opts
                               , HI.hiiSrcTimeStamp         = Cfg.verTimestamp Cfg.version
                               , HI.hiiSrcSig               = Cfg.verSig Cfg.version
                               , HI.hiiSrcVersionMajor      = Cfg.verMajor Cfg.version
                               , HI.hiiSrcVersionMinor      = Cfg.verMinor Cfg.version
                               , HI.hiiSrcVersionMinorMinor = Cfg.verMinorMinor Cfg.version
                               , HI.hiiSrcVersionSvn        = Cfg.verSvnRevision Cfg.version
                               , HI.hiiCompileFlags         = optsDiscrRecompileRepr opts
                               , HI.hiiCompiler             = Cfg.installVariant opts
                               , HI.hiiTransClosedUsedModMp = Map.unions $
                                                                Map.singleton modNm impNmS : [ ecuTransClosedUsedModMp $ crCU m cr | m <- Set.toList impNmS ]
                               , HI.hiiTransClosedOrphanModS= Set.unions $
                                                                [ Set.unions [if ecuIsOrphan me then Set.singleton m else Set.empty, ecuTransClosedOrphanModS me]
                                                                | m <- Set.toList impNmS
                                                                , let me = crCU m cr
                                                                ]
                               }
%%[[50
                 hii3   = hii2
%%][9999
                 ehInh  = crsiEHInh crsi
                 hii3 = HI.hiiIncludeCacheOfImport (ecuAnHIInfo . flip crCU cr) (mentrelFilterMpExtendViaValGam modNm (EHSem.valGam_Inh_AGItf ehInh) (ecuUsedNames ecu)) hii2
                 -- hii3   = hii2
%%]]
                 fpH    = mkOutputFPath opts modNm fp suff
                 fnH    = fpathToStr fpH
         ;  cpMsg modNm VerboseALot "Emit HI"
         ;  hiExists <- lift $ doesFileExist fnH
         ;  when (hiExists)
                 (lift $ removeFile fnH)
         ;  when (ehcOptVerbosity opts > VerboseALot)
                 (do { lift $ putPPLn ("hii3: " >#< hii3)
                     ; lift $ putPPLn ("orph: " >#< vlist [ m >#< (fmap Set.toList $ HI.hiiMbOrphan $ ecuHIInfo me) | m <- Set.toList impNmS, let me = crCU m cr ])
%%[[99
                     ; lift $ putPPLn ("used nms: " >#< (pp $ show $ ecuUsedNames ecu))
%%]]
                     })
         ;  lift $ do { fpathEnsureExists fpH
                      ; putSerializeFile fnH hii3
                      }
         ;  now <- lift $ getClockTime
         ;  cpUpdCU modNm ( ecuStoreHIInfoTime now
%%[[99
                          . ecuStoreHIInfo hii3
%%]]
                          )
         }

%%]


