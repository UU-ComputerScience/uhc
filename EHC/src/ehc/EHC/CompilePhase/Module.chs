%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module analysis

%%[50 module {%{EH}EHC.CompilePhase.Module}
%%]

-- general imports
%%[50 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[50 import(qualified UHC.Util.Rel as Rel)
%%]
%%[50 import(UHC.Util.Time, UHC.Util.FPath)
%%]
%%[50 import(System.Directory)
%%]

%%[50 import({%{EH}EHC.Common})
%%]
%%[50 import({%{EH}EHC.CompileUnit})
%%]
%%[50 import({%{EH}EHC.CompileRun})
%%]

%%[50 import({%{EH}Module})
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%[50 import({%{EH}Base.Debug})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpCheckMods')
cpCheckMods' :: (HsName -> ModMpInfo) -> [Mod] -> EHCompilePhase ()
cpCheckMods' dfltMod modL@(Mod {modName = modNm} : _)
  = do { cr <- get
       -- ; cpMsg modNm VerboseDebug $ "cpCheckMods' modL: " ++ show modL
       ; let crsi   = crStateInfo cr
             (mm,e) = modMpCombine' dfltMod modL (crsiModMp crsi)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = mm})
%%[[50
       ; when (ehcOptVerbosity (crsiOpts crsi) >= VerboseDebug)
              (do { cpMsg modNm VerboseDebug "cpCheckMods'"
                  ; lift $ putWidthPPLn 120 (pp modNm >-< pp modL >-< ppModMp mm)
                  })
%%][99
%%]]
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }
%%]

%%[50 export(cpCheckMods)
cpCheckMods :: [HsName] -> EHCompilePhase ()
cpCheckMods modNmL
  = do { cr <- get
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckMods' (\n -> panic $ "cpCheckMods: " ++ show n) modL
       }
  where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get info for module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(GetMeta(..),allGetMeta)
data GetMeta
  = GetMeta_HS
  | GetMeta_HI
  | GetMeta_Core
  | GetMeta_Grin
  | GetMeta_Dir
  deriving (Eq,Ord)

allGetMeta = [GetMeta_HS, GetMeta_HI, GetMeta_Core, GetMeta_Grin, GetMeta_Dir]

%%]

%%[50 export(cpGetHsImports,cpGetHsMod,cpGetMetaInfo)
cpGetHsImports :: HsName -> EHCompilePhase HsName
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
                 modNm'     = HSSemMod.realModuleNm_Syn_AGItf hsSemMod
                 upd        = ecuStoreHSDeclImpS ( -- (\v -> tr "XX" (pp $ Set.toList v) v) $ 
                                                  HSSemMod.modImpNmS_Syn_AGItf hsSemMod)
         ;  case mbHsSemMod of
              Just _ | ecuIsTopMod ecu -> cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
                     | otherwise       -> do { cpUpdCU modNm upd ; return modNm }
              _      -> return modNm
         }

cpGetHsMod :: HsName -> EHCompilePhase ()
cpGetHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsMod" mbHsSemMod
                 mod        = HSSemMod.mod_Syn_AGItf hsSemMod
         ;  when (ehcOptVerbosity opts >= VerboseDebug)
                 (do { cpMsg modNm VerboseDebug "cpGetHsMod"
                     ; lift $ putWidthPPLn 120 (pp mod)
                     })
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm (ecuStoreMod mod))
         }

cpGetMetaInfo :: [GetMeta] -> HsName -> EHCompilePhase ()
cpGetMetaInfo gm modNm
  =  do  {  cr <- get
         ;  let (ecu,_,opts,fp) = crBaseInfo modNm cr
         ;  when (GetMeta_HS `elem` gm)
                 (tm opts ecu ecuStoreHSTime        (ecuSrcFilePath ecu))
         {-
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHITime
%%[[50
                                              (fpathSetSuff "hi"        fp     )
%%][99
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
%%]]
                 )
         -}
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHIInfoTime
%%[[50
                                              (fpathSetSuff "hi"        fp     )
%%][99
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
%%]]
                 )
%%[[(50 codegen)
         ;  when (GetMeta_Grin `elem` gm)
                 (tm opts ecu ecuStoreGrinTime      (fpathSetSuff "grin"      fp     ))
%%]]
%%[[(50 codegen)
         ;  when (GetMeta_Core `elem` gm)
                 (tm opts ecu ecuStoreCoreTime      (fpathSetSuff "core"      fp     ))
%%]]
%%[[50
         ;  when (GetMeta_Dir `elem` gm)
                 (wr opts ecu ecuStoreDirIsWritable (                         fp     ))
%%]]
         }
  where tm :: EHCOpts -> EHCompileUnit -> (ClockTime -> EHCompileUnit -> EHCompileUnit) -> FPath -> EHCompilePhase ()
        tm opts ecu store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when (ehcOptVerbosity opts >= VerboseDebug)
                      (do { lift $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                          })
               ; when nExists
                      (do { t <- lift $ fpathGetModificationTime fp
                          ; when (ehcOptVerbosity opts >= VerboseDebug)
                                 (do { lift $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                                     })
                          ; cpUpdCU modNm $ store t
                          })
               }
%%[[50
        wr opts ecu store fp
          = do { pm <- lift $ getPermissions (maybe "." id $ fpathMbDir fp)
               -- ; lift $ putStrLn (fpathToStr fp ++ " writ " ++ show (writable pm))
               ; cpUpdCU modNm $ store (writable pm)
               }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Create dummy module info for .eh's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpGetDummyCheckEhMod)
cpGetDummyCheckEhMod :: HsName -> EHCompilePhase ()
cpGetDummyCheckEhMod modNm
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty Rel.empty []
       ; cpUpdCU modNm (ecuStoreMod mod)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm emptyModMpInfo mm})
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update module offset info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen grin) export(cpUpdateModOffMp)
cpUpdateModOffMp :: [HsName] -> EHCompilePhase ()
cpUpdateModOffMp modNmL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             offMp  = crsiModOffMp crsi
             -- offMp' = Map.fromList [ (m,(o,crsiExpNmOffMp m crsi)) | (m,o) <- zip modNmL [Map.size offMp ..] ] `Map.union` offMp
             (offMp',_)
                    = foldr add (offMp,Map.size offMp) modNmL
                    where add modNm (offMp,offset)
                            = case Map.lookup modNm offMp of
                                Just (o,_) -> (Map.insert modNm (o     ,new) offMp, offset  )
                                _          -> (Map.insert modNm (offset,new) offMp, offset+1)
                            where new = crsiExpNmOffMp modNm crsi
       ; cpUpdSI (\crsi -> crsi {crsiModOffMp = offMp'})
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update new hidden exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer) export(cpUpdHiddenExports)
cpUpdHiddenExports :: HsName -> [(HsName,IdOccKind)] -> EHCompilePhase ()
cpUpdHiddenExports modNm exps
  = when (not $ null exps)
         (do { cpUpdSI (\crsi -> crsi { crsiModMp = modMpAddHiddenExps modNm exps $ crsiModMp crsi
                                      })
             ; cpUpdateModOffMp [modNm]
             })

%%]

