%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module analysis

%%[20 module {%{EH}EHC.CompilePhase.Module}
%%]

-- general imports
%%[20 import(qualified Data.Map as Map)
%%]
%%[20 import(qualified EH.Util.Rel as Rel)
%%]

%%[20 import({%{EH}EHC.Common})
%%]
%%[20 import({%{EH}EHC.CompileUnit})
%%]
%%[20 import({%{EH}EHC.CompileRun})
%%]

%%[20 import({%{EH}Module})
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(cpCheckMods')
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
       ; cpUpdSI (\crsi -> crsi {crsiModMp = mm})
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }
%%]

%%[20 export(cpCheckMods)
cpCheckMods :: [HsName] -> EHCompilePhase ()
cpCheckMods modNmL
  = do { cr <- get
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckMods' modL
       }
  where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get info for module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(GetMeta(..),allGetMeta)
data GetMeta
  = GetMeta_HS
  | GetMeta_HI
  | GetMeta_Core
  | GetMeta_Grin
  | GetMeta_Dir
  deriving (Eq,Ord)

allGetMeta = [GetMeta_HS, GetMeta_HI, GetMeta_Core, GetMeta_Grin, GetMeta_Dir]

%%]

%%[20 export(cpGetHsImports,cpGetHsMod,cpGetMetaInfo)
cpGetHsImports :: HsName -> EHCompilePhase HsName
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
                 modNm'     = HSSemMod.realModuleNm_Syn_AGItf hsSemMod
                 upd        = ecuStoreHSDeclImpL (HSSemMod.modImpNmL_Syn_AGItf hsSemMod)
         -- ; lift $ putWidthPPLn 120 (pp mod)
         ;  case mbHsSemMod of
              Just _ | ecuIsTopMod ecu -> cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
                     | otherwise       -> do cpUpdCU modNm upd ; return modNm
              _      -> return modNm
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

cpGetMetaInfo :: [GetMeta] -> HsName -> EHCompilePhase ()
cpGetMetaInfo gm modNm
  =  do  {  cr <- get
         ;  let (ecu,_,opts,fp) = crBaseInfo modNm cr
         ;  when (GetMeta_HS `elem` gm)
                 (tm opts ecu ecuStoreHSTime        (ecuSrcFilePath ecu))
         {-
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHITime
%%[[20
                                              (fpathSetSuff "hi"        fp     )
%%][99
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
%%]]
                 )
         -}
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHIInfoTime
%%[[20
                                              (fpathSetSuff "hi"        fp     )
%%][99
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
%%]]
                 )
%%[[(20 codegen)
         ;  when (GetMeta_Grin `elem` gm)
                 (tm opts ecu ecuStoreGrinTime      (fpathSetSuff "grin"      fp     ))
%%]]
%%[[(20 codegen)
         ;  when (GetMeta_Core `elem` gm)
                 (tm opts ecu ecuStoreCoreTime      (fpathSetSuff "core"      fp     ))
%%]]
%%[[20
         ;  when (GetMeta_Dir `elem` gm)
                 (wr opts ecu ecuStoreDirIsWritable (                         fp     ))
%%]]
         }
  where tm opts ecu store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when (ehcOptVerbosity opts >= VerboseDebug)
                      (do { lift $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                          })
               ; when nExists
                      (do { t <- lift $ getModificationTime n
                          ; when (ehcOptVerbosity opts >= VerboseDebug)
                                 (do { lift $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                                     })
                          ; cpUpdCU modNm $ store t
                          })
               }
%%[[20
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

%%[20 export(cpGetDummyCheckEhMod)
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

%%[(20 codegen grin) export(cpUpdateModOffMp)
cpUpdateModOffMp :: [HsName] -> EHCompilePhase ()
cpUpdateModOffMp modNmL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             offMp  = crsiModOffMp crsi
             offMp' = Map.fromList [ (m,(o,crsiExpNmOffMp m crsi)) | (m,o) <- zip modNmL [Map.size offMp ..] ] `Map.union` offMp
       ; cpUpdSI (\crsi -> crsi {crsiModOffMp = offMp'})
       }
%%]

