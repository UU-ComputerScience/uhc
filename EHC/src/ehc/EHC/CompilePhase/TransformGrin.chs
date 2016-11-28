%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Grin transformation

%%[(8 codegen grin) module {%{EH}EHC.CompilePhase.TransformGrin}
%%]

%%[(8 codegen grin) import({%{EH}Base.Target})
%%]

-- general imports
%%[(8 codegen grin) import(qualified Data.Map as Map)
%%]

%%[(8 codegen grin) import(Control.Monad.State)
%%]

%%[(8 codegen grin) import({%{EH}EHC.Common})
%%]
%%[(8 codegen grin) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen grin) import({%{EH}EHC.CompileRun})
%%]

-- Language syntax: Grin
%%[8 import({%{EH}EHC.ASTTypes})
%%]
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin)
%%]
-- Language syntax: Grin bytecode
%%[(8 codegen grin) import(qualified {%{EH}GrinByteCode} as Bytecode(tagAllowsUnboxedLife))
%%]
-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputGrin)})
%%]

-- Grin transformations
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.UnusedMetaInfoElim}, {%{EH}GrinCode.Trf.UnusedNameElim}, {%{EH}GrinCode.Trf.AliasElim}, {%{EH}GrinCode.Trf.MayLiveUnboxed})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.BasicAnnotAliasElim})
%%]
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.ConstPropagation}, {%{EH}GrinCode.Trf.FlattenSeq}, {%{EH}GrinCode.Trf.EvalElim}, {%{EH}GrinCode.Trf.Inline})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin)
cpMsgGrinTrf :: EHCCompileRunner m => HsName -> String -> EHCompilePhaseT m ()
cpMsgGrinTrf modNm m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Local GRIN optim" (Just m) fp     -- '
       }

cpFromGrinTrf :: EHCCompileRunner m => HsName -> (AST_Grin -> AST_Grin) -> String -> EHCompilePhaseT m ()
cpFromGrinTrf modNm trf m
  = do { cr <- get
       ; let (ecu,_,_,fp) = crBaseInfo modNm cr
       ; cpMsgGrinTrf modNm m
       ; cpUpdCU modNm $ ecuStoreGrin $ trf $ fromJust $ _ecuMbGrin ecu
       }
%%]

%%[(8 codegen grin) export(cpTransformGrin)
cpTransformGrin :: forall m . EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTransformGrin modNm
  =  do  {  cr <- get
         ;  cpMsg modNm VerboseALot "cpTransformGrin"         
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 forBytecode          = targetIsGrinBytecode (ehcOptTarget opts)
                 forBytecodeOrSimilar = not doesHPT -- targetIsGrinBytecode (ehcOptTarget opts)
                 doesHPT     = targetDoesHPTAnalysis (ehcOptTarget opts)
                 needMetaInfo= doesHPT
                 optimizing  = ehcOptOptimizes Optimize_GrinLocal opts
         
{- for debugging 
                 trafos  =     mk [mte,unb,flt,cpr,nme]
-}
                 trafos :: [(EHCompilePhaseT m (), String)]
                 trafos  =     (                                  mk [flt,bae]                             )
                           ++  (if not needMetaInfo          then mk [mte]                    else []      )
                           ++  (if forBytecode               then mk [unb]                    else []      )
                           ++  (if optimizing                then mk evel1                    else mk evel0)
                           ++  (if forBytecodeOrSimilar && optimizing
                                                             then inline ++ mk (evel2++[cpr]) else []      )
                           ++  (if optimizing                then mk [nme]                    else []      )

                   where mk   = map (\(trf,msg) -> (cpFromGrinTrf modNm trf msg,msg))
                         inl  = ( grInline True                  , "inline"           )
                         flt  = ( grFlattenSeq                   , "flatten"          )
                         bae  = ( grBasicAnnotAliasElim          , "ffi unwrap alias elim")
                         ale  = ( grAliasElim                    , "alias elim"       )
                         nme  = ( grUnusedNameElim               , "unused name elim" )
                         eve  = ( grEvalElim opts                , "eval elim"        )
                         mte  = ( grUnusedMetaInfoElim           , "meta info elim"   )
                         cpr  = ( grConstPropagation             , "const prop"       )
                         unb  = ( grMayLiveUnboxed (Bytecode.tagAllowsUnboxedLife opts)
                                                                 , "unbox"            )
%%[[8
                         evel0 = [ ale ] --, eve, flt, ale ]
                         evel1 = evel0 ++ [ eve, flt, ale ]
                         evel2 = [ flt ] ++ evel1
%%]]
%%[[8                              
                         inline = mk [inl]
%%][50                                
                         inline = [ ( do { cr <- get
                                         ; let (ecu,crsi,_,_) = crBaseInfo modNm cr
                                               expNmFldMp     = crsiExpNmOffMp modNm crsi
                                               optim          = crsiOptim crsi
                                               (g,gathInlMp)  = grInline True (Map.keysSet expNmFldMp) (optimGrInlMp optim) $ fromJust $ _ecuMbGrin ecu
                                         ; cpMsgGrinTrf modNm "inline"
                                         ; cpUpdCU modNm (ecuStoreOptim (defaultOptim {optimGrInlMp = gathInlMp}) . ecuStoreGrin g)
                                         }
                                    , "inline" 
                                    ) 
                                  ]
%%]]                              
                              
                 optGrinNormal = map fst trafos
                 optGrinDump   = out 0 "from core" : concat [ [o,out n nm] | (n,(o,nm)) <- zip [1..] trafos ]
                        where out n nm = void $ cpOutputGrin ASTFileContent_Text ("-0" ++ show (10+n) ++ "-" ++ filter isAlpha nm) modNm
         ;  when (isJust $ _ecuMbGrin ecu)
                 (cpSeq (if ehcOptDumpGrinStages opts then optGrinDump else optGrinNormal))
         }
%%]




