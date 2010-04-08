%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Grin transformation

%%[8 module {%{EH}EHC.CompilePhase.TransformGrin}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- Language syntax: Grin
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
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.ConstPropagation}, {%{EH}GrinCode.Trf.FlattenSeq}, {%{EH}GrinCode.Trf.EvalElim}, {%{EH}GrinCode.Trf.Inline})
%%]
%%[(8_2 codegen grin) hs import({%{EH}GrinCode.Trf.PrettyVarNames})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin)
cpMsgGrinTrf :: HsName -> String -> EHCompilePhase ()
cpMsgGrinTrf modNm m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Local GRIN optim" (Just m) fp     -- '
       }

cpFromGrinTrf :: HsName -> (Grin.GrModule -> Grin.GrModule) -> String -> EHCompilePhase ()
cpFromGrinTrf modNm trf m
  = do { cr <- get
       ; let (ecu,_,_,fp) = crBaseInfo modNm cr
       ; cpMsgGrinTrf modNm m
       ; cpUpdCU modNm $ ecuStoreGrin $ trf $ fromJust $ ecuMbGrin ecu
       }
%%]

%%[(8 codegen grin) export(cpTransformGrin)
cpTransformGrin :: HsName -> EHCompilePhase ()
cpTransformGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 forBytecode = not (ehcOptFullProgAnalysis opts)
                 optimizing  = ehcOptOptimise opts >= OptimiseNormal
         
{- for debugging 
                 trafos  =     mk [mte,unb,flt,cpr,nme]
-}
                 trafos  =     (if forBytecode               then mk [mte,unb]               else [])
                           ++  (if optimizing                then mk evel                    else mk [flt])
                           ++  (if forBytecode && optimizing then inline ++ mk (evel++[cpr]) else [])
                           ++  (if optimizing                then mk [nme]                   else [])

                   where mk   = map (\(trf,msg) -> (cpFromGrinTrf modNm trf msg,msg))
                         inl  = ( grInline True                  , "inline"           )
                         flt  = ( grFlattenSeq                   , "flatten"          )
                         ale  = ( grAliasElim                    , "alias elim"       )
                         nme  = ( grUnusedNameElim               , "unused name elim" )
                         eve  = ( grEvalElim opts                , "eval elim"        )
                         mte  = ( grUnusedMetaInfoElim           , "meta info elim"   )
                         cpr  = ( grConstPropagation             , "const prop"       )
                         unb  = ( grMayLiveUnboxed (Bytecode.tagAllowsUnboxedLife opts)
                                                                 , "unbox"            )
%%[[8_2
                         frm  = ( grPrettyNames                  , "rename uniform"   ) 
%%]]
%%[[8
                         evel = [ flt, ale, eve, flt, ale ]
%%][8_2
                         evel = [ flt, ale, frm, eve, flt, ale ]
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
                        where out n nm = cpOutputGrin False ("-0" ++ show (10+n) ++ "-" ++ filter isAlpha nm) modNm
         ;  when (isJust $ ecuMbGrin ecu)
                 (cpSeq (if ehcOptDumpGrinStages opts then optGrinDump else optGrinNormal))
         }
%%]




