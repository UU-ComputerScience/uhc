module EH101.EHC.CompilePhase.TransformGrin
( cpTransformGrin )
where
import EH101.Base.Target
import qualified Data.Map as Map
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.GrinCode as Grin
import qualified EH101.GrinByteCode as Bytecode (tagAllowsUnboxedLife)
import EH101.EHC.CompilePhase.Output(cpOutputGrin)
import EH101.GrinCode.Trf.UnusedMetaInfoElim
import EH101.GrinCode.Trf.UnusedNameElim
import EH101.GrinCode.Trf.AliasElim
import EH101.GrinCode.Trf.MayLiveUnboxed
import EH101.GrinCode.Trf.BasicAnnotAliasElim
import EH101.GrinCode.Trf.ConstPropagation
import EH101.GrinCode.Trf.FlattenSeq
import EH101.GrinCode.Trf.EvalElim
import EH101.GrinCode.Trf.Inline




{-# LINE 47 "src/ehc/EHC/CompilePhase/TransformGrin.chs" #-}
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

{-# LINE 64 "src/ehc/EHC/CompilePhase/TransformGrin.chs" #-}
cpTransformGrin :: HsName -> EHCompilePhase ()
cpTransformGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 forBytecode = targetIsGrinBytecode (ehcOptTarget opts)
                 optimizing  = ehcOptOptimizes Optimize_GrinLocal opts

{- for debugging
                 trafos  =     mk [mte,unb,flt,cpr,nme]
-}
                 trafos  =     (                                  mk [flt,bae]                             )
                           ++  (if forBytecode               then mk [mte,unb]                else []      )
                           ++  (if optimizing                then mk evel1                    else []      )
                           ++  (if forBytecode && optimizing then inline ++ mk (evel2++[cpr]) else []      )
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
                         evel1 = [ ale, eve, flt, ale ]
                         evel2 = [ flt ] ++ evel1
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

                 optGrinNormal = map fst trafos
                 optGrinDump   = out 0 "from core" : concat [ [o,out n nm] | (n,(o,nm)) <- zip [1..] trafos ]
                        where out n nm = cpOutputGrin False ("-0" ++ show (10+n) ++ "-" ++ filter isAlpha nm) modNm
         ;  when (isJust $ ecuMbGrin ecu)
                 (cpSeq (if ehcOptDumpGrinStages opts then optGrinDump else optGrinNormal))
         }

