

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag)
module EH101.Core.Trf.AnaRelevance(cmodTrfAnaRelevance) where

import EH101.Ty
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Gam
import EH101.Opts
import EH101.Base.Optimize
import EH101.Core
import EH101.VarMp
import EH101.Substitutable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import EH.Util.Utils
import EH101.Foreign.Extract
import EH101.AbstractCore
import EH101.AnaDomain
import EH101.AnaDomain.Utils
import EH101.Gam
import EH101.Gam.DataGam
import EH101.LamInfo
import EH101.AnaDomain.Trf.Instantiate
import EH101.Base.Debug
import EH.Util.Pretty
import EH101.AnaDomain.Pretty
import EH101.BuiltinPrims
import EH101.Foreign






















cmodTrfAnaRelevance
  :: EHCOpts
     -> DataGam
     -> LamMp
     -> CModule
     -> ( CModule
        , LamMp
        )
cmodTrfAnaRelevance
     opts dataGam
     lamMp
     cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf
                               { opts_Inh_CodeAGItf = opts
                               , dataGam_Inh_CodeAGItf = dataGam
                               , lamMp_Inh_CodeAGItf = lamMp
                               })
     in   ( cTrf_Syn_CodeAGItf t
          , gathLamMp_Syn_CodeAGItf t
          )



data WhatToRelevInfer
  = WhatToRelevInfer_InstToBot			-- specialize/instantiate types to bot (strict)
  | WhatToRelevInfer_Quant				-- quantify
  deriving Eq



type REnv    = Gam HsName RelevTy

-- | lookup in REnv first, then global LamMp
renvLookup :: HsName -> REnv -> LamMp -> Maybe RelevTy
renvLookup n renv lammp = gamLookup n renv <|> (fmap libindaspRelevTy $ lamMpLookupAsp n acbaspkeyDefaultRelevTy lammp)



dbg opts t1 t2 amso ams = CExpr_Ann (CExprAnn_Debug $ showPP $ "?:" >#< t1 >#< "<=" >#< t2 >-< "ams:" >#< ams >-< "amso:" >#< amso)
dbgApp opts a f f2 = CExpr_Ann (CExprAnn_Debug $ showPP $ "argTy:" >#< a >#< "funTy:" >#< f >#< "func.ty:" >#< f2)
dbgBind opts m env = CExpr_Ann (CExprAnn_Debug $ showPP $ "env:" >#< (m `varUpd` env))
dbgCase opts bnd as asslv asinter = CExpr_Ann (CExprAnn_Debug $ showPP $ "bnd:" >#< pp1 bnd >-< "altQualSL:" >#< ppl as >-< "altQualSLSlv:" >#< ppl asslv >-< "altQualSLInt:" >#< pp1 asinter)
  where pp1 s = ppParensCommas $ Set.toList s
        ppl l = ppCurlysCommasBlock (map pp1 l)



annCoe :: RelevCoe -> CExpr -> CExpr
annCoe RelevCoe_Id e =                            e
annCoe c           e = CExpr_Ann (CExprAnn_Coe c) e

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         coe                  : RelevCoe
         fvS                  : FvS
         mbCTag               : Maybe CTag
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local mbCTagEnv   : _
            local patFldGivenTyL : _
            local patFldUsedTyL : {[RelevTy]}
            local patEnv      : _
            local altMbScrutTy : _
            local lev         : _
            local whatAbove   : {WhatExpr}
            local fvS         : _
            local _tup1       : {(UID,UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local lUniq3      : {UID}
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = UIDS ->
               DataGam ->
               REnv ->
               EvalCtx ->
               RVarMp ->
               UID ->
               Bool ->
               Bool ->
               RelevTy ->
               RelevTy ->
               LamMp ->
               Int ->
               EHCOpts ->
               RVarMp ->
               ([WhatToRelevInfer]) ->
               ( CAlt ,RelevCoe,FvS,UID,(Maybe CTag),CAlt ,RelevQualS,RVarMp)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _patFldUsedTyL :: ([RelevTy])
              _patOpatFldTyL :: ([(RelevTy,RelevTy)])
              _exprOenv :: REnv
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              __tup1 :: ((UID,UID,UID,UID))
              _patOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lUniq3 :: UID
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CAlt 
              _lhsOoTrf :: CAlt 
              _lhsOcoe :: RelevCoe
              _lhsOgUniq :: UID
              _lhsOmbCTag :: (Maybe CTag)
              _lhsOrvarMp :: RVarMp
              _patOboundRelevTyVarS :: UIDS
              _patOdataGam :: DataGam
              _patOenv :: REnv
              _patOfinalRVarMp :: RVarMp
              _patOknTy :: RelevTy
              _patOknTyCase :: RelevTy
              _patOlamMp :: LamMp
              _patOlev :: Int
              _patOopts :: EHCOpts
              _patOrvarMp :: RVarMp
              _patOwhatTo :: ([WhatToRelevInfer])
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _patIcTrf :: CPat 
              _patIfldNmL :: ([HsName])
              _patIfvS :: FvS
              _patIgUniq :: UID
              _patImbCTag :: (Maybe CTag)
              _patInmL :: ([HsName])
              _patIoTrf :: CPat 
              _patIpatFldTyL :: ([(RelevTy,RelevTy)])
              _patIqualS :: RelevQualS
              _patIrvarMp :: RVarMp
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 165, column 17)
              _mbCTagEnv =
                  do { ct <- _patImbCTag
                     ; (argTyL,_) <- relevTyArgsFromCTag True ct Nothing (length _patIfldNmL) _lhsIdataGam _lUniq
                     ; return (gamFromAssocL $ zip _patIfldNmL argTyL, argTyL)
                     }
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 169, column 17)
              _patFldGivenTyL =
                  maybe (map freshLazy $ mkNewLevUIDL (length _patIfldNmL) _lUniq3)
                        snd _mbCTagEnv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 169, column 17)
              _patFldUsedTyL =
                  map fresh $ mkNewLevUIDL (length _patIfldNmL) _lUniq2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 169, column 17)
              _patEnv =
                  gamFromAssocL $ zip _patIfldNmL _patFldUsedTyL
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 175, column 17)
              _patOpatFldTyL =
                  zip _patFldGivenTyL _patFldUsedTyL
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 176, column 17)
              _exprOenv =
                  gamAddGam _patEnv _lhsIenv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 10, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 82, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 14, column 17)
              _fvS =
                  _exprIfvS `Set.difference` Set.fromList _patInmL
              -- -- generated by the unique rule mechanism.
              __tup1 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> (__cont, lUniq,lUniq2,lUniq3)}}} )
              -- -- generated by the unique rule mechanism.
              (_patOgUniq,_,_,_) =
                  __tup1
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_,_) =
                  __tup1
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2,_) =
                  __tup1
              -- -- generated by the unique rule mechanism.
              (_,_,_,_lUniq3) =
                  __tup1
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _patIqualS `Set.union` _exprIqualS
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- self rule
              _oTrf =
                  CAlt_Alt _patIoTrf _exprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOcoe =
                  _exprIcoe
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOmbCTag =
                  _patImbCTag
              -- copy rule (up)
              _lhsOrvarMp =
                  _exprIrvarMp
              -- copy rule (down)
              _patOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _patOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _patOenv =
                  _lhsIenv
              -- copy rule (down)
              _patOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _patOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _patOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _patOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _patOlev =
                  _lev
              -- copy rule (down)
              _patOopts =
                  _lhsIopts
              -- copy rule (down)
              _patOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _patOwhatTo =
                  _lhsIwhatTo
              -- copy rule (from local)
              _exprOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _exprOgUniq =
                  _patIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _exprOlev =
                  _lev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exprOrvarMp =
                  _patIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _patIcTrf,_patIfldNmL,_patIfvS,_patIgUniq,_patImbCTag,_patInmL,_patIoTrf,_patIpatFldTyL,_patIqualS,_patIrvarMp) =
                  pat_ _patOboundRelevTyVarS _patOdataGam _patOenv _patOfinalRVarMp _patOgUniq _patOknTy _patOknTyCase _patOlamMp _patOlev _patOopts _patOpatFldTyL _patOrvarMp _patOwhatTo 
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOcTrf,_lhsOcoe,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         altId                : UID
         altMbScrutTy         : MbRelevTy
         altNrMax             : Int
         altSolveLVarMp       : [RVarMp]
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         altNr                : Int
         altQualSL            : [RelevQualS]
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
         ty                   : RelevTy
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local _tup2       : {(AMSOut RelevTy,AnaMatchState)}
            local amsoDw      : {AMSOut RelevTy}
            local amsDw       : {AnaMatchState}
            local rvarMpDw    : _
            local rvarMpUp    : _
            local altNr       : {Int}
            local altKnUpTy   : {RelevTy}
            local altKnDwTy   : {RelevTy}
            local hereAltCoe  : _
            local hdFinalRVarMp : _
            local _tup3       : _
            local altSolveVarMp : _
            local _tup4       : {(UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative Nil:
         visit 0:
            local altNr       : {Int}
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = UID ->
                MbRelevTy ->
                Int ->
                ([RVarMp]) ->
                UIDS ->
                DataGam ->
                REnv ->
                EvalCtx ->
                RVarMp ->
                UID ->
                Bool ->
                Bool ->
                RelevTy ->
                RelevTy ->
                LamMp ->
                Int ->
                EHCOpts ->
                RVarMp ->
                ([WhatToRelevInfer]) ->
                ( Int,([RelevQualS]),CAltL ,FvS,UID,CAltL ,RelevQualS,RVarMp,RelevTy)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIaltId
       _lhsIaltMbScrutTy
       _lhsIaltNrMax
       _lhsIaltSolveLVarMp
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _hdOknTy :: RelevTy
              __tup2 :: ((AMSOut RelevTy,AnaMatchState))
              _amsoDw :: (AMSOut RelevTy)
              _amsDw :: AnaMatchState
              _altNr :: Int
              _altKnUpTy :: RelevTy
              _altKnDwTy :: RelevTy
              _lhsOty :: RelevTy
              _hdOrvarMp :: RVarMp
              _hdOfinalRVarMp :: RVarMp
              _lhsOaltQualSL :: ([RelevQualS])
              _tlOaltSolveLVarMp :: ([RVarMp])
              _lhsOcTrf :: CAltL 
              __tup4 :: ((UID,UID,UID))
              _hdOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOoTrf :: CAltL 
              _lhsOaltNr :: Int
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _hdOboundRelevTyVarS :: UIDS
              _hdOdataGam :: DataGam
              _hdOenv :: REnv
              _hdOevalCtx :: EvalCtx
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOknTyCase :: RelevTy
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOopts :: EHCOpts
              _hdOwhatTo :: ([WhatToRelevInfer])
              _tlOaltId :: UID
              _tlOaltMbScrutTy :: MbRelevTy
              _tlOaltNrMax :: Int
              _tlOboundRelevTyVarS :: UIDS
              _tlOdataGam :: DataGam
              _tlOenv :: REnv
              _tlOevalCtx :: EvalCtx
              _tlOfinalRVarMp :: RVarMp
              _tlOgUniq :: UID
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOknTy :: RelevTy
              _tlOknTyCase :: RelevTy
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOopts :: EHCOpts
              _tlOrvarMp :: RVarMp
              _tlOwhatTo :: ([WhatToRelevInfer])
              _hdIcTrf :: CAlt 
              _hdIcoe :: RelevCoe
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdImbCTag :: (Maybe CTag)
              _hdIoTrf :: CAlt 
              _hdIqualS :: RelevQualS
              _hdIrvarMp :: RVarMp
              _tlIaltNr :: Int
              _tlIaltQualSL :: ([RelevQualS])
              _tlIcTrf :: CAltL 
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlIoTrf :: CAltL 
              _tlIqualS :: RelevQualS
              _tlIrvarMp :: RVarMp
              _tlIty :: RelevTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 225, column 17)
              _hdOknTy =
                  _altKnDwTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 277, column 33)
              __tup2 =
                  amsLE _lhsIrvarMp _altKnDwTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 277, column 33)
              (_amsoDw,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 277, column 33)
              (_,_amsDw) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 277, column 17)
              _rvarMpDw =
                  amsLocalVarMp _amsDw |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 277, column 17)
              _rvarMpUp =
                  _rvarMpDw
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 286, column 17)
              _altNr =
                  _tlIaltNr + 1
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 295, column 17)
              _altKnUpTy =
                  fresh _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 297, column 17)
              _altKnDwTy =
                  fresh _lUniq2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 337, column 17)
              _hereAltCoe =
                  _hdIcoe <.> (_hdFinalRVarMp `varUpd` amsoCoe _amsoDw)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 366, column 17)
              _lhsOty =
                  _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 384, column 17)
              _hdOrvarMp =
                  _rvarMpUp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 404, column 17)
              _hdFinalRVarMp =
                  _altSolveVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 405, column 17)
              _hdOfinalRVarMp =
                  _hdFinalRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 429, column 17)
              _lhsOaltQualSL =
                  Set.unions [_hdIqualS, amsGathQual _amsDw] : _tlIaltQualSL
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 459, column 17)
              __tup3 =
                  hdAndTl' (panic "altSolveLVarMp") _lhsIaltSolveLVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 459, column 17)
              (_altSolveVarMp,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 459, column 17)
              (_,_tlOaltSolveLVarMp) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 620, column 17)
              _lhsOcTrf =
                  case _hdIcTrf of
                    CAlt_Alt p e -> CAlt_Alt p (annCoe _hereAltCoe e) : _tlIcTrf
              -- -- generated by the unique rule mechanism.
              __tup4 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )
              -- -- generated by the unique rule mechanism.
              (_hdOgUniq,_,_) =
                  __tup4
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_) =
                  __tup4
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2) =
                  __tup4
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _hdIqualS `Set.union` _tlIqualS
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _oTrf =
                  (:) _hdIoTrf _tlIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOaltNr =
                  _altNr
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _tlIrvarMp
              -- copy rule (down)
              _hdOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _hdOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _tlOaltId =
                  _lhsIaltId
              -- copy rule (down)
              _tlOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _tlOaltNrMax =
                  _lhsIaltNrMax
              -- copy rule (down)
              _tlOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _tlOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _tlOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOrvarMp =
                  _hdIrvarMp
              -- copy rule (down)
              _tlOwhatTo =
                  _lhsIwhatTo
              ( _hdIcTrf,_hdIcoe,_hdIfvS,_hdIgUniq,_hdImbCTag,_hdIoTrf,_hdIqualS,_hdIrvarMp) =
                  hd_ _hdOboundRelevTyVarS _hdOdataGam _hdOenv _hdOevalCtx _hdOfinalRVarMp _hdOgUniq _hdOisLamBody _hdOisStrict _hdOknTy _hdOknTyCase _hdOlamMp _hdOlev _hdOopts _hdOrvarMp _hdOwhatTo 
              ( _tlIaltNr,_tlIaltQualSL,_tlIcTrf,_tlIfvS,_tlIgUniq,_tlIoTrf,_tlIqualS,_tlIrvarMp,_tlIty) =
                  tl_ _tlOaltId _tlOaltMbScrutTy _tlOaltNrMax _tlOaltSolveLVarMp _tlOboundRelevTyVarS _tlOdataGam _tlOenv _tlOevalCtx _tlOfinalRVarMp _tlOgUniq _tlOisLamBody _tlOisStrict _tlOknTy _tlOknTyCase _tlOlamMp _tlOlev _tlOopts _tlOrvarMp _tlOwhatTo 
          in  ( _lhsOaltNr,_lhsOaltQualSL,_lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIaltId
       _lhsIaltMbScrutTy
       _lhsIaltNrMax
       _lhsIaltSolveLVarMp
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _altNr :: Int
              _lhsOty :: RelevTy
              _lhsOaltQualSL :: ([RelevQualS])
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CAltL 
              _lhsOoTrf :: CAltL 
              _lhsOaltNr :: Int
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 285, column 17)
              _altNr =
                  0
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 366, column 17)
              _lhsOty =
                  _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 430, column 17)
              _lhsOaltQualSL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _oTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOaltNr =
                  _altNr
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOaltNr,_lhsOaltQualSL,_lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty)))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         forQuantRVarMp       : RVarMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         altMbScrutTy         : MbRelevTy
         bindLamMp            : LamMp
         cTrf                 : SELF 
         extraBindRVarMp      : RVarMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathEnv              : REnv
         gathRecEnv           : REnv
         nm                   : HsName
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = UIDS ->
                DataGam ->
                REnv ->
                EvalCtx ->
                RVarMp ->
                RVarMp ->
                UID ->
                Bool ->
                Bool ->
                Bool ->
                RelevTy ->
                LamMp ->
                CBindCateg ->
                Int ->
                EHCOpts ->
                RVarMp ->
                ([WhatToRelevInfer]) ->
                ( MbRelevTy,LamMp,CBind ,RVarMp,FvS,FvSMp,UID,REnv,REnv,HsName,([HsName]),CBind ,RelevQualS,RVarMp)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _lhsOfvSMp :: FvSMp
              _lhsOnmL :: ([HsName])
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBind 
              _lhsOoTrf :: CBind 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _bindAspectsOboundRelevTyVarS :: UIDS
              _bindAspectsOdataGam :: DataGam
              _bindAspectsOenv :: REnv
              _bindAspectsOevalCtx :: EvalCtx
              _bindAspectsOfinalRVarMp :: RVarMp
              _bindAspectsOforQuantRVarMp :: RVarMp
              _bindAspectsOgUniq :: UID
              _bindAspectsOisGlobal :: Bool
              _bindAspectsOisLamBody :: Bool
              _bindAspectsOisStrict :: Bool
              _bindAspectsOknTy :: RelevTy
              _bindAspectsOlamMp :: LamMp
              _bindAspectsOletBindingsCateg :: CBindCateg
              _bindAspectsOlev :: Int
              _bindAspectsOopts :: EHCOpts
              _bindAspectsOrvarMp :: RVarMp
              _bindAspectsOwhatTo :: ([WhatToRelevInfer])
              _bindAspectsIaltMbScrutTy :: MbRelevTy
              _bindAspectsIbindLamMp :: LamMp
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIextraBindRVarMp :: RVarMp
              _bindAspectsIfvS :: FvS
              _bindAspectsIfvSMp :: FvSMp
              _bindAspectsIgUniq :: UID
              _bindAspectsIgathEnv :: REnv
              _bindAspectsIgathRecEnv :: REnv
              _bindAspectsInmL :: ([HsName])
              _bindAspectsIoTrf :: CBoundL 
              _bindAspectsIqualS :: RelevQualS
              _bindAspectsIrvarMp :: RVarMp
              _bindAspectsItyAspectL :: ([CBound])
              _bindAspectsIvalAspectL :: ([CBound])
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 605, column 17)
              _cTrf =
                  CBind_Bind nm_ (_bindAspectsItyAspectL ++ _bindAspectsIvalAspectL ++ _bindAspectsIcTrf)
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 4, column 17)
              _bindAspectsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 12, column 17)
              _lhsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 11, column 17)
              _lhsOfvSMp =
                  Map.singleton nm_ _bindAspectsIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 19, column 17)
              _lhsOnmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _bindAspectsIaltMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _bindAspectsIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  _bindAspectsIextraBindRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindAspectsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  _bindAspectsIgathEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  _bindAspectsIgathRecEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _bindAspectsIqualS
              -- self rule
              _oTrf =
                  CBind_Bind nm_ _bindAspectsIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bindAspectsIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _bindAspectsIrvarMp
              -- copy rule (down)
              _bindAspectsOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindAspectsOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bindAspectsOenv =
                  _lhsIenv
              -- copy rule (down)
              _bindAspectsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bindAspectsOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _bindAspectsOforQuantRVarMp =
                  _lhsIforQuantRVarMp
              -- copy rule (down)
              _bindAspectsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bindAspectsOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _bindAspectsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindAspectsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindAspectsOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindAspectsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindAspectsOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _bindAspectsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindAspectsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindAspectsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _bindAspectsOwhatTo =
                  _lhsIwhatTo
              ( _bindAspectsIaltMbScrutTy,_bindAspectsIbindLamMp,_bindAspectsIcTrf,_bindAspectsIextraBindRVarMp,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsIgUniq,_bindAspectsIgathEnv,_bindAspectsIgathRecEnv,_bindAspectsInmL,_bindAspectsIoTrf,_bindAspectsIqualS,_bindAspectsIrvarMp,_bindAspectsItyAspectL,_bindAspectsIvalAspectL) =
                  bindAspects_ _bindAspectsOboundRelevTyVarS _bindAspectsOdataGam _bindAspectsOenv _bindAspectsOevalCtx _bindAspectsOfinalRVarMp _bindAspectsOforQuantRVarMp _bindAspectsOgUniq _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOknTy _bindAspectsOlamMp _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOnm _bindAspectsOopts _bindAspectsOrvarMp _bindAspectsOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnm,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = UIDS ->
                   DataGam ->
                   REnv ->
                   RVarMp ->
                   UID ->
                   RelevTy ->
                   RelevTy ->
                   LamMp ->
                   Int ->
                   EHCOpts ->
                   RVarMp ->
                   ([WhatToRelevInfer]) ->
                   ( CBindAnn ,FvS,UID,([HsName]),CBindAnn ,RelevQualS,RVarMp)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBindAnn 
              _lhsOoTrf :: CBindAnn 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CBindAnn_Coe coe_
              -- self rule
              _oTrf =
                  CBindAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = UIDS ->
                    DataGam ->
                    REnv ->
                    RVarMp ->
                    UID ->
                    RelevTy ->
                    RelevTy ->
                    LamMp ->
                    Int ->
                    EHCOpts ->
                    RVarMp ->
                    ([WhatToRelevInfer]) ->
                    ( CBindAnnL ,FvS,UID,([HsName]),CBindAnnL ,RelevQualS,RVarMp)
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBindAnnL 
              _lhsOoTrf :: CBindAnnL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _hdOboundRelevTyVarS :: UIDS
              _hdOdataGam :: DataGam
              _hdOenv :: REnv
              _hdOfinalRVarMp :: RVarMp
              _hdOgUniq :: UID
              _hdOknTy :: RelevTy
              _hdOknTyCase :: RelevTy
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOopts :: EHCOpts
              _hdOrvarMp :: RVarMp
              _hdOwhatTo :: ([WhatToRelevInfer])
              _tlOboundRelevTyVarS :: UIDS
              _tlOdataGam :: DataGam
              _tlOenv :: REnv
              _tlOfinalRVarMp :: RVarMp
              _tlOgUniq :: UID
              _tlOknTy :: RelevTy
              _tlOknTyCase :: RelevTy
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOopts :: EHCOpts
              _tlOrvarMp :: RVarMp
              _tlOwhatTo :: ([WhatToRelevInfer])
              _hdIcTrf :: CBindAnn 
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdInmL :: ([HsName])
              _hdIoTrf :: CBindAnn 
              _hdIqualS :: RelevQualS
              _hdIrvarMp :: RVarMp
              _tlIcTrf :: CBindAnnL 
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlInmL :: ([HsName])
              _tlIoTrf :: CBindAnnL 
              _tlIqualS :: RelevQualS
              _tlIrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _hdIqualS `Set.union` _tlIqualS
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _oTrf =
                  (:) _hdIoTrf _tlIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _tlIrvarMp
              -- copy rule (down)
              _hdOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _hdOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _hdOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _hdOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _tlOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _tlOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _tlOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOrvarMp =
                  _hdIrvarMp
              -- copy rule (down)
              _tlOwhatTo =
                  _lhsIwhatTo
              ( _hdIcTrf,_hdIfvS,_hdIgUniq,_hdInmL,_hdIoTrf,_hdIqualS,_hdIrvarMp) =
                  hd_ _hdOboundRelevTyVarS _hdOdataGam _hdOenv _hdOfinalRVarMp _hdOgUniq _hdOknTy _hdOknTyCase _hdOlamMp _hdOlev _hdOopts _hdOrvarMp _hdOwhatTo 
              ( _tlIcTrf,_tlIfvS,_tlIgUniq,_tlInmL,_tlIoTrf,_tlIqualS,_tlIrvarMp) =
                  tl_ _tlOboundRelevTyVarS _tlOdataGam _tlOenv _tlOfinalRVarMp _tlOgUniq _tlOknTy _tlOknTyCase _tlOlamMp _tlOlev _tlOopts _tlOrvarMp _tlOwhatTo 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBindAnnL 
              _lhsOoTrf :: CBindAnnL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _oTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         forQuantRVarMp       : RVarMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         altMbScrutTy         : MbRelevTy
         bindLamMp            : LamMp
         cTrf                 : SELF 
         extraBindRVarMp      : RVarMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathEnv              : REnv
         gathRecEnv           : REnv
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = UIDS ->
                 DataGam ->
                 REnv ->
                 EvalCtx ->
                 RVarMp ->
                 RVarMp ->
                 UID ->
                 Bool ->
                 Bool ->
                 Bool ->
                 RelevTy ->
                 LamMp ->
                 CBindCateg ->
                 Int ->
                 EHCOpts ->
                 RVarMp ->
                 ([WhatToRelevInfer]) ->
                 ( MbRelevTy,LamMp,CBindL ,RVarMp,FvS,FvSMp,UID,REnv,REnv,([HsName]),CBindL ,RelevQualS,RVarMp)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBindL 
              _lhsOoTrf :: CBindL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _hdOboundRelevTyVarS :: UIDS
              _hdOdataGam :: DataGam
              _hdOenv :: REnv
              _hdOevalCtx :: EvalCtx
              _hdOfinalRVarMp :: RVarMp
              _hdOforQuantRVarMp :: RVarMp
              _hdOgUniq :: UID
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOknTy :: RelevTy
              _hdOlamMp :: LamMp
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOopts :: EHCOpts
              _hdOrvarMp :: RVarMp
              _hdOwhatTo :: ([WhatToRelevInfer])
              _tlOboundRelevTyVarS :: UIDS
              _tlOdataGam :: DataGam
              _tlOenv :: REnv
              _tlOevalCtx :: EvalCtx
              _tlOfinalRVarMp :: RVarMp
              _tlOforQuantRVarMp :: RVarMp
              _tlOgUniq :: UID
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOknTy :: RelevTy
              _tlOlamMp :: LamMp
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOopts :: EHCOpts
              _tlOrvarMp :: RVarMp
              _tlOwhatTo :: ([WhatToRelevInfer])
              _hdIaltMbScrutTy :: MbRelevTy
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBind 
              _hdIextraBindRVarMp :: RVarMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: UID
              _hdIgathEnv :: REnv
              _hdIgathRecEnv :: REnv
              _hdInm :: HsName
              _hdInmL :: ([HsName])
              _hdIoTrf :: CBind 
              _hdIqualS :: RelevQualS
              _hdIrvarMp :: RVarMp
              _tlIaltMbScrutTy :: MbRelevTy
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBindL 
              _tlIextraBindRVarMp :: RVarMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: UID
              _tlIgathEnv :: REnv
              _tlIgathRecEnv :: REnv
              _tlInmL :: ([HsName])
              _tlIoTrf :: CBindL 
              _tlIqualS :: RelevQualS
              _tlIrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _hdIaltMbScrutTy <|> _tlIaltMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  _hdIextraBindRVarMp |+> _tlIextraBindRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  _hdIgathEnv `gamUnion` _tlIgathEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  _hdIgathRecEnv `gamUnion` _tlIgathRecEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _hdIqualS `Set.union` _tlIqualS
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _oTrf =
                  (:) _hdIoTrf _tlIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _tlIrvarMp
              -- copy rule (down)
              _hdOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _hdOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _hdOforQuantRVarMp =
                  _lhsIforQuantRVarMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _hdOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _tlOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _tlOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _tlOforQuantRVarMp =
                  _lhsIforQuantRVarMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOrvarMp =
                  _hdIrvarMp
              -- copy rule (down)
              _tlOwhatTo =
                  _lhsIwhatTo
              ( _hdIaltMbScrutTy,_hdIbindLamMp,_hdIcTrf,_hdIextraBindRVarMp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIgathEnv,_hdIgathRecEnv,_hdInm,_hdInmL,_hdIoTrf,_hdIqualS,_hdIrvarMp) =
                  hd_ _hdOboundRelevTyVarS _hdOdataGam _hdOenv _hdOevalCtx _hdOfinalRVarMp _hdOforQuantRVarMp _hdOgUniq _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOknTy _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOopts _hdOrvarMp _hdOwhatTo 
              ( _tlIaltMbScrutTy,_tlIbindLamMp,_tlIcTrf,_tlIextraBindRVarMp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIgathEnv,_tlIgathRecEnv,_tlInmL,_tlIoTrf,_tlIqualS,_tlIrvarMp) =
                  tl_ _tlOboundRelevTyVarS _tlOdataGam _tlOenv _tlOevalCtx _tlOfinalRVarMp _tlOforQuantRVarMp _tlOgUniq _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOknTy _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOopts _tlOrvarMp _tlOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CBindL 
              _lhsOoTrf :: CBindL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _oTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         forQuantRVarMp       : RVarMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         nm                   : HsName
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         altMbScrutTy         : MbRelevTy
         bindLamMp            : LamMp
         cTrf                 : SELF 
         coe                  : RelevCoe
         extraBindRVarMp      : RVarMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathEnv              : REnv
         gathRecEnv           : REnv
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
         tyAspectL            : [CBound]
         valAspectL           : [CBound]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local doBindStrict : {Bool}
            local gathEnv     : _
            local gathRecEnv  : _
            local altMbScrutTy : _
            local hereBindCoe : _
            local finalRVarMp : _
            local extraBindRVarMp : _
            local _tup5       : {(RelevTy,RelevTy,REnv,UIDS,RVarMp -> RelevTy -> RelevTy -> (AnaMatchState,Maybe RelevCoe))}
            local ty          : {RelevTy}
            local exprKnTy    : {RelevTy}
            local exprEnv     : {REnv}
            local exprBoundS  : {UIDS}
            local bindConstrain : {RVarMp -> RelevTy -> RelevTy -> (AnaMatchState,Maybe RelevCoe)}
            local _tup6       : {(AnaMatchState,Maybe RelevCoe)}
            local amsBind     : {AnaMatchState}
            local mbBindCoe   : {Maybe RelevCoe}
            local rvarMpExpr  : _
            local _tup7       : {(RelevTy,RVarMp,RelevQualS)}
            local quantTy     : {RelevTy}
            local quantVarMp  : {RVarMp}
            local quantRemQualS : {RelevQualS}
            local bindTy      : {RelevTy}
            local _tup8       : {(RelevTy,RVarMp)}
            local strictTy    : {RelevTy}
            local strictVarMp : {RVarMp}
            local debugTy1    : _
            local debugTy2    : _
            local strictTy2   : {RelevTy}
            local tyAspectL   : _
            local valAspectL  : _
            local whatAbove   : {WhatExpr}
            local _tup9       : {(UID,UID)}
            local lUniq       : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local altMbScrutTy : _
            local argTyLresTy : _
            local argTyL      : _
            local resTy       : _
            local foreignEntInfo : _
            local expEntNm    : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local altMbScrutTy : _
            local cTrf        : _
            local oTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local altMbScrutTy : _
            local cTrf        : _
            local oTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local altMbScrutTy : _
            local cTrf        : _
            local oTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local altMbScrutTy : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBound :: CBound  ->
              T_CBound 
sem_CBound (CBound_Bind _bindMeta _expr )  =
    (sem_CBound_Bind (sem_CMetas _bindMeta ) (sem_CExpr _expr ) )
sem_CBound (CBound_FFE _callconv _expEnt _expr _ty )  =
    (sem_CBound_FFE _callconv _expEnt (sem_CExpr _expr ) _ty )
sem_CBound (CBound_Meta _aspectKeyS _cmetas )  =
    (sem_CBound_Meta _aspectKeyS (sem_CMetas _cmetas ) )
sem_CBound (CBound_RelevTy _aspectKeyS _relevTy )  =
    (sem_CBound_RelevTy _aspectKeyS _relevTy )
sem_CBound (CBound_Ty _aspectKeyS _ty )  =
    (sem_CBound_Ty _aspectKeyS _ty )
sem_CBound (CBound_Val _aspectKeyS _expr )  =
    (sem_CBound_Val _aspectKeyS (sem_CExpr _expr ) )
-- semantic domain
type T_CBound  = UIDS ->
                 DataGam ->
                 REnv ->
                 EvalCtx ->
                 RVarMp ->
                 RVarMp ->
                 UID ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 RelevTy ->
                 LamMp ->
                 CBindCateg ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 RVarMp ->
                 ([WhatToRelevInfer]) ->
                 ( MbRelevTy,LamMp,CBound ,RelevCoe,RVarMp,FvS,FvSMp,UID,REnv,REnv,([HsName]),CBound ,RelevQualS,RVarMp,([CBound]),([CBound]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _doBindStrict :: Bool
              _lhsObindLamMp :: LamMp
              _exprOenv :: REnv
              _exprOknTy :: RelevTy
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsOcoe :: RelevCoe
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _exprOboundRelevTyVarS :: UIDS
              __tup5 :: ((RelevTy,RelevTy,REnv,UIDS,RVarMp -> RelevTy -> RelevTy -> (AnaMatchState,Maybe RelevCoe)))
              _ty :: RelevTy
              _exprKnTy :: RelevTy
              _exprEnv :: REnv
              _exprBoundS :: UIDS
              _bindConstrain :: (RVarMp -> RelevTy -> RelevTy -> (AnaMatchState,Maybe RelevCoe))
              __tup6 :: ((AnaMatchState,Maybe RelevCoe))
              _amsBind :: AnaMatchState
              _mbBindCoe :: (Maybe RelevCoe)
              __tup7 :: ((RelevTy,RVarMp,RelevQualS))
              _quantTy :: RelevTy
              _quantVarMp :: RVarMp
              _quantRemQualS :: RelevQualS
              _bindTy :: RelevTy
              __tup8 :: ((RelevTy,RVarMp))
              _strictTy :: RelevTy
              _strictVarMp :: RVarMp
              _strictTy2 :: RelevTy
              _lhsOcTrf :: CBound 
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              __tup9 :: ((UID,UID))
              _bindMetaOgUniq :: UID
              _lUniq :: UID
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOoTrf :: CBound 
              _lhsOgUniq :: UID
              _bindMetaOboundRelevTyVarS :: UIDS
              _bindMetaOdataGam :: DataGam
              _bindMetaOenv :: REnv
              _bindMetaOfinalRVarMp :: RVarMp
              _bindMetaOknTy :: RelevTy
              _bindMetaOlamMp :: LamMp
              _bindMetaOlev :: Int
              _bindMetaOopts :: EHCOpts
              _bindMetaOrvarMp :: RVarMp
              _bindMetaOwhatTo :: ([WhatToRelevInfer])
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOdataGam :: DataGam
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _bindMetaIcTrf :: CMetas 
              _bindMetaIfvS :: FvS
              _bindMetaIgUniq :: UID
              _bindMetaIoTrf :: CMetas 
              _bindMetaIqualS :: RelevQualS
              _bindMetaIrvarMp :: RVarMp
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 114, column 17)
              _doBindStrict =
                  WhatToRelevInfer_InstToBot `elem` _lhsIwhatTo
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 115, column 17)
              _lhsObindLamMp =
                  let l = [ (acbaspkeyDefaultRelevTy, LamInfoBindAsp_RelevTy _bindTy) ]
                  in  Map.singleton _lhsInm (emptyLamInfo {laminfoBindAspMp = Map.fromList l})
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 153, column 17)
              _exprOenv =
                  gamAddGam _exprEnv _lhsIenv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 195, column 17)
              _gathEnv =
                  gamSingleton _lhsInm _quantTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 195, column 17)
              _gathRecEnv =
                  gamSingleton _lhsInm $ tup123to1 $ relevtyQuant [RelevTyQuantHow_Rec] emptyRVarMp Set.empty _ty
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 204, column 17)
              _exprOknTy =
                  _exprKnTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 306, column 17)
              _lhsOaltMbScrutTy =
                  do v <- _exprImbVar
                     renvLookup v _lhsIenv Map.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 333, column 17)
              _hereBindCoe =
                  _exprIcoe <.> maybe RelevCoe_Id (_finalRVarMp `varUpd`) _mbBindCoe
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 334, column 25)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 375, column 17)
              _lhsOrvarMp =
                  _quantVarMp |+> _rvarMpExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 401, column 17)
              _finalRVarMp =
                  _lhsIfinalRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 411, column 17)
              _extraBindRVarMp =
                  if _doBindStrict then _strictVarMp else emptyRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 424, column 17)
              _lhsOqualS =
                  Set.unions [amsGathQual _amsBind, _exprIqualS]
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 485, column 17)
              _exprOboundRelevTyVarS =
                  _exprBoundS
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              __tup5 =
                  let noConstrain _ _ _ = (emptyAnaMatchState, Nothing)
                      strictConstrain m t knTy = (ams, Just $ amsoCoe amso)
                        where (amso,ams) = amsLE m t knTy
                  in  case _exprImbLam of
                        Just nmL
                          -> (ty, r', exprEnv, varFreeSet ty, noConstrain)
                          where us@(r:as) = mkNewLevUIDL (1 + length nmL) _lUniq
                                r' = fresh r
                                as' = map fresh as
                                exprEnv = gamFromAssocL (zip nmL as')
                                ty = RelevTy_Fun RQuant_None [] [] as' r'
                        _ | isStrict  -> (bTy, bTy, emptyGam, Set.empty, strictConstrain)
                          | otherwise -> (bTy, bTy, emptyGam, Set.empty, noConstrain)
                          where isStrict = _lhsIletBindingsCateg == CBindCateg_Strict
                                (_,u1,u2) = mkNewLevUID2 _lUniq
                                eTy@(RelevTy_Ana eAna) = fresh u1
                                bTy@(RelevTy_Ana bAna) = fresh u2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              (_ty,_,_,_,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              (_,_exprKnTy,_,_,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              (_,_,_exprEnv,_,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              (_,_,_,_exprBoundS,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 33)
              (_,_,_,_,_bindConstrain) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 510, column 33)
              __tup6 =
                  _bindConstrain _exprIrvarMp _exprKnTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 510, column 33)
              (_amsBind,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 510, column 33)
              (_,_mbBindCoe) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _rvarMpExpr =
                  amsLocalVarMp _amsBind |+> _exprIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 513, column 33)
              __tup7 =
                  case _ty of
                    t@(RelevTy_Fun _ _ _ _ _)
                      -> relevtyQuant ([RelevTyQuantHow_Solve] ++ how)
                                      _rvarMpExpr _exprIqualS t
                      where how | WhatToRelevInfer_Quant `elem` _lhsIwhatTo = [RelevTyQuantHow_RemoveAmbig,RelevTyQuantHow_Quant]
                                | otherwise                                 = []
                    t -> (m `varUpd` t, m, q)
                      where (q,m) = assSolve _lhsIboundRelevTyVarS (Set.map (_rvarMpExpr `varUpd`) _exprIqualS)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 513, column 33)
              (_quantTy,_,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 513, column 33)
              (_,_quantVarMp,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 513, column 33)
              (_,_,_quantRemQualS) =
                  __tup7
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _bindTy =
                  _lhsIforQuantRVarMp `varUpd` _quantTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 523, column 33)
              __tup8 =
                  case _bindTy of
                    t@(RelevTy_Fun _ _ qs a r@(RelevTy_Ana (AnaEval_Var rv)))
                      -> (sty, smp2 |+> smpAssume2 |+> smp1 |+> smpAssume1)
                      where smpAssume1 = rvarmpEvalUnit rv bot
                            (RelevTy_Fun _ vs' qs' a' r',smp1,rem1)
                              = relevtyQuant [RelevTyQuantHow_Solve,RelevTyQuantHow_RemoveAmbig,RelevTyQuantHow_Quant]
                                             smpAssume1 (Set.fromList qs `Set.union` _quantRemQualS) (RelevTy_Fun RQuant_None [] [] a (smpAssume1 `varUpd` r))
                            smpAssume2 = rvarmpUnions [ rvarmpEvalUnit v top | v <- vs' ]
                            (sty,smp2,_)
                              = relevtyQuant [RelevTyQuantHow_Solve,RelevTyQuantHow_RemoveAmbig,RelevTyQuantHow_Quant]
                                             smpAssume2 (Set.fromList qs' `Set.union` rem1) (RelevTy_Fun RQuant_None [] [] (smpAssume2 `varUpd` a') r')
                    t -> (m `varUpd` t, m)
                      where (q,m) = assSolve Set.empty (Set.map (_lhsIforQuantRVarMp `varUpd`) _quantRemQualS)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 523, column 33)
              (_strictTy,_) =
                  __tup8
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 523, column 33)
              (_,_strictVarMp) =
                  __tup8
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _debugTy1 =
                  case _rvarMpExpr `varUpd` _ty of
                    RelevTy_Fun _ _ _ a r
                      -> RelevTy_Fun RQuant_None v q a r
                      where q = _rvarMpExpr `varUpd` Set.toList _exprIqualS
                            v = Set.toList $ Set.unions [varFreeSet q, varFreeSet a, varFreeSet r]
                    t -> t
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _debugTy2 =
                  case _lhsIfinalRVarMp `varUpd` _ty of
                    RelevTy_Fun _ _ _ a r
                      -> RelevTy_Fun RQuant_None [] q a r
                      where q = _lhsIfinalRVarMp `varUpd` Set.toList _exprIqualS
                    t -> t
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _strictTy2 =
                  _lhsIfinalRVarMp `varUpd` _strictTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 491, column 17)
              _tyAspectL =
                  [ CBound_RelevTy acbaspkeyDefault _bindTy
                  ]
                  ++ (if _doBindStrict
                      then [CBound_RelevTy acbaspkeyStrict _strictTy2]
                      else []
                     )
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 600, column 17)
              _lhsOcTrf =
                  CBound_Bind _bindMetaIcTrf $
                  dbgBind _lhsIopts _lhsIfinalRVarMp _gathEnv $
                  annCoe _hereBindCoe _exprIcTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 629, column 17)
              _valAspectL =
                  [
                  ]
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- -- generated by the unique rule mechanism.
              __tup9 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_bindMetaOgUniq,_) =
                  __tup9
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup9
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  _extraBindRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindMetaIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  _gathEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  _gathRecEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  _tyAspectL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  _valAspectL
              -- self rule
              _cTrf =
                  CBound_Bind _bindMetaIcTrf _exprIcTrf
              -- self rule
              _oTrf =
                  CBound_Bind _bindMetaIoTrf _exprIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _bindMetaOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindMetaOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bindMetaOenv =
                  _lhsIenv
              -- copy rule (from local)
              _bindMetaOfinalRVarMp =
                  _finalRVarMp
              -- copy rule (down)
              _bindMetaOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindMetaOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindMetaOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindMetaOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindMetaOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _bindMetaOwhatTo =
                  _lhsIwhatTo
              -- copy rule (from local)
              _exprOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (from local)
              _exprOfinalRVarMp =
                  _finalRVarMp
              -- copy rule (chain)
              _exprOgUniq =
                  _bindMetaIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exprOrvarMp =
                  _bindMetaIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _bindMetaIcTrf,_bindMetaIfvS,_bindMetaIgUniq,_bindMetaIoTrf,_bindMetaIqualS,_bindMetaIrvarMp) =
                  bindMeta_ _bindMetaOboundRelevTyVarS _bindMetaOdataGam _bindMetaOenv _bindMetaOfinalRVarMp _bindMetaOgUniq _bindMetaOknTy _bindMetaOlamMp _bindMetaOlev _bindMetaOopts _bindMetaOrvarMp _bindMetaOwhatTo 
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBound 
              _lhsOoTrf :: CBound 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 334, column 25)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 21, column 17)
              _argTyLresTy =
                  tyArrowArgsRes ty_
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 21, column 17)
              _argTyL =
                  fst _argTyLresTy
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 21, column 17)
              _resTy =
                  snd _argTyLresTy
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 21, column 17)
              _foreignEntInfo =
                  foreignEntExtract expEnt_
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 21, column 17)
              _expEntNm =
                  forextractEnt _foreignEntInfo
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 76, column 17)
              _whatAbove =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _altMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  CBound_FFE callconv_ expEnt_ _exprIcTrf ty_
              -- self rule
              _oTrf =
                  CBound_FFE callconv_ expEnt_ _exprIoTrf ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _exprOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBound 
              _lhsOoTrf :: CBound 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _cmetasOboundRelevTyVarS :: UIDS
              _cmetasOdataGam :: DataGam
              _cmetasOenv :: REnv
              _cmetasOfinalRVarMp :: RVarMp
              _cmetasOgUniq :: UID
              _cmetasOknTy :: RelevTy
              _cmetasOlamMp :: LamMp
              _cmetasOlev :: Int
              _cmetasOopts :: EHCOpts
              _cmetasOrvarMp :: RVarMp
              _cmetasOwhatTo :: ([WhatToRelevInfer])
              _cmetasIcTrf :: CMetas 
              _cmetasIfvS :: FvS
              _cmetasIgUniq :: UID
              _cmetasIoTrf :: CMetas 
              _cmetasIqualS :: RelevQualS
              _cmetasIrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 334, column 25)
              _lhsOcoe =
                  RelevCoe_Id
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _altMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _cmetasIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _cmetasIqualS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  CBound_Meta aspectKeyS_ _cmetasIcTrf
              -- self rule
              _oTrf =
                  CBound_Meta aspectKeyS_ _cmetasIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _cmetasIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _cmetasIrvarMp
              -- copy rule (down)
              _cmetasOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _cmetasOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _cmetasOenv =
                  _lhsIenv
              -- copy rule (down)
              _cmetasOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _cmetasOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _cmetasOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _cmetasOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _cmetasOlev =
                  _lhsIlev
              -- copy rule (down)
              _cmetasOopts =
                  _lhsIopts
              -- copy rule (down)
              _cmetasOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _cmetasOwhatTo =
                  _lhsIwhatTo
              ( _cmetasIcTrf,_cmetasIfvS,_cmetasIgUniq,_cmetasIoTrf,_cmetasIqualS,_cmetasIrvarMp) =
                  cmetas_ _cmetasOboundRelevTyVarS _cmetasOdataGam _cmetasOenv _cmetasOfinalRVarMp _cmetasOgUniq _cmetasOknTy _cmetasOlamMp _cmetasOlev _cmetasOopts _cmetasOrvarMp _cmetasOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBound 
              _lhsOoTrf :: CBound 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 334, column 25)
              _lhsOcoe =
                  RelevCoe_Id
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _altMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  CBound_RelevTy aspectKeyS_ relevTy_
              -- self rule
              _oTrf =
                  CBound_RelevTy aspectKeyS_ relevTy_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBound 
              _lhsOoTrf :: CBound 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 334, column 25)
              _lhsOcoe =
                  RelevCoe_Id
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _altMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  CBound_Ty aspectKeyS_ ty_
              -- self rule
              _oTrf =
                  CBound_Ty aspectKeyS_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBound 
              _lhsOoTrf :: CBound 
              _lhsOcoe :: RelevCoe
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _altMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  CBound_Val aspectKeyS_ _exprIcTrf
              -- self rule
              _oTrf =
                  CBound_Val aspectKeyS_ _exprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOcoe =
                  _exprIcoe
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _exprOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOcoe,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         forQuantRVarMp       : RVarMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         nm                   : HsName
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         altMbScrutTy         : MbRelevTy
         bindLamMp            : LamMp
         cTrf                 : SELF 
         extraBindRVarMp      : RVarMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathEnv              : REnv
         gathRecEnv           : REnv
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
         tyAspectL            : [CBound]
         valAspectL           : [CBound]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = UIDS ->
                  DataGam ->
                  REnv ->
                  EvalCtx ->
                  RVarMp ->
                  RVarMp ->
                  UID ->
                  Bool ->
                  Bool ->
                  Bool ->
                  RelevTy ->
                  LamMp ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  RVarMp ->
                  ([WhatToRelevInfer]) ->
                  ( MbRelevTy,LamMp,CBoundL ,RVarMp,FvS,FvSMp,UID,REnv,REnv,([HsName]),CBoundL ,RelevQualS,RVarMp,([CBound]),([CBound]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _hdOisTopApp :: Bool
              _hdOisTopTup :: Bool
              _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBoundL 
              _lhsOoTrf :: CBoundL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _hdOboundRelevTyVarS :: UIDS
              _hdOdataGam :: DataGam
              _hdOenv :: REnv
              _hdOevalCtx :: EvalCtx
              _hdOfinalRVarMp :: RVarMp
              _hdOforQuantRVarMp :: RVarMp
              _hdOgUniq :: UID
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOknTy :: RelevTy
              _hdOlamMp :: LamMp
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOnm :: HsName
              _hdOopts :: EHCOpts
              _hdOrvarMp :: RVarMp
              _hdOwhatTo :: ([WhatToRelevInfer])
              _tlOboundRelevTyVarS :: UIDS
              _tlOdataGam :: DataGam
              _tlOenv :: REnv
              _tlOevalCtx :: EvalCtx
              _tlOfinalRVarMp :: RVarMp
              _tlOforQuantRVarMp :: RVarMp
              _tlOgUniq :: UID
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOknTy :: RelevTy
              _tlOlamMp :: LamMp
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOnm :: HsName
              _tlOopts :: EHCOpts
              _tlOrvarMp :: RVarMp
              _tlOwhatTo :: ([WhatToRelevInfer])
              _hdIaltMbScrutTy :: MbRelevTy
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBound 
              _hdIcoe :: RelevCoe
              _hdIextraBindRVarMp :: RVarMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: UID
              _hdIgathEnv :: REnv
              _hdIgathRecEnv :: REnv
              _hdInmL :: ([HsName])
              _hdIoTrf :: CBound 
              _hdIqualS :: RelevQualS
              _hdIrvarMp :: RVarMp
              _hdItyAspectL :: ([CBound])
              _hdIvalAspectL :: ([CBound])
              _tlIaltMbScrutTy :: MbRelevTy
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBoundL 
              _tlIextraBindRVarMp :: RVarMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: UID
              _tlIgathEnv :: REnv
              _tlIgathRecEnv :: REnv
              _tlInmL :: ([HsName])
              _tlIoTrf :: CBoundL 
              _tlIqualS :: RelevQualS
              _tlIrvarMp :: RVarMp
              _tlItyAspectL :: ([CBound])
              _tlIvalAspectL :: ([CBound])
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopTup =
                  True
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  _hdIaltMbScrutTy <|> _tlIaltMbScrutTy
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  _hdIextraBindRVarMp |+> _tlIextraBindRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  _hdIgathEnv `gamUnion` _tlIgathEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  _hdIgathRecEnv `gamUnion` _tlIgathRecEnv
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _hdIqualS `Set.union` _tlIqualS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  _hdItyAspectL ++ _tlItyAspectL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  _hdIvalAspectL ++ _tlIvalAspectL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _oTrf =
                  (:) _hdIoTrf _tlIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _tlIrvarMp
              -- copy rule (down)
              _hdOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _hdOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _hdOforQuantRVarMp =
                  _lhsIforQuantRVarMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _hdOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _tlOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _tlOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _tlOforQuantRVarMp =
                  _lhsIforQuantRVarMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOrvarMp =
                  _hdIrvarMp
              -- copy rule (down)
              _tlOwhatTo =
                  _lhsIwhatTo
              ( _hdIaltMbScrutTy,_hdIbindLamMp,_hdIcTrf,_hdIcoe,_hdIextraBindRVarMp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIgathEnv,_hdIgathRecEnv,_hdInmL,_hdIoTrf,_hdIqualS,_hdIrvarMp,_hdItyAspectL,_hdIvalAspectL) =
                  hd_ _hdOboundRelevTyVarS _hdOdataGam _hdOenv _hdOevalCtx _hdOfinalRVarMp _hdOforQuantRVarMp _hdOgUniq _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOknTy _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOnm _hdOopts _hdOrvarMp _hdOwhatTo 
              ( _tlIaltMbScrutTy,_tlIbindLamMp,_tlIcTrf,_tlIextraBindRVarMp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIgathEnv,_tlIgathRecEnv,_tlInmL,_tlIoTrf,_tlIqualS,_tlIrvarMp,_tlItyAspectL,_tlIvalAspectL) =
                  tl_ _tlOboundRelevTyVarS _tlOdataGam _tlOenv _tlOevalCtx _tlOfinalRVarMp _tlOforQuantRVarMp _tlOgUniq _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOknTy _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOnm _tlOopts _tlOrvarMp _tlOwhatTo 
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIforQuantRVarMp
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOaltMbScrutTy :: MbRelevTy
              _lhsObindLamMp :: LamMp
              _lhsOextraBindRVarMp :: RVarMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathEnv :: REnv
              _lhsOgathRecEnv :: REnv
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOtyAspectL :: ([CBound])
              _lhsOvalAspectL :: ([CBound])
              _lhsOcTrf :: CBoundL 
              _lhsOoTrf :: CBoundL 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 303, column 33)
              _lhsOaltMbScrutTy =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 408, column 36)
              _lhsOextraBindRVarMp =
                  emptyRVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 191, column 32)
              _lhsOgathEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 192, column 32)
              _lhsOgathRecEnv =
                  emptyGam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 571, column 32)
              _lhsOtyAspectL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 572, column 32)
              _lhsOvalAspectL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _oTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOaltMbScrutTy,_lhsObindLamMp,_lhsOcTrf,_lhsOextraBindRVarMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathEnv,_lhsOgathRecEnv,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOtyAspectL,_lhsOvalAspectL)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         altMbScrutTy         : MbRelevTy
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatAbove            : WhatExpr
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         appFunKind           : AppFunKind
         argL                 : [CBound]
         cTrf                 : SELF 
         coe                  : RelevCoe
         funCoe               : RelevCoe
         fvS                  : FvS
         gathLamMp            : LamMp
         mbFFIApp             : Maybe ( Ty
                                  , Bool
                                  , FFIWay
                                  , ForeignEnt
                                  , [Ty]
                                  )
         mbFunVar             : Maybe HsName
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
         oTrf                 : SELF 
         qualS                : RelevQualS
         ty                   : RelevTy
         whatBelow            : WhatExpr
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local funTy       : {RelevTy}
            local _tup10      : _
            local argTy       : {RelevTy}
            local resTy       : _
            local resFunTy    : _
            local hereArgCoe  : _
            local hereFunCoe  : _
            local _tup11      : _
            local argCoe      : _
            local resCoe      : _
            local forQuantRVarMp : _
            local isGlobal    : _
            local argL        : _
            local letBindingsCateg : _
            local isTopTup    : _
            local whatBelow   : _
            local isTopApp'   : _
            local whatAbove   : {WhatExpr}
            local fvS         : _
            local _tup12      : {(UID,UID)}
            local lUniq       : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local altQualSLSubs : {[RelevQualS]}
            local altQualSLSolv1 : {[(RelevQualS,RVarMp)]}
            local altQualSLSolv : {[RelevQualS]}
            local altQualSIntersect : {RelevQualS}
            local altQualSLSolv2 : _
            local altSolveLVarMp : {[RVarMp]}
            local dbg         : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local _tup13      : {(UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local litTy       : {RelevTy}
            local _tup14      : {(AMSOut RelevTy,AnaMatchState)}
            local amso        : {AMSOut RelevTy}
            local ams         : {AnaMatchState}
            local dbg         : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative CoeArg:
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local _tup15      : _
            local ffiTy       : {RelevTy}
            local ffiQualS    : _
            local _tup16      : {(AMSOut RelevTy,AnaMatchState)}
            local amso        : {AMSOut RelevTy}
            local ams         : {AnaMatchState}
            local dbg         : _
            local argTyLresTy : {( TyL, Ty )}
            local argTyL      : {TyL}
            local resTy       : _
            local foreignEntInfo : _
            local mbPrimNeedEval : {Maybe PrimitiveNeedsEval}
            local primArgNeedsEvalL : _
            local primResNeedsEval : {Bool}
            local argMbConL   : _
            local resMbCon    : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local _tup17      : {(UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local litTy       : {RelevTy}
            local _tup18      : {(AMSOut RelevTy,AnaMatchState)}
            local amso        : {AMSOut RelevTy}
            local ams         : {AnaMatchState}
            local dbg         : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local hereBodyCoe : _
            local forQuantRVarMp : _
            local lev         : _
            local isGlobal    : _
            local argNm       : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local fvS         : _
            local cTrf        : _
            local oTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local bindsCanParticpateInOuterSolving : {Bool}
            local bindsQualSSubs : _
            local bindsQualSOuter : _
            local bindsQualSSolv1 : _
            local bindsQualSSolv2 : _
            local bindsSolveVarMp : _
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local isTopLet    : _
            local evalCtx     : _
            local fvS         : _
            local cTrf        : _
            local oTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local _tup19      : _
            local tupTy       : {RelevTy}
            local tupQualS    : _
            local _tup20      : {(AMSOut RelevTy,AnaMatchState)}
            local amso        : {AMSOut RelevTy}
            local ams         : {AnaMatchState}
            local dbg         : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local _tup21      : {(UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local cTrf        : _
            local oTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local mbEnvTy     : _
            local _tup22      : {(RelevTy,RelevQualS)}
            local envTy       : {RelevTy}
            local envQualS    : {RelevQualS}
            local _tup23      : {(AMSOut RelevTy,AnaMatchState)}
            local amso        : {AMSOut RelevTy}
            local ams         : {AnaMatchState}
            local dbg         : _
            local nm          : {HsName}
            local nmAsp       : {HsName}
            local mbVar       : {Maybe HsName}
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local _tup24      : {(UID,UID,UID,UID)}
            local lUniq       : {UID}
            local lUniq2      : {UID}
            local lUniq3      : {UID}
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CExpr :: CExpr  ->
             T_CExpr 
sem_CExpr (CExpr_Ann _ann _expr )  =
    (sem_CExpr_Ann (sem_CExprAnn _ann ) (sem_CExpr _expr ) )
sem_CExpr (CExpr_App _func _arg )  =
    (sem_CExpr_App (sem_CExpr _func ) (sem_CBound _arg ) )
sem_CExpr (CExpr_Case _expr _alts _dflt )  =
    (sem_CExpr_Case (sem_CExpr _expr ) (sem_CAltL _alts ) (sem_CExpr _dflt ) )
sem_CExpr (CExpr_CaseAltFail _failReason _errorExpr )  =
    (sem_CExpr_CaseAltFail _failReason (sem_CExpr _errorExpr ) )
sem_CExpr (CExpr_Char _char )  =
    (sem_CExpr_Char _char )
sem_CExpr (CExpr_CoeArg )  =
    (sem_CExpr_CoeArg )
sem_CExpr (CExpr_FFI _callconv _safety _impEnt _ty )  =
    (sem_CExpr_FFI _callconv _safety _impEnt _ty )
sem_CExpr (CExpr_Hole _uid )  =
    (sem_CExpr_Hole _uid )
sem_CExpr (CExpr_HoleLet _bindsUid _body )  =
    (sem_CExpr_HoleLet _bindsUid (sem_CExpr _body ) )
sem_CExpr (CExpr_ImplsApp _func _uid )  =
    (sem_CExpr_ImplsApp (sem_CExpr _func ) _uid )
sem_CExpr (CExpr_ImplsLam _uid _body )  =
    (sem_CExpr_ImplsLam _uid (sem_CExpr _body ) )
sem_CExpr (CExpr_Int _int )  =
    (sem_CExpr_Int _int )
sem_CExpr (CExpr_Integer _integer )  =
    (sem_CExpr_Integer _integer )
sem_CExpr (CExpr_Lam _bind _body )  =
    (sem_CExpr_Lam (sem_CBind _bind ) (sem_CExpr _body ) )
sem_CExpr (CExpr_Let _categ _binds _body )  =
    (sem_CExpr_Let _categ (sem_CBindL _binds ) (sem_CExpr _body ) )
sem_CExpr (CExpr_String _str )  =
    (sem_CExpr_String _str )
sem_CExpr (CExpr_Tup _tag )  =
    (sem_CExpr_Tup _tag )
sem_CExpr (CExpr_TupDel _expr _tag _nm _offset )  =
    (sem_CExpr_TupDel (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) )
sem_CExpr (CExpr_TupIns _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupIns (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_TupUpd _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupUpd (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_Var _ref )  =
    (sem_CExpr_Var _ref )
-- semantic domain
type T_CExpr  = MbRelevTy ->
                UIDS ->
                DataGam ->
                REnv ->
                EvalCtx ->
                RVarMp ->
                UID ->
                Bool ->
                Bool ->
                Bool ->
                Bool ->
                RelevTy ->
                LamMp ->
                Int ->
                EHCOpts ->
                RVarMp ->
                WhatExpr ->
                ([WhatToRelevInfer]) ->
                ( AppFunKind,([CBound]),CExpr ,RelevCoe,RelevCoe,FvS,UID,LamMp,(Maybe ( Ty
                                                                                                                  , Bool
                                                                                                                  , FFIWay
                                                                                                                  , ForeignEnt
                                                                                                                  , [Ty]
                                                                                                                  )),(Maybe HsName),(Maybe [HsName]),(Maybe HsName),CExpr ,RelevQualS,RVarMp,RelevTy,WhatExpr)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOappFunKind :: AppFunKind
              _lhsOargL :: ([CBound])
              _lhsOfunCoe :: RelevCoe
              _lhsOgUniq :: UID
              _lhsOgathLamMp :: LamMp
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _lhsOrvarMp :: RVarMp
              _lhsOty :: RelevTy
              _lhsOwhatBelow :: WhatExpr
              _annOboundRelevTyVarS :: UIDS
              _annOdataGam :: DataGam
              _annOenv :: REnv
              _annOfinalRVarMp :: RVarMp
              _annOgUniq :: UID
              _annOknTy :: RelevTy
              _annOlamMp :: LamMp
              _annOlev :: Int
              _annOopts :: EHCOpts
              _annOrvarMp :: RVarMp
              _annOwhatTo :: ([WhatToRelevInfer])
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _annIcTrf :: CExprAnn 
              _annIfvS :: FvS
              _annIgUniq :: UID
              _annIoTrf :: CExprAnn 
              _annIqualS :: RelevQualS
              _annIrvarMp :: RVarMp
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _annIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _annIqualS `Set.union` _exprIqualS
              -- self rule
              _cTrf =
                  CExpr_Ann _annIcTrf _exprIcTrf
              -- self rule
              _oTrf =
                  CExpr_Ann _annIoTrf _exprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOappFunKind =
                  _exprIappFunKind
              -- copy rule (up)
              _lhsOargL =
                  _exprIargL
              -- copy rule (up)
              _lhsOfunCoe =
                  _exprIfunCoe
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (up)
              _lhsOmbFFIApp =
                  _exprImbFFIApp
              -- copy rule (up)
              _lhsOmbFunVar =
                  _exprImbFunVar
              -- copy rule (up)
              _lhsOmbLam =
                  _exprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _exprImbVar
              -- copy rule (up)
              _lhsOrvarMp =
                  _exprIrvarMp
              -- copy rule (up)
              _lhsOty =
                  _exprIty
              -- copy rule (up)
              _lhsOwhatBelow =
                  _exprIwhatBelow
              -- copy rule (down)
              _annOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _annOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _annOenv =
                  _lhsIenv
              -- copy rule (down)
              _annOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _annOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _annOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _annOlev =
                  _lhsIlev
              -- copy rule (down)
              _annOopts =
                  _lhsIopts
              -- copy rule (down)
              _annOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _annOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _exprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _exprOgUniq =
                  _annIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exprOrvarMp =
                  _annIrvarMp
              -- copy rule (down)
              _exprOwhatAbove =
                  _lhsIwhatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _annIcTrf,_annIfvS,_annIgUniq,_annIoTrf,_annIqualS,_annIrvarMp) =
                  ann_ _annOboundRelevTyVarS _annOdataGam _annOenv _annOfinalRVarMp _annOgUniq _annOknTy _annOlamMp _annOlev _annOopts _annOrvarMp _annOwhatTo 
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _argOknTy :: RelevTy
              _funcOknTy :: RelevTy
              _funTy :: RelevTy
              _argTy :: RelevTy
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _argOnm :: HsName
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _funcOisTopApp :: Bool
              _argOisTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              __tup12 :: ((UID,UID))
              _funcOgUniq :: UID
              _lUniq :: UID
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOoTrf :: CExpr 
              _lhsOargL :: ([CBound])
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _funcOaltMbScrutTy :: MbRelevTy
              _funcOboundRelevTyVarS :: UIDS
              _funcOdataGam :: DataGam
              _funcOenv :: REnv
              _funcOevalCtx :: EvalCtx
              _funcOfinalRVarMp :: RVarMp
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopTup :: Bool
              _funcOlamMp :: LamMp
              _funcOlev :: Int
              _funcOopts :: EHCOpts
              _funcOrvarMp :: RVarMp
              _funcOwhatAbove :: WhatExpr
              _funcOwhatTo :: ([WhatToRelevInfer])
              _argOboundRelevTyVarS :: UIDS
              _argOdataGam :: DataGam
              _argOenv :: REnv
              _argOevalCtx :: EvalCtx
              _argOfinalRVarMp :: RVarMp
              _argOforQuantRVarMp :: RVarMp
              _argOgUniq :: UID
              _argOisGlobal :: Bool
              _argOisLamBody :: Bool
              _argOisStrict :: Bool
              _argOisTopTup :: Bool
              _argOlamMp :: LamMp
              _argOletBindingsCateg :: CBindCateg
              _argOlev :: Int
              _argOopts :: EHCOpts
              _argOrvarMp :: RVarMp
              _argOwhatTo :: ([WhatToRelevInfer])
              _funcIappFunKind :: AppFunKind
              _funcIargL :: ([CBound])
              _funcIcTrf :: CExpr 
              _funcIcoe :: RelevCoe
              _funcIfunCoe :: RelevCoe
              _funcIfvS :: FvS
              _funcIgUniq :: UID
              _funcIgathLamMp :: LamMp
              _funcImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _funcImbFunVar :: (Maybe HsName)
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              _funcIoTrf :: CExpr 
              _funcIqualS :: RelevQualS
              _funcIrvarMp :: RVarMp
              _funcIty :: RelevTy
              _funcIwhatBelow :: WhatExpr
              _argIaltMbScrutTy :: MbRelevTy
              _argIbindLamMp :: LamMp
              _argIcTrf :: CBound 
              _argIcoe :: RelevCoe
              _argIextraBindRVarMp :: RVarMp
              _argIfvS :: FvS
              _argIfvSMp :: FvSMp
              _argIgUniq :: UID
              _argIgathEnv :: REnv
              _argIgathRecEnv :: REnv
              _argInmL :: ([HsName])
              _argIoTrf :: CBound 
              _argIqualS :: RelevQualS
              _argIrvarMp :: RVarMp
              _argItyAspectL :: ([CBound])
              _argIvalAspectL :: ([CBound])
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 207, column 17)
              _argOknTy =
                  _argTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 208, column 17)
              _funcOknTy =
                  case _lhsIknTy of
                    RelevTy_Fun q v qs as r -> RelevTy_Fun q           v  qs (fresh _lUniq : as) r
                    t                       -> RelevTy_Fun RQuant_None [] [] [fresh _lUniq     ] t
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 267, column 17)
              _funTy =
                  case _funcIty of
                    t@(RelevTy_Fun _ _ _ (_:_) _) -> t
                    _                             -> anaMkBotFun 1
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 270, column 33)
              __tup10 =
                  case _funTy of
                    RelevTy_Fun q v qs (a:as) r -> (a,r,RelevTy_Fun q v qs as r)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 270, column 33)
              (_argTy,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 270, column 33)
              (_,_resTy,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 270, column 33)
              (_,_,_resFunTy) =
                  __tup10
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 317, column 17)
              _lhsOcoe =
                  if _lhsIisTopApp
                  then _lhsIfinalRVarMp `varUpd` _resCoe
                  else RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 324, column 17)
              _hereArgCoe =
                  _argIcoe <.> (_lhsIfinalRVarMp `varUpd` _argCoe)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 324, column 17)
              _hereFunCoe =
                  case _funcIwhatBelow of
                    ExprIsApp _ -> RelevCoe_Id
                    _           -> _funcIcoe
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 348, column 17)
              __tup11 =
                  case _funcIfunCoe of
                    RelevCoe_Fun (a:as) r -> (a,r,RelevCoe_Fun as r)
                    _                     -> (RelevCoe_Err "CExpr.App.a", RelevCoe_Err "CExpr.App.r", RelevCoe_Err "CExpr.App.f")
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 348, column 17)
              (_argCoe,_,_) =
                  __tup11
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 348, column 17)
              (_,_resCoe,_) =
                  __tup11
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 348, column 17)
              (_,_,_lhsOfunCoe) =
                  __tup11
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 360, column 17)
              _lhsOty =
                  if _lhsIisTopApp then _resTy else _resFunTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 390, column 17)
              _forQuantRVarMp =
                  emptyRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 612, column 17)
              _lhsOcTrf =
                  let a =
                          maybe _argIcTrf (\(a,e) -> acoreBound1AspkeyVal a (annCoe _hereArgCoe e)) $ acoreBoundMbVal _argIcTrf
                      app = acoreApp1Bound (annCoe _hereFunCoe _funcIcTrf) a
                  in  app
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 41, column 17)
              _lhsOmbFFIApp =
                  _funcImbFFIApp
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 7, column 17)
              _argOnm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 6, column 17)
              _lhsOmbFunVar =
                  _funcImbFunVar
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 13, column 17)
              _argL =
                  _argIcTrf : _funcIargL
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 14, column 17)
              _lhsOappFunKind =
                  _funcIappFunKind
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 13, column 17)
              _funcOisTopApp =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 14, column 17)
              _argOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 56, column 17)
              _whatBelow =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 61, column 17)
              _isTopApp' =
                  isNothing $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 67, column 17)
              _whatAbove =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 8, column 17)
              _fvS =
                  _funcIfvS `Set.union` _argIfvS
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- -- generated by the unique rule mechanism.
              __tup12 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_funcOgUniq,_) =
                  __tup12
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup12
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _funcIqualS `Set.union` _argIqualS
              -- self rule
              _cTrf =
                  CExpr_App _funcIcTrf _argIcTrf
              -- self rule
              _oTrf =
                  CExpr_App _funcIoTrf _argIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOargL =
                  _argL
              -- copy rule (up)
              _lhsOgUniq =
                  _argIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _argIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _funcOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _funcOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _funcOenv =
                  _lhsIenv
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              -- copy rule (down)
              _funcOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _funcOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _argOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _argOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _argOenv =
                  _lhsIenv
              -- copy rule (down)
              _argOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _argOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (from local)
              _argOforQuantRVarMp =
                  _forQuantRVarMp
              -- copy rule (chain)
              _argOgUniq =
                  _funcIgUniq
              -- copy rule (from local)
              _argOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _argOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _argOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _argOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _argOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _argOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _argOlev =
                  _lhsIlev
              -- copy rule (down)
              _argOopts =
                  _lhsIopts
              -- copy rule (chain)
              _argOrvarMp =
                  _funcIrvarMp
              -- copy rule (down)
              _argOwhatTo =
                  _lhsIwhatTo
              ( _funcIappFunKind,_funcIargL,_funcIcTrf,_funcIcoe,_funcIfunCoe,_funcIfvS,_funcIgUniq,_funcIgathLamMp,_funcImbFFIApp,_funcImbFunVar,_funcImbLam,_funcImbVar,_funcIoTrf,_funcIqualS,_funcIrvarMp,_funcIty,_funcIwhatBelow) =
                  func_ _funcOaltMbScrutTy _funcOboundRelevTyVarS _funcOdataGam _funcOenv _funcOevalCtx _funcOfinalRVarMp _funcOgUniq _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOknTy _funcOlamMp _funcOlev _funcOopts _funcOrvarMp _funcOwhatAbove _funcOwhatTo 
              ( _argIaltMbScrutTy,_argIbindLamMp,_argIcTrf,_argIcoe,_argIextraBindRVarMp,_argIfvS,_argIfvSMp,_argIgUniq,_argIgathEnv,_argIgathRecEnv,_argInmL,_argIoTrf,_argIqualS,_argIrvarMp,_argItyAspectL,_argIvalAspectL) =
                  arg_ _argOboundRelevTyVarS _argOdataGam _argOenv _argOevalCtx _argOfinalRVarMp _argOforQuantRVarMp _argOgUniq _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOknTy _argOlamMp _argOletBindingsCateg _argOlev _argOnm _argOopts _argOrvarMp _argOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _exprOknTy :: RelevTy
              _altsOknTyCase :: RelevTy
              _altsOaltNrMax :: Int
              _altsOaltId :: UID
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _altQualSLSubs :: ([RelevQualS])
              _altQualSLSolv1 :: ([(RelevQualS,RVarMp)])
              _altQualSLSolv :: ([RelevQualS])
              _altQualSIntersect :: RelevQualS
              _altSolveLVarMp :: ([RVarMp])
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              __tup13 :: ((UID,UID,UID))
              _exprOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lhsOfvS :: FvS
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _altsOaltMbScrutTy :: MbRelevTy
              _altsOaltSolveLVarMp :: ([RVarMp])
              _altsOboundRelevTyVarS :: UIDS
              _altsOdataGam :: DataGam
              _altsOenv :: REnv
              _altsOevalCtx :: EvalCtx
              _altsOfinalRVarMp :: RVarMp
              _altsOgUniq :: UID
              _altsOisLamBody :: Bool
              _altsOisStrict :: Bool
              _altsOknTy :: RelevTy
              _altsOlamMp :: LamMp
              _altsOlev :: Int
              _altsOopts :: EHCOpts
              _altsOrvarMp :: RVarMp
              _altsOwhatTo :: ([WhatToRelevInfer])
              _dfltOaltMbScrutTy :: MbRelevTy
              _dfltOboundRelevTyVarS :: UIDS
              _dfltOdataGam :: DataGam
              _dfltOenv :: REnv
              _dfltOevalCtx :: EvalCtx
              _dfltOfinalRVarMp :: RVarMp
              _dfltOgUniq :: UID
              _dfltOisLamBody :: Bool
              _dfltOisStrict :: Bool
              _dfltOisTopApp :: Bool
              _dfltOisTopTup :: Bool
              _dfltOknTy :: RelevTy
              _dfltOlamMp :: LamMp
              _dfltOlev :: Int
              _dfltOopts :: EHCOpts
              _dfltOrvarMp :: RVarMp
              _dfltOwhatAbove :: WhatExpr
              _dfltOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              _altsIaltNr :: Int
              _altsIaltQualSL :: ([RelevQualS])
              _altsIcTrf :: CAltL 
              _altsIfvS :: FvS
              _altsIgUniq :: UID
              _altsIoTrf :: CAltL 
              _altsIqualS :: RelevQualS
              _altsIrvarMp :: RVarMp
              _altsIty :: RelevTy
              _dfltIappFunKind :: AppFunKind
              _dfltIargL :: ([CBound])
              _dfltIcTrf :: CExpr 
              _dfltIcoe :: RelevCoe
              _dfltIfunCoe :: RelevCoe
              _dfltIfvS :: FvS
              _dfltIgUniq :: UID
              _dfltIgathLamMp :: LamMp
              _dfltImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _dfltImbFunVar :: (Maybe HsName)
              _dfltImbLam :: (Maybe [HsName])
              _dfltImbVar :: (Maybe HsName)
              _dfltIoTrf :: CExpr 
              _dfltIqualS :: RelevQualS
              _dfltIrvarMp :: RVarMp
              _dfltIty :: RelevTy
              _dfltIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 212, column 17)
              _exprOknTy =
                  _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 222, column 17)
              _altsOknTyCase =
                  _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 290, column 17)
              _altsOaltNrMax =
                  _altsIaltNr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 290, column 17)
              _altsOaltId =
                  _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 361, column 17)
              _lhsOty =
                  _altsIty
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 380, column 17)
              _lhsOrvarMp =
                  _altsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 421, column 17)
              _lhsOqualS =
                  Set.unions [_exprIqualS, _altQualSIntersect]
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 433, column 17)
              _altQualSLSubs =
                  map (Set.map (_altsIrvarMp `varUpd`)) _altsIaltQualSL
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 433, column 17)
              _altQualSLSolv1 =
                  let bnd = _lhsIboundRelevTyVarS
                      s q = (Set.map (m `varUpd`) q', m)
                          where (q',m) = assSolve bnd q
                  in  map s _altQualSLSubs
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 433, column 17)
              _altQualSLSolv =
                  map fst _altQualSLSolv1
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 433, column 17)
              _altQualSIntersect =
                  foldr1 Set.intersection _altQualSLSolv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 448, column 17)
              _altQualSLSolv2 =
                  let s (q,m) = (Set.map (m3 `varUpd`) q', m4)
                              where m2 = m |+> _lhsIfinalRVarMp
                                    (q',m3) = assSolve Set.empty (Set.map (m2 `varUpd`) q)
                                    m4 = m3 |+> m2
                  in  map s _altQualSLSolv1
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 448, column 17)
              _altSolveLVarMp =
                  map snd _altQualSLSolv2
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 594, column 17)
              _dbg =
                  dbgCase _lhsIopts _lhsIboundRelevTyVarS _altQualSLSubs _altQualSLSolv _altQualSIntersect
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 616, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- -- generated by the unique rule mechanism.
              __tup13 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )
              -- -- generated by the unique rule mechanism.
              (_exprOgUniq,_,_) =
                  __tup13
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_) =
                  __tup13
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2) =
                  __tup13
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS
              -- self rule
              _cTrf =
                  CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf
              -- self rule
              _oTrf =
                  CExpr_Case _exprIoTrf _altsIoTrf _dfltIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _dfltIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _exprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _altsOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (from local)
              _altsOaltSolveLVarMp =
                  _altSolveLVarMp
              -- copy rule (down)
              _altsOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _altsOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _altsOenv =
                  _lhsIenv
              -- copy rule (down)
              _altsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _altsOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _altsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _altsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _altsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _altsOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _altsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _altsOlev =
                  _lhsIlev
              -- copy rule (down)
              _altsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _altsOrvarMp =
                  _exprIrvarMp
              -- copy rule (down)
              _altsOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _dfltOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _dfltOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _dfltOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _dfltOenv =
                  _lhsIenv
              -- copy rule (down)
              _dfltOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _dfltOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _dfltOgUniq =
                  _altsIgUniq
              -- copy rule (down)
              _dfltOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _dfltOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _dfltOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _dfltOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _dfltOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _dfltOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _dfltOlev =
                  _lhsIlev
              -- copy rule (down)
              _dfltOopts =
                  _lhsIopts
              -- copy rule (chain)
              _dfltOrvarMp =
                  _altsIrvarMp
              -- copy rule (from local)
              _dfltOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _dfltOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
              ( _altsIaltNr,_altsIaltQualSL,_altsIcTrf,_altsIfvS,_altsIgUniq,_altsIoTrf,_altsIqualS,_altsIrvarMp,_altsIty) =
                  alts_ _altsOaltId _altsOaltMbScrutTy _altsOaltNrMax _altsOaltSolveLVarMp _altsOboundRelevTyVarS _altsOdataGam _altsOenv _altsOevalCtx _altsOfinalRVarMp _altsOgUniq _altsOisLamBody _altsOisStrict _altsOknTy _altsOknTyCase _altsOlamMp _altsOlev _altsOopts _altsOrvarMp _altsOwhatTo 
              ( _dfltIappFunKind,_dfltIargL,_dfltIcTrf,_dfltIcoe,_dfltIfunCoe,_dfltIfvS,_dfltIgUniq,_dfltIgathLamMp,_dfltImbFFIApp,_dfltImbFunVar,_dfltImbLam,_dfltImbVar,_dfltIoTrf,_dfltIqualS,_dfltIrvarMp,_dfltIty,_dfltIwhatBelow) =
                  dflt_ _dfltOaltMbScrutTy _dfltOboundRelevTyVarS _dfltOdataGam _dfltOenv _dfltOevalCtx _dfltOfinalRVarMp _dfltOgUniq _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOknTy _dfltOlamMp _dfltOlev _dfltOopts _dfltOrvarMp _dfltOwhatAbove _dfltOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOappFunKind :: AppFunKind
              _lhsOargL :: ([CBound])
              _lhsOfunCoe :: RelevCoe
              _lhsOgUniq :: UID
              _lhsOgathLamMp :: LamMp
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _lhsOrvarMp :: RVarMp
              _lhsOty :: RelevTy
              _lhsOwhatBelow :: WhatExpr
              _errorExprOaltMbScrutTy :: MbRelevTy
              _errorExprOboundRelevTyVarS :: UIDS
              _errorExprOdataGam :: DataGam
              _errorExprOenv :: REnv
              _errorExprOevalCtx :: EvalCtx
              _errorExprOfinalRVarMp :: RVarMp
              _errorExprOgUniq :: UID
              _errorExprOisLamBody :: Bool
              _errorExprOisStrict :: Bool
              _errorExprOisTopApp :: Bool
              _errorExprOisTopTup :: Bool
              _errorExprOknTy :: RelevTy
              _errorExprOlamMp :: LamMp
              _errorExprOlev :: Int
              _errorExprOopts :: EHCOpts
              _errorExprOrvarMp :: RVarMp
              _errorExprOwhatAbove :: WhatExpr
              _errorExprOwhatTo :: ([WhatToRelevInfer])
              _errorExprIappFunKind :: AppFunKind
              _errorExprIargL :: ([CBound])
              _errorExprIcTrf :: CExpr 
              _errorExprIcoe :: RelevCoe
              _errorExprIfunCoe :: RelevCoe
              _errorExprIfvS :: FvS
              _errorExprIgUniq :: UID
              _errorExprIgathLamMp :: LamMp
              _errorExprImbFFIApp :: (Maybe ( Ty
                                                                        , Bool
                                                                        , FFIWay
                                                                        , ForeignEnt
                                                                        , [Ty]
                                                                        ))
              _errorExprImbFunVar :: (Maybe HsName)
              _errorExprImbLam :: (Maybe [HsName])
              _errorExprImbVar :: (Maybe HsName)
              _errorExprIoTrf :: CExpr 
              _errorExprIqualS :: RelevQualS
              _errorExprIrvarMp :: RVarMp
              _errorExprIty :: RelevTy
              _errorExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _errorExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _errorExprIqualS
              -- self rule
              _cTrf =
                  CExpr_CaseAltFail failReason_ _errorExprIcTrf
              -- self rule
              _oTrf =
                  CExpr_CaseAltFail failReason_ _errorExprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOappFunKind =
                  _errorExprIappFunKind
              -- copy rule (up)
              _lhsOargL =
                  _errorExprIargL
              -- copy rule (up)
              _lhsOfunCoe =
                  _errorExprIfunCoe
              -- copy rule (up)
              _lhsOgUniq =
                  _errorExprIgUniq
              -- copy rule (up)
              _lhsOgathLamMp =
                  _errorExprIgathLamMp
              -- copy rule (up)
              _lhsOmbFunVar =
                  _errorExprImbFunVar
              -- copy rule (up)
              _lhsOmbLam =
                  _errorExprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _errorExprImbVar
              -- copy rule (up)
              _lhsOrvarMp =
                  _errorExprIrvarMp
              -- copy rule (up)
              _lhsOty =
                  _errorExprIty
              -- copy rule (up)
              _lhsOwhatBelow =
                  _errorExprIwhatBelow
              -- copy rule (down)
              _errorExprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _errorExprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _errorExprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _errorExprOenv =
                  _lhsIenv
              -- copy rule (down)
              _errorExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _errorExprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _errorExprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _errorExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _errorExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _errorExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _errorExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _errorExprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _errorExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _errorExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _errorExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _errorExprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _errorExprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _errorExprOwhatTo =
                  _lhsIwhatTo
              ( _errorExprIappFunKind,_errorExprIargL,_errorExprIcTrf,_errorExprIcoe,_errorExprIfunCoe,_errorExprIfvS,_errorExprIgUniq,_errorExprIgathLamMp,_errorExprImbFFIApp,_errorExprImbFunVar,_errorExprImbLam,_errorExprImbVar,_errorExprIoTrf,_errorExprIqualS,_errorExprIrvarMp,_errorExprIty,_errorExprIwhatBelow) =
                  errorExpr_ _errorExprOaltMbScrutTy _errorExprOboundRelevTyVarS _errorExprOdataGam _errorExprOenv _errorExprOevalCtx _errorExprOfinalRVarMp _errorExprOgUniq _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOknTy _errorExprOlamMp _errorExprOlev _errorExprOopts _errorExprOrvarMp _errorExprOwhatAbove _errorExprOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _litTy :: RelevTy
              __tup14 :: ((AMSOut RelevTy,AnaMatchState))
              _amso :: (AMSOut RelevTy)
              _ams :: AnaMatchState
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 228, column 17)
              _litTy =
                  bot
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              __tup14 =
                  amsLE _lhsIrvarMp _litTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              (_amso,_) =
                  __tup14
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              (_,_ams) =
                  __tup14
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 316, column 17)
              _lhsOcoe =
                  _lhsIfinalRVarMp `varUpd` amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 347, column 17)
              _lhsOfunCoe =
                  last $ relevCoeToComposeList $ amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 359, column 17)
              _lhsOty =
                  amsoHi _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 379, column 17)
              _lhsOrvarMp =
                  amsLocalVarMp _ams |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 420, column 17)
              _lhsOqualS =
                  amsGathQual _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 591, column 17)
              _dbg =
                  dbg     _lhsIopts _litTy _lhsIknTy _amso _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 609, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_Char char_
              -- self rule
              _oTrf =
                  CExpr_Char char_
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_CoeArg
              -- self rule
              _oTrf =
                  CExpr_CoeArg
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _ffiTy :: RelevTy
              __tup16 :: ((AMSOut RelevTy,AnaMatchState))
              _amso :: (AMSOut RelevTy)
              _ams :: AnaMatchState
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _argTyLresTy :: (( TyL, Ty ))
              _argTyL :: TyL
              _mbPrimNeedEval :: (Maybe PrimitiveNeedsEval)
              _primResNeedsEval :: Bool
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              __tup17 :: ((UID,UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lhsOfvS :: FvS
              _lhsOoTrf :: CExpr 
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 260, column 33)
              __tup15 =
                  let (r@(RelevTy_Ana re) ,qr) = (fresh _lUniq, [])
                      (as,qa) = relevTyArgs (const fresh) (\(RelevTy_Ana x) (RelevTy_Ana y) -> [RelevQual_SubEval x y]) _lUniq2 (take (length _argTyL) _primArgNeedsEvalL) r
                  in  (RelevTy_Fun RQuant_None [] [] as r, Set.fromList $ qr ++ qa)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 260, column 33)
              (_ffiTy,_) =
                  __tup15
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 260, column 33)
              (_,_ffiQualS) =
                  __tup15
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 263, column 33)
              __tup16 =
                  amsLE _lhsIrvarMp _ffiTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 263, column 33)
              (_amso,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 263, column 33)
              (_,_ams) =
                  __tup16
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 316, column 17)
              _lhsOcoe =
                  _lhsIfinalRVarMp `varUpd` amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 347, column 17)
              _lhsOfunCoe =
                  last $ relevCoeToComposeList $ amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 359, column 17)
              _lhsOty =
                  amsoHi _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 379, column 17)
              _lhsOrvarMp =
                  amsLocalVarMp _ams |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 418, column 17)
              _lhsOqualS =
                  Set.union _ffiQualS (amsGathQual _ams)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 593, column 17)
              _dbg =
                  dbg     _lhsIopts _ffiTy _lhsIknTy _amso _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 609, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 2, column 17)
              _argTyLresTy =
                  tyArrowArgsRes ty_
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 2, column 17)
              _argTyL =
                  fst _argTyLresTy
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 2, column 17)
              _resTy =
                  snd _argTyLresTy
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 2, column 17)
              _foreignEntInfo =
                  foreignEntExtract impEnt_
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 6, column 17)
              _mbPrimNeedEval =
                  maybe Nothing lookupPrimNeedsEval $ forextractMbEnt _foreignEntInfo
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 6, column 17)
              _primArgNeedsEvalL =
                  maybe (repeat True) (\p -> primArgNeedEval p ++ repeat True) _mbPrimNeedEval
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 6, column 17)
              _primResNeedsEval =
                  maybe False primResNeedEval _mbPrimNeedEval
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 11, column 17)
              _argMbConL =
                  map tyAppFunMbConNm _argTyL
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 11, column 17)
              _resMbCon =
                  tyAppFunMbConNm _resTy
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 35, column 17)
              _lhsOmbFFIApp =
                  Just ( _resTy
                       , _primResNeedsEval
                       , callconv_
                       , impEnt_
                       , _argTyL
                       )
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 12, column 17)
              _lhsOappFunKind =
                  AppFunKind_FFI
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- -- generated by the unique rule mechanism.
              __tup17 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_,_) =
                  __tup17
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_) =
                  __tup17
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2) =
                  __tup17
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_FFI callconv_ safety_ impEnt_ ty_
              -- self rule
              _oTrf =
                  CExpr_FFI callconv_ safety_ impEnt_ ty_
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_Hole uid_
              -- self rule
              _oTrf =
                  CExpr_Hole uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _bodyOaltMbScrutTy :: MbRelevTy
              _bodyOboundRelevTyVarS :: UIDS
              _bodyOdataGam :: DataGam
              _bodyOenv :: REnv
              _bodyOevalCtx :: EvalCtx
              _bodyOfinalRVarMp :: RVarMp
              _bodyOgUniq :: UID
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOknTy :: RelevTy
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOopts :: EHCOpts
              _bodyOrvarMp :: RVarMp
              _bodyOwhatAbove :: WhatExpr
              _bodyOwhatTo :: ([WhatToRelevInfer])
              _bodyIappFunKind :: AppFunKind
              _bodyIargL :: ([CBound])
              _bodyIcTrf :: CExpr 
              _bodyIcoe :: RelevCoe
              _bodyIfunCoe :: RelevCoe
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIgathLamMp :: LamMp
              _bodyImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _bodyImbFunVar :: (Maybe HsName)
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIoTrf :: CExpr 
              _bodyIqualS :: RelevQualS
              _bodyIrvarMp :: RVarMp
              _bodyIty :: RelevTy
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _bodyIqualS
              -- self rule
              _cTrf =
                  CExpr_HoleLet bindsUid_ _bodyIcTrf
              -- self rule
              _oTrf =
                  CExpr_HoleLet bindsUid_ _bodyIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _bodyIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _bodyOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bodyOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _bodyOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bodyOwhatTo =
                  _lhsIwhatTo
              ( _bodyIappFunKind,_bodyIargL,_bodyIcTrf,_bodyIcoe,_bodyIfunCoe,_bodyIfvS,_bodyIgUniq,_bodyIgathLamMp,_bodyImbFFIApp,_bodyImbFunVar,_bodyImbLam,_bodyImbVar,_bodyIoTrf,_bodyIqualS,_bodyIrvarMp,_bodyIty,_bodyIwhatBelow) =
                  body_ _bodyOaltMbScrutTy _bodyOboundRelevTyVarS _bodyOdataGam _bodyOenv _bodyOevalCtx _bodyOfinalRVarMp _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOknTy _bodyOlamMp _bodyOlev _bodyOopts _bodyOrvarMp _bodyOwhatAbove _bodyOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _funcOaltMbScrutTy :: MbRelevTy
              _funcOboundRelevTyVarS :: UIDS
              _funcOdataGam :: DataGam
              _funcOenv :: REnv
              _funcOevalCtx :: EvalCtx
              _funcOfinalRVarMp :: RVarMp
              _funcOgUniq :: UID
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopApp :: Bool
              _funcOisTopTup :: Bool
              _funcOknTy :: RelevTy
              _funcOlamMp :: LamMp
              _funcOlev :: Int
              _funcOopts :: EHCOpts
              _funcOrvarMp :: RVarMp
              _funcOwhatAbove :: WhatExpr
              _funcOwhatTo :: ([WhatToRelevInfer])
              _funcIappFunKind :: AppFunKind
              _funcIargL :: ([CBound])
              _funcIcTrf :: CExpr 
              _funcIcoe :: RelevCoe
              _funcIfunCoe :: RelevCoe
              _funcIfvS :: FvS
              _funcIgUniq :: UID
              _funcIgathLamMp :: LamMp
              _funcImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _funcImbFunVar :: (Maybe HsName)
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              _funcIoTrf :: CExpr 
              _funcIqualS :: RelevQualS
              _funcIrvarMp :: RVarMp
              _funcIty :: RelevTy
              _funcIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _funcIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _funcIqualS
              -- self rule
              _cTrf =
                  CExpr_ImplsApp _funcIcTrf uid_
              -- self rule
              _oTrf =
                  CExpr_ImplsApp _funcIoTrf uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _funcIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _funcIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _funcOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _funcOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _funcOenv =
                  _lhsIenv
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _funcOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              -- copy rule (down)
              _funcOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _funcOwhatTo =
                  _lhsIwhatTo
              ( _funcIappFunKind,_funcIargL,_funcIcTrf,_funcIcoe,_funcIfunCoe,_funcIfvS,_funcIgUniq,_funcIgathLamMp,_funcImbFFIApp,_funcImbFunVar,_funcImbLam,_funcImbVar,_funcIoTrf,_funcIqualS,_funcIrvarMp,_funcIty,_funcIwhatBelow) =
                  func_ _funcOaltMbScrutTy _funcOboundRelevTyVarS _funcOdataGam _funcOenv _funcOevalCtx _funcOfinalRVarMp _funcOgUniq _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOknTy _funcOlamMp _funcOlev _funcOopts _funcOrvarMp _funcOwhatAbove _funcOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _bodyOaltMbScrutTy :: MbRelevTy
              _bodyOboundRelevTyVarS :: UIDS
              _bodyOdataGam :: DataGam
              _bodyOenv :: REnv
              _bodyOevalCtx :: EvalCtx
              _bodyOfinalRVarMp :: RVarMp
              _bodyOgUniq :: UID
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOknTy :: RelevTy
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOopts :: EHCOpts
              _bodyOrvarMp :: RVarMp
              _bodyOwhatAbove :: WhatExpr
              _bodyOwhatTo :: ([WhatToRelevInfer])
              _bodyIappFunKind :: AppFunKind
              _bodyIargL :: ([CBound])
              _bodyIcTrf :: CExpr 
              _bodyIcoe :: RelevCoe
              _bodyIfunCoe :: RelevCoe
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIgathLamMp :: LamMp
              _bodyImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _bodyImbFunVar :: (Maybe HsName)
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIoTrf :: CExpr 
              _bodyIqualS :: RelevQualS
              _bodyIrvarMp :: RVarMp
              _bodyIty :: RelevTy
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _bodyIqualS
              -- self rule
              _cTrf =
                  CExpr_ImplsLam uid_ _bodyIcTrf
              -- self rule
              _oTrf =
                  CExpr_ImplsLam uid_ _bodyIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _bodyIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _bodyOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bodyOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _bodyOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bodyOwhatTo =
                  _lhsIwhatTo
              ( _bodyIappFunKind,_bodyIargL,_bodyIcTrf,_bodyIcoe,_bodyIfunCoe,_bodyIfvS,_bodyIgUniq,_bodyIgathLamMp,_bodyImbFFIApp,_bodyImbFunVar,_bodyImbLam,_bodyImbVar,_bodyIoTrf,_bodyIqualS,_bodyIrvarMp,_bodyIty,_bodyIwhatBelow) =
                  body_ _bodyOaltMbScrutTy _bodyOboundRelevTyVarS _bodyOdataGam _bodyOenv _bodyOevalCtx _bodyOfinalRVarMp _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOknTy _bodyOlamMp _bodyOlev _bodyOopts _bodyOrvarMp _bodyOwhatAbove _bodyOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _litTy :: RelevTy
              __tup18 :: ((AMSOut RelevTy,AnaMatchState))
              _amso :: (AMSOut RelevTy)
              _ams :: AnaMatchState
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 228, column 17)
              _litTy =
                  bot
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              __tup18 =
                  amsLE _lhsIrvarMp _litTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              (_amso,_) =
                  __tup18
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 229, column 33)
              (_,_ams) =
                  __tup18
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 316, column 17)
              _lhsOcoe =
                  _lhsIfinalRVarMp `varUpd` amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 347, column 17)
              _lhsOfunCoe =
                  last $ relevCoeToComposeList $ amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 359, column 17)
              _lhsOty =
                  amsoHi _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 379, column 17)
              _lhsOrvarMp =
                  amsLocalVarMp _ams |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 420, column 17)
              _lhsOqualS =
                  amsGathQual _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 591, column 17)
              _dbg =
                  dbg     _lhsIopts _litTy _lhsIknTy _amso _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 609, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 55, column 17)
              _whatBelow =
                  ExprIsInt int_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_Int int_
              -- self rule
              _oTrf =
                  CExpr_Int int_
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_Integer integer_
              -- self rule
              _oTrf =
                  CExpr_Integer integer_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bodyOlamMp :: LamMp
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _bindOboundRelevTyVarS :: UIDS
              _bindOdataGam :: DataGam
              _bindOenv :: REnv
              _bindOevalCtx :: EvalCtx
              _bindOfinalRVarMp :: RVarMp
              _bindOforQuantRVarMp :: RVarMp
              _bindOgUniq :: UID
              _bindOisGlobal :: Bool
              _bindOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOknTy :: RelevTy
              _bindOlamMp :: LamMp
              _bindOletBindingsCateg :: CBindCateg
              _bindOlev :: Int
              _bindOopts :: EHCOpts
              _bindOrvarMp :: RVarMp
              _bindOwhatTo :: ([WhatToRelevInfer])
              _bodyOaltMbScrutTy :: MbRelevTy
              _bodyOboundRelevTyVarS :: UIDS
              _bodyOdataGam :: DataGam
              _bodyOenv :: REnv
              _bodyOevalCtx :: EvalCtx
              _bodyOfinalRVarMp :: RVarMp
              _bodyOgUniq :: UID
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOknTy :: RelevTy
              _bodyOlev :: Int
              _bodyOopts :: EHCOpts
              _bodyOrvarMp :: RVarMp
              _bodyOwhatAbove :: WhatExpr
              _bodyOwhatTo :: ([WhatToRelevInfer])
              _bindIaltMbScrutTy :: MbRelevTy
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindIextraBindRVarMp :: RVarMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: UID
              _bindIgathEnv :: REnv
              _bindIgathRecEnv :: REnv
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _bindIoTrf :: CBind 
              _bindIqualS :: RelevQualS
              _bindIrvarMp :: RVarMp
              _bodyIappFunKind :: AppFunKind
              _bodyIargL :: ([CBound])
              _bodyIcTrf :: CExpr 
              _bodyIcoe :: RelevCoe
              _bodyIfunCoe :: RelevCoe
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIgathLamMp :: LamMp
              _bodyImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _bodyImbFunVar :: (Maybe HsName)
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIoTrf :: CExpr 
              _bodyIqualS :: RelevQualS
              _bodyIrvarMp :: RVarMp
              _bodyIty :: RelevTy
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 328, column 17)
              _hereBodyCoe =
                  case _bodyIwhatBelow of
                    ExprIsLam -> RelevCoe_Id
                    _         -> _bodyIcoe
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 390, column 17)
              _forQuantRVarMp =
                  emptyRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 611, column 17)
              _lhsOcTrf =
                  acoreLam1 _argNm $ annCoe _hereBodyCoe _bodyIcTrf
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 7, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 19, column 25)
              _argNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 4, column 17)
              _lhsOmbLam =
                  Just $ maybe [_argNm] (_argNm:) _bodyImbLam
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 53, column 17)
              _whatBelow =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 66, column 17)
              _whatAbove =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 5, column 17)
              _fvS =
                  _argNm `Set.delete` _bodyIfvS
              -- "build/101/lib-ehc/EH101/Core/CommonLamInfo.ag"(line 7, column 17)
              _bodyOlamMp =
                  Map.delete _argNm _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _bindIqualS `Set.union` _bodyIqualS
              -- self rule
              _cTrf =
                  CExpr_Lam _bindIcTrf _bodyIcTrf
              -- self rule
              _oTrf =
                  CExpr_Lam _bindIoTrf _bodyIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _bodyIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bindOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bindOenv =
                  _lhsIenv
              -- copy rule (down)
              _bindOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bindOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (from local)
              _bindOforQuantRVarMp =
                  _forQuantRVarMp
              -- copy rule (down)
              _bindOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _bindOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _bindOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _bindOlev =
                  _lev
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _bindOwhatTo =
                  _lhsIwhatTo
              -- copy rule (chain)
              _bodyOaltMbScrutTy =
                  _bindIaltMbScrutTy
              -- copy rule (down)
              _bodyOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bodyOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _bodyOgUniq =
                  _bindIgUniq
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOknTy =
                  _lhsIknTy
              -- copy rule (from local)
              _bodyOlev =
                  _lev
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bodyOrvarMp =
                  _bindIrvarMp
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bodyOwhatTo =
                  _lhsIwhatTo
              ( _bindIaltMbScrutTy,_bindIbindLamMp,_bindIcTrf,_bindIextraBindRVarMp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIgathEnv,_bindIgathRecEnv,_bindInm,_bindInmL,_bindIoTrf,_bindIqualS,_bindIrvarMp) =
                  bind_ _bindOboundRelevTyVarS _bindOdataGam _bindOenv _bindOevalCtx _bindOfinalRVarMp _bindOforQuantRVarMp _bindOgUniq _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOknTy _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOopts _bindOrvarMp _bindOwhatTo 
              ( _bodyIappFunKind,_bodyIargL,_bodyIcTrf,_bodyIcoe,_bodyIfunCoe,_bodyIfvS,_bodyIgUniq,_bodyIgathLamMp,_bodyImbFFIApp,_bodyImbFunVar,_bodyImbLam,_bodyImbVar,_bodyIoTrf,_bodyIqualS,_bodyIrvarMp,_bodyIty,_bodyIwhatBelow) =
                  body_ _bodyOaltMbScrutTy _bodyOboundRelevTyVarS _bodyOdataGam _bodyOenv _bodyOevalCtx _bodyOfinalRVarMp _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOknTy _bodyOlamMp _bodyOlev _bodyOopts _bodyOrvarMp _bodyOwhatAbove _bodyOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _bindsOenv :: REnv
              _bodyOenv :: REnv
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _bodyOrvarMp :: RVarMp
              _bindsOforQuantRVarMp :: RVarMp
              _bindsCanParticpateInOuterSolving :: Bool
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bindsOisStrict :: Bool
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOcoe :: RelevCoe
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _bindsOboundRelevTyVarS :: UIDS
              _bindsOdataGam :: DataGam
              _bindsOevalCtx :: EvalCtx
              _bindsOfinalRVarMp :: RVarMp
              _bindsOgUniq :: UID
              _bindsOisGlobal :: Bool
              _bindsOisLamBody :: Bool
              _bindsOknTy :: RelevTy
              _bindsOlamMp :: LamMp
              _bindsOletBindingsCateg :: CBindCateg
              _bindsOlev :: Int
              _bindsOopts :: EHCOpts
              _bindsOrvarMp :: RVarMp
              _bindsOwhatTo :: ([WhatToRelevInfer])
              _bodyOaltMbScrutTy :: MbRelevTy
              _bodyOboundRelevTyVarS :: UIDS
              _bodyOdataGam :: DataGam
              _bodyOevalCtx :: EvalCtx
              _bodyOfinalRVarMp :: RVarMp
              _bodyOgUniq :: UID
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOknTy :: RelevTy
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOopts :: EHCOpts
              _bodyOwhatAbove :: WhatExpr
              _bodyOwhatTo :: ([WhatToRelevInfer])
              _bindsIaltMbScrutTy :: MbRelevTy
              _bindsIbindLamMp :: LamMp
              _bindsIcTrf :: CBindL 
              _bindsIextraBindRVarMp :: RVarMp
              _bindsIfvS :: FvS
              _bindsIfvSMp :: FvSMp
              _bindsIgUniq :: UID
              _bindsIgathEnv :: REnv
              _bindsIgathRecEnv :: REnv
              _bindsInmL :: ([HsName])
              _bindsIoTrf :: CBindL 
              _bindsIqualS :: RelevQualS
              _bindsIrvarMp :: RVarMp
              _bodyIappFunKind :: AppFunKind
              _bodyIargL :: ([CBound])
              _bodyIcTrf :: CExpr 
              _bodyIcoe :: RelevCoe
              _bodyIfunCoe :: RelevCoe
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIgathLamMp :: LamMp
              _bodyImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _bodyImbFunVar :: (Maybe HsName)
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIoTrf :: CExpr 
              _bodyIqualS :: RelevQualS
              _bodyIrvarMp :: RVarMp
              _bodyIty :: RelevTy
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 161, column 17)
              _bindsOenv =
                  gamAddGam _bindsIgathRecEnv _lhsIenv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 162, column 17)
              _bodyOenv =
                  gamAddGam _bindsIgathEnv    _lhsIenv
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 381, column 17)
              _bodyOrvarMp =
                  _bindsIextraBindRVarMp |+> _bindsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 389, column 17)
              _bindsOforQuantRVarMp =
                  _bindsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 463, column 17)
              _bindsCanParticpateInOuterSolving =
                  categ_ == CBindCateg_Strict
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 463, column 17)
              _bindsQualSSubs =
                  Set.map (_bindsIrvarMp `varUpd`) _bindsIqualS
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 463, column 17)
              _bindsQualSOuter =
                  if _bindsCanParticpateInOuterSolving then _bindsQualSSubs else Set.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 463, column 17)
              _bindsQualSSolv1 =
                  if _bindsCanParticpateInOuterSolving
                  then (Set.empty, emptyRVarMp)
                  else let (q,m) = assSolve _lhsIboundRelevTyVarS _bindsQualSSubs
                       in  (Set.map (m `varUpd`) q, m)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 476, column 17)
              _bindsQualSSolv2 =
                  let (q,m) = _bindsQualSSolv1
                      m2 = m |+> _lhsIfinalRVarMp
                      (q',m3) = assSolve Set.empty (Set.map (m2 `varUpd`) q)
                  in  (Set.map (m3 `varUpd`) q', m3 |+> m2)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 476, column 17)
              _bindsSolveVarMp =
                  snd _bindsQualSSolv2
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 15, column 17)
              _isGlobal =
                  _lhsIlev == cLevModule
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 4, column 17)
              _letBindingsCateg =
                  categ_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 72, column 17)
              _isTopLet =
                  _lhsIwhatAbove == ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 111, column 17)
              _bindsOisStrict =
                  _isGlobal || categ_ == CBindCateg_Strict
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 124, column 17)
              _evalCtx =
                  if categ_ == CBindCateg_Strict
                  then EvalCtx_Eval
                  else EvalCtx_None
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 6, column 17)
              _fvS =
                  (_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 7, column 17)
              _lhsOgathLamMp =
                  _bindsIbindLamMp `Map.union` _bodyIgathLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _bindsIqualS `Set.union` _bodyIqualS
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- self rule
              _oTrf =
                  CExpr_Let categ_ _bindsIoTrf _bodyIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOcoe =
                  _bodyIcoe
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _bodyIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bindsOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindsOdataGam =
                  _lhsIdataGam
              -- copy rule (from local)
              _bindsOevalCtx =
                  _evalCtx
              -- copy rule (down)
              _bindsOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _bindsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _bindsOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindsOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _bindsOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _bindsOwhatTo =
                  _lhsIwhatTo
              -- copy rule (chain)
              _bodyOaltMbScrutTy =
                  _bindsIaltMbScrutTy
              -- copy rule (down)
              _bodyOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bodyOdataGam =
                  _lhsIdataGam
              -- copy rule (from local)
              _bodyOevalCtx =
                  _evalCtx
              -- copy rule (down)
              _bodyOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _bodyOgUniq =
                  _bindsIgUniq
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bodyOwhatTo =
                  _lhsIwhatTo
              ( _bindsIaltMbScrutTy,_bindsIbindLamMp,_bindsIcTrf,_bindsIextraBindRVarMp,_bindsIfvS,_bindsIfvSMp,_bindsIgUniq,_bindsIgathEnv,_bindsIgathRecEnv,_bindsInmL,_bindsIoTrf,_bindsIqualS,_bindsIrvarMp) =
                  binds_ _bindsOboundRelevTyVarS _bindsOdataGam _bindsOenv _bindsOevalCtx _bindsOfinalRVarMp _bindsOforQuantRVarMp _bindsOgUniq _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOknTy _bindsOlamMp _bindsOletBindingsCateg _bindsOlev _bindsOopts _bindsOrvarMp _bindsOwhatTo 
              ( _bodyIappFunKind,_bodyIargL,_bodyIcTrf,_bodyIcoe,_bodyIfunCoe,_bodyIfvS,_bodyIgUniq,_bodyIgathLamMp,_bodyImbFFIApp,_bodyImbFunVar,_bodyImbLam,_bodyImbVar,_bodyIoTrf,_bodyIqualS,_bodyIrvarMp,_bodyIty,_bodyIwhatBelow) =
                  body_ _bodyOaltMbScrutTy _bodyOboundRelevTyVarS _bodyOdataGam _bodyOenv _bodyOevalCtx _bodyOfinalRVarMp _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOknTy _bodyOlamMp _bodyOlev _bodyOopts _bodyOrvarMp _bodyOwhatAbove _bodyOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_String str_
              -- self rule
              _oTrf =
                  CExpr_String str_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _tupTy :: RelevTy
              __tup20 :: ((AMSOut RelevTy,AnaMatchState))
              _amso :: (AMSOut RelevTy)
              _ams :: AnaMatchState
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              __tup21 :: ((UID,UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lhsOfvS :: FvS
              _lhsOoTrf :: CExpr 
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 252, column 33)
              __tup19 =
                  let r = fresh _lUniq2
                      arity = whatExprAppArity _lhsIwhatAbove
                  in  maybe (RelevTy_Err "CExpr.Tup.tupTy", Set.empty) (\(a,qs) -> (RelevTy_Fun RQuant_None [] [] a r, qs))
                      $ relevTyArgsFromCTag False tag_ (Just r) arity _lhsIdataGam _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 252, column 33)
              (_tupTy,_) =
                  __tup19
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 252, column 33)
              (_,_tupQualS) =
                  __tup19
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 256, column 33)
              __tup20 =
                  amsLE _lhsIrvarMp _tupTy _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 256, column 33)
              (_amso,_) =
                  __tup20
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 256, column 33)
              (_,_ams) =
                  __tup20
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 316, column 17)
              _lhsOcoe =
                  _lhsIfinalRVarMp `varUpd` amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 347, column 17)
              _lhsOfunCoe =
                  last $ relevCoeToComposeList $ amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 359, column 17)
              _lhsOty =
                  amsoHi _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 379, column 17)
              _lhsOrvarMp =
                  amsLocalVarMp _ams |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 419, column 17)
              _lhsOqualS =
                  Set.union _tupQualS (amsGathQual _ams)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 592, column 17)
              _dbg =
                  dbg     _lhsIopts _tupTy _lhsIknTy _amso _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 609, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 11, column 17)
              _lhsOappFunKind =
                  AppFunKind_Tag tag_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- -- generated by the unique rule mechanism.
              __tup21 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_,_) =
                  __tup21
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_) =
                  __tup21
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2) =
                  __tup21
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExpr_Tup tag_
              -- self rule
              _oTrf =
                  CExpr_Tup tag_
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _offsetOaltMbScrutTy :: MbRelevTy
              _offsetOboundRelevTyVarS :: UIDS
              _offsetOdataGam :: DataGam
              _offsetOenv :: REnv
              _offsetOevalCtx :: EvalCtx
              _offsetOfinalRVarMp :: RVarMp
              _offsetOgUniq :: UID
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOknTy :: RelevTy
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOopts :: EHCOpts
              _offsetOrvarMp :: RVarMp
              _offsetOwhatAbove :: WhatExpr
              _offsetOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIargL :: ([CBound])
              _offsetIcTrf :: CExpr 
              _offsetIcoe :: RelevCoe
              _offsetIfunCoe :: RelevCoe
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIgathLamMp :: LamMp
              _offsetImbFFIApp :: (Maybe ( Ty
                                                                     , Bool
                                                                     , FFIWay
                                                                     , ForeignEnt
                                                                     , [Ty]
                                                                     ))
              _offsetImbFunVar :: (Maybe HsName)
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIoTrf :: CExpr 
              _offsetIqualS :: RelevQualS
              _offsetIrvarMp :: RVarMp
              _offsetIty :: RelevTy
              _offsetIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS `Set.union` _offsetIqualS
              -- self rule
              _cTrf =
                  CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf
              -- self rule
              _oTrf =
                  CExpr_TupDel _exprIoTrf tag_ nm_ _offsetIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _offsetIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _offsetIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _offsetOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _offsetOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _offsetOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _offsetOenv =
                  _lhsIenv
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
              ( _offsetIappFunKind,_offsetIargL,_offsetIcTrf,_offsetIcoe,_offsetIfunCoe,_offsetIfvS,_offsetIgUniq,_offsetIgathLamMp,_offsetImbFFIApp,_offsetImbFunVar,_offsetImbLam,_offsetImbVar,_offsetIoTrf,_offsetIqualS,_offsetIrvarMp,_offsetIty,_offsetIwhatBelow) =
                  offset_ _offsetOaltMbScrutTy _offsetOboundRelevTyVarS _offsetOdataGam _offsetOenv _offsetOevalCtx _offsetOfinalRVarMp _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOknTy _offsetOlamMp _offsetOlev _offsetOopts _offsetOrvarMp _offsetOwhatAbove _offsetOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _offsetOaltMbScrutTy :: MbRelevTy
              _offsetOboundRelevTyVarS :: UIDS
              _offsetOdataGam :: DataGam
              _offsetOenv :: REnv
              _offsetOevalCtx :: EvalCtx
              _offsetOfinalRVarMp :: RVarMp
              _offsetOgUniq :: UID
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOknTy :: RelevTy
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOopts :: EHCOpts
              _offsetOrvarMp :: RVarMp
              _offsetOwhatAbove :: WhatExpr
              _offsetOwhatTo :: ([WhatToRelevInfer])
              _fldExprOaltMbScrutTy :: MbRelevTy
              _fldExprOboundRelevTyVarS :: UIDS
              _fldExprOdataGam :: DataGam
              _fldExprOenv :: REnv
              _fldExprOevalCtx :: EvalCtx
              _fldExprOfinalRVarMp :: RVarMp
              _fldExprOgUniq :: UID
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOknTy :: RelevTy
              _fldExprOlamMp :: LamMp
              _fldExprOlev :: Int
              _fldExprOopts :: EHCOpts
              _fldExprOrvarMp :: RVarMp
              _fldExprOwhatAbove :: WhatExpr
              _fldExprOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIargL :: ([CBound])
              _offsetIcTrf :: CExpr 
              _offsetIcoe :: RelevCoe
              _offsetIfunCoe :: RelevCoe
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIgathLamMp :: LamMp
              _offsetImbFFIApp :: (Maybe ( Ty
                                                                     , Bool
                                                                     , FFIWay
                                                                     , ForeignEnt
                                                                     , [Ty]
                                                                     ))
              _offsetImbFunVar :: (Maybe HsName)
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIoTrf :: CExpr 
              _offsetIqualS :: RelevQualS
              _offsetIrvarMp :: RVarMp
              _offsetIty :: RelevTy
              _offsetIwhatBelow :: WhatExpr
              _fldExprIappFunKind :: AppFunKind
              _fldExprIargL :: ([CBound])
              _fldExprIcTrf :: CExpr 
              _fldExprIcoe :: RelevCoe
              _fldExprIfunCoe :: RelevCoe
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: UID
              _fldExprIgathLamMp :: LamMp
              _fldExprImbFFIApp :: (Maybe ( Ty
                                                                      , Bool
                                                                      , FFIWay
                                                                      , ForeignEnt
                                                                      , [Ty]
                                                                      ))
              _fldExprImbFunVar :: (Maybe HsName)
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              _fldExprIoTrf :: CExpr 
              _fldExprIqualS :: RelevQualS
              _fldExprIrvarMp :: RVarMp
              _fldExprIty :: RelevTy
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS `Set.union` _offsetIqualS `Set.union` _fldExprIqualS
              -- self rule
              _cTrf =
                  CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _oTrf =
                  CExpr_TupIns _exprIoTrf tag_ nm_ _offsetIoTrf _fldExprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _fldExprIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _offsetOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _offsetOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _offsetOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _offsetOenv =
                  _lhsIenv
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _fldExprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _fldExprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _fldExprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _fldExprOenv =
                  _lhsIenv
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _fldExprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _fldExprOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldExprOrvarMp =
                  _offsetIrvarMp
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
              ( _offsetIappFunKind,_offsetIargL,_offsetIcTrf,_offsetIcoe,_offsetIfunCoe,_offsetIfvS,_offsetIgUniq,_offsetIgathLamMp,_offsetImbFFIApp,_offsetImbFunVar,_offsetImbLam,_offsetImbVar,_offsetIoTrf,_offsetIqualS,_offsetIrvarMp,_offsetIty,_offsetIwhatBelow) =
                  offset_ _offsetOaltMbScrutTy _offsetOboundRelevTyVarS _offsetOdataGam _offsetOenv _offsetOevalCtx _offsetOfinalRVarMp _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOknTy _offsetOlamMp _offsetOlev _offsetOopts _offsetOrvarMp _offsetOwhatAbove _offsetOwhatTo 
              ( _fldExprIappFunKind,_fldExprIargL,_fldExprIcTrf,_fldExprIcoe,_fldExprIfunCoe,_fldExprIfvS,_fldExprIgUniq,_fldExprIgathLamMp,_fldExprImbFFIApp,_fldExprImbFunVar,_fldExprImbLam,_fldExprImbVar,_fldExprIoTrf,_fldExprIqualS,_fldExprIrvarMp,_fldExprIty,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOaltMbScrutTy _fldExprOboundRelevTyVarS _fldExprOdataGam _fldExprOenv _fldExprOevalCtx _fldExprOfinalRVarMp _fldExprOgUniq _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOknTy _fldExprOlamMp _fldExprOlev _fldExprOopts _fldExprOrvarMp _fldExprOwhatAbove _fldExprOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOoTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _lhsOwhatBelow :: WhatExpr
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOevalCtx :: EvalCtx
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOknTy :: RelevTy
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _offsetOaltMbScrutTy :: MbRelevTy
              _offsetOboundRelevTyVarS :: UIDS
              _offsetOdataGam :: DataGam
              _offsetOenv :: REnv
              _offsetOevalCtx :: EvalCtx
              _offsetOfinalRVarMp :: RVarMp
              _offsetOgUniq :: UID
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOknTy :: RelevTy
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOopts :: EHCOpts
              _offsetOrvarMp :: RVarMp
              _offsetOwhatAbove :: WhatExpr
              _offsetOwhatTo :: ([WhatToRelevInfer])
              _fldExprOaltMbScrutTy :: MbRelevTy
              _fldExprOboundRelevTyVarS :: UIDS
              _fldExprOdataGam :: DataGam
              _fldExprOenv :: REnv
              _fldExprOevalCtx :: EvalCtx
              _fldExprOfinalRVarMp :: RVarMp
              _fldExprOgUniq :: UID
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOknTy :: RelevTy
              _fldExprOlamMp :: LamMp
              _fldExprOlev :: Int
              _fldExprOopts :: EHCOpts
              _fldExprOrvarMp :: RVarMp
              _fldExprOwhatAbove :: WhatExpr
              _fldExprOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIargL :: ([CBound])
              _offsetIcTrf :: CExpr 
              _offsetIcoe :: RelevCoe
              _offsetIfunCoe :: RelevCoe
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIgathLamMp :: LamMp
              _offsetImbFFIApp :: (Maybe ( Ty
                                                                     , Bool
                                                                     , FFIWay
                                                                     , ForeignEnt
                                                                     , [Ty]
                                                                     ))
              _offsetImbFunVar :: (Maybe HsName)
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIoTrf :: CExpr 
              _offsetIqualS :: RelevQualS
              _offsetIrvarMp :: RVarMp
              _offsetIty :: RelevTy
              _offsetIwhatBelow :: WhatExpr
              _fldExprIappFunKind :: AppFunKind
              _fldExprIargL :: ([CBound])
              _fldExprIcTrf :: CExpr 
              _fldExprIcoe :: RelevCoe
              _fldExprIfunCoe :: RelevCoe
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: UID
              _fldExprIgathLamMp :: LamMp
              _fldExprImbFFIApp :: (Maybe ( Ty
                                                                      , Bool
                                                                      , FFIWay
                                                                      , ForeignEnt
                                                                      , [Ty]
                                                                      ))
              _fldExprImbFunVar :: (Maybe HsName)
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              _fldExprIoTrf :: CExpr 
              _fldExprIqualS :: RelevQualS
              _fldExprIrvarMp :: RVarMp
              _fldExprIty :: RelevTy
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 321, column 17)
              _lhsOcoe =
                  RelevCoe_Id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 353, column 17)
              _lhsOfunCoe =
                  RelevCoe_Err "CExpr.unimpl"
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 363, column 17)
              _lhsOty =
                  RelevTy_Err "unimpl"
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 8, column 33)
              _lhsOmbFunVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS `Set.union` _offsetIqualS `Set.union` _fldExprIqualS
              -- self rule
              _cTrf =
                  CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _oTrf =
                  CExpr_TupUpd _exprIoTrf tag_ nm_ _offsetIoTrf _fldExprIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _fldExprIrvarMp
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _exprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _offsetOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _offsetOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _offsetOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _offsetOenv =
                  _lhsIenv
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _fldExprOaltMbScrutTy =
                  _lhsIaltMbScrutTy
              -- copy rule (down)
              _fldExprOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _fldExprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _fldExprOenv =
                  _lhsIenv
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _fldExprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _fldExprOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldExprOrvarMp =
                  _offsetIrvarMp
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
              ( _offsetIappFunKind,_offsetIargL,_offsetIcTrf,_offsetIcoe,_offsetIfunCoe,_offsetIfvS,_offsetIgUniq,_offsetIgathLamMp,_offsetImbFFIApp,_offsetImbFunVar,_offsetImbLam,_offsetImbVar,_offsetIoTrf,_offsetIqualS,_offsetIrvarMp,_offsetIty,_offsetIwhatBelow) =
                  offset_ _offsetOaltMbScrutTy _offsetOboundRelevTyVarS _offsetOdataGam _offsetOenv _offsetOevalCtx _offsetOfinalRVarMp _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOknTy _offsetOlamMp _offsetOlev _offsetOopts _offsetOrvarMp _offsetOwhatAbove _offsetOwhatTo 
              ( _fldExprIappFunKind,_fldExprIargL,_fldExprIcTrf,_fldExprIcoe,_fldExprIfunCoe,_fldExprIfvS,_fldExprIgUniq,_fldExprIgathLamMp,_fldExprImbFFIApp,_fldExprImbFunVar,_fldExprImbLam,_fldExprImbVar,_fldExprIoTrf,_fldExprIqualS,_fldExprIrvarMp,_fldExprIty,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOaltMbScrutTy _fldExprOboundRelevTyVarS _fldExprOdataGam _fldExprOenv _fldExprOevalCtx _fldExprOfinalRVarMp _fldExprOgUniq _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOknTy _fldExprOlamMp _fldExprOlev _fldExprOopts _fldExprOrvarMp _fldExprOwhatAbove _fldExprOwhatTo 
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIaltMbScrutTy
       _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatAbove
       _lhsIwhatTo ->
         (let __tup22 :: ((RelevTy,RelevQualS))
              _envTy :: RelevTy
              _envQualS :: RelevQualS
              __tup23 :: ((AMSOut RelevTy,AnaMatchState))
              _amso :: (AMSOut RelevTy)
              _ams :: AnaMatchState
              _lhsOcoe :: RelevCoe
              _lhsOfunCoe :: RelevCoe
              _lhsOty :: RelevTy
              _lhsOrvarMp :: RVarMp
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExpr 
              _lhsOmbFFIApp :: (Maybe ( Ty
                                                                  , Bool
                                                                  , FFIWay
                                                                  , ForeignEnt
                                                                  , [Ty]
                                                                  ))
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOmbFunVar :: (Maybe HsName)
              _lhsOargL :: ([CBound])
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _mbVar :: (Maybe HsName)
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOgathLamMp :: LamMp
              __tup24 :: ((UID,UID,UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lUniq2 :: UID
              _lUniq3 :: UID
              _lhsOoTrf :: CExpr 
              _lhsOmbVar :: (Maybe HsName)
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 156, column 17)
              _mbEnvTy =
                  fmap (relevtyInst _lUniq) (renvLookup _nm _lhsIenv _lhsIlamMp)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 157, column 33)
              __tup22 =
                  maybe (top, Set.empty) id _mbEnvTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 157, column 33)
              (_envTy,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 157, column 33)
              (_,_envQualS) =
                  __tup22
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 231, column 33)
              __tup23 =
                  let mk envTy knTy
                        = case (envTy,knTy) of
                            (RelevTy_Ana ana,t@(RelevTy_Fun _ _ _ a r@(RelevTy_Ana rAna)))
                              -> ( amso' {amsoCoe = RelevCoe_Eval ana rAna <.> RelevCoe_CastTy envTy envTy' <.> amsoCoe amso'}
                                 , ams'  {amsGathQual = Set.insert (RelevQual_SubEval ana rAna) $ amsGathQual ams'}
                                 )
                              where (amso',ams') = amsLE _lhsIrvarMp envTy' t
                                    envTy' = anaMkBotFun $ length a
                                    ty'@(RelevTy_Ana ana') = fresh _lUniq2
                            (t1,t2)
                              -> ( amsoMkOk t1 t2 (RelevCoe_CastTy t1 t2)
                                 , emptyAnaMatchState
                                 )
                  in  case _mbEnvTy of
                        Just (envTy,_)
                          | amsoIsOk amso -> o
                          | otherwise     -> mk envTy _lhsIknTy
                          where o@(amso,ams) = amsLE _lhsIrvarMp envTy _lhsIknTy
                        _                 -> mk (fresh _lUniq3) _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 231, column 33)
              (_amso,_) =
                  __tup23
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 231, column 33)
              (_,_ams) =
                  __tup23
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 316, column 17)
              _lhsOcoe =
                  _lhsIfinalRVarMp `varUpd` amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 347, column 17)
              _lhsOfunCoe =
                  last $ relevCoeToComposeList $ amsoCoe _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 359, column 17)
              _lhsOty =
                  amsoHi _amso
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 379, column 17)
              _lhsOrvarMp =
                  amsLocalVarMp _ams |+> _lhsIrvarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 417, column 17)
              _lhsOqualS =
                  Set.union _envQualS (amsGathQual _ams)
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 590, column 17)
              _dbg =
                  dbg     _lhsIopts _envTy _lhsIknTy _amso _ams
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 609, column 17)
              _lhsOcTrf =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonFFI.ag"(line 43, column 17)
              _lhsOmbFFIApp =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nm =
                  acbrefNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nmAsp =
                  mkHNm ref_
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 5, column 17)
              _lhsOmbFunVar =
                  _mbVar
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 15, column 33)
              _lhsOargL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 13, column 17)
              _lhsOappFunKind =
                  AppFunKind_Fun ref_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 21, column 17)
              _mbVar =
                  Just _nm
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 54, column 17)
              _whatBelow =
                  ExprIsVar _nm
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 7, column 17)
              _lhsOfvS =
                  Set.singleton _nm
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- -- generated by the unique rule mechanism.
              __tup24 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> (__cont, lUniq,lUniq2,lUniq3)}}} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_,_,_) =
                  __tup24
              -- -- generated by the unique rule mechanism.
              (_,_lUniq,_,_) =
                  __tup24
              -- -- generated by the unique rule mechanism.
              (_,_,_lUniq2,_) =
                  __tup24
              -- -- generated by the unique rule mechanism.
              (_,_,_,_lUniq3) =
                  __tup24
              -- self rule
              _cTrf =
                  CExpr_Var ref_
              -- self rule
              _oTrf =
                  CExpr_Var ref_
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (from local)
              _lhsOmbVar =
                  _mbVar
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsOargL,_lhsOcTrf,_lhsOcoe,_lhsOfunCoe,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOmbFFIApp,_lhsOmbFunVar,_lhsOmbLam,_lhsOmbVar,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp,_lhsOty,_lhsOwhatBelow)))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CExprAnn :: CExprAnn  ->
                T_CExprAnn 
sem_CExprAnn (CExprAnn_Coe _coe )  =
    (sem_CExprAnn_Coe _coe )
sem_CExprAnn (CExprAnn_Debug _info )  =
    (sem_CExprAnn_Debug _info )
sem_CExprAnn (CExprAnn_Ty _ty )  =
    (sem_CExprAnn_Ty _ty )
-- semantic domain
type T_CExprAnn  = UIDS ->
                   DataGam ->
                   REnv ->
                   RVarMp ->
                   UID ->
                   RelevTy ->
                   LamMp ->
                   Int ->
                   EHCOpts ->
                   RVarMp ->
                   ([WhatToRelevInfer]) ->
                   ( CExprAnn ,FvS,UID,CExprAnn ,RelevQualS,RVarMp)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExprAnn 
              _lhsOoTrf :: CExprAnn 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExprAnn_Coe coe_
              -- self rule
              _oTrf =
                  CExprAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExprAnn 
              _lhsOoTrf :: CExprAnn 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExprAnn_Debug info_
              -- self rule
              _oTrf =
                  CExprAnn_Debug info_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CExprAnn 
              _lhsOoTrf :: CExprAnn 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CExprAnn_Ty ty_
              -- self rule
              _oTrf =
                  CExprAnn_Ty ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Apply0:
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Function0:
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Function1:
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Plain:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CMetaBind :: CMetaBind  ->
                 T_CMetaBind 
sem_CMetaBind (CMetaBind_Apply0 )  =
    (sem_CMetaBind_Apply0 )
sem_CMetaBind (CMetaBind_Function0 )  =
    (sem_CMetaBind_Function0 )
sem_CMetaBind (CMetaBind_Function1 )  =
    (sem_CMetaBind_Function1 )
sem_CMetaBind (CMetaBind_Plain )  =
    (sem_CMetaBind_Plain )
-- semantic domain
type T_CMetaBind  = UIDS ->
                    DataGam ->
                    REnv ->
                    RVarMp ->
                    UID ->
                    RelevTy ->
                    LamMp ->
                    Int ->
                    EHCOpts ->
                    RVarMp ->
                    ([WhatToRelevInfer]) ->
                    ( CMetaBind ,FvS,UID,CMetaBind ,RelevQualS,RVarMp)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaBind 
              _lhsOoTrf :: CMetaBind 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaBind_Apply0
              -- self rule
              _oTrf =
                  CMetaBind_Apply0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaBind 
              _lhsOoTrf :: CMetaBind 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaBind_Function0
              -- self rule
              _oTrf =
                  CMetaBind_Function0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaBind 
              _lhsOoTrf :: CMetaBind 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaBind_Function1
              -- self rule
              _oTrf =
                  CMetaBind_Function1
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaBind 
              _lhsOoTrf :: CMetaBind 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaBind_Plain
              -- self rule
              _oTrf =
                  CMetaBind_Plain
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Dict:
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Val:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CMetaVal :: CMetaVal  ->
                T_CMetaVal 
sem_CMetaVal (CMetaVal_Dict )  =
    (sem_CMetaVal_Dict )
sem_CMetaVal (CMetaVal_DictClass _tracks )  =
    (sem_CMetaVal_DictClass _tracks )
sem_CMetaVal (CMetaVal_DictInstance _tracks )  =
    (sem_CMetaVal_DictInstance _tracks )
sem_CMetaVal (CMetaVal_Track _track )  =
    (sem_CMetaVal_Track _track )
sem_CMetaVal (CMetaVal_Val )  =
    (sem_CMetaVal_Val )
-- semantic domain
type T_CMetaVal  = UIDS ->
                   DataGam ->
                   REnv ->
                   RVarMp ->
                   UID ->
                   RelevTy ->
                   LamMp ->
                   Int ->
                   EHCOpts ->
                   RVarMp ->
                   ([WhatToRelevInfer]) ->
                   ( CMetaVal ,FvS,UID,CMetaVal ,RelevQualS,RVarMp)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaVal 
              _lhsOoTrf :: CMetaVal 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaVal_Dict
              -- self rule
              _oTrf =
                  CMetaVal_Dict
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaVal 
              _lhsOoTrf :: CMetaVal 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaVal_DictClass tracks_
              -- self rule
              _oTrf =
                  CMetaVal_DictClass tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaVal 
              _lhsOoTrf :: CMetaVal 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _oTrf =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaVal 
              _lhsOoTrf :: CMetaVal 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaVal_Track track_
              -- self rule
              _oTrf =
                  CMetaVal_Track track_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetaVal 
              _lhsOoTrf :: CMetaVal 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CMetaVal_Val
              -- self rule
              _oTrf =
                  CMetaVal_Val
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = UIDS ->
                 DataGam ->
                 REnv ->
                 RVarMp ->
                 UID ->
                 RelevTy ->
                 LamMp ->
                 Int ->
                 EHCOpts ->
                 RVarMp ->
                 ([WhatToRelevInfer]) ->
                 ( CMetas ,FvS,UID,CMetas ,RelevQualS,RVarMp)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CMetas 
              _lhsOoTrf :: CMetas 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _x1OboundRelevTyVarS :: UIDS
              _x1OdataGam :: DataGam
              _x1Oenv :: REnv
              _x1OfinalRVarMp :: RVarMp
              _x1OgUniq :: UID
              _x1OknTy :: RelevTy
              _x1OlamMp :: LamMp
              _x1Olev :: Int
              _x1Oopts :: EHCOpts
              _x1OrvarMp :: RVarMp
              _x1OwhatTo :: ([WhatToRelevInfer])
              _x2OboundRelevTyVarS :: UIDS
              _x2OdataGam :: DataGam
              _x2Oenv :: REnv
              _x2OfinalRVarMp :: RVarMp
              _x2OgUniq :: UID
              _x2OknTy :: RelevTy
              _x2OlamMp :: LamMp
              _x2Olev :: Int
              _x2Oopts :: EHCOpts
              _x2OrvarMp :: RVarMp
              _x2OwhatTo :: ([WhatToRelevInfer])
              _x1IcTrf :: CMetaBind 
              _x1IfvS :: FvS
              _x1IgUniq :: UID
              _x1IoTrf :: CMetaBind 
              _x1IqualS :: RelevQualS
              _x1IrvarMp :: RVarMp
              _x2IcTrf :: CMetaVal 
              _x2IfvS :: FvS
              _x2IgUniq :: UID
              _x2IoTrf :: CMetaVal 
              _x2IqualS :: RelevQualS
              _x2IrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _x1IfvS `Set.union` _x2IfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _x1IqualS `Set.union` _x2IqualS
              -- self rule
              _cTrf =
                  (_x1IcTrf,_x2IcTrf)
              -- self rule
              _oTrf =
                  (_x1IoTrf,_x2IoTrf)
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _x2IgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _x2IrvarMp
              -- copy rule (down)
              _x1OboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _x1OdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _x1Oenv =
                  _lhsIenv
              -- copy rule (down)
              _x1OfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _x1OgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _x1OknTy =
                  _lhsIknTy
              -- copy rule (down)
              _x1OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x1Olev =
                  _lhsIlev
              -- copy rule (down)
              _x1Oopts =
                  _lhsIopts
              -- copy rule (down)
              _x1OrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _x1OwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _x2OboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _x2OdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _x2Oenv =
                  _lhsIenv
              -- copy rule (down)
              _x2OfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _x2OgUniq =
                  _x1IgUniq
              -- copy rule (down)
              _x2OknTy =
                  _lhsIknTy
              -- copy rule (down)
              _x2OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x2Olev =
                  _lhsIlev
              -- copy rule (down)
              _x2Oopts =
                  _lhsIopts
              -- copy rule (chain)
              _x2OrvarMp =
                  _x1IrvarMp
              -- copy rule (down)
              _x2OwhatTo =
                  _lhsIwhatTo
              ( _x1IcTrf,_x1IfvS,_x1IgUniq,_x1IoTrf,_x1IqualS,_x1IrvarMp) =
                  x1_ _x1OboundRelevTyVarS _x1OdataGam _x1Oenv _x1OfinalRVarMp _x1OgUniq _x1OknTy _x1OlamMp _x1Olev _x1Oopts _x1OrvarMp _x1OwhatTo 
              ( _x2IcTrf,_x2IfvS,_x2IgUniq,_x2IoTrf,_x2IqualS,_x2IrvarMp) =
                  x2_ _x2OboundRelevTyVarS _x2OdataGam _x2Oenv _x2OfinalRVarMp _x2OgUniq _x2OknTy _x2OlamMp _x2Olev _x2Oopts _x2OrvarMp _x2OwhatTo 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         gathLamMp            : LamMp
         qualS                : RelevQualS
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local altMbScrutTy : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = DataGam ->
                  REnv ->
                  RVarMp ->
                  UID ->
                  LamMp ->
                  Int ->
                  EHCOpts ->
                  RVarMp ->
                  ([WhatToRelevInfer]) ->
                  ( CModule ,FvS,UID,LamMp,RelevQualS,RVarMp)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _exprOknTy :: RelevTy
              _exprOboundRelevTyVarS :: UIDS
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _exprOisLamBody :: Bool
              _exprOevalCtx :: EvalCtx
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CModule 
              _lhsOgUniq :: UID
              _lhsOgathLamMp :: LamMp
              _lhsOrvarMp :: RVarMp
              _exprOaltMbScrutTy :: MbRelevTy
              _exprOdataGam :: DataGam
              _exprOenv :: REnv
              _exprOfinalRVarMp :: RVarMp
              _exprOgUniq :: UID
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOopts :: EHCOpts
              _exprOrvarMp :: RVarMp
              _exprOwhatAbove :: WhatExpr
              _exprOwhatTo :: ([WhatToRelevInfer])
              _exprIappFunKind :: AppFunKind
              _exprIargL :: ([CBound])
              _exprIcTrf :: CExpr 
              _exprIcoe :: RelevCoe
              _exprIfunCoe :: RelevCoe
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIgathLamMp :: LamMp
              _exprImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _exprImbFunVar :: (Maybe HsName)
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIoTrf :: CExpr 
              _exprIqualS :: RelevQualS
              _exprIrvarMp :: RVarMp
              _exprIty :: RelevTy
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 201, column 17)
              _exprOknTy =
                  RelevTy_None
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 488, column 17)
              _exprOboundRelevTyVarS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 85, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 116, column 17)
              _exprOevalCtx =
                  EvalCtx_Eval
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _exprIqualS
              -- self rule
              _cTrf =
                  CModule_Mod moduleNm_ _exprIcTrf ctagsMp_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (up)
              _lhsOrvarMp =
                  _exprIrvarMp
              -- copy rule (from local)
              _exprOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _exprOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (down)
              _exprOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _exprOwhatTo =
                  _lhsIwhatTo
              ( _exprIappFunKind,_exprIargL,_exprIcTrf,_exprIcoe,_exprIfunCoe,_exprIfvS,_exprIgUniq,_exprIgathLamMp,_exprImbFFIApp,_exprImbFunVar,_exprImbLam,_exprImbVar,_exprIoTrf,_exprIqualS,_exprIrvarMp,_exprIty,_exprIwhatBelow) =
                  expr_ _exprOaltMbScrutTy _exprOboundRelevTyVarS _exprOdataGam _exprOenv _exprOevalCtx _exprOfinalRVarMp _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOknTy _exprOlamMp _exprOlev _exprOopts _exprOrvarMp _exprOwhatAbove _exprOwhatTo 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOgathLamMp,_lhsOqualS,_lhsOrvarMp)))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         patFldTyL            : [(RelevTy,RelevTy)]
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         mbCTag               : Maybe CTag
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CPat :: CPat  ->
            T_CPat 
sem_CPat (CPat_BoolExpr _cexpr )  =
    (sem_CPat_BoolExpr _cexpr )
sem_CPat (CPat_Char _char )  =
    (sem_CPat_Char _char )
sem_CPat (CPat_Con _tag _rest _binds )  =
    (sem_CPat_Con _tag (sem_CPatRest _rest ) (sem_CPatFldL _binds ) )
sem_CPat (CPat_Int _int )  =
    (sem_CPat_Int _int )
sem_CPat (CPat_Var _pnm )  =
    (sem_CPat_Var _pnm )
-- semantic domain
type T_CPat  = UIDS ->
               DataGam ->
               REnv ->
               RVarMp ->
               UID ->
               RelevTy ->
               RelevTy ->
               LamMp ->
               Int ->
               EHCOpts ->
               ([(RelevTy,RelevTy)]) ->
               RVarMp ->
               ([WhatToRelevInfer]) ->
               ( CPat ,([HsName]),FvS,UID,(Maybe CTag),([HsName]),CPat ,([(RelevTy,RelevTy)]),RelevQualS,RVarMp)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOmbCTag :: (Maybe CTag)
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPat 
              _lhsOoTrf :: CPat 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 22, column 33)
              _lhsOmbCTag =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPat_BoolExpr cexpr_
              -- self rule
              _oTrf =
                  CPat_BoolExpr cexpr_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOmbCTag :: (Maybe CTag)
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPat 
              _lhsOoTrf :: CPat 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 22, column 33)
              _lhsOmbCTag =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPat_Char char_
              -- self rule
              _oTrf =
                  CPat_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOmbCTag :: (Maybe CTag)
              _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPat 
              _lhsOoTrf :: CPat 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              _restOboundRelevTyVarS :: UIDS
              _restOdataGam :: DataGam
              _restOenv :: REnv
              _restOfinalRVarMp :: RVarMp
              _restOgUniq :: UID
              _restOknTy :: RelevTy
              _restOknTyCase :: RelevTy
              _restOlamMp :: LamMp
              _restOlev :: Int
              _restOopts :: EHCOpts
              _restOrvarMp :: RVarMp
              _restOwhatTo :: ([WhatToRelevInfer])
              _bindsOboundRelevTyVarS :: UIDS
              _bindsOdataGam :: DataGam
              _bindsOenv :: REnv
              _bindsOfinalRVarMp :: RVarMp
              _bindsOgUniq :: UID
              _bindsOknTy :: RelevTy
              _bindsOknTyCase :: RelevTy
              _bindsOlamMp :: LamMp
              _bindsOlev :: Int
              _bindsOopts :: EHCOpts
              _bindsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _bindsOrvarMp :: RVarMp
              _bindsOwhatTo :: ([WhatToRelevInfer])
              _restIcTrf :: CPatRest 
              _restIfvS :: FvS
              _restIgUniq :: UID
              _restInmL :: ([HsName])
              _restIoTrf :: CPatRest 
              _restIqualS :: RelevQualS
              _restIrvarMp :: RVarMp
              _bindsIcTrf :: CPatFldL 
              _bindsIfldNmL :: ([HsName])
              _bindsIfvS :: FvS
              _bindsIgUniq :: UID
              _bindsInmL :: ([HsName])
              _bindsIoTrf :: CPatFldL 
              _bindsIpatFldTyL :: ([(RelevTy,RelevTy)])
              _bindsIqualS :: RelevQualS
              _bindsIrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 20, column 25)
              _lhsOmbCTag =
                  Just tag_
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 29, column 17)
              _lhsOnmL =
                  _restInmL ++ _bindsInmL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _bindsIfldNmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _restIfvS `Set.union` _bindsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _restIqualS `Set.union` _bindsIqualS
              -- self rule
              _cTrf =
                  CPat_Con tag_ _restIcTrf _bindsIcTrf
              -- self rule
              _oTrf =
                  CPat_Con tag_ _restIoTrf _bindsIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bindsIgUniq
              -- copy rule (up)
              _lhsOpatFldTyL =
                  _bindsIpatFldTyL
              -- copy rule (up)
              _lhsOrvarMp =
                  _bindsIrvarMp
              -- copy rule (down)
              _restOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _restOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _restOenv =
                  _lhsIenv
              -- copy rule (down)
              _restOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _restOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _restOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _restOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _restOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _restOlev =
                  _lhsIlev
              -- copy rule (down)
              _restOopts =
                  _lhsIopts
              -- copy rule (down)
              _restOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _restOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _bindsOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindsOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bindsOenv =
                  _lhsIenv
              -- copy rule (down)
              _bindsOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _bindsOgUniq =
                  _restIgUniq
              -- copy rule (down)
              _bindsOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindsOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _bindsOrvarMp =
                  _restIrvarMp
              -- copy rule (down)
              _bindsOwhatTo =
                  _lhsIwhatTo
              ( _restIcTrf,_restIfvS,_restIgUniq,_restInmL,_restIoTrf,_restIqualS,_restIrvarMp) =
                  rest_ _restOboundRelevTyVarS _restOdataGam _restOenv _restOfinalRVarMp _restOgUniq _restOknTy _restOknTyCase _restOlamMp _restOlev _restOopts _restOrvarMp _restOwhatTo 
              ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsIgUniq,_bindsInmL,_bindsIoTrf,_bindsIpatFldTyL,_bindsIqualS,_bindsIrvarMp) =
                  binds_ _bindsOboundRelevTyVarS _bindsOdataGam _bindsOenv _bindsOfinalRVarMp _bindsOgUniq _bindsOknTy _bindsOknTyCase _bindsOlamMp _bindsOlev _bindsOopts _bindsOpatFldTyL _bindsOrvarMp _bindsOwhatTo 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOmbCTag :: (Maybe CTag)
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPat 
              _lhsOoTrf :: CPat 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 22, column 33)
              _lhsOmbCTag =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPat_Int int_
              -- self rule
              _oTrf =
                  CPat_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOmbCTag :: (Maybe CTag)
              _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPat 
              _lhsOoTrf :: CPat 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonStructureInfo.ag"(line 22, column 33)
              _lhsOmbCTag =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 28, column 17)
              _lhsOnmL =
                  [pnm_]
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPat_Var pnm_
              -- self rule
              _oTrf =
                  CPat_Var pnm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOmbCTag,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         patFldTyL            : [(RelevTy,RelevTy)]
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local _tup25      : _
            local patFldTy    : _
            local fldCoe      : _
            local altMbScrutTy : _
            local hereOffCoe  : _
            local hereFldCoe  : _
            local fldNm       : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = UIDS ->
                  DataGam ->
                  REnv ->
                  RVarMp ->
                  UID ->
                  RelevTy ->
                  RelevTy ->
                  LamMp ->
                  Int ->
                  EHCOpts ->
                  ([(RelevTy,RelevTy)]) ->
                  RVarMp ->
                  ([WhatToRelevInfer]) ->
                  ( CPatFld ,([HsName]),FvS,UID,([HsName]),CPatFld ,([(RelevTy,RelevTy)]),RelevQualS,RVarMp)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _offsetOknTy :: RelevTy
              _bindOforQuantRVarMp :: RVarMp
              _lhsOcTrf :: CPatFld 
              _bindOisGlobal :: Bool
              _lhsOfldNmL :: ([HsName])
              _bindOletBindingsCateg :: CBindCateg
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _offsetOisStrict :: Bool
              _offsetOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOisLamBody :: Bool
              _offsetOevalCtx :: EvalCtx
              _bindOevalCtx :: EvalCtx
              _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOoTrf :: CPatFld 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _offsetOaltMbScrutTy :: MbRelevTy
              _offsetOboundRelevTyVarS :: UIDS
              _offsetOdataGam :: DataGam
              _offsetOenv :: REnv
              _offsetOfinalRVarMp :: RVarMp
              _offsetOgUniq :: UID
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOopts :: EHCOpts
              _offsetOrvarMp :: RVarMp
              _offsetOwhatAbove :: WhatExpr
              _offsetOwhatTo :: ([WhatToRelevInfer])
              _bindOboundRelevTyVarS :: UIDS
              _bindOdataGam :: DataGam
              _bindOenv :: REnv
              _bindOfinalRVarMp :: RVarMp
              _bindOgUniq :: UID
              _bindOknTy :: RelevTy
              _bindOlamMp :: LamMp
              _bindOlev :: Int
              _bindOopts :: EHCOpts
              _bindOrvarMp :: RVarMp
              _bindOwhatTo :: ([WhatToRelevInfer])
              _fldAnnsOboundRelevTyVarS :: UIDS
              _fldAnnsOdataGam :: DataGam
              _fldAnnsOenv :: REnv
              _fldAnnsOfinalRVarMp :: RVarMp
              _fldAnnsOgUniq :: UID
              _fldAnnsOknTy :: RelevTy
              _fldAnnsOknTyCase :: RelevTy
              _fldAnnsOlamMp :: LamMp
              _fldAnnsOlev :: Int
              _fldAnnsOopts :: EHCOpts
              _fldAnnsOrvarMp :: RVarMp
              _fldAnnsOwhatTo :: ([WhatToRelevInfer])
              _offsetIappFunKind :: AppFunKind
              _offsetIargL :: ([CBound])
              _offsetIcTrf :: CExpr 
              _offsetIcoe :: RelevCoe
              _offsetIfunCoe :: RelevCoe
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIgathLamMp :: LamMp
              _offsetImbFFIApp :: (Maybe ( Ty
                                                                     , Bool
                                                                     , FFIWay
                                                                     , ForeignEnt
                                                                     , [Ty]
                                                                     ))
              _offsetImbFunVar :: (Maybe HsName)
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIoTrf :: CExpr 
              _offsetIqualS :: RelevQualS
              _offsetIrvarMp :: RVarMp
              _offsetIty :: RelevTy
              _offsetIwhatBelow :: WhatExpr
              _bindIaltMbScrutTy :: MbRelevTy
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindIextraBindRVarMp :: RVarMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: UID
              _bindIgathEnv :: REnv
              _bindIgathRecEnv :: REnv
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _bindIoTrf :: CBind 
              _bindIqualS :: RelevQualS
              _bindIrvarMp :: RVarMp
              _fldAnnsIcTrf :: CBindAnnL 
              _fldAnnsIfvS :: FvS
              _fldAnnsIgUniq :: UID
              _fldAnnsInmL :: ([HsName])
              _fldAnnsIoTrf :: CBindAnnL 
              _fldAnnsIqualS :: RelevQualS
              _fldAnnsIrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 185, column 17)
              __tup25 =
                  hdAndTl' (panic $ "patFldTyL: " ++ show _fldNm) _lhsIpatFldTyL
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 185, column 17)
              (_patFldTy,_) =
                  __tup25
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 185, column 17)
              (_,_lhsOpatFldTyL) =
                  __tup25
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 186, column 17)
              _fldCoe =
                  let (tgiven,tused) = _patFldTy
                  in  RelevCoe_CastTy tgiven tused
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 216, column 17)
              _offsetOknTy =
                  _lhsIknTy
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 340, column 17)
              _hereOffCoe =
                  _lhsIfinalRVarMp `varUpd` _offsetIcoe
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 340, column 17)
              _hereFldCoe =
                  _lhsIfinalRVarMp `varUpd` _fldCoe
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 393, column 25)
              _bindOforQuantRVarMp =
                  emptyRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 624, column 17)
              _lhsOcTrf =
                  CPatFld_Fld lbl_ (annCoe _hereOffCoe _offsetIcTrf) _bindIcTrf [CBindAnn_Coe _hereFldCoe]
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 19, column 17)
              _bindOisGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 23, column 17)
              _fldNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 28, column 17)
              _lhsOfldNmL =
                  [_fldNm]
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 8, column 17)
              _bindOletBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 79, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisStrict =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 119, column 17)
              _offsetOevalCtx =
                  EvalCtx_Eval
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 120, column 33)
              _bindOevalCtx =
                  EvalCtx_None
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 22, column 17)
              _lhsOnmL =
                  [_fldNm]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _offsetIqualS `Set.union` _bindIqualS `Set.union` _fldAnnsIqualS
              -- self rule
              _cTrf =
                  CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf
              -- self rule
              _oTrf =
                  CPatFld_Fld lbl_ _offsetIoTrf _bindIoTrf _fldAnnsIoTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _fldAnnsIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _fldAnnsIrvarMp
              -- copy rule (from local)
              _offsetOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _offsetOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _offsetOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _offsetOenv =
                  _lhsIenv
              -- copy rule (down)
              _offsetOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _offsetOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (down)
              _offsetOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _bindOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _bindOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _bindOenv =
                  _lhsIenv
              -- copy rule (down)
              _bindOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _bindOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _bindOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bindOrvarMp =
                  _offsetIrvarMp
              -- copy rule (down)
              _bindOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _fldAnnsOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _fldAnnsOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _fldAnnsOenv =
                  _lhsIenv
              -- copy rule (down)
              _fldAnnsOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _fldAnnsOgUniq =
                  _bindIgUniq
              -- copy rule (down)
              _fldAnnsOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _fldAnnsOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _fldAnnsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldAnnsOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldAnnsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldAnnsOrvarMp =
                  _bindIrvarMp
              -- copy rule (down)
              _fldAnnsOwhatTo =
                  _lhsIwhatTo
              ( _offsetIappFunKind,_offsetIargL,_offsetIcTrf,_offsetIcoe,_offsetIfunCoe,_offsetIfvS,_offsetIgUniq,_offsetIgathLamMp,_offsetImbFFIApp,_offsetImbFunVar,_offsetImbLam,_offsetImbVar,_offsetIoTrf,_offsetIqualS,_offsetIrvarMp,_offsetIty,_offsetIwhatBelow) =
                  offset_ _offsetOaltMbScrutTy _offsetOboundRelevTyVarS _offsetOdataGam _offsetOenv _offsetOevalCtx _offsetOfinalRVarMp _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOknTy _offsetOlamMp _offsetOlev _offsetOopts _offsetOrvarMp _offsetOwhatAbove _offsetOwhatTo 
              ( _bindIaltMbScrutTy,_bindIbindLamMp,_bindIcTrf,_bindIextraBindRVarMp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIgathEnv,_bindIgathRecEnv,_bindInm,_bindInmL,_bindIoTrf,_bindIqualS,_bindIrvarMp) =
                  bind_ _bindOboundRelevTyVarS _bindOdataGam _bindOenv _bindOevalCtx _bindOfinalRVarMp _bindOforQuantRVarMp _bindOgUniq _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOknTy _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOopts _bindOrvarMp _bindOwhatTo 
              ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsIgUniq,_fldAnnsInmL,_fldAnnsIoTrf,_fldAnnsIqualS,_fldAnnsIrvarMp) =
                  fldAnns_ _fldAnnsOboundRelevTyVarS _fldAnnsOdataGam _fldAnnsOenv _fldAnnsOfinalRVarMp _fldAnnsOgUniq _fldAnnsOknTy _fldAnnsOknTyCase _fldAnnsOlamMp _fldAnnsOlev _fldAnnsOopts _fldAnnsOrvarMp _fldAnnsOwhatTo 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         patFldTyL            : [(RelevTy,RelevTy)]
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = UIDS ->
                   DataGam ->
                   REnv ->
                   RVarMp ->
                   UID ->
                   RelevTy ->
                   RelevTy ->
                   LamMp ->
                   Int ->
                   EHCOpts ->
                   ([(RelevTy,RelevTy)]) ->
                   RVarMp ->
                   ([WhatToRelevInfer]) ->
                   ( CPatFldL ,([HsName]),FvS,UID,([HsName]),CPatFldL ,([(RelevTy,RelevTy)]),RelevQualS,RVarMp)
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPatFldL 
              _lhsOoTrf :: CPatFldL 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              _hdOboundRelevTyVarS :: UIDS
              _hdOdataGam :: DataGam
              _hdOenv :: REnv
              _hdOfinalRVarMp :: RVarMp
              _hdOgUniq :: UID
              _hdOknTy :: RelevTy
              _hdOknTyCase :: RelevTy
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOopts :: EHCOpts
              _hdOpatFldTyL :: ([(RelevTy,RelevTy)])
              _hdOrvarMp :: RVarMp
              _hdOwhatTo :: ([WhatToRelevInfer])
              _tlOboundRelevTyVarS :: UIDS
              _tlOdataGam :: DataGam
              _tlOenv :: REnv
              _tlOfinalRVarMp :: RVarMp
              _tlOgUniq :: UID
              _tlOknTy :: RelevTy
              _tlOknTyCase :: RelevTy
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOopts :: EHCOpts
              _tlOpatFldTyL :: ([(RelevTy,RelevTy)])
              _tlOrvarMp :: RVarMp
              _tlOwhatTo :: ([WhatToRelevInfer])
              _hdIcTrf :: CPatFld 
              _hdIfldNmL :: ([HsName])
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdInmL :: ([HsName])
              _hdIoTrf :: CPatFld 
              _hdIpatFldTyL :: ([(RelevTy,RelevTy)])
              _hdIqualS :: RelevQualS
              _hdIrvarMp :: RVarMp
              _tlIcTrf :: CPatFldL 
              _tlIfldNmL :: ([HsName])
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlInmL :: ([HsName])
              _tlIoTrf :: CPatFldL 
              _tlIpatFldTyL :: ([(RelevTy,RelevTy)])
              _tlIqualS :: RelevQualS
              _tlIrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _hdIfldNmL ++ _tlIfldNmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _hdIqualS `Set.union` _tlIqualS
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _oTrf =
                  (:) _hdIoTrf _tlIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOpatFldTyL =
                  _tlIpatFldTyL
              -- copy rule (up)
              _lhsOrvarMp =
                  _tlIrvarMp
              -- copy rule (down)
              _hdOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _hdOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (down)
              _hdOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _hdOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (down)
              _hdOrvarMp =
                  _lhsIrvarMp
              -- copy rule (down)
              _hdOwhatTo =
                  _lhsIwhatTo
              -- copy rule (down)
              _tlOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _tlOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _tlOenv =
                  _lhsIenv
              -- copy rule (down)
              _tlOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _tlOknTyCase =
                  _lhsIknTyCase
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOpatFldTyL =
                  _hdIpatFldTyL
              -- copy rule (chain)
              _tlOrvarMp =
                  _hdIrvarMp
              -- copy rule (down)
              _tlOwhatTo =
                  _lhsIwhatTo
              ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdIgUniq,_hdInmL,_hdIoTrf,_hdIpatFldTyL,_hdIqualS,_hdIrvarMp) =
                  hd_ _hdOboundRelevTyVarS _hdOdataGam _hdOenv _hdOfinalRVarMp _hdOgUniq _hdOknTy _hdOknTyCase _hdOlamMp _hdOlev _hdOopts _hdOpatFldTyL _hdOrvarMp _hdOwhatTo 
              ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlIgUniq,_tlInmL,_tlIoTrf,_tlIpatFldTyL,_tlIqualS,_tlIrvarMp) =
                  tl_ _tlOboundRelevTyVarS _tlOdataGam _tlOenv _tlOfinalRVarMp _tlOgUniq _tlOknTy _tlOknTyCase _tlOlamMp _tlOlev _tlOopts _tlOpatFldTyL _tlOrvarMp _tlOwhatTo 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIpatFldTyL
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPatFldL 
              _lhsOoTrf :: CPatFldL 
              _lhsOgUniq :: UID
              _lhsOpatFldTyL :: ([(RelevTy,RelevTy)])
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _oTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOpatFldTyL =
                  _lhsIpatFldTyL
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOpatFldTyL,_lhsOqualS,_lhsOrvarMp)))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         finalRVarMp          : RVarMp
         knTy                 : RelevTy
         knTyCase             : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Empty:
         visit 0:
            local cTrf        : _
            local oTrf        : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = UIDS ->
                   DataGam ->
                   REnv ->
                   RVarMp ->
                   UID ->
                   RelevTy ->
                   RelevTy ->
                   LamMp ->
                   Int ->
                   EHCOpts ->
                   RVarMp ->
                   ([WhatToRelevInfer]) ->
                   ( CPatRest ,FvS,UID,([HsName]),CPatRest ,RelevQualS,RVarMp)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOnmL :: ([HsName])
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPatRest 
              _lhsOoTrf :: CPatRest 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPatRest_Empty
              -- self rule
              _oTrf =
                  CPatRest_Empty
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIknTy
       _lhsIknTyCase
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: CPatRest 
              _lhsOoTrf :: CPatRest 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 25, column 17)
              _lhsOnmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  CPatRest_Var nm_
              -- self rule
              _oTrf =
                  CPatRest_Var nm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOnmL,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : CModule 
         gathLamMp            : LamMp
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local whatTo      : _
            local howUnionGathLamInfo : _
            local howMergeLamInfo : _
            local gUniq       : _
            local env         : _
            local rvarMp      : _
            local gathLamMp   : _
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = DataGam ->
                    LamMp ->
                    EHCOpts ->
                    ( CModule ,LamMp)
data Inh_CodeAGItf  = Inh_CodeAGItf {dataGam_Inh_CodeAGItf :: !(DataGam),lamMp_Inh_CodeAGItf :: !(LamMp),opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule ),gathLamMp_Syn_CodeAGItf :: !(LamMp)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIdataGam _lhsIlamMp _lhsIopts )  =
    (let ( _lhsOcTrf,_lhsOgathLamMp) = sem _lhsIdataGam _lhsIlamMp _lhsIopts 
     in  (Syn_CodeAGItf _lhsOcTrf _lhsOgathLamMp ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIopts ->
         (let _moduleOfinalRVarMp :: RVarMp
              _moduleOlev :: Int
              _moduleOlamMp :: LamMp
              _lhsOcTrf :: CModule 
              _lhsOgathLamMp :: LamMp
              _moduleOdataGam :: DataGam
              _moduleOenv :: REnv
              _moduleOgUniq :: UID
              _moduleOopts :: EHCOpts
              _moduleOrvarMp :: RVarMp
              _moduleOwhatTo :: ([WhatToRelevInfer])
              _moduleIcTrf :: CModule 
              _moduleIfvS :: FvS
              _moduleIgUniq :: UID
              _moduleIgathLamMp :: LamMp
              _moduleIqualS :: RelevQualS
              _moduleIrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 101, column 17)
              _whatTo =
                  case optimizeOptionStictnessAnalysisQuant (ehcOptOptimizeOptionMp _lhsIopts) of
                    OptimizeOptionValue_StrictnessAnalysis_NoQuant          -> [                                                    ]
                    OptimizeOptionValue_StrictnessAnalysis_QuantInstantiate -> [ WhatToRelevInfer_Quant, WhatToRelevInfer_InstToBot ]
                    _                                                       -> [ WhatToRelevInfer_Quant                             ]
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 107, column 17)
              _howUnionGathLamInfo =
                  id
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 111, column 17)
              _howMergeLamInfo =
                  (\(LamInfo {laminfoBindAspMp=m}) i -> i {laminfoBindAspMp = m `Map.union` laminfoBindAspMp i})
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 143, column 17)
              _gUniq =
                  uidStart
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 150, column 17)
              _env =
                  emptyGam
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 372, column 17)
              _rvarMp =
                  emptyRVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 398, column 17)
              _moduleOfinalRVarMp =
                  _moduleIrvarMp
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 4, column 17)
              _moduleOlev =
                  cLevModule
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 15, column 17)
              _gathLamMp =
                  lamMpMergeInto _howMergeLamInfo const _moduleIgathLamMp _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 16, column 17)
              _moduleOlamMp =
                  _howUnionGathLamInfo _lhsIlamMp
              -- copy rule (up)
              _lhsOcTrf =
                  _moduleIcTrf
              -- copy rule (from local)
              _lhsOgathLamMp =
                  _gathLamMp
              -- copy rule (down)
              _moduleOdataGam =
                  _lhsIdataGam
              -- copy rule (from local)
              _moduleOenv =
                  _env
              -- copy rule (from local)
              _moduleOgUniq =
                  _gUniq
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              -- copy rule (from local)
              _moduleOrvarMp =
                  _rvarMp
              -- copy rule (from local)
              _moduleOwhatTo =
                  _whatTo
              ( _moduleIcTrf,_moduleIfvS,_moduleIgUniq,_moduleIgathLamMp,_moduleIqualS,_moduleIrvarMp) =
                  module_ _moduleOdataGam _moduleOenv _moduleOfinalRVarMp _moduleOgUniq _moduleOlamMp _moduleOlev _moduleOopts _moduleOrvarMp _moduleOwhatTo 
          in  ( _lhsOcTrf,_lhsOgathLamMp)))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         boundRelevTyVarS     : UIDS
         dataGam              : DataGam
         env                  : REnv
         evalCtx              : EvalCtx
         finalRVarMp          : RVarMp
         isLamBody            : Bool
         isStrict             : Bool
         knTy                 : RelevTy
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
         whatTo               : [WhatToRelevInfer]
      chained attributes:
         gUniq                : UID
         rvarMp               : RVarMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         oTrf                 : SELF 
         qualS                : RelevQualS
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local altMbScrutTy : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local oTrf        : _
      alternative Nothing:
         visit 0:
            local altMbScrutTy : _
            local cTrf        : _
            local oTrf        : _
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = UIDS ->
                  DataGam ->
                  REnv ->
                  EvalCtx ->
                  RVarMp ->
                  UID ->
                  Bool ->
                  Bool ->
                  RelevTy ->
                  LamMp ->
                  Int ->
                  EHCOpts ->
                  RVarMp ->
                  ([WhatToRelevInfer]) ->
                  ( MbCExpr ,FvS,UID,MbCExpr ,RelevQualS,RVarMp)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _justOisTopApp :: Bool
              _justOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: MbCExpr 
              _lhsOoTrf :: MbCExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              _justOaltMbScrutTy :: MbRelevTy
              _justOboundRelevTyVarS :: UIDS
              _justOdataGam :: DataGam
              _justOenv :: REnv
              _justOevalCtx :: EvalCtx
              _justOfinalRVarMp :: RVarMp
              _justOgUniq :: UID
              _justOisLamBody :: Bool
              _justOisStrict :: Bool
              _justOknTy :: RelevTy
              _justOlamMp :: LamMp
              _justOlev :: Int
              _justOopts :: EHCOpts
              _justOrvarMp :: RVarMp
              _justOwhatAbove :: WhatExpr
              _justOwhatTo :: ([WhatToRelevInfer])
              _justIappFunKind :: AppFunKind
              _justIargL :: ([CBound])
              _justIcTrf :: CExpr 
              _justIcoe :: RelevCoe
              _justIfunCoe :: RelevCoe
              _justIfvS :: FvS
              _justIgUniq :: UID
              _justIgathLamMp :: LamMp
              _justImbFFIApp :: (Maybe ( Ty
                                                                   , Bool
                                                                   , FFIWay
                                                                   , ForeignEnt
                                                                   , [Ty]
                                                                   ))
              _justImbFunVar :: (Maybe HsName)
              _justImbLam :: (Maybe [HsName])
              _justImbVar :: (Maybe HsName)
              _justIoTrf :: CExpr 
              _justIqualS :: RelevQualS
              _justIrvarMp :: RVarMp
              _justIty :: RelevTy
              _justIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 88, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _justIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  _justIqualS
              -- self rule
              _cTrf =
                  Just _justIcTrf
              -- self rule
              _oTrf =
                  Just _justIoTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOrvarMp =
                  _justIrvarMp
              -- copy rule (from local)
              _justOaltMbScrutTy =
                  _altMbScrutTy
              -- copy rule (down)
              _justOboundRelevTyVarS =
                  _lhsIboundRelevTyVarS
              -- copy rule (down)
              _justOdataGam =
                  _lhsIdataGam
              -- copy rule (down)
              _justOenv =
                  _lhsIenv
              -- copy rule (down)
              _justOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _justOfinalRVarMp =
                  _lhsIfinalRVarMp
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _justOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _justOknTy =
                  _lhsIknTy
              -- copy rule (down)
              _justOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _justOlev =
                  _lhsIlev
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOrvarMp =
                  _lhsIrvarMp
              -- copy rule (from local)
              _justOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _justOwhatTo =
                  _lhsIwhatTo
              ( _justIappFunKind,_justIargL,_justIcTrf,_justIcoe,_justIfunCoe,_justIfvS,_justIgUniq,_justIgathLamMp,_justImbFFIApp,_justImbFunVar,_justImbLam,_justImbVar,_justIoTrf,_justIqualS,_justIrvarMp,_justIty,_justIwhatBelow) =
                  just_ _justOaltMbScrutTy _justOboundRelevTyVarS _justOdataGam _justOenv _justOevalCtx _justOfinalRVarMp _justOgUniq _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOknTy _justOlamMp _justOlev _justOopts _justOrvarMp _justOwhatAbove _justOwhatTo 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIboundRelevTyVarS
       _lhsIdataGam
       _lhsIenv
       _lhsIevalCtx
       _lhsIfinalRVarMp
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIknTy
       _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIrvarMp
       _lhsIwhatTo ->
         (let _lhsOfvS :: FvS
              _lhsOqualS :: RelevQualS
              _lhsOcTrf :: MbCExpr 
              _lhsOoTrf :: MbCExpr 
              _lhsOgUniq :: UID
              _lhsOrvarMp :: RVarMp
              -- "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 310, column 17)
              _altMbScrutTy =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/AnaRelevance.ag"(line 414, column 28)
              _lhsOqualS =
                  Set.empty
              -- self rule
              _cTrf =
                  Nothing
              -- self rule
              _oTrf =
                  Nothing
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOoTrf =
                  _oTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOrvarMp =
                  _lhsIrvarMp
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOoTrf,_lhsOqualS,_lhsOrvarMp)))