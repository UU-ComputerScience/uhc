

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Subst.ag)
module EH101.Core.Subst(CSubst
, cSubstAppExpr
, cAppCoeArg
, coeEvalOnAsSubst
, coeWipeWeaveAsSubst2
, mkLamBodyCoe
, lrcoeWipeWeaveAsSubst, lrcoeForLamTyAppAsSubst) where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Opts.Base
import EH101.Base.Common
import EH101.Ty
import EH101.Core
import EH101.VarMp
import EH101.Core.Pretty
import EH101.Core.FvS
import EH101.AbstractCore
import EH101.Core.Coercion









type CSubstInfo = CSubstInfo' CExpr CMetaVal CBind CBound Ty
type CSubst     = CSubst'     CExpr CMetaVal CBind CBound Ty



cSubstAppExpr :: Bool -> CSubst -> CExpr -> Maybe CExpr -> CExpr
cSubstAppExpr doDeepSubst cs ce mbOnCe
  = cRepl_Syn_CExpr t
  where t = wrap_CExpr
              (sem_CExpr ce)
              (Inh_CExpr { cSubst_Inh_CExpr = cs
                         , coeArg_Inh_CExpr = maybe CExpr_CoeArg id mbOnCe
                         , doDeepSubst_Inh_CExpr = doDeepSubst
                         })



cAppCoeArg :: CExpr -> CExpr -> CExpr
cAppCoeArg ce coeArg
  = cSubstAppExpr False emptyCSubst ce (Just coeArg)



instance CSubstitutable CExpr CMetaVal CBind CBound Ty CExpr where
  cSubstApp cs ce | Map.null cs
    =  ce
  cSubstApp cs ce
    = cSubstAppExpr False cs ce Nothing



cStopSubst = CExpr_String "Core.Subst.cStopSubst: may not happen"



coeEvalOnAsSubst :: UID -> Coe -> CExpr -> (CExpr,CSubst)
coeEvalOnAsSubst uniq coe ce
  = (c,s)
  where (_,c,s) = ev uniq coe ce
        ev uniq coe ce
          = case coe of
              c | acoreCoeIsId c-> mk ce
              Coe_Map  f        -> mk $ f ce
              Coe_App1 a        -> mk $ acoreApp1 ce a
              Coe_Lam n   _     -> mk $ acoreLam1 n ce
              Coe_LamLet n _ i  -> mk $ n `acoreLam1` acoreHoleLet i ce
              Coe_LetRec b      -> mk $ acoreLetRec b ce
              Coe_Compose c1 c2 -> (u2, c1', s2 `cSubstAppSubst` s1)
                                where (u1,c2',s1) = ev uniq c2 ce
                                      (u2,c1',s2) = ev u1   c1 c2'
              Coe_C e           -> (u', e `cAppCoeArg` acoreUidHole u, acoreCSubstFromUidExprL [(u,ce)])
                                where (u',u) = mkNewUID uniq
              Coe_ImplApp iv    -> mk $ CExpr_ImplsApp ce iv
              Coe_ImplLam iv    -> mk $ CExpr_ImplsLam iv ce
          where mk c = (uniq,c,emptyCSubst)



coeEvalOn :: Coe -> CExpr -> CExpr
coeEvalOn coe ce
  = s `cSubstApp` ce'
  where (ce',s) = coeEvalOnAsSubst uidStart coe ce



coeWeaveOnAsSubst :: UID -> [Coe] -> [Coe] -> CExpr -> (CExpr,CSubst)
coeWeaveOnAsSubst = coeWeaveWithSubstOnAsSubst emptyCSubst

coeWeaveWithSubstOnAsSubst :: CSubst -> UID -> [Coe] -> [Coe] -> CExpr -> (CExpr,CSubst)
coeWeaveWithSubstOnAsSubst cs uniq lCoeL rCoeL ce
  = snd $ foldr ev (foldr ev (uniq,(ce,emptyCSubst)) (reverse lCoeL)) rCoeL
  where ev c (uniq,(e,s)) = (u',(cs `cSubstApp` e',s' `cSubstAppSubst` s))
          where (u',u ) = mkNewUID uniq
                (e',s') = coeEvalOnAsSubst u c e



coeWeaveOn2 :: CSubst -> [Coe] -> [Coe] -> CExpr -> CExpr
coeWeaveOn2 cs lCoeL rCoeL ce
  = cSubstAppExpr True s e Nothing
  where (e,s) = coeWeaveWithSubstOnAsSubst cs uidStart lCoeL rCoeL ce



coeWipe :: [Coe] -> [Coe] -> ([Coe],[Coe])
coeWipe l r
  = (reverse l', reverse r')
  where w l r =  case lr of
                   (Coe_ImplApp li:ls,Coe_ImplLam ri:rs)
                                  | li == ri   -> w ls rs
                                  | otherwise  -> lr
                   _                           -> lr
              where lr = (l,r)
        (l',r') = w (reverse l) (reverse r)



coeWipeWeaveAsSubst :: EHCOpts -> UID -> VarMp -> [Coe] -> [Coe] -> (Coe,CSubst)
coeWipeWeaveAsSubst opts uniq c lCoeL rCoeL
  = (Coe_C e,s)
  where (lCoeL',rCoeL') = coeWipe (concatMap (coeImplsAppLVarMp opts c) lCoeL) (concatMap (coeImplsAppRVarMp c) rCoeL)
        (e,s) = coeWeaveOnAsSubst uniq lCoeL' rCoeL' CExpr_CoeArg

coeWipeWeaveAsSubst2 :: EHCOpts -> UID -> VarMp -> CSubst -> [Coe] -> [Coe] -> (Coe,CSubst)
coeWipeWeaveAsSubst2 opts uniq c cs lCoeL rCoeL
  = (Coe_C e,s)
  where (lCoeL',rCoeL') = coeWipe (concatMap (coeImplsAppLVarMp opts c) lCoeL) (concatMap (coeImplsAppRVarMp c) rCoeL)
        (e,s) = coeWeaveWithSubstOnAsSubst cs uniq lCoeL' rCoeL' CExpr_CoeArg



coeImplsAppLVarMp :: EHCOpts -> VarMp -> Coe -> [Coe]
coeImplsAppLVarMp opts c coe
  =  case coe of
       Coe_ImplApp i  -> maybe [coe] (acoreCoeImplsApp) (varmpImplsLookupCyc i c)
       _              -> [coe]

coeImplsAppRVarMp :: VarMp -> Coe -> [Coe]
coeImplsAppRVarMp c coe
  =  case coe of
       Coe_ImplLam i  -> maybe [coe] (acoreCoeImplsLam acoreCoeId) (varmpImplsLookupCyc i c)
       _              -> [coe]

mkLamBodyCoe :: Coe -> [Coe] -> [Coe]
mkLamBodyCoe onLast l
  =  case l of
       (_:_)              -> h ++ [onLast `acoreCoeCompose` t]
                          where h = init l
                                t = last l
       _ | acoreCoeIsId onLast -> []
         | otherwise           -> [onLast]



instance PP Coe where
  pp c = "<" >|< pp (fst $ coeEvalOnAsSubst uidStart c CExpr_CoeArg) >|< ">"



lrcoeWipeWeaveAsSubst :: EHCOpts -> UID -> VarMp -> LRCoe -> (Coe,CSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr (LRCoe LRCoeId _ _) = (acoreCoeId,emptyCSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr lrcoe               = coeWipeWeaveAsSubst opts uniq cnstr (lrcoeLeftL lrcoe) (lrcoeRightL lrcoe)

lrcoeForLamTyAppAsSubst :: EHCOpts -> UID -> LRCoe -> LRCoe -> (LRCoe,CSubst)
lrcoeForLamTyAppAsSubst opts uniq f a
  = (LRCoe k [l] [r] `lrcoeUnion` a, s)
  where (u',u1,u2,u3) = mkNewLevUID3 uniq
        n = uidHNm u1
        r = acoreCoeLam1 n
        (k,l,s)
          = case f of
              lr@(LRCoe LRCoeOther _ _)
                -> (lrcoeKindOfCoe c, acoreCoeAppN [a], cSubstAppSubst s1 s2)
                where (c,s1) = lrcoeWipeWeaveAsSubst opts u2 emptyVarMp lr
                      (a,s2) = coeEvalOnAsSubst u3 c (acoreVar n)
              LRCoe LRCoeId _ _
                -> (LRCoeId, l, emptyCSubst)
                where l = acoreCoeAppN [acoreVar n]


-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = CSubst ->
               CExpr  ->
               Bool ->
               ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _patOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _patOcoeArg ->
              (case (_lhsIcSubst) of
               { _patOcSubst ->
               (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
                { ( _exprIcRepl) ->
                    (case (pat_ _patOcSubst _patOcoeArg _patOdoDeepSubst ) of
                     { ( _patIcRepl) ->
                         (case (CAlt_Alt _patIcRepl _exprIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local cRepl       : _
      alternative Nil:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = CSubst ->
                CExpr  ->
                Bool ->
                ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _tlOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _tlOcoeArg ->
           (case (_lhsIcSubst) of
            { _tlOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _hdOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _hdOcoeArg ->
              (case (_lhsIcSubst) of
               { _hdOcSubst ->
               (case (tl_ _tlOcSubst _tlOcoeArg _tlOdoDeepSubst ) of
                { ( _tlIcRepl) ->
                    (case (hd_ _hdOcSubst _hdOcoeArg _hdOdoDeepSubst ) of
                     { ( _hdIcRepl) ->
                         (case ((:) _hdIcRepl _tlIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ([]) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = CSubst ->
                CExpr  ->
                Bool ->
                ( CBind )
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bindAspectsOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bindAspectsOcoeArg ->
           (case (_lhsIcSubst) of
            { _bindAspectsOcSubst ->
            (case (bindAspects_ _bindAspectsOcSubst _bindAspectsOcoeArg _bindAspectsOdoDeepSubst ) of
             { ( _bindAspectsIcRepl) ->
                 (case (CBind_Bind nm_ _bindAspectsIcRepl) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = CSubst ->
                   CExpr  ->
                   Bool ->
                   ( CBindAnn )
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CBindAnn_Coe coe_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local cRepl       : _
      alternative Nil:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = CSubst ->
                    CExpr  ->
                    Bool ->
                    ( CBindAnnL )
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _tlOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _tlOcoeArg ->
           (case (_lhsIcSubst) of
            { _tlOcSubst ->
            (case (tl_ _tlOcSubst _tlOcoeArg _tlOdoDeepSubst ) of
             { ( _tlIcRepl) ->
                 (case (_lhsIdoDeepSubst) of
                  { _hdOdoDeepSubst ->
                  (case (_lhsIcoeArg) of
                   { _hdOcoeArg ->
                   (case (_lhsIcSubst) of
                    { _hdOcSubst ->
                    (case (hd_ _hdOcSubst _hdOcoeArg _hdOdoDeepSubst ) of
                     { ( _hdIcRepl) ->
                         (case ((:) _hdIcRepl _tlIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ([]) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local cRepl       : _
      alternative Nil:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = CSubst ->
                 CExpr  ->
                 Bool ->
                 ( CBindL )
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _tlOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _tlOcoeArg ->
           (case (_lhsIcSubst) of
            { _tlOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _hdOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _hdOcoeArg ->
              (case (_lhsIcSubst) of
               { _hdOcSubst ->
               (case (tl_ _tlOcSubst _tlOcoeArg _tlOdoDeepSubst ) of
                { ( _tlIcRepl) ->
                    (case (hd_ _hdOcSubst _hdOcoeArg _hdOdoDeepSubst ) of
                     { ( _hdIcRepl) ->
                         (case ((:) _hdIcRepl _tlIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ([]) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local cRepl       : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local cRepl       : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local cRepl       : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local cRepl       : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local cRepl       : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local cRepl       : _
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
type T_CBound  = CSubst ->
                 CExpr  ->
                 Bool ->
                 ( CBound )
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
             { ( _exprIcRepl) ->
                 (case (_lhsIdoDeepSubst) of
                  { _bindMetaOdoDeepSubst ->
                  (case (_lhsIcoeArg) of
                   { _bindMetaOcoeArg ->
                   (case (_lhsIcSubst) of
                    { _bindMetaOcSubst ->
                    (case (bindMeta_ _bindMetaOcSubst _bindMetaOcoeArg _bindMetaOdoDeepSubst ) of
                     { ( _bindMetaIcRepl) ->
                         (case (CBound_Bind _bindMetaIcRepl _exprIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
             { ( _exprIcRepl) ->
                 (case (CBound_FFE callconv_ expEnt_ _exprIcRepl ty_) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _cmetasOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _cmetasOcoeArg ->
           (case (_lhsIcSubst) of
            { _cmetasOcSubst ->
            (case (cmetas_ _cmetasOcSubst _cmetasOcoeArg _cmetasOdoDeepSubst ) of
             { ( _cmetasIcRepl) ->
                 (case (CBound_Meta aspectKeyS_ _cmetasIcRepl) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CBound_RelevTy aspectKeyS_ relevTy_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CBound_Ty aspectKeyS_ ty_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
             { ( _exprIcRepl) ->
                 (case (CBound_Val aspectKeyS_ _exprIcRepl) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local cRepl       : _
      alternative Nil:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = CSubst ->
                  CExpr  ->
                  Bool ->
                  ( CBoundL )
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _tlOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _tlOcoeArg ->
           (case (_lhsIcSubst) of
            { _tlOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _hdOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _hdOcoeArg ->
              (case (_lhsIcSubst) of
               { _hdOcSubst ->
               (case (tl_ _tlOcSubst _tlOcoeArg _tlOdoDeepSubst ) of
                { ( _tlIcRepl) ->
                    (case (hd_ _hdOcSubst _hdOcoeArg _hdOdoDeepSubst ) of
                     { ( _hdIcRepl) ->
                         (case ((:) _hdIcRepl _tlIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ([]) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cRepl       : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local cRepl       : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local cRepl       : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local cRepl       : _
            local _tup1       : _
            local uid         : _
            local canSubst    : _
            local _tup2       : _
            local isRepl      : _
            local replv       : _
            inst  repl'       : CExpr 
      alternative Char:
         child char           : {Char}
         visit 0:
            local cRepl       : _
      alternative CoeArg:
         visit 0:
            local _tup3       : _
            local replv       : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local cRepl       : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local cRepl       : _
            local _tup4       : _
            local isRepl      : _
            local replv       : _
            inst  repl'       : CExpr 
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local cRepl       : _
            local _tup5       : _
            local isRepl      : _
            local replv       : _
            inst  repl'       : CExpr 
            local uid         : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local _tup6       : _
            local coeAppL     : _
            local replv       : _
            local isRepl      : _
            inst  repl'       : CExpr 
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local _tup7       : _
            local coeLamL     : _
            local replv       : _
            local isRepl      : _
            inst  repl'       : CExpr 
      alternative Int:
         child int            : {Int}
         visit 0:
            local cRepl       : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local cRepl       : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local cRepl       : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local cRepl       : _
      alternative String:
         child str            : {String}
         visit 0:
            local cRepl       : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local cRepl       : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local cRepl       : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cRepl       : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cRepl       : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local cRepl       : _
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
type T_CExpr  = CSubst ->
                CExpr  ->
                Bool ->
                ( CExpr )
data Inh_CExpr  = Inh_CExpr {cSubst_Inh_CExpr :: !(CSubst),coeArg_Inh_CExpr :: !(CExpr ),doDeepSubst_Inh_CExpr :: !(Bool)}
data Syn_CExpr  = Syn_CExpr {cRepl_Syn_CExpr :: !(CExpr )}
wrap_CExpr :: T_CExpr  ->
              Inh_CExpr  ->
              Syn_CExpr 
wrap_CExpr sem (Inh_CExpr _lhsIcSubst _lhsIcoeArg _lhsIdoDeepSubst )  =
    (let ( _lhsOcRepl) = sem _lhsIcSubst _lhsIcoeArg _lhsIdoDeepSubst 
     in  (Syn_CExpr _lhsOcRepl ))
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
             { ( _exprIcRepl) ->
                 (case (_lhsIdoDeepSubst) of
                  { _annOdoDeepSubst ->
                  (case (_lhsIcoeArg) of
                   { _annOcoeArg ->
                   (case (_lhsIcSubst) of
                    { _annOcSubst ->
                    (case (ann_ _annOcSubst _annOcoeArg _annOdoDeepSubst ) of
                     { ( _annIcRepl) ->
                         (case (CExpr_Ann _annIcRepl _exprIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _argOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _argOcoeArg ->
           (case (_lhsIcSubst) of
            { _argOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _funcOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _funcOcoeArg ->
              (case (_lhsIcSubst) of
               { _funcOcSubst ->
               (case (arg_ _argOcSubst _argOcoeArg _argOdoDeepSubst ) of
                { ( _argIcRepl) ->
                    (case (func_ _funcOcSubst _funcOcoeArg _funcOdoDeepSubst ) of
                     { ( _funcIcRepl) ->
                         (case (CExpr_App _funcIcRepl _argIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _dfltOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _dfltOcoeArg ->
           (case (_lhsIcSubst) of
            { _dfltOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _altsOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _altsOcoeArg ->
              (case (_lhsIcSubst) of
               { _altsOcSubst ->
               (case (_lhsIdoDeepSubst) of
                { _exprOdoDeepSubst ->
                (case (_lhsIcoeArg) of
                 { _exprOcoeArg ->
                 (case (_lhsIcSubst) of
                  { _exprOcSubst ->
                  (case (dflt_ _dfltOcSubst _dfltOcoeArg _dfltOdoDeepSubst ) of
                   { ( _dfltIcRepl) ->
                       (case (alts_ _altsOcSubst _altsOcoeArg _altsOdoDeepSubst ) of
                        { ( _altsIcRepl) ->
                            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
                             { ( _exprIcRepl) ->
                                 (case (CExpr_Case _exprIcRepl _altsIcRepl _dfltIcRepl) of
                                  { _cRepl ->
                                  (case (_cRepl) of
                                   { _lhsOcRepl ->
                                   ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _errorExprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _errorExprOcoeArg ->
           (case (_lhsIcSubst) of
            { _errorExprOcSubst ->
            (case (errorExpr_ _errorExprOcSubst _errorExprOcoeArg _errorExprOdoDeepSubst ) of
             { ( _errorExprIcRepl) ->
                 (case (CExpr_CaseAltFail failReason_ _errorExprIcRepl) of
                  { _cRepl ->
                  (case (cafailHasId failReason_) of
                   { __tup1 ->
                   (case (__tup1) of
                    { (_,_uid) ->
                    (case (__tup1) of
                     { (_canSubst,_) ->
                     (case (if _canSubst
                            then case Map.lookup (CSKey_UID _uid) _lhsIcSubst of
                                   Just (CSIExpr ce)  -> (ce,True)
                                   _                  -> (_cRepl,False)
                            else (_cRepl,False)) of
                      { __tup2 ->
                      (case (__tup2) of
                       { (_,_isRepl) ->
                       (case (__tup2) of
                        { (_replv,_) ->
                        (case (if _lhsIdoDeepSubst && _isRepl then _replv else cStopSubst) of
                         { repl'_val_ ->
                         (case ((sem_CExpr repl'_val_ )) of
                          { repl'_inst_ ->
                          (case (_lhsIdoDeepSubst) of
                           { _repl'OdoDeepSubst ->
                           (case (_lhsIcoeArg) of
                            { _repl'OcoeArg ->
                            (case (Map.delete (CSKey_UID _uid) _lhsIcSubst) of
                             { _repl'OcSubst ->
                             (case (repl'_inst_ _repl'OcSubst _repl'OcoeArg _repl'OdoDeepSubst ) of
                              { ( _repl'IcRepl) ->
                                  (case (if _lhsIdoDeepSubst && _isRepl then _repl'IcRepl else _replv) of
                                   { _lhsOcRepl ->
                                   ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Char char_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ((_lhsIcoeArg,True)) of
          { __tup3 ->
          (case (__tup3) of
           { (_replv,_) ->
           (case (_replv) of
            { _lhsOcRepl ->
            ( _lhsOcRepl) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Hole uid_) of
          { _cRepl ->
          (case (case Map.lookup (CSKey_UID uid_) _lhsIcSubst of
                   Just (CSIExpr ce)  -> (ce,True)
                   _                  -> (_cRepl,False)) of
           { __tup4 ->
           (case (__tup4) of
            { (_,_isRepl) ->
            (case (__tup4) of
             { (_replv,_) ->
             (case (if _lhsIdoDeepSubst && _isRepl then _replv else cStopSubst) of
              { repl'_val_ ->
              (case ((sem_CExpr repl'_val_ )) of
               { repl'_inst_ ->
               (case (_lhsIdoDeepSubst) of
                { _repl'OdoDeepSubst ->
                (case (_lhsIcoeArg) of
                 { _repl'OcoeArg ->
                 (case (Map.delete (CSKey_UID uid_) _lhsIcSubst) of
                  { _repl'OcSubst ->
                  (case (repl'_inst_ _repl'OcSubst _repl'OcoeArg _repl'OdoDeepSubst ) of
                   { ( _repl'IcRepl) ->
                       (case (if _lhsIdoDeepSubst && _isRepl then _repl'IcRepl else _replv) of
                        { _lhsOcRepl ->
                        ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bodyOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bodyOcoeArg ->
           (case (_lhsIcSubst) of
            { _bodyOcSubst ->
            (case (body_ _bodyOcSubst _bodyOcoeArg _bodyOdoDeepSubst ) of
             { ( _bodyIcRepl) ->
                 (case (CExpr_HoleLet bindsUid_ _bodyIcRepl) of
                  { _cRepl ->
                  (case (case Map.lookup (CSKey_UID bindsUid_) _lhsIcSubst of
                           Just (CSIBinds b)  -> (_lhsIcSubst `cSubstApp` acoreLetRec b _bodyIcRepl,True)
                           _                  -> (_cRepl,False)) of
                   { __tup5 ->
                   (case (__tup5) of
                    { (_,_isRepl) ->
                    (case (__tup5) of
                     { (_replv,_) ->
                     (case (if _lhsIdoDeepSubst && _isRepl then _replv else cStopSubst) of
                      { repl'_val_ ->
                      (case ((sem_CExpr repl'_val_ )) of
                       { repl'_inst_ ->
                       (case (_lhsIdoDeepSubst) of
                        { _repl'OdoDeepSubst ->
                        (case (_lhsIcoeArg) of
                         { _repl'OcoeArg ->
                         (case (bindsUid_) of
                          { _uid ->
                          (case (Map.delete (CSKey_UID _uid) _lhsIcSubst) of
                           { _repl'OcSubst ->
                           (case (repl'_inst_ _repl'OcSubst _repl'OcoeArg _repl'OdoDeepSubst ) of
                            { ( _repl'IcRepl) ->
                                (case (if _lhsIdoDeepSubst && _isRepl then _repl'IcRepl else _replv) of
                                 { _lhsOcRepl ->
                                 ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _funcOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _funcOcoeArg ->
           (case (_lhsIcSubst) of
            { _funcOcSubst ->
            (case (case Map.lookup (CSKey_UID uid_) _lhsIcSubst of
                     Just (CSIImpls ca cl)  -> (ca,cl,True)
                     _                      -> ([],[],False)) of
             { __tup6 ->
             (case (__tup6) of
              { (_coeAppL,_,_) ->
              (case (func_ _funcOcSubst _funcOcoeArg _funcOdoDeepSubst ) of
               { ( _funcIcRepl) ->
                   (case (coeWeaveOn2 emptyCSubst _coeAppL [] _funcIcRepl) of
                    { _replv ->
                    (case (__tup6) of
                     { (_,_,_isRepl) ->
                     (case (if _lhsIdoDeepSubst && _isRepl then _replv else cStopSubst) of
                      { repl'_val_ ->
                      (case ((sem_CExpr repl'_val_ )) of
                       { repl'_inst_ ->
                       (case (_lhsIdoDeepSubst) of
                        { _repl'OdoDeepSubst ->
                        (case (_lhsIcoeArg) of
                         { _repl'OcoeArg ->
                         (case (Map.delete (CSKey_UID uid_) _lhsIcSubst) of
                          { _repl'OcSubst ->
                          (case (repl'_inst_ _repl'OcSubst _repl'OcoeArg _repl'OdoDeepSubst ) of
                           { ( _repl'IcRepl) ->
                               (case (if _lhsIdoDeepSubst && _isRepl then _repl'IcRepl else _replv) of
                                { _lhsOcRepl ->
                                ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bodyOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bodyOcoeArg ->
           (case (_lhsIcSubst) of
            { _bodyOcSubst ->
            (case (case Map.lookup (CSKey_UID uid_) _lhsIcSubst of
                     Just (CSIImpls ca cl)  -> (ca,cl,True)
                     _                      -> ([],[],False)) of
             { __tup7 ->
             (case (__tup7) of
              { (_,_coeLamL,_) ->
              (case (body_ _bodyOcSubst _bodyOcoeArg _bodyOdoDeepSubst ) of
               { ( _bodyIcRepl) ->
                   (case (coeWeaveOn2 emptyCSubst [] _coeLamL _bodyIcRepl) of
                    { _replv ->
                    (case (__tup7) of
                     { (_,_,_isRepl) ->
                     (case (if _lhsIdoDeepSubst && _isRepl then _replv else cStopSubst) of
                      { repl'_val_ ->
                      (case ((sem_CExpr repl'_val_ )) of
                       { repl'_inst_ ->
                       (case (_lhsIdoDeepSubst) of
                        { _repl'OdoDeepSubst ->
                        (case (_lhsIcoeArg) of
                         { _repl'OcoeArg ->
                         (case (Map.delete (CSKey_UID uid_) _lhsIcSubst) of
                          { _repl'OcSubst ->
                          (case (repl'_inst_ _repl'OcSubst _repl'OcoeArg _repl'OdoDeepSubst ) of
                           { ( _repl'IcRepl) ->
                               (case (if _lhsIdoDeepSubst && _isRepl then _repl'IcRepl else _replv) of
                                { _lhsOcRepl ->
                                ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Int int_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Integer integer_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bodyOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bodyOcoeArg ->
           (case (_lhsIcSubst) of
            { _bodyOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _bindOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _bindOcoeArg ->
              (case (_lhsIcSubst) of
               { _bindOcSubst ->
               (case (body_ _bodyOcSubst _bodyOcoeArg _bodyOdoDeepSubst ) of
                { ( _bodyIcRepl) ->
                    (case (bind_ _bindOcSubst _bindOcoeArg _bindOdoDeepSubst ) of
                     { ( _bindIcRepl) ->
                         (case (CExpr_Lam _bindIcRepl _bodyIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bodyOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bodyOcoeArg ->
           (case (_lhsIcSubst) of
            { _bodyOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _bindsOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _bindsOcoeArg ->
              (case (_lhsIcSubst) of
               { _bindsOcSubst ->
               (case (body_ _bodyOcSubst _bodyOcoeArg _bodyOdoDeepSubst ) of
                { ( _bodyIcRepl) ->
                    (case (binds_ _bindsOcSubst _bindsOcoeArg _bindsOdoDeepSubst ) of
                     { ( _bindsIcRepl) ->
                         (case (CExpr_Let categ_ _bindsIcRepl _bodyIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_String str_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Tup tag_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _offsetOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _offsetOcoeArg ->
           (case (_lhsIcSubst) of
            { _offsetOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _exprOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _exprOcoeArg ->
              (case (_lhsIcSubst) of
               { _exprOcSubst ->
               (case (offset_ _offsetOcSubst _offsetOcoeArg _offsetOdoDeepSubst ) of
                { ( _offsetIcRepl) ->
                    (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
                     { ( _exprIcRepl) ->
                         (case (CExpr_TupDel _exprIcRepl tag_ nm_ _offsetIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _fldExprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _fldExprOcoeArg ->
           (case (_lhsIcSubst) of
            { _fldExprOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _offsetOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _offsetOcoeArg ->
              (case (_lhsIcSubst) of
               { _offsetOcSubst ->
               (case (_lhsIdoDeepSubst) of
                { _exprOdoDeepSubst ->
                (case (_lhsIcoeArg) of
                 { _exprOcoeArg ->
                 (case (_lhsIcSubst) of
                  { _exprOcSubst ->
                  (case (fldExpr_ _fldExprOcSubst _fldExprOcoeArg _fldExprOdoDeepSubst ) of
                   { ( _fldExprIcRepl) ->
                       (case (offset_ _offsetOcSubst _offsetOcoeArg _offsetOdoDeepSubst ) of
                        { ( _offsetIcRepl) ->
                            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
                             { ( _exprIcRepl) ->
                                 (case (CExpr_TupIns _exprIcRepl tag_ nm_ _offsetIcRepl _fldExprIcRepl) of
                                  { _cRepl ->
                                  (case (_cRepl) of
                                   { _lhsOcRepl ->
                                   ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _fldExprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _fldExprOcoeArg ->
           (case (_lhsIcSubst) of
            { _fldExprOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _offsetOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _offsetOcoeArg ->
              (case (_lhsIcSubst) of
               { _offsetOcSubst ->
               (case (_lhsIdoDeepSubst) of
                { _exprOdoDeepSubst ->
                (case (_lhsIcoeArg) of
                 { _exprOcoeArg ->
                 (case (_lhsIcSubst) of
                  { _exprOcSubst ->
                  (case (fldExpr_ _fldExprOcSubst _fldExprOcoeArg _fldExprOdoDeepSubst ) of
                   { ( _fldExprIcRepl) ->
                       (case (offset_ _offsetOcSubst _offsetOcoeArg _offsetOdoDeepSubst ) of
                        { ( _offsetIcRepl) ->
                            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
                             { ( _exprIcRepl) ->
                                 (case (CExpr_TupUpd _exprIcRepl tag_ nm_ _offsetIcRepl _fldExprIcRepl) of
                                  { _cRepl ->
                                  (case (_cRepl) of
                                   { _lhsOcRepl ->
                                   ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExpr_Var ref_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cRepl       : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local cRepl       : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local cRepl       : _
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
type T_CExprAnn  = CSubst ->
                   CExpr  ->
                   Bool ->
                   ( CExprAnn )
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExprAnn_Coe coe_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExprAnn_Debug info_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CExprAnn_Ty ty_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Apply0:
         visit 0:
            local cRepl       : _
      alternative Function0:
         visit 0:
            local cRepl       : _
      alternative Function1:
         visit 0:
            local cRepl       : _
      alternative Plain:
         visit 0:
            local cRepl       : _
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
type T_CMetaBind  = CSubst ->
                    CExpr  ->
                    Bool ->
                    ( CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaBind_Apply0) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaBind_Function0) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaBind_Function1) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaBind_Plain) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Dict:
         visit 0:
            local cRepl       : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local cRepl       : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local cRepl       : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local cRepl       : _
      alternative Val:
         visit 0:
            local cRepl       : _
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
type T_CMetaVal  = CSubst ->
                   CExpr  ->
                   Bool ->
                   ( CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaVal_Dict) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaVal_DictClass tracks_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaVal_Track track_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CMetaVal_Val) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = CSubst ->
                 CExpr  ->
                 Bool ->
                 ( CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _x2OdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _x2OcoeArg ->
           (case (_lhsIcSubst) of
            { _x2OcSubst ->
            (case (x2_ _x2OcSubst _x2OcoeArg _x2OdoDeepSubst ) of
             { ( _x2IcRepl) ->
                 (case (_lhsIdoDeepSubst) of
                  { _x1OdoDeepSubst ->
                  (case (_lhsIcoeArg) of
                   { _x1OcoeArg ->
                   (case (_lhsIcSubst) of
                    { _x1OcSubst ->
                    (case (x1_ _x1OcSubst _x1OcoeArg _x1OdoDeepSubst ) of
                     { ( _x1IcRepl) ->
                         (case ((_x1IcRepl,_x2IcRepl)) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = CSubst ->
                  CExpr  ->
                  Bool ->
                  ( CModule )
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _exprOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _exprOcoeArg ->
           (case (_lhsIcSubst) of
            { _exprOcSubst ->
            (case (expr_ _exprOcSubst _exprOcoeArg _exprOdoDeepSubst ) of
             { ( _exprIcRepl) ->
                 (case (CModule_Mod moduleNm_ _exprIcRepl ctagsMp_) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local cRepl       : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cRepl       : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local cRepl       : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cRepl       : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local cRepl       : _
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
type T_CPat  = CSubst ->
               CExpr  ->
               Bool ->
               ( CPat )
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPat_BoolExpr cexpr_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPat_Char char_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bindsOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bindsOcoeArg ->
           (case (_lhsIcSubst) of
            { _bindsOcSubst ->
            (case (binds_ _bindsOcSubst _bindsOcoeArg _bindsOdoDeepSubst ) of
             { ( _bindsIcRepl) ->
                 (case (_lhsIdoDeepSubst) of
                  { _restOdoDeepSubst ->
                  (case (_lhsIcoeArg) of
                   { _restOcoeArg ->
                   (case (_lhsIcSubst) of
                    { _restOcSubst ->
                    (case (rest_ _restOcSubst _restOcoeArg _restOdoDeepSubst ) of
                     { ( _restIcRepl) ->
                         (case (CPat_Con tag_ _restIcRepl _bindsIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPat_Int int_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPat_Var pnm_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = CSubst ->
                  CExpr  ->
                  Bool ->
                  ( CPatFld )
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _bindOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _bindOcoeArg ->
           (case (_lhsIcSubst) of
            { _bindOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _offsetOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _offsetOcoeArg ->
              (case (_lhsIcSubst) of
               { _offsetOcSubst ->
               (case (_lhsIdoDeepSubst) of
                { _fldAnnsOdoDeepSubst ->
                (case (_lhsIcoeArg) of
                 { _fldAnnsOcoeArg ->
                 (case (_lhsIcSubst) of
                  { _fldAnnsOcSubst ->
                  (case (fldAnns_ _fldAnnsOcSubst _fldAnnsOcoeArg _fldAnnsOdoDeepSubst ) of
                   { ( _fldAnnsIcRepl) ->
                       (case (bind_ _bindOcSubst _bindOcoeArg _bindOdoDeepSubst ) of
                        { ( _bindIcRepl) ->
                            (case (offset_ _offsetOcSubst _offsetOcoeArg _offsetOdoDeepSubst ) of
                             { ( _offsetIcRepl) ->
                                 (case (CPatFld_Fld lbl_ _offsetIcRepl _bindIcRepl _fldAnnsIcRepl) of
                                  { _cRepl ->
                                  (case (_cRepl) of
                                   { _lhsOcRepl ->
                                   ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local cRepl       : _
      alternative Nil:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = CSubst ->
                   CExpr  ->
                   Bool ->
                   ( CPatFldL )
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _tlOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _tlOcoeArg ->
           (case (_lhsIcSubst) of
            { _tlOcSubst ->
            (case (_lhsIdoDeepSubst) of
             { _hdOdoDeepSubst ->
             (case (_lhsIcoeArg) of
              { _hdOcoeArg ->
              (case (_lhsIcSubst) of
               { _hdOcSubst ->
               (case (tl_ _tlOcSubst _tlOcoeArg _tlOdoDeepSubst ) of
                { ( _tlIcRepl) ->
                    (case (hd_ _hdOcSubst _hdOcoeArg _hdOdoDeepSubst ) of
                     { ( _hdIcRepl) ->
                         (case ((:) _hdIcRepl _tlIcRepl) of
                          { _cRepl ->
                          (case (_cRepl) of
                           { _lhsOcRepl ->
                           ( _lhsOcRepl) }) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case ([]) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local cRepl       : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = CSubst ->
                   CExpr  ->
                   Bool ->
                   ( CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPatRest_Empty) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (CPatRest_Var nm_) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local cRepl       : _
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = CSubst ->
                    CExpr  ->
                    Bool ->
                    ( CodeAGItf )
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _moduleOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _moduleOcoeArg ->
           (case (_lhsIcSubst) of
            { _moduleOcSubst ->
            (case (module_ _moduleOcSubst _moduleOcoeArg _moduleOdoDeepSubst ) of
             { ( _moduleIcRepl) ->
                 (case (CodeAGItf_AGItf _moduleIcRepl) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cSubst               : CSubst
         coeArg               : CExpr 
         doDeepSubst          : Bool
      synthesized attribute:
         cRepl                : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local cRepl       : _
      alternative Nothing:
         visit 0:
            local cRepl       : _
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = CSubst ->
                  CExpr  ->
                  Bool ->
                  ( MbCExpr )
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (_lhsIdoDeepSubst) of
          { _justOdoDeepSubst ->
          (case (_lhsIcoeArg) of
           { _justOcoeArg ->
           (case (_lhsIcSubst) of
            { _justOcSubst ->
            (case (just_ _justOcSubst _justOcoeArg _justOdoDeepSubst ) of
             { ( _justIcRepl) ->
                 (case (Just _justIcRepl) of
                  { _cRepl ->
                  (case (_cRepl) of
                   { _lhsOcRepl ->
                   ( _lhsOcRepl) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIcSubst
       _lhsIcoeArg
       _lhsIdoDeepSubst ->
         (case (Nothing) of
          { _cRepl ->
          (case (_cRepl) of
           { _lhsOcRepl ->
           ( _lhsOcRepl) }) }))