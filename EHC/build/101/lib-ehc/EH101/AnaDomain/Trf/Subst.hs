

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/AnaDomain/Trf/Subst.ag)
module EH101.AnaDomain.Trf.Subst(relevtyAppVarLookup
, relevqualAppVarLookup
, relevcoeAppVarLookup) where

import EH101.Base.Common
import EH101.AnaDomain
import EH101.VarMp
import EH.Util.Utils







relevtyAppVarLookup :: VarLookup m UID RVarMpInfo => m -> RelevTy -> RelevTy
relevtyAppVarLookup varmp ty
  = repl_Syn_TyAGItf t
  where t = wrap_TyAGItf
              (sem_TyAGItf (TyAGItf_AGItf ty))
              (Inh_TyAGItf {rvarLookup_Inh_TyAGItf = varlookupFix varmp})



relevqualAppVarLookup :: VarLookup m UID RVarMpInfo => m -> RelevQual -> RelevQual
relevqualAppVarLookup varmp qual
  = repl_Syn_QualAGItf t
  where t = wrap_QualAGItf
              (sem_QualAGItf (QualAGItf_AGItf qual))
              (Inh_QualAGItf {rvarLookup_Inh_QualAGItf = varlookupFix varmp})



relevcoeAppVarLookup :: VarLookup m UID RVarMpInfo => m -> RelevCoe -> RelevCoe
relevcoeAppVarLookup varmp coe
  = repl_Syn_CoeAGItf t
  where t = wrap_CoeAGItf
              (sem_CoeAGItf (CoeAGItf_AGItf coe))
              (Inh_CoeAGItf {rvarLookup_Inh_CoeAGItf = varlookupFix varmp})

-- AnaEval -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Join:
         child opnds          : AnaEvalL 
         visit 0:
            local repl        : _
      alternative Lazy:
         visit 0:
            local repl        : _
      alternative Meet:
         child opnds          : AnaEvalL 
         visit 0:
            local repl        : _
      alternative Var:
         child av             : {UID}
         visit 0:
            local repl        : _
            local _tup1       : _
            local isRepl      : _
            local replv       : _
            inst  repl'       : AnaEval 
      alternative WHNF:
         visit 0:
            local repl        : _
-}
-- cata
sem_AnaEval :: AnaEval  ->
               T_AnaEval 
sem_AnaEval (AnaEval_Join _opnds )  =
    (sem_AnaEval_Join (sem_AnaEvalL _opnds ) )
sem_AnaEval (AnaEval_Lazy )  =
    (sem_AnaEval_Lazy )
sem_AnaEval (AnaEval_Meet _opnds )  =
    (sem_AnaEval_Meet (sem_AnaEvalL _opnds ) )
sem_AnaEval (AnaEval_Var _av )  =
    (sem_AnaEval_Var _av )
sem_AnaEval (AnaEval_WHNF )  =
    (sem_AnaEval_WHNF )
-- semantic domain
type T_AnaEval  = (VarLookupFix UID RVarMpInfo) ->
                  ( AnaEval )
sem_AnaEval_Join :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Join opnds_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _opndsOrvarLookup ->
          (case (opnds_ _opndsOrvarLookup ) of
           { ( _opndsIrepl) ->
               (case (AnaEval_Join _opndsIrepl) of
                { _repl ->
                (case (_repl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
sem_AnaEval_Lazy :: T_AnaEval 
sem_AnaEval_Lazy  =
    (\ _lhsIrvarLookup ->
         (case (AnaEval_Lazy) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_AnaEval_Meet :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Meet opnds_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _opndsOrvarLookup ->
          (case (opnds_ _opndsOrvarLookup ) of
           { ( _opndsIrepl) ->
               (case (AnaEval_Meet _opndsIrepl) of
                { _repl ->
                (case (_repl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
sem_AnaEval_Var :: UID ->
                   T_AnaEval 
sem_AnaEval_Var av_  =
    (\ _lhsIrvarLookup ->
         (case (AnaEval_Var av_) of
          { _repl ->
          (case (maybe (_repl,False) (\t -> (t,True)) $ rvmiMbEval $? _lhsIrvarLookup av_) of
           { __tup1 ->
           (case (__tup1) of
            { (_,_isRepl) ->
            (case (__tup1) of
             { (_replv,_) ->
             (case (if _isRepl then _replv else AnaEval_WHNF) of
              { repl'_val_ ->
              (case ((sem_AnaEval repl'_val_ )) of
               { repl'_inst_ ->
               (case (_lhsIrvarLookup) of
                { _repl'OrvarLookup ->
                (case (repl'_inst_ _repl'OrvarLookup ) of
                 { ( _repl'Irepl) ->
                     (case (if _isRepl
                            then _repl'Irepl
                            else _repl) of
                      { _lhsOrepl ->
                      ( _lhsOrepl) }) }) }) }) }) }) }) }) }))
sem_AnaEval_WHNF :: T_AnaEval 
sem_AnaEval_WHNF  =
    (\ _lhsIrvarLookup ->
         (case (AnaEval_WHNF) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- AnaEvalL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AnaEval 
         child tl             : AnaEvalL 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_AnaEvalL :: AnaEvalL  ->
                T_AnaEvalL 
sem_AnaEvalL list  =
    (Prelude.foldr sem_AnaEvalL_Cons sem_AnaEvalL_Nil (Prelude.map sem_AnaEval list) )
-- semantic domain
type T_AnaEvalL  = (VarLookupFix UID RVarMpInfo) ->
                   ( AnaEvalL )
sem_AnaEvalL_Cons :: T_AnaEval  ->
                     T_AnaEvalL  ->
                     T_AnaEvalL 
sem_AnaEvalL_Cons hd_ tl_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _tlOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _hdOrvarLookup ->
           (case (tl_ _tlOrvarLookup ) of
            { ( _tlIrepl) ->
                (case (hd_ _hdOrvarLookup ) of
                 { ( _hdIrepl) ->
                     (case ((:) _hdIrepl _tlIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_AnaEvalL_Nil :: T_AnaEvalL 
sem_AnaEvalL_Nil  =
    (\ _lhsIrvarLookup ->
         (case ([]) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- CoeAGItf ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : RelevCoe 
   alternatives:
      alternative AGItf:
         child relevCoe       : RelevCoe 
-}
-- cata
sem_CoeAGItf :: CoeAGItf  ->
                T_CoeAGItf 
sem_CoeAGItf (CoeAGItf_AGItf _relevCoe )  =
    (sem_CoeAGItf_AGItf (sem_RelevCoe _relevCoe ) )
-- semantic domain
type T_CoeAGItf  = (VarLookupFix UID RVarMpInfo) ->
                   ( RelevCoe )
data Inh_CoeAGItf  = Inh_CoeAGItf {rvarLookup_Inh_CoeAGItf :: !((VarLookupFix UID RVarMpInfo))}
data Syn_CoeAGItf  = Syn_CoeAGItf {repl_Syn_CoeAGItf :: !(RelevCoe )}
wrap_CoeAGItf :: T_CoeAGItf  ->
                 Inh_CoeAGItf  ->
                 Syn_CoeAGItf 
wrap_CoeAGItf sem (Inh_CoeAGItf _lhsIrvarLookup )  =
    (let ( _lhsOrepl) = sem _lhsIrvarLookup 
     in  (Syn_CoeAGItf _lhsOrepl ))
sem_CoeAGItf_AGItf :: T_RelevCoe  ->
                      T_CoeAGItf 
sem_CoeAGItf_AGItf relevCoe_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _relevCoeOrvarLookup ->
          (case (relevCoe_ _relevCoeOrvarLookup ) of
           { ( _relevCoeIrepl) ->
               (case (_relevCoeIrepl) of
                { _lhsOrepl ->
                ( _lhsOrepl) }) }) }))
-- MbRelevTy ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Just:
         child just           : RelevTy 
         visit 0:
            local repl        : _
      alternative Nothing:
         visit 0:
            local repl        : _
-}
-- cata
sem_MbRelevTy :: MbRelevTy  ->
                 T_MbRelevTy 
sem_MbRelevTy (Prelude.Just x )  =
    (sem_MbRelevTy_Just (sem_RelevTy x ) )
sem_MbRelevTy Prelude.Nothing  =
    sem_MbRelevTy_Nothing
-- semantic domain
type T_MbRelevTy  = (VarLookupFix UID RVarMpInfo) ->
                    ( MbRelevTy )
sem_MbRelevTy_Just :: T_RelevTy  ->
                      T_MbRelevTy 
sem_MbRelevTy_Just just_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _justOrvarLookup ->
          (case (just_ _justOrvarLookup ) of
           { ( _justIrepl) ->
               (case (Just _justIrepl) of
                { _repl ->
                (case (_repl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
sem_MbRelevTy_Nothing :: T_MbRelevTy 
sem_MbRelevTy_Nothing  =
    (\ _lhsIrvarLookup ->
         (case (Nothing) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- QualAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : RelevQual 
   alternatives:
      alternative AGItf:
         child relevQual      : RelevQual 
-}
-- cata
sem_QualAGItf :: QualAGItf  ->
                 T_QualAGItf 
sem_QualAGItf (QualAGItf_AGItf _relevQual )  =
    (sem_QualAGItf_AGItf (sem_RelevQual _relevQual ) )
-- semantic domain
type T_QualAGItf  = (VarLookupFix UID RVarMpInfo) ->
                    ( RelevQual )
data Inh_QualAGItf  = Inh_QualAGItf {rvarLookup_Inh_QualAGItf :: !((VarLookupFix UID RVarMpInfo))}
data Syn_QualAGItf  = Syn_QualAGItf {repl_Syn_QualAGItf :: !(RelevQual )}
wrap_QualAGItf :: T_QualAGItf  ->
                  Inh_QualAGItf  ->
                  Syn_QualAGItf 
wrap_QualAGItf sem (Inh_QualAGItf _lhsIrvarLookup )  =
    (let ( _lhsOrepl) = sem _lhsIrvarLookup 
     in  (Syn_QualAGItf _lhsOrepl ))
sem_QualAGItf_AGItf :: T_RelevQual  ->
                       T_QualAGItf 
sem_QualAGItf_AGItf relevQual_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _relevQualOrvarLookup ->
          (case (relevQual_ _relevQualOrvarLookup ) of
           { ( _relevQualIrepl) ->
               (case (_relevQualIrepl) of
                { _lhsOrepl ->
                ( _lhsOrepl) }) }) }))
-- RelevCoe ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cast:
         child coe            : RelevCoe 
         visit 0:
            local repl        : _
      alternative CastTy:
         child l              : RelevTy 
         child r              : RelevTy 
         visit 0:
            local repl        : _
      alternative Comp:
         child l              : RelevCoe 
         child r              : RelevCoe 
         visit 0:
            local repl        : _
      alternative Err:
         child str            : {String}
         visit 0:
            local repl        : _
      alternative Eval:
         child from           : AnaEval 
         child to             : AnaEval 
         visit 0:
            local repl        : _
      alternative Fun:
         child args           : RelevCoeL 
         child res            : RelevCoe 
         visit 0:
            local repl        : _
      alternative Id:
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevCoe :: RelevCoe  ->
                T_RelevCoe 
sem_RelevCoe (RelevCoe_Cast _coe )  =
    (sem_RelevCoe_Cast (sem_RelevCoe _coe ) )
sem_RelevCoe (RelevCoe_CastTy _l _r )  =
    (sem_RelevCoe_CastTy (sem_RelevTy _l ) (sem_RelevTy _r ) )
sem_RelevCoe (RelevCoe_Comp _l _r )  =
    (sem_RelevCoe_Comp (sem_RelevCoe _l ) (sem_RelevCoe _r ) )
sem_RelevCoe (RelevCoe_Err _str )  =
    (sem_RelevCoe_Err _str )
sem_RelevCoe (RelevCoe_Eval _from _to )  =
    (sem_RelevCoe_Eval (sem_AnaEval _from ) (sem_AnaEval _to ) )
sem_RelevCoe (RelevCoe_Fun _args _res )  =
    (sem_RelevCoe_Fun (sem_RelevCoeL _args ) (sem_RelevCoe _res ) )
sem_RelevCoe (RelevCoe_Id )  =
    (sem_RelevCoe_Id )
-- semantic domain
type T_RelevCoe  = (VarLookupFix UID RVarMpInfo) ->
                   ( RelevCoe )
sem_RelevCoe_Cast :: T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Cast coe_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _coeOrvarLookup ->
          (case (coe_ _coeOrvarLookup ) of
           { ( _coeIrepl) ->
               (case (RelevCoe_Cast _coeIrepl) of
                { _repl ->
                (case (_repl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
sem_RelevCoe_CastTy :: T_RelevTy  ->
                       T_RelevTy  ->
                       T_RelevCoe 
sem_RelevCoe_CastTy l_ r_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _rOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _lOrvarLookup ->
           (case (r_ _rOrvarLookup ) of
            { ( _rIrepl) ->
                (case (l_ _lOrvarLookup ) of
                 { ( _lIrepl) ->
                     (case (RelevCoe_CastTy _lIrepl _rIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevCoe_Comp :: T_RelevCoe  ->
                     T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Comp l_ r_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _rOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _lOrvarLookup ->
           (case (r_ _rOrvarLookup ) of
            { ( _rIrepl) ->
                (case (l_ _lOrvarLookup ) of
                 { ( _lIrepl) ->
                     (case (RelevCoe_Comp _lIrepl _rIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevCoe_Err :: String ->
                    T_RelevCoe 
sem_RelevCoe_Err str_  =
    (\ _lhsIrvarLookup ->
         (case (RelevCoe_Err str_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_RelevCoe_Eval :: T_AnaEval  ->
                     T_AnaEval  ->
                     T_RelevCoe 
sem_RelevCoe_Eval from_ to_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _toOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _fromOrvarLookup ->
           (case (to_ _toOrvarLookup ) of
            { ( _toIrepl) ->
                (case (from_ _fromOrvarLookup ) of
                 { ( _fromIrepl) ->
                     (case (RelevCoe_Eval _fromIrepl _toIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevCoe_Fun :: T_RelevCoeL  ->
                    T_RelevCoe  ->
                    T_RelevCoe 
sem_RelevCoe_Fun args_ res_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _resOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _argsOrvarLookup ->
           (case (res_ _resOrvarLookup ) of
            { ( _resIrepl) ->
                (case (args_ _argsOrvarLookup ) of
                 { ( _argsIrepl) ->
                     (case (RelevCoe_Fun _argsIrepl _resIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevCoe_Id :: T_RelevCoe 
sem_RelevCoe_Id  =
    (\ _lhsIrvarLookup ->
         (case (RelevCoe_Id) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- RelevCoeL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : RelevCoe 
         child tl             : RelevCoeL 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevCoeL :: RelevCoeL  ->
                 T_RelevCoeL 
sem_RelevCoeL list  =
    (Prelude.foldr sem_RelevCoeL_Cons sem_RelevCoeL_Nil (Prelude.map sem_RelevCoe list) )
-- semantic domain
type T_RelevCoeL  = (VarLookupFix UID RVarMpInfo) ->
                    ( RelevCoeL )
sem_RelevCoeL_Cons :: T_RelevCoe  ->
                      T_RelevCoeL  ->
                      T_RelevCoeL 
sem_RelevCoeL_Cons hd_ tl_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _tlOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _hdOrvarLookup ->
           (case (tl_ _tlOrvarLookup ) of
            { ( _tlIrepl) ->
                (case (hd_ _hdOrvarLookup ) of
                 { ( _hdIrepl) ->
                     (case ((:) _hdIrepl _tlIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevCoeL_Nil :: T_RelevCoeL 
sem_RelevCoeL_Nil  =
    (\ _lhsIrvarLookup ->
         (case ([]) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- RelevQual ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative SubEval:
         child l              : AnaEval 
         child r              : AnaEval 
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevQual :: RelevQual  ->
                 T_RelevQual 
sem_RelevQual (RelevQual_SubEval _l _r )  =
    (sem_RelevQual_SubEval (sem_AnaEval _l ) (sem_AnaEval _r ) )
-- semantic domain
type T_RelevQual  = (VarLookupFix UID RVarMpInfo) ->
                    ( RelevQual )
sem_RelevQual_SubEval :: T_AnaEval  ->
                         T_AnaEval  ->
                         T_RelevQual 
sem_RelevQual_SubEval l_ r_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _rOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _lOrvarLookup ->
           (case (r_ _rOrvarLookup ) of
            { ( _rIrepl) ->
                (case (l_ _lOrvarLookup ) of
                 { ( _lIrepl) ->
                     (case (RelevQual_SubEval _lIrepl _rIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
-- RelevQualL --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : RelevQual 
         child tl             : RelevQualL 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevQualL :: RelevQualL  ->
                  T_RelevQualL 
sem_RelevQualL list  =
    (Prelude.foldr sem_RelevQualL_Cons sem_RelevQualL_Nil (Prelude.map sem_RelevQual list) )
-- semantic domain
type T_RelevQualL  = (VarLookupFix UID RVarMpInfo) ->
                     ( RelevQualL )
sem_RelevQualL_Cons :: T_RelevQual  ->
                       T_RelevQualL  ->
                       T_RelevQualL 
sem_RelevQualL_Cons hd_ tl_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _tlOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _hdOrvarLookup ->
           (case (tl_ _tlOrvarLookup ) of
            { ( _tlIrepl) ->
                (case (hd_ _hdOrvarLookup ) of
                 { ( _hdIrepl) ->
                     (case ((:) _hdIrepl _tlIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevQualL_Nil :: T_RelevQualL 
sem_RelevQualL_Nil  =
    (\ _lhsIrvarLookup ->
         (case ([]) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- RelevTy -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Ana:
         child eval           : AnaEval 
         visit 0:
            local repl        : _
      alternative Err:
         child str            : {String}
         visit 0:
            local repl        : _
      alternative Fun:
         child quant          : {RQuant}
         child quants         : {[UID]}
         child quals          : RelevQualL 
         child args           : RelevTyL 
         child res            : RelevTy 
         visit 0:
            local rvarLookup  : _
            local repl        : _
      alternative None:
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevTy :: RelevTy  ->
               T_RelevTy 
sem_RelevTy (RelevTy_Ana _eval )  =
    (sem_RelevTy_Ana (sem_AnaEval _eval ) )
sem_RelevTy (RelevTy_Err _str )  =
    (sem_RelevTy_Err _str )
sem_RelevTy (RelevTy_Fun _quant _quants _quals _args _res )  =
    (sem_RelevTy_Fun _quant _quants (sem_RelevQualL _quals ) (sem_RelevTyL _args ) (sem_RelevTy _res ) )
sem_RelevTy (RelevTy_None )  =
    (sem_RelevTy_None )
-- semantic domain
type T_RelevTy  = (VarLookupFix UID RVarMpInfo) ->
                  ( RelevTy )
sem_RelevTy_Ana :: T_AnaEval  ->
                   T_RelevTy 
sem_RelevTy_Ana eval_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _evalOrvarLookup ->
          (case (eval_ _evalOrvarLookup ) of
           { ( _evalIrepl) ->
               (case (RelevTy_Ana _evalIrepl) of
                { _repl ->
                (case (_repl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
sem_RelevTy_Err :: String ->
                   T_RelevTy 
sem_RelevTy_Err str_  =
    (\ _lhsIrvarLookup ->
         (case (RelevTy_Err str_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_RelevTy_Fun :: RQuant ->
                   ([UID]) ->
                   T_RelevQualL  ->
                   T_RelevTyL  ->
                   T_RelevTy  ->
                   T_RelevTy 
sem_RelevTy_Fun quant_ quants_ quals_ args_ res_  =
    (\ _lhsIrvarLookup ->
         (case (varlookupFixDel quants_ _lhsIrvarLookup) of
          { _rvarLookup ->
          (case (_rvarLookup) of
           { _resOrvarLookup ->
           (case (_rvarLookup) of
            { _argsOrvarLookup ->
            (case (_rvarLookup) of
             { _qualsOrvarLookup ->
             (case (res_ _resOrvarLookup ) of
              { ( _resIrepl) ->
                  (case (args_ _argsOrvarLookup ) of
                   { ( _argsIrepl) ->
                       (case (quals_ _qualsOrvarLookup ) of
                        { ( _qualsIrepl) ->
                            (case (RelevTy_Fun quant_ quants_ _qualsIrepl _argsIrepl _resIrepl) of
                             { _repl ->
                             (case (_repl) of
                              { _lhsOrepl ->
                              ( _lhsOrepl) }) }) }) }) }) }) }) }) }))
sem_RelevTy_None :: T_RelevTy 
sem_RelevTy_None  =
    (\ _lhsIrvarLookup ->
         (case (RelevTy_None) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- RelevTyL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : RelevTy 
         child tl             : RelevTyL 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_RelevTyL :: RelevTyL  ->
                T_RelevTyL 
sem_RelevTyL list  =
    (Prelude.foldr sem_RelevTyL_Cons sem_RelevTyL_Nil (Prelude.map sem_RelevTy list) )
-- semantic domain
type T_RelevTyL  = (VarLookupFix UID RVarMpInfo) ->
                   ( RelevTyL )
sem_RelevTyL_Cons :: T_RelevTy  ->
                     T_RelevTyL  ->
                     T_RelevTyL 
sem_RelevTyL_Cons hd_ tl_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _tlOrvarLookup ->
          (case (_lhsIrvarLookup) of
           { _hdOrvarLookup ->
           (case (tl_ _tlOrvarLookup ) of
            { ( _tlIrepl) ->
                (case (hd_ _hdOrvarLookup ) of
                 { ( _hdIrepl) ->
                     (case ((:) _hdIrepl _tlIrepl) of
                      { _repl ->
                      (case (_repl) of
                       { _lhsOrepl ->
                       ( _lhsOrepl) }) }) }) }) }) }))
sem_RelevTyL_Nil :: T_RelevTyL 
sem_RelevTyL_Nil  =
    (\ _lhsIrvarLookup ->
         (case ([]) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rvarLookup           : VarLookupFix UID RVarMpInfo
      synthesized attribute:
         repl                 : RelevTy 
   alternatives:
      alternative AGItf:
         child relevTy        : RelevTy 
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _relevTy )  =
    (sem_TyAGItf_AGItf (sem_RelevTy _relevTy ) )
-- semantic domain
type T_TyAGItf  = (VarLookupFix UID RVarMpInfo) ->
                  ( RelevTy )
data Inh_TyAGItf  = Inh_TyAGItf {rvarLookup_Inh_TyAGItf :: !((VarLookupFix UID RVarMpInfo))}
data Syn_TyAGItf  = Syn_TyAGItf {repl_Syn_TyAGItf :: !(RelevTy )}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIrvarLookup )  =
    (let ( _lhsOrepl) = sem _lhsIrvarLookup 
     in  (Syn_TyAGItf _lhsOrepl ))
sem_TyAGItf_AGItf :: T_RelevTy  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf relevTy_  =
    (\ _lhsIrvarLookup ->
         (case (_lhsIrvarLookup) of
          { _relevTyOrvarLookup ->
          (case (relevTy_ _relevTyOrvarLookup ) of
           { ( _relevTyIrepl) ->
               (case (_relevTyIrepl) of
                { _lhsOrepl ->
                ( _lhsOrepl) }) }) }))