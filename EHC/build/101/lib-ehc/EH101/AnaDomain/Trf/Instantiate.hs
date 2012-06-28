

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/AnaDomain/Trf/Instantiate.a)
module EH101.AnaDomain.Trf.Instantiate(relevtyInst) where

import EH101.Base.Common
import EH101.AnaDomain
import EH101.VarMp
import EH101.Substitutable
import qualified Data.Map as Map
import qualified Data.Set as Set









relevtyInst :: UID -> RelevTy -> (RelevTy,RelevQualS)
relevtyInst uniq ty
  = (ty', extraQualS `Set.union` qualS_Syn_TyAGItf t)
  where t = wrap_TyAGItf
              (sem_TyAGItf (TyAGItf_AGItf ty))
              (Inh_TyAGItf
                 { gUniq_Inh_TyAGItf        = uniq
                 })
        ty' = repl_Syn_TyAGItf t
        extraQualS
          = case ty of
              RelevTy_Fun RQuant_Rec _ _ a r
                -> amsGathQual ams
                where (amso,ams) = amsLE emptyRVarMp (RelevTy_Fun RQuant_None [] [] a r) ty'
              _ -> Set.empty

-- AnaEval -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_AnaEval  = (Map.Map UID UID) ->
                  UID ->
                  ( UID,RelevQualS,AnaEval )
sem_AnaEval_Join :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Join opnds_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _opndsOgUniq ->
          (case (_lhsIfreshMp) of
           { _opndsOfreshMp ->
           (case (opnds_ _opndsOfreshMp _opndsOgUniq ) of
            { ( _opndsIgUniq,_opndsIqualS,_opndsIrepl) ->
                (case (_opndsIgUniq) of
                 { _lhsOgUniq ->
                 (case (_opndsIqualS) of
                  { _lhsOqualS ->
                  (case (AnaEval_Join _opndsIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }))
sem_AnaEval_Lazy :: T_AnaEval 
sem_AnaEval_Lazy  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (AnaEval_Lazy) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
sem_AnaEval_Meet :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Meet opnds_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _opndsOgUniq ->
          (case (_lhsIfreshMp) of
           { _opndsOfreshMp ->
           (case (opnds_ _opndsOfreshMp _opndsOgUniq ) of
            { ( _opndsIgUniq,_opndsIqualS,_opndsIrepl) ->
                (case (_opndsIgUniq) of
                 { _lhsOgUniq ->
                 (case (_opndsIqualS) of
                  { _lhsOqualS ->
                  (case (AnaEval_Meet _opndsIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }))
sem_AnaEval_Var :: UID ->
                   T_AnaEval 
sem_AnaEval_Var av_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (AnaEval_Var $ Map.findWithDefault av_ av_ _lhsIfreshMp) of
            { _lhsOrepl ->
            ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }))
sem_AnaEval_WHNF :: T_AnaEval 
sem_AnaEval_WHNF  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (AnaEval_WHNF) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- AnaEvalL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_AnaEvalL  = (Map.Map UID UID) ->
                   UID ->
                   ( UID,RelevQualS,AnaEvalL )
sem_AnaEvalL_Cons :: T_AnaEval  ->
                     T_AnaEvalL  ->
                     T_AnaEvalL 
sem_AnaEvalL_Cons hd_ tl_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsIfreshMp) of
           { _hdOfreshMp ->
           (case (hd_ _hdOfreshMp _hdOgUniq ) of
            { ( _hdIgUniq,_hdIqualS,_hdIrepl) ->
                (case (_hdIgUniq) of
                 { _tlOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _tlOfreshMp ->
                  (case (tl_ _tlOfreshMp _tlOgUniq ) of
                   { ( _tlIgUniq,_tlIqualS,_tlIrepl) ->
                       (case (_tlIgUniq) of
                        { _lhsOgUniq ->
                        (case (_hdIqualS `Set.union` _tlIqualS) of
                         { _lhsOqualS ->
                         (case ((:) _hdIrepl _tlIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_AnaEvalL_Nil :: T_AnaEvalL 
sem_AnaEvalL_Nil  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case ([]) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- CoeAGItf ----------------------------------------------------
{-
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
type T_CoeAGItf  = ( )
sem_CoeAGItf_AGItf :: T_RelevCoe  ->
                      T_CoeAGItf 
sem_CoeAGItf_AGItf relevCoe_  =
    ( )
-- MbRelevTy ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_MbRelevTy  = (Map.Map UID UID) ->
                    UID ->
                    ( UID,RelevQualS,MbRelevTy )
sem_MbRelevTy_Just :: T_RelevTy  ->
                      T_MbRelevTy 
sem_MbRelevTy_Just just_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _justOgUniq ->
          (case (_lhsIfreshMp) of
           { _justOfreshMp ->
           (case (just_ _justOfreshMp _justOgUniq ) of
            { ( _justIgUniq,_justIqualS,_justIrepl) ->
                (case (_justIgUniq) of
                 { _lhsOgUniq ->
                 (case (_justIqualS) of
                  { _lhsOqualS ->
                  (case (Just _justIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }))
sem_MbRelevTy_Nothing :: T_MbRelevTy 
sem_MbRelevTy_Nothing  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (Nothing) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- QualAGItf ---------------------------------------------------
{-
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
type T_QualAGItf  = ( )
sem_QualAGItf_AGItf :: T_RelevQual  ->
                       T_QualAGItf 
sem_QualAGItf_AGItf relevQual_  =
    ( )
-- RelevCoe ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_RelevCoe  = (Map.Map UID UID) ->
                   UID ->
                   ( UID,RelevQualS,RelevCoe )
sem_RelevCoe_Cast :: T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Cast coe_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _coeOgUniq ->
          (case (_lhsIfreshMp) of
           { _coeOfreshMp ->
           (case (coe_ _coeOfreshMp _coeOgUniq ) of
            { ( _coeIgUniq,_coeIqualS,_coeIrepl) ->
                (case (_coeIgUniq) of
                 { _lhsOgUniq ->
                 (case (_coeIqualS) of
                  { _lhsOqualS ->
                  (case (RelevCoe_Cast _coeIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }))
sem_RelevCoe_CastTy :: T_RelevTy  ->
                       T_RelevTy  ->
                       T_RelevCoe 
sem_RelevCoe_CastTy l_ r_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lOgUniq ->
          (case (_lhsIfreshMp) of
           { _lOfreshMp ->
           (case (l_ _lOfreshMp _lOgUniq ) of
            { ( _lIgUniq,_lIqualS,_lIrepl) ->
                (case (_lIgUniq) of
                 { _rOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _rOfreshMp ->
                  (case (r_ _rOfreshMp _rOgUniq ) of
                   { ( _rIgUniq,_rIqualS,_rIrepl) ->
                       (case (_rIgUniq) of
                        { _lhsOgUniq ->
                        (case (_lIqualS `Set.union` _rIqualS) of
                         { _lhsOqualS ->
                         (case (RelevCoe_CastTy _lIrepl _rIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevCoe_Comp :: T_RelevCoe  ->
                     T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Comp l_ r_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lOgUniq ->
          (case (_lhsIfreshMp) of
           { _lOfreshMp ->
           (case (l_ _lOfreshMp _lOgUniq ) of
            { ( _lIgUniq,_lIqualS,_lIrepl) ->
                (case (_lIgUniq) of
                 { _rOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _rOfreshMp ->
                  (case (r_ _rOfreshMp _rOgUniq ) of
                   { ( _rIgUniq,_rIqualS,_rIrepl) ->
                       (case (_rIgUniq) of
                        { _lhsOgUniq ->
                        (case (_lIqualS `Set.union` _rIqualS) of
                         { _lhsOqualS ->
                         (case (RelevCoe_Comp _lIrepl _rIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevCoe_Err :: String ->
                    T_RelevCoe 
sem_RelevCoe_Err str_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (RelevCoe_Err str_) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
sem_RelevCoe_Eval :: T_AnaEval  ->
                     T_AnaEval  ->
                     T_RelevCoe 
sem_RelevCoe_Eval from_ to_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _fromOgUniq ->
          (case (_lhsIfreshMp) of
           { _fromOfreshMp ->
           (case (from_ _fromOfreshMp _fromOgUniq ) of
            { ( _fromIgUniq,_fromIqualS,_fromIrepl) ->
                (case (_fromIgUniq) of
                 { _toOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _toOfreshMp ->
                  (case (to_ _toOfreshMp _toOgUniq ) of
                   { ( _toIgUniq,_toIqualS,_toIrepl) ->
                       (case (_toIgUniq) of
                        { _lhsOgUniq ->
                        (case (_fromIqualS `Set.union` _toIqualS) of
                         { _lhsOqualS ->
                         (case (RelevCoe_Eval _fromIrepl _toIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevCoe_Fun :: T_RelevCoeL  ->
                    T_RelevCoe  ->
                    T_RelevCoe 
sem_RelevCoe_Fun args_ res_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _argsOgUniq ->
          (case (_lhsIfreshMp) of
           { _argsOfreshMp ->
           (case (args_ _argsOfreshMp _argsOgUniq ) of
            { ( _argsIgUniq,_argsIqualS,_argsIrepl) ->
                (case (_argsIgUniq) of
                 { _resOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _resOfreshMp ->
                  (case (res_ _resOfreshMp _resOgUniq ) of
                   { ( _resIgUniq,_resIqualS,_resIrepl) ->
                       (case (_resIgUniq) of
                        { _lhsOgUniq ->
                        (case (_argsIqualS `Set.union` _resIqualS) of
                         { _lhsOqualS ->
                         (case (RelevCoe_Fun _argsIrepl _resIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevCoe_Id :: T_RelevCoe 
sem_RelevCoe_Id  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (RelevCoe_Id) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- RelevCoeL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_RelevCoeL  = (Map.Map UID UID) ->
                    UID ->
                    ( UID,RelevQualS,RelevCoeL )
sem_RelevCoeL_Cons :: T_RelevCoe  ->
                      T_RelevCoeL  ->
                      T_RelevCoeL 
sem_RelevCoeL_Cons hd_ tl_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsIfreshMp) of
           { _hdOfreshMp ->
           (case (hd_ _hdOfreshMp _hdOgUniq ) of
            { ( _hdIgUniq,_hdIqualS,_hdIrepl) ->
                (case (_hdIgUniq) of
                 { _tlOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _tlOfreshMp ->
                  (case (tl_ _tlOfreshMp _tlOgUniq ) of
                   { ( _tlIgUniq,_tlIqualS,_tlIrepl) ->
                       (case (_tlIgUniq) of
                        { _lhsOgUniq ->
                        (case (_hdIqualS `Set.union` _tlIqualS) of
                         { _lhsOqualS ->
                         (case ((:) _hdIrepl _tlIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevCoeL_Nil :: T_RelevCoeL 
sem_RelevCoeL_Nil  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case ([]) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- RelevQual ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_RelevQual  = (Map.Map UID UID) ->
                    UID ->
                    ( UID,RelevQualS,RelevQual )
sem_RelevQual_SubEval :: T_AnaEval  ->
                         T_AnaEval  ->
                         T_RelevQual 
sem_RelevQual_SubEval l_ r_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lOgUniq ->
          (case (_lhsIfreshMp) of
           { _lOfreshMp ->
           (case (l_ _lOfreshMp _lOgUniq ) of
            { ( _lIgUniq,_lIqualS,_lIrepl) ->
                (case (_lIgUniq) of
                 { _rOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _rOfreshMp ->
                  (case (r_ _rOfreshMp _rOgUniq ) of
                   { ( _rIgUniq,_rIqualS,_rIrepl) ->
                       (case (_rIgUniq) of
                        { _lhsOgUniq ->
                        (case (_lIqualS `Set.union` _rIqualS) of
                         { _lhsOqualS ->
                         (case (RelevQual_SubEval _lIrepl _rIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
-- RelevQualL --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_RelevQualL  = (Map.Map UID UID) ->
                     UID ->
                     ( UID,RelevQualS,RelevQualL )
sem_RelevQualL_Cons :: T_RelevQual  ->
                       T_RelevQualL  ->
                       T_RelevQualL 
sem_RelevQualL_Cons hd_ tl_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsIfreshMp) of
           { _hdOfreshMp ->
           (case (hd_ _hdOfreshMp _hdOgUniq ) of
            { ( _hdIgUniq,_hdIqualS,_hdIrepl) ->
                (case (_hdIgUniq) of
                 { _tlOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _tlOfreshMp ->
                  (case (tl_ _tlOfreshMp _tlOgUniq ) of
                   { ( _tlIgUniq,_tlIqualS,_tlIrepl) ->
                       (case (_tlIgUniq) of
                        { _lhsOgUniq ->
                        (case (_hdIqualS `Set.union` _tlIqualS) of
                         { _lhsOqualS ->
                         (case ((:) _hdIrepl _tlIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevQualL_Nil :: T_RelevQualL 
sem_RelevQualL_Nil  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case ([]) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- RelevTy -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
            local _tup1       : {(UID,UID)}
            local lUniq       : {UID}
            local freshTvL    : _
            local freshMp     : _
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
type T_RelevTy  = (Map.Map UID UID) ->
                  UID ->
                  ( UID,RelevQualS,RelevTy )
sem_RelevTy_Ana :: T_AnaEval  ->
                   T_RelevTy 
sem_RelevTy_Ana eval_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _evalOgUniq ->
          (case (_lhsIfreshMp) of
           { _evalOfreshMp ->
           (case (eval_ _evalOfreshMp _evalOgUniq ) of
            { ( _evalIgUniq,_evalIqualS,_evalIrepl) ->
                (case (_evalIgUniq) of
                 { _lhsOgUniq ->
                 (case (_evalIqualS) of
                  { _lhsOqualS ->
                  (case (RelevTy_Ana _evalIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }))
sem_RelevTy_Err :: String ->
                   T_RelevTy 
sem_RelevTy_Err str_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (RelevTy_Err str_) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
sem_RelevTy_Fun :: RQuant ->
                   ([UID]) ->
                   T_RelevQualL  ->
                   T_RelevTyL  ->
                   T_RelevTy  ->
                   T_RelevTy 
sem_RelevTy_Fun quant_ quants_ quals_ args_ res_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
          { __tup1 ->
          (case (__tup1) of
           { (_qualsOgUniq,_) ->
           (case (__tup1) of
            { (_,_lUniq) ->
            (case (mkNewLevUIDL (length quants_) _lUniq) of
             { _freshTvL ->
             (case (Map.union (Map.fromList $ zip quants_ _freshTvL) _lhsIfreshMp) of
              { _freshMp ->
              (case (_freshMp) of
               { _qualsOfreshMp ->
               (case (quals_ _qualsOfreshMp _qualsOgUniq ) of
                { ( _qualsIgUniq,_qualsIqualS,_qualsIrepl) ->
                    (case (_qualsIgUniq) of
                     { _argsOgUniq ->
                     (case (_freshMp) of
                      { _argsOfreshMp ->
                      (case (args_ _argsOfreshMp _argsOgUniq ) of
                       { ( _argsIgUniq,_argsIqualS,_argsIrepl) ->
                           (case (_argsIgUniq) of
                            { _resOgUniq ->
                            (case (_freshMp) of
                             { _resOfreshMp ->
                             (case (res_ _resOfreshMp _resOgUniq ) of
                              { ( _resIgUniq,_resIqualS,_resIrepl) ->
                                  (case (_resIgUniq) of
                                   { _lhsOgUniq ->
                                   (case (Set.unions [ Set.fromList _qualsIrepl, _argsIqualS, _resIqualS ]) of
                                    { _lhsOqualS ->
                                    (case (RelevTy_Fun RQuant_None [] [] _argsIrepl _resIrepl) of
                                     { _lhsOrepl ->
                                     ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_RelevTy_None :: T_RelevTy 
sem_RelevTy_None  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case (RelevTy_None) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- RelevTyL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         freshMp              : Map.Map UID UID
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
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
type T_RelevTyL  = (Map.Map UID UID) ->
                   UID ->
                   ( UID,RelevQualS,RelevTyL )
sem_RelevTyL_Cons :: T_RelevTy  ->
                     T_RelevTyL  ->
                     T_RelevTyL 
sem_RelevTyL_Cons hd_ tl_  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsIfreshMp) of
           { _hdOfreshMp ->
           (case (hd_ _hdOfreshMp _hdOgUniq ) of
            { ( _hdIgUniq,_hdIqualS,_hdIrepl) ->
                (case (_hdIgUniq) of
                 { _tlOgUniq ->
                 (case (_lhsIfreshMp) of
                  { _tlOfreshMp ->
                  (case (tl_ _tlOfreshMp _tlOgUniq ) of
                   { ( _tlIgUniq,_tlIqualS,_tlIrepl) ->
                       (case (_tlIgUniq) of
                        { _lhsOgUniq ->
                        (case (_hdIqualS `Set.union` _tlIqualS) of
                         { _lhsOqualS ->
                         (case ((:) _hdIrepl _tlIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_RelevTyL_Nil :: T_RelevTyL 
sem_RelevTyL_Nil  =
    (\ _lhsIfreshMp
       _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Set.empty) of
           { _lhsOqualS ->
           (case ([]) of
            { _repl ->
            (case (_repl) of
             { _lhsOrepl ->
             ( _lhsOgUniq,_lhsOqualS,_lhsOrepl) }) }) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         gUniq                : UID
      synthesized attributes:
         qualS                : RelevQualS
         repl                 : RelevTy 
   alternatives:
      alternative AGItf:
         child relevTy        : RelevTy 
         visit 0:
            local freshMp     : _
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _relevTy )  =
    (sem_TyAGItf_AGItf (sem_RelevTy _relevTy ) )
-- semantic domain
type T_TyAGItf  = UID ->
                  ( RelevQualS,RelevTy )
data Inh_TyAGItf  = Inh_TyAGItf {gUniq_Inh_TyAGItf :: !(UID)}
data Syn_TyAGItf  = Syn_TyAGItf {qualS_Syn_TyAGItf :: !(RelevQualS),repl_Syn_TyAGItf :: !(RelevTy )}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIgUniq )  =
    (let ( _lhsOqualS,_lhsOrepl) = sem _lhsIgUniq 
     in  (Syn_TyAGItf _lhsOqualS _lhsOrepl ))
sem_TyAGItf_AGItf :: T_RelevTy  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf relevTy_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _relevTyOgUniq ->
          (case (Map.empty) of
           { _freshMp ->
           (case (_freshMp) of
            { _relevTyOfreshMp ->
            (case (relevTy_ _relevTyOfreshMp _relevTyOgUniq ) of
             { ( _relevTyIgUniq,_relevTyIqualS,_relevTyIrepl) ->
                 (case (_relevTyIqualS) of
                  { _lhsOqualS ->
                  (case (_relevTyIrepl) of
                   { _lhsOrepl ->
                   ( _lhsOqualS,_lhsOrepl) }) }) }) }) }) }))