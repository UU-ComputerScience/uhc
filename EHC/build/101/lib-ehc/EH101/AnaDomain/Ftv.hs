

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/AnaDomain/Ftv.ag)
module EH101.AnaDomain.Ftv(relevTyFtv
, relevQualFtv
, relevCoeFtv) where

import EH101.Base.Common
import EH101.AnaDomain
import qualified Data.Set as Set







relevTyFtv :: RelevTy -> UIDS
relevTyFtv ty
  = fvS_Syn_TyAGItf t
  where t = wrap_TyAGItf
              (sem_TyAGItf (TyAGItf_AGItf ty))
              (Inh_TyAGItf )



relevQualFtv :: RelevQual -> UIDS
relevQualFtv qual
  = fvS_Syn_QualAGItf t
  where t = wrap_QualAGItf
              (sem_QualAGItf (QualAGItf_AGItf qual))
              (Inh_QualAGItf )



relevCoeFtv :: RelevCoe -> UIDS
relevCoeFtv coe
  = fvS_Syn_CoeAGItf t
  where t = wrap_CoeAGItf
              (sem_CoeAGItf (CoeAGItf_AGItf coe))
              (Inh_CoeAGItf )

-- AnaEval -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Join:
         child opnds          : AnaEvalL 
      alternative Lazy:
      alternative Meet:
         child opnds          : AnaEvalL 
      alternative Var:
         child av             : {UID}
      alternative WHNF:
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
type T_AnaEval  = ( UIDS)
sem_AnaEval_Join :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Join opnds_  =
    (case (opnds_ ) of
     { ( _opndsIfvS) ->
         (case (_opndsIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
sem_AnaEval_Lazy :: T_AnaEval 
sem_AnaEval_Lazy  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
sem_AnaEval_Meet :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Meet opnds_  =
    (case (opnds_ ) of
     { ( _opndsIfvS) ->
         (case (_opndsIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
sem_AnaEval_Var :: UID ->
                   T_AnaEval 
sem_AnaEval_Var av_  =
    (case (Set.singleton av_) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
sem_AnaEval_WHNF :: T_AnaEval 
sem_AnaEval_WHNF  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- AnaEvalL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Cons:
         child hd             : AnaEval 
         child tl             : AnaEvalL 
      alternative Nil:
-}
-- cata
sem_AnaEvalL :: AnaEvalL  ->
                T_AnaEvalL 
sem_AnaEvalL list  =
    (Prelude.foldr sem_AnaEvalL_Cons sem_AnaEvalL_Nil (Prelude.map sem_AnaEval list) )
-- semantic domain
type T_AnaEvalL  = ( UIDS)
sem_AnaEvalL_Cons :: T_AnaEval  ->
                     T_AnaEvalL  ->
                     T_AnaEvalL 
sem_AnaEvalL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIfvS) ->
         (case (hd_ ) of
          { ( _hdIfvS) ->
              (case (_hdIfvS `Set.union` _tlIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_AnaEvalL_Nil :: T_AnaEvalL 
sem_AnaEvalL_Nil  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- CoeAGItf ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
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
type T_CoeAGItf  = ( UIDS)
data Inh_CoeAGItf  = Inh_CoeAGItf {}
data Syn_CoeAGItf  = Syn_CoeAGItf {fvS_Syn_CoeAGItf :: !(UIDS)}
wrap_CoeAGItf :: T_CoeAGItf  ->
                 Inh_CoeAGItf  ->
                 Syn_CoeAGItf 
wrap_CoeAGItf sem (Inh_CoeAGItf )  =
    (let ( _lhsOfvS) = sem 
     in  (Syn_CoeAGItf _lhsOfvS ))
sem_CoeAGItf_AGItf :: T_RelevCoe  ->
                      T_CoeAGItf 
sem_CoeAGItf_AGItf relevCoe_  =
    (case (relevCoe_ ) of
     { ( _relevCoeIfvS) ->
         (case (_relevCoeIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
-- MbRelevTy ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Just:
         child just           : RelevTy 
      alternative Nothing:
-}
-- cata
sem_MbRelevTy :: MbRelevTy  ->
                 T_MbRelevTy 
sem_MbRelevTy (Prelude.Just x )  =
    (sem_MbRelevTy_Just (sem_RelevTy x ) )
sem_MbRelevTy Prelude.Nothing  =
    sem_MbRelevTy_Nothing
-- semantic domain
type T_MbRelevTy  = ( UIDS)
sem_MbRelevTy_Just :: T_RelevTy  ->
                      T_MbRelevTy 
sem_MbRelevTy_Just just_  =
    (case (just_ ) of
     { ( _justIfvS) ->
         (case (_justIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
sem_MbRelevTy_Nothing :: T_MbRelevTy 
sem_MbRelevTy_Nothing  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- QualAGItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
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
type T_QualAGItf  = ( UIDS)
data Inh_QualAGItf  = Inh_QualAGItf {}
data Syn_QualAGItf  = Syn_QualAGItf {fvS_Syn_QualAGItf :: !(UIDS)}
wrap_QualAGItf :: T_QualAGItf  ->
                  Inh_QualAGItf  ->
                  Syn_QualAGItf 
wrap_QualAGItf sem (Inh_QualAGItf )  =
    (let ( _lhsOfvS) = sem 
     in  (Syn_QualAGItf _lhsOfvS ))
sem_QualAGItf_AGItf :: T_RelevQual  ->
                       T_QualAGItf 
sem_QualAGItf_AGItf relevQual_  =
    (case (relevQual_ ) of
     { ( _relevQualIfvS) ->
         (case (_relevQualIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
-- RelevCoe ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Cast:
         child coe            : RelevCoe 
      alternative CastTy:
         child l              : RelevTy 
         child r              : RelevTy 
      alternative Comp:
         child l              : RelevCoe 
         child r              : RelevCoe 
      alternative Err:
         child str            : {String}
      alternative Eval:
         child from           : AnaEval 
         child to             : AnaEval 
      alternative Fun:
         child args           : RelevCoeL 
         child res            : RelevCoe 
      alternative Id:
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
type T_RelevCoe  = ( UIDS)
sem_RelevCoe_Cast :: T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Cast coe_  =
    (case (coe_ ) of
     { ( _coeIfvS) ->
         (case (_coeIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
sem_RelevCoe_CastTy :: T_RelevTy  ->
                       T_RelevTy  ->
                       T_RelevCoe 
sem_RelevCoe_CastTy l_ r_  =
    (case (r_ ) of
     { ( _rIfvS) ->
         (case (l_ ) of
          { ( _lIfvS) ->
              (case (_lIfvS `Set.union` _rIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevCoe_Comp :: T_RelevCoe  ->
                     T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Comp l_ r_  =
    (case (r_ ) of
     { ( _rIfvS) ->
         (case (l_ ) of
          { ( _lIfvS) ->
              (case (_lIfvS `Set.union` _rIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevCoe_Err :: String ->
                    T_RelevCoe 
sem_RelevCoe_Err str_  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
sem_RelevCoe_Eval :: T_AnaEval  ->
                     T_AnaEval  ->
                     T_RelevCoe 
sem_RelevCoe_Eval from_ to_  =
    (case (to_ ) of
     { ( _toIfvS) ->
         (case (from_ ) of
          { ( _fromIfvS) ->
              (case (_fromIfvS `Set.union` _toIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevCoe_Fun :: T_RelevCoeL  ->
                    T_RelevCoe  ->
                    T_RelevCoe 
sem_RelevCoe_Fun args_ res_  =
    (case (res_ ) of
     { ( _resIfvS) ->
         (case (args_ ) of
          { ( _argsIfvS) ->
              (case (_argsIfvS `Set.union` _resIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevCoe_Id :: T_RelevCoe 
sem_RelevCoe_Id  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- RelevCoeL ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Cons:
         child hd             : RelevCoe 
         child tl             : RelevCoeL 
      alternative Nil:
-}
-- cata
sem_RelevCoeL :: RelevCoeL  ->
                 T_RelevCoeL 
sem_RelevCoeL list  =
    (Prelude.foldr sem_RelevCoeL_Cons sem_RelevCoeL_Nil (Prelude.map sem_RelevCoe list) )
-- semantic domain
type T_RelevCoeL  = ( UIDS)
sem_RelevCoeL_Cons :: T_RelevCoe  ->
                      T_RelevCoeL  ->
                      T_RelevCoeL 
sem_RelevCoeL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIfvS) ->
         (case (hd_ ) of
          { ( _hdIfvS) ->
              (case (_hdIfvS `Set.union` _tlIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevCoeL_Nil :: T_RelevCoeL 
sem_RelevCoeL_Nil  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- RelevQual ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative SubEval:
         child l              : AnaEval 
         child r              : AnaEval 
-}
-- cata
sem_RelevQual :: RelevQual  ->
                 T_RelevQual 
sem_RelevQual (RelevQual_SubEval _l _r )  =
    (sem_RelevQual_SubEval (sem_AnaEval _l ) (sem_AnaEval _r ) )
-- semantic domain
type T_RelevQual  = ( UIDS)
sem_RelevQual_SubEval :: T_AnaEval  ->
                         T_AnaEval  ->
                         T_RelevQual 
sem_RelevQual_SubEval l_ r_  =
    (case (r_ ) of
     { ( _rIfvS) ->
         (case (l_ ) of
          { ( _lIfvS) ->
              (case (_lIfvS `Set.union` _rIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
-- RelevQualL --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Cons:
         child hd             : RelevQual 
         child tl             : RelevQualL 
      alternative Nil:
-}
-- cata
sem_RelevQualL :: RelevQualL  ->
                  T_RelevQualL 
sem_RelevQualL list  =
    (Prelude.foldr sem_RelevQualL_Cons sem_RelevQualL_Nil (Prelude.map sem_RelevQual list) )
-- semantic domain
type T_RelevQualL  = ( UIDS)
sem_RelevQualL_Cons :: T_RelevQual  ->
                       T_RelevQualL  ->
                       T_RelevQualL 
sem_RelevQualL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIfvS) ->
         (case (hd_ ) of
          { ( _hdIfvS) ->
              (case (_hdIfvS `Set.union` _tlIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevQualL_Nil :: T_RelevQualL 
sem_RelevQualL_Nil  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- RelevTy -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Ana:
         child eval           : AnaEval 
      alternative Err:
         child str            : {String}
      alternative Fun:
         child quant          : {RQuant}
         child quants         : {[UID]}
         child quals          : RelevQualL 
         child args           : RelevTyL 
         child res            : RelevTy 
      alternative None:
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
type T_RelevTy  = ( UIDS)
sem_RelevTy_Ana :: T_AnaEval  ->
                   T_RelevTy 
sem_RelevTy_Ana eval_  =
    (case (eval_ ) of
     { ( _evalIfvS) ->
         (case (_evalIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })
sem_RelevTy_Err :: String ->
                   T_RelevTy 
sem_RelevTy_Err str_  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
sem_RelevTy_Fun :: RQuant ->
                   ([UID]) ->
                   T_RelevQualL  ->
                   T_RelevTyL  ->
                   T_RelevTy  ->
                   T_RelevTy 
sem_RelevTy_Fun quant_ quants_ quals_ args_ res_  =
    (case (res_ ) of
     { ( _resIfvS) ->
         (case (args_ ) of
          { ( _argsIfvS) ->
              (case (quals_ ) of
               { ( _qualsIfvS) ->
                   (case (_qualsIfvS `Set.union` _argsIfvS `Set.union` _resIfvS) of
                    { _lhsOfvS ->
                    ( _lhsOfvS) }) }) }) })
sem_RelevTy_None :: T_RelevTy 
sem_RelevTy_None  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- RelevTyL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
   alternatives:
      alternative Cons:
         child hd             : RelevTy 
         child tl             : RelevTyL 
      alternative Nil:
-}
-- cata
sem_RelevTyL :: RelevTyL  ->
                T_RelevTyL 
sem_RelevTyL list  =
    (Prelude.foldr sem_RelevTyL_Cons sem_RelevTyL_Nil (Prelude.map sem_RelevTy list) )
-- semantic domain
type T_RelevTyL  = ( UIDS)
sem_RelevTyL_Cons :: T_RelevTy  ->
                     T_RelevTyL  ->
                     T_RelevTyL 
sem_RelevTyL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIfvS) ->
         (case (hd_ ) of
          { ( _hdIfvS) ->
              (case (_hdIfvS `Set.union` _tlIfvS) of
               { _lhsOfvS ->
               ( _lhsOfvS) }) }) })
sem_RelevTyL_Nil :: T_RelevTyL 
sem_RelevTyL_Nil  =
    (case (Set.empty) of
     { _lhsOfvS ->
     ( _lhsOfvS) })
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         fvS                  : UIDS
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
type T_TyAGItf  = ( UIDS)
data Inh_TyAGItf  = Inh_TyAGItf {}
data Syn_TyAGItf  = Syn_TyAGItf {fvS_Syn_TyAGItf :: !(UIDS)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf )  =
    (let ( _lhsOfvS) = sem 
     in  (Syn_TyAGItf _lhsOfvS ))
sem_TyAGItf_AGItf :: T_RelevTy  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf relevTy_  =
    (case (relevTy_ ) of
     { ( _relevTyIfvS) ->
         (case (_relevTyIfvS) of
          { _lhsOfvS ->
          ( _lhsOfvS) }) })