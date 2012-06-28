

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/AnaDomain/Pretty.ag)
module EH101.AnaDomain.Pretty(ppRelevTy
, ppRelevQual
, ppAnaEval
, ppRelevCoe
, VarPPMp) where

import EH.Util.Pretty
import EH101.AnaDomain
import EH101.Base.Common
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char









ppRelevTy :: VarPPMp -> RelevTy -> (PP_Doc,VarPPMp)
ppRelevTy vm x
  =  let  t = wrap_RelevTy (sem_RelevTy x)
                           (Inh_RelevTy
                             { varMp_Inh_RelevTy = vm
                             })
     in   (pp_Syn_RelevTy t, quantVarMp_Syn_RelevTy t)

instance PP RelevTy where
  pp x = fst $ ppRelevTy Map.empty x



ppRelevQual :: RelevQual -> PP_Doc
ppRelevQual x
  =  let  t = wrap_RelevQual (sem_RelevQual x)
                             (Inh_RelevQual
                               { varMp_Inh_RelevQual = Map.empty
                               })
     in   (pp_Syn_RelevQual t)

instance PP RelevQual where
  pp x = ppRelevQual x



ppAnaEval :: AnaEval -> PP_Doc
ppAnaEval x
  =  let  t = wrap_AnaEval (sem_AnaEval x)
                             (Inh_AnaEval
                               { varMp_Inh_AnaEval = Map.empty
                               })
     in   (pp_Syn_AnaEval t)

instance PP AnaEval where
  pp x = ppAnaEval x



ppRelevCoe :: VarPPMp -> RelevCoe -> PP_Doc
ppRelevCoe vm x
  =  let  t = wrap_RelevCoe (sem_RelevCoe x)
                             (Inh_RelevCoe
                               { varMp_Inh_RelevCoe = vm
                               })
     in   (pp_Syn_RelevCoe t)

instance PP RelevCoe where
  pp x = ppRelevCoe Map.empty x



instance Show RVarMpInfo where
  show _ = "RVarMpInfo"

instance PP RVarMpInfo where
  pp (RVMIEval a) = pp a



instance PP AnaMatchState where
  pp x = ppParens (amsOuterVarMp x) >#< amsLocalVarMp x >#< "~>" >#< ppCommas (Set.toList $ amsGathQual x)

instance PP a => PP (AMSOut a) where
  pp x = amsoLo x >#< "<=" >#< amsoHi x >#< "~" >#< amsoCoe x



type VarPPMp = Map.Map UID PP_Doc

-- AnaEval -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
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
type T_AnaEval  = VarPPMp ->
                  ( PP_Doc)
data Inh_AnaEval  = Inh_AnaEval {varMp_Inh_AnaEval :: !(VarPPMp)}
data Syn_AnaEval  = Syn_AnaEval {pp_Syn_AnaEval :: !(PP_Doc)}
wrap_AnaEval :: T_AnaEval  ->
                Inh_AnaEval  ->
                Syn_AnaEval 
wrap_AnaEval sem (Inh_AnaEval _lhsIvarMp )  =
    (let ( _lhsOpp) = sem _lhsIvarMp 
     in  (Syn_AnaEval _lhsOpp ))
sem_AnaEval_Join :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Join opnds_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _opndsOvarMp ->
          (case (opnds_ _opndsOvarMp ) of
           { ( _opndsIpp,_opndsIppL) ->
               (case (ppListSep "" "" " \\/ " _opndsIppL) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_AnaEval_Lazy :: T_AnaEval 
sem_AnaEval_Lazy  =
    (\ _lhsIvarMp ->
         (case (pp "L") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_AnaEval_Meet :: T_AnaEvalL  ->
                    T_AnaEval 
sem_AnaEval_Meet opnds_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _opndsOvarMp ->
          (case (opnds_ _opndsOvarMp ) of
           { ( _opndsIpp,_opndsIppL) ->
               (case (ppListSep "" "" " /\\ " _opndsIppL) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_AnaEval_Var :: UID ->
                   T_AnaEval 
sem_AnaEval_Var av_  =
    (\ _lhsIvarMp ->
         (case (Map.findWithDefault (pp av_) av_ _lhsIvarMp) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_AnaEval_WHNF :: T_AnaEval 
sem_AnaEval_WHNF  =
    (\ _lhsIvarMp ->
         (case (pp "S") of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- AnaEvalL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
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
type T_AnaEvalL  = VarPPMp ->
                   ( PP_Doc,([PP_Doc]))
sem_AnaEvalL_Cons :: T_AnaEval  ->
                     T_AnaEvalL  ->
                     T_AnaEvalL 
sem_AnaEvalL_Cons hd_ tl_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _tlOvarMp ->
          (case (_lhsIvarMp) of
           { _hdOvarMp ->
           (case (tl_ _tlOvarMp ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOvarMp ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_AnaEvalL_Nil :: T_AnaEvalL 
sem_AnaEvalL_Nil  =
    (\ _lhsIvarMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
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
         varMp                : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
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
type T_MbRelevTy  = VarPPMp ->
                    ( PP_Doc)
sem_MbRelevTy_Just :: T_RelevTy  ->
                      T_MbRelevTy 
sem_MbRelevTy_Just just_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _justOvarMp ->
          (case (just_ _justOvarMp ) of
           { ( _justIpp,_justIquantVarMp) ->
               (case (_justIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_MbRelevTy_Nothing :: T_MbRelevTy 
sem_MbRelevTy_Nothing  =
    (\ _lhsIvarMp ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
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
         varMp                : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
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
type T_RelevCoe  = VarPPMp ->
                   ( PP_Doc)
data Inh_RelevCoe  = Inh_RelevCoe {varMp_Inh_RelevCoe :: !(VarPPMp)}
data Syn_RelevCoe  = Syn_RelevCoe {pp_Syn_RelevCoe :: !(PP_Doc)}
wrap_RelevCoe :: T_RelevCoe  ->
                 Inh_RelevCoe  ->
                 Syn_RelevCoe 
wrap_RelevCoe sem (Inh_RelevCoe _lhsIvarMp )  =
    (let ( _lhsOpp) = sem _lhsIvarMp 
     in  (Syn_RelevCoe _lhsOpp ))
sem_RelevCoe_Cast :: T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Cast coe_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _coeOvarMp ->
          (case (coe_ _coeOvarMp ) of
           { ( _coeIpp) ->
               (case ("(#!" >#< _coeIpp >|< ")") of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_RelevCoe_CastTy :: T_RelevTy  ->
                       T_RelevTy  ->
                       T_RelevCoe 
sem_RelevCoe_CastTy l_ r_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _rOvarMp ->
          (case (_lhsIvarMp) of
           { _lOvarMp ->
           (case (r_ _rOvarMp ) of
            { ( _rIpp,_rIquantVarMp) ->
                (case (l_ _lOvarMp ) of
                 { ( _lIpp,_lIquantVarMp) ->
                     (case (ppParens $ _lIpp >#< "`castTy`" >#< _rIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_RelevCoe_Comp :: T_RelevCoe  ->
                     T_RelevCoe  ->
                     T_RelevCoe 
sem_RelevCoe_Comp l_ r_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _rOvarMp ->
          (case (_lhsIvarMp) of
           { _lOvarMp ->
           (case (r_ _rOvarMp ) of
            { ( _rIpp) ->
                (case (l_ _lOvarMp ) of
                 { ( _lIpp) ->
                     (case (_lIpp >#< "._c" >#< _rIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_RelevCoe_Err :: String ->
                    T_RelevCoe 
sem_RelevCoe_Err str_  =
    (\ _lhsIvarMp ->
         (case ("#_c:" >#< str_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_RelevCoe_Eval :: T_AnaEval  ->
                     T_AnaEval  ->
                     T_RelevCoe 
sem_RelevCoe_Eval from_ to_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _toOvarMp ->
          (case (_lhsIvarMp) of
           { _fromOvarMp ->
           (case (to_ _toOvarMp ) of
            { ( _toIpp) ->
                (case (from_ _fromOvarMp ) of
                 { ( _fromIpp) ->
                     (case (ppParens $ _fromIpp >#< "~e>" >#< _toIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_RelevCoe_Fun :: T_RelevCoeL  ->
                    T_RelevCoe  ->
                    T_RelevCoe 
sem_RelevCoe_Fun args_ res_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _resOvarMp ->
          (case (_lhsIvarMp) of
           { _argsOvarMp ->
           (case (res_ _resOvarMp ) of
            { ( _resIpp) ->
                (case (args_ _argsOvarMp ) of
                 { ( _argsIpp,_argsIppL) ->
                     (case (ppParensCommas' _argsIppL >#< "-c>" >#< _resIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_RelevCoe_Id :: T_RelevCoe 
sem_RelevCoe_Id  =
    (\ _lhsIvarMp ->
         (case (pp "#_c") of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- RelevCoeL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
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
type T_RelevCoeL  = VarPPMp ->
                    ( PP_Doc,([PP_Doc]))
sem_RelevCoeL_Cons :: T_RelevCoe  ->
                      T_RelevCoeL  ->
                      T_RelevCoeL 
sem_RelevCoeL_Cons hd_ tl_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _tlOvarMp ->
          (case (_lhsIvarMp) of
           { _hdOvarMp ->
           (case (tl_ _tlOvarMp ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOvarMp ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_RelevCoeL_Nil :: T_RelevCoeL 
sem_RelevCoeL_Nil  =
    (\ _lhsIvarMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- RelevQual ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
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
type T_RelevQual  = VarPPMp ->
                    ( PP_Doc)
data Inh_RelevQual  = Inh_RelevQual {varMp_Inh_RelevQual :: !(VarPPMp)}
data Syn_RelevQual  = Syn_RelevQual {pp_Syn_RelevQual :: !(PP_Doc)}
wrap_RelevQual :: T_RelevQual  ->
                  Inh_RelevQual  ->
                  Syn_RelevQual 
wrap_RelevQual sem (Inh_RelevQual _lhsIvarMp )  =
    (let ( _lhsOpp) = sem _lhsIvarMp 
     in  (Syn_RelevQual _lhsOpp ))
sem_RelevQual_SubEval :: T_AnaEval  ->
                         T_AnaEval  ->
                         T_RelevQual 
sem_RelevQual_SubEval l_ r_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _rOvarMp ->
          (case (_lhsIvarMp) of
           { _lOvarMp ->
           (case (r_ _rOvarMp ) of
            { ( _rIpp) ->
                (case (l_ _lOvarMp ) of
                 { ( _lIpp) ->
                     (case (_lIpp >#< "<e=" >#< _rIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
-- RelevQualL --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
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
type T_RelevQualL  = VarPPMp ->
                     ( PP_Doc,([PP_Doc]))
sem_RelevQualL_Cons :: T_RelevQual  ->
                       T_RelevQualL  ->
                       T_RelevQualL 
sem_RelevQualL_Cons hd_ tl_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _tlOvarMp ->
          (case (_lhsIvarMp) of
           { _hdOvarMp ->
           (case (tl_ _tlOvarMp ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOvarMp ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_RelevQualL_Nil :: T_RelevQualL 
sem_RelevQualL_Nil  =
    (\ _lhsIvarMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- RelevTy -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         quantVarMp           : VarPPMp
   alternatives:
      alternative Ana:
         child eval           : AnaEval 
         visit 0:
            local quantVarMp  : _
      alternative Err:
         child str            : {String}
         visit 0:
            local quantVarMp  : _
      alternative Fun:
         child quant          : {RQuant}
         child quants         : {[UID]}
         child quals          : RelevQualL 
         child args           : RelevTyL 
         child res            : RelevTy 
         visit 0:
            local _tup1       : _
            local varMp       : _
            local quantNmL    : _
            local quantVarMp  : _
      alternative None:
         visit 0:
            local quantVarMp  : _
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
type T_RelevTy  = VarPPMp ->
                  ( PP_Doc,VarPPMp)
data Inh_RelevTy  = Inh_RelevTy {varMp_Inh_RelevTy :: !(VarPPMp)}
data Syn_RelevTy  = Syn_RelevTy {pp_Syn_RelevTy :: !(PP_Doc),quantVarMp_Syn_RelevTy :: !(VarPPMp)}
wrap_RelevTy :: T_RelevTy  ->
                Inh_RelevTy  ->
                Syn_RelevTy 
wrap_RelevTy sem (Inh_RelevTy _lhsIvarMp )  =
    (let ( _lhsOpp,_lhsOquantVarMp) = sem _lhsIvarMp 
     in  (Syn_RelevTy _lhsOpp _lhsOquantVarMp ))
sem_RelevTy_Ana :: T_AnaEval  ->
                   T_RelevTy 
sem_RelevTy_Ana eval_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _evalOvarMp ->
          (case (eval_ _evalOvarMp ) of
           { ( _evalIpp) ->
               (case (_evalIpp) of
                { _lhsOpp ->
                (case (Map.empty) of
                 { _quantVarMp ->
                 (case (_quantVarMp) of
                  { _lhsOquantVarMp ->
                  ( _lhsOpp,_lhsOquantVarMp) }) }) }) }) }))
sem_RelevTy_Err :: String ->
                   T_RelevTy 
sem_RelevTy_Err str_  =
    (\ _lhsIvarMp ->
         (case ("#:" >#< str_) of
          { _lhsOpp ->
          (case (Map.empty) of
           { _quantVarMp ->
           (case (_quantVarMp) of
            { _lhsOquantVarMp ->
            ( _lhsOpp,_lhsOquantVarMp) }) }) }))
sem_RelevTy_Fun :: RQuant ->
                   ([UID]) ->
                   T_RelevQualL  ->
                   T_RelevTyL  ->
                   T_RelevTy  ->
                   T_RelevTy 
sem_RelevTy_Fun quant_ quants_ quals_ args_ res_  =
    (\ _lhsIvarMp ->
         (case (genNmMap pp quants_ _lhsIvarMp) of
          { __tup1 ->
          (case (__tup1) of
           { (_varMp,_) ->
           (case (_varMp) of
            { _resOvarMp ->
            (case (_varMp) of
             { _argsOvarMp ->
             (case (_varMp) of
              { _qualsOvarMp ->
              (case (__tup1) of
               { (_,_quantNmL) ->
               (case (res_ _resOvarMp ) of
                { ( _resIpp,_resIquantVarMp) ->
                    (case (args_ _argsOvarMp ) of
                     { ( _argsIpp,_argsIppL) ->
                         (case (quals_ _qualsOvarMp ) of
                          { ( _qualsIpp,_qualsIppL) ->
                              (case ((if null _quantNmL then empty else "forall" >|< ppParens (show quant_) >#< ppSpaces _quantNmL >#< ". ")
                                     >|< (if null _qualsIppL then empty else ppParensCommas' _qualsIppL >#< "=> ")
                                     >|< ppParensCommas _argsIppL >#< "->" >#< ppParens _resIpp) of
                               { _lhsOpp ->
                               (case (_varMp) of
                                { _quantVarMp ->
                                (case (_quantVarMp) of
                                 { _lhsOquantVarMp ->
                                 ( _lhsOpp,_lhsOquantVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_RelevTy_None :: T_RelevTy 
sem_RelevTy_None  =
    (\ _lhsIvarMp ->
         (case (pp "#") of
          { _lhsOpp ->
          (case (Map.empty) of
           { _quantVarMp ->
           (case (_quantVarMp) of
            { _lhsOquantVarMp ->
            ( _lhsOpp,_lhsOquantVarMp) }) }) }))
-- RelevTyL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         varMp                : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
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
type T_RelevTyL  = VarPPMp ->
                   ( PP_Doc,([PP_Doc]))
sem_RelevTyL_Cons :: T_RelevTy  ->
                     T_RelevTyL  ->
                     T_RelevTyL 
sem_RelevTyL_Cons hd_ tl_  =
    (\ _lhsIvarMp ->
         (case (_lhsIvarMp) of
          { _tlOvarMp ->
          (case (_lhsIvarMp) of
           { _hdOvarMp ->
           (case (tl_ _tlOvarMp ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOvarMp ) of
                 { ( _hdIpp,_hdIquantVarMp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_RelevTyL_Nil :: T_RelevTyL 
sem_RelevTyL_Nil  =
    (\ _lhsIvarMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         quantVarMp           : VarPPMp
   alternatives:
      alternative AGItf:
         child relevTy        : RelevTy 
         visit 0:
            local varMp       : _
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _relevTy )  =
    (sem_TyAGItf_AGItf (sem_RelevTy _relevTy ) )
-- semantic domain
type T_TyAGItf  = ( VarPPMp)
sem_TyAGItf_AGItf :: T_RelevTy  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf relevTy_  =
    (case (Map.empty) of
     { _varMp ->
     (case (_varMp) of
      { _relevTyOvarMp ->
      (case (relevTy_ _relevTyOvarMp ) of
       { ( _relevTyIpp,_relevTyIquantVarMp) ->
           (case (_relevTyIquantVarMp) of
            { _lhsOquantVarMp ->
            ( _lhsOquantVarMp) }) }) }) })