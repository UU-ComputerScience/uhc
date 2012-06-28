

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/Quantify.ag)
module EH101.Ty.Trf.Quantify(tyQuantify, tyQuantifyClosed
, TyQuOpts (..), defaultTyQuOpts
, TyQuOut (..)
, tyQuantifyOuter
, tyKiQuantify, valTyQuantify
, tyQuantifyRank, tyQuantifyRank') where

import EH.Util.Utils
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable
import EH101.Base.Debug
import EH.Util.Pretty
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List as List
import Data.Maybe
import EH.Util.Utils
import EH.Util.Pretty













tyQuantifyClosed :: Ty -> Ty
tyQuantifyClosed = tyQuantifyRank [1..]

tyQuantifyRank' :: TvIsBound -> [Int] -> Ty -> Ty
tyQuantifyRank' tvIsBound rL ty
  = tqoTy (tyQuantifyWithOpts (defaultTyQuOpts {tqoptQuRanks = rL, tqoptBaseQuant = tyQu_Forall, tqoptTvIsBound = tvIsBound}) ty)

tyQuantifyRank :: [Int] -> Ty -> Ty
tyQuantifyRank = tyQuantifyRank' (const False)



tyQuantify :: TvIsBound -> Ty -> Ty
tyQuantify tvIsBound ty = tyQuantify' tvIsBound tyQu_Forall ty



tyQuantifyOuter :: (TyVarId -> Ty) -> TvIsBound -> Ty -> Ty
tyQuantifyOuter tvKi tvIsBound ty
  = tqoTy tqo
  where tqo = tyQuantifyWithOpts
                (defaultTyQuOpts
                  { tqoptAllowInnerQuant = False
                  , tqoptTvIsBound       = tvIsBound
                  , tqoptBaseQuant       = tyQu_Forall
                  , tqoptTvL1       	 = tvKi
                  }
                )
                ty



tyKiQuantify :: TvIsBound -> Ty -> Ty
tyKiQuantify tvIsBound ty -- = tyQuantify' tvIsBound tyQu_Forall {- tyQu_KiForall -} ty
  = tqoTy $ tyQuantifyWithOpts opts ty
  where opts = defaultTyQuOpts
                  { tqoptAllowInnerQuant = False
                  , tqoptTvIsBound       = tvIsBound
                  , tqoptBaseQuant       = tyQu_Forall
                  }

valTyQuantify :: (TyVarId -> Ty) -> TvIsBound -> Ty -> Ty
valTyQuantify tvKi tvIsBound ty
  = tqoTy $ tyQuantifyWithOpts opts ty
  where opts = defaultTyQuOpts
                  { tqoptAllowInnerQuant = False
                  , tqoptTvIsBound       = tvIsBound
                  , tqoptTvL1       	 = tvKi
                  }



tyQuantify' :: TvIsBound -> TyQu -> Ty -> Ty
tyQuantify' tvIsBound baseQuant ty
  = tqoTy tqo
  where tqo = tyQuantifyWithOpts
                (defaultTyQuOpts
                  { tqoptTvIsBound       = tvIsBound
                  , tqoptBaseQuant       = baseQuant
                  }
                )
                ty



data TyQuOpts
  = TyQuOpts
      { tqoptTvIsBound			:: TvIsBound
      , tqoptAllowInnerQuant	:: Bool
      , tqoptBaseQuant			:: TyQu
      , tqoptTvL1				:: TyVarId -> Ty		-- mapping of tvar to 1 higher metalevel ty (i.e. kind)
      , tqoptQuRanks 			:: [Int]
      }

defaultTyQuOpts :: TyQuOpts
defaultTyQuOpts
  = TyQuOpts
      (const False)
      True
      tyQu_Forall
      (const kiStar)
      [1..]



data TyQuOut
  = TyQuOut   { tqoTy               ::  Ty
              }



tyQuantifyWithOpts :: TyQuOpts -> Ty -> TyQuOut
tyQuantifyWithOpts opts ty
  = let  t  =  wrap_TyAGItf
                  (sem_TyAGItf  (TyAGItf_AGItf ty))
                  (Inh_TyAGItf  { opts_Inh_TyAGItf          = opts
                                , baseQu_Inh_TyAGItf        = tqoptBaseQuant opts
                                })
    in   TyQuOut
            { tqoTy                 = quTy_Syn_TyAGItf t
            }



type TvIsBound = TyVarId -> Bool
type TvLevIsBound = MetaLev -> TvIsBound

tvIsBound2L :: MetaLev -> TvIsBound -> TvLevIsBound
tvIsBound2L mlev b = \mlev' -> if mlev' == mlev then b else const False

tvIsBound2L0, tvIsBound2L1 :: TvIsBound -> TvLevIsBound
tvIsBound2L0 = tvIsBound2L 0
tvIsBound2L1 = tvIsBound2L 1



type TyVarIdsToBind = AssocL TyVarId Ty



tvBoundAddS :: MetaLev -> TyVarIdS -> TvLevIsBound -> TvLevIsBound
tvBoundAddS mlev tvS tvIsBound = \mlev' v -> (mlev == mlev' && v `Set.member` tvS) || tvIsBound mlev' v

tvNotBound :: MetaLev -> TvLevIsBound -> TyVarIdS -> TyVarIdS
tvNotBound mlev tvIsBound = Set.filter (\tv -> not (tvIsBound mlev tv))



tvarsToQuantL1 :: TyVarIdsToBind -> TvLevIsBound -> (TyVarIdsToBind,TyVarIdS,TvLevIsBound)
tvarsToQuantL1 boundablesL tvIsBound
  = (boundablesL1L,boundablesL1S,tvBoundAddS 1 boundablesL1S tvIsBound)
  where boundablesL1S = tvNotBound 1 tvIsBound $ Set.unions [ varFreeSet l1 | (_,l1) <- boundablesL ]
        boundablesL1L = [ (v,kiStar) | v <- Set.toList boundablesL1S ]



tvarsToQuant :: TyQuOpts -> Bool -> TvCatMp -> (TyVarId -> Bool) -> TvLevIsBound -> TyVarIdS -> (TyVarIdsToBind,TyVarIdsToBind,TyVarIdsToBind,TvLevIsBound)
tvarsToQuant opts isQuLoc _ mayQuFx tvIsBound tvS
  =  if isQuLoc
     then  let boundablesS = tvNotBound 0 tvIsBound tvS
               boundablesL                = [ (v,{- trm "tvarsToQuant" (\k -> v >|< "::" >|< k) $ -} tqoptTvL1 opts v) | v <- Set.toList boundablesS ]
               (boundablesL1L,_,tvIsBound') = tvarsToQuantL1 boundablesL (tvBoundAddS 0 boundablesS tvIsBound)
               -- tvIsBound'  = tvBoundAddS (boundablesS `Set.union` boundablesL1S) tvIsBound
           in ( boundablesL
              , []
              , boundablesL1L
              , tvIsBound'
              )
     else  ([],[],[],tvIsBound)



tvMayQuFx :: TyQu -> TvCatMp -> Bool -> TyVarId -> Bool
tvMayQuFx qu fxTvM isQuFxLoc tv
  = False -- isQuFxLoc -- True



mkTyQuEx :: TyQuOpts -> TyQu -> TyVarIdsToBind -> TyVarIdsToBind-> TyVarIdsToBind -> Ty -> Ty
mkTyQuEx opts q tvsL1 tvs tvsEx t
  =
    mkTyQu (TyQu_Forall (tyquMetaLev q + 1)) tvsL1 $
    mkTyQu q tvs $
    mkTyQu (tyquExists q) tvsEx t



mkTyQuForRank :: TyQuOpts -> Bool -> TyQu -> TyVarIdsToBind -> TyVarIdsToBind -> TyVarIdsToBind -> Ty -> Ty
mkTyQuForRank opts allow q tvsL1 tvs tvsEx t
  | allow     = mkTyQuEx opts q tvsL1 tvs tvsEx t
  | otherwise = t



{-|
There are some conventions/restrictions on the structure of types that are not enforced
by the abstract syntax:

Encoding of prove-constraints:
  concrete syntax:
    {! impls !} -> ty
  abstract syntax:
    Ty_App (Ty_App (Ty_Con "->") (Ty_Impls impls)) ty

Encoding of assume-constraints:
  concrete syntax:
    (ty, {! pred1 !}, ..., {! predn !})
  abstract syntax:
    Ty_Ext (... (Ty_Ext ty (prod m+1) (Ty_Pred pred_1) ) ...) (prod m+n) (Ty_Pred pred_n)

  In other words: the predicates are at the outset of a product, pred_n "more outermost"
  than pred_{n-1}.

-}


{-|
The basic alternatives encode the following:
- Con: data type constructors, including tuple constructors
- App: application to 1 argument, for example 'a -> b' is encoded as (App (App -> a) b)
- Any: representing Bot/Top depending on context: (1) unknown expected type, (2) error type
- Var: type variables, including a category: plain tyvars, fixed tyvars (aka skolems)

-}


data TyQuCtxt = TyQuCtxtArrow | TyQuCtxtProd | TyQuCtxtOnTop | TyQuCtxtOther deriving (Show,Eq)



tvarSOccurCount :: [TyVarIdS] -> AssocL TyVarId Int
tvarSOccurCount = map (\vl@(v:_) -> (v,length vl)) . group . sort . concat . map Set.toList

tvarSOccurGE2 :: [TyVarIdS] -> TyVarIdS
tvarSOccurGE2 =  Set.fromList . map fst . filter ((>1).snd) . tvarSOccurCount

-- Impls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isAtTop              : Bool
         opts                 : TyQuOpts
         pol                  : Polarity
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
   visit 1:
      inherited attributes:
         baseQu               : TyQu 
         fxTvM                : TvCatMp
         isRow                : Bool
         qSurrTvS             : TyVarIdS
         rank                 : Int
         tvIsBound            : TvLevIsBound
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Cons:
         child iv             : {ImplsVarId}
         child pr             : Pred 
         child pv             : {PredOccId}
         child prange         : {Range}
         child proveOccs      : {[ImplsProveOcc]}
         child tl             : Impls 
         visit 1:
            local quTy        : _
            local self        : _
      alternative Nil:
         visit 1:
            local quTy        : _
            local self        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 1:
            local quTy        : _
            local self        : _
-}
-- cata
sem_Impls :: Impls  ->
             T_Impls 
sem_Impls (Impls_Cons _iv _pr _pv _prange _proveOccs _tl )  =
    (sem_Impls_Cons _iv (sem_Pred _pr ) _pv _prange _proveOccs (sem_Impls _tl ) )
sem_Impls (Impls_Nil )  =
    (sem_Impls_Nil )
sem_Impls (Impls_Tail _iv _proveOccs )  =
    (sem_Impls_Tail _iv _proveOccs )
-- semantic domain
type T_Impls  = Bool ->
                TyQuOpts ->
                Polarity ->
                ( ([TyVarIdS]),TvCatMp,TyVarIdS,TyVarIdS,T_Impls_1 )
type T_Impls_1  = TyQu  ->
                  TvCatMp ->
                  Bool ->
                  TyVarIdS ->
                  Int ->
                  TvLevIsBound ->
                  TyQuCtxt ->
                  ( Impls ,Impls )
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIpol) of
          { _tlOpol ->
          (case (_lhsIopts) of
           { _tlOopts ->
           (case (_lhsIisAtTop) of
            { _tlOisAtTop ->
            (case (tl_ _tlOisAtTop _tlOopts _tlOpol ) of
             { ( _tlIfrTvSL,_tlIgathFxTvM,_tlIqInsideTvS,_tlIqOrphanTvS,tl_1) ->
                 (case (_lhsIpol) of
                  { _prOpol ->
                  (case (_lhsIopts) of
                   { _prOopts ->
                   (case (_lhsIisAtTop) of
                    { _prOisAtTop ->
                    (case (pr_ _prOisAtTop _prOopts _prOpol ) of
                     { ( _prIfrTvSL,_prIgathFxTvM,_prIqInsideTvS,_prIqOrphanTvS,pr_1) ->
                         (case (_prIfrTvSL ++ _tlIfrTvSL) of
                          { _lhsOfrTvSL ->
                          (case (_prIgathFxTvM `Map.union` _tlIgathFxTvM) of
                           { _lhsOgathFxTvM ->
                           (case (_prIqInsideTvS `Set.union` _tlIqInsideTvS) of
                            { _lhsOqInsideTvS ->
                            (case (_prIqOrphanTvS `Set.union` _tlIqOrphanTvS) of
                             { _lhsOqOrphanTvS ->
                             (case ((let sem_Impls_Cons_1 :: T_Impls_1 
                                         sem_Impls_Cons_1  =
                                             (\ _lhsIbaseQu
                                                _lhsIfxTvM
                                                _lhsIisRow
                                                _lhsIqSurrTvS
                                                _lhsIrank
                                                _lhsItvIsBound
                                                _lhsItyCtxt ->
                                                  (case (_lhsItyCtxt) of
                                                   { _tlOtyCtxt ->
                                                   (case (_lhsItvIsBound) of
                                                    { _tlOtvIsBound ->
                                                    (case (_lhsIrank) of
                                                     { _tlOrank ->
                                                     (case (_lhsIfxTvM) of
                                                      { _tlOfxTvM ->
                                                      (case (_lhsIbaseQu) of
                                                       { _tlObaseQu ->
                                                       (case (_lhsItyCtxt) of
                                                        { _prOtyCtxt ->
                                                        (case (_lhsItvIsBound) of
                                                         { _prOtvIsBound ->
                                                         (case (_lhsIrank) of
                                                          { _prOrank ->
                                                          (case (_lhsIfxTvM) of
                                                           { _prOfxTvM ->
                                                           (case (_lhsIbaseQu) of
                                                            { _prObaseQu ->
                                                            (case (_lhsIqSurrTvS) of
                                                             { _tlOqSurrTvS ->
                                                             (case (_lhsIisRow) of
                                                              { _tlOisRow ->
                                                              (case (tl_1 _tlObaseQu _tlOfxTvM _tlOisRow _tlOqSurrTvS _tlOrank _tlOtvIsBound _tlOtyCtxt ) of
                                                               { ( _tlIquTy,_tlIself) ->
                                                                   (case (_lhsIqSurrTvS) of
                                                                    { _prOqSurrTvS ->
                                                                    (case (_lhsIisRow) of
                                                                     { _prOisRow ->
                                                                     (case (pr_1 _prObaseQu _prOfxTvM _prOisRow _prOqSurrTvS _prOrank _prOtvIsBound _prOtyCtxt ) of
                                                                      { ( _prIquTy,_prIself) ->
                                                                          (case (Impls_Cons iv_ _prIquTy pv_ prange_ proveOccs_ _tlIquTy) of
                                                                           { _quTy ->
                                                                           (case (_quTy) of
                                                                            { _lhsOquTy ->
                                                                            (case (Impls_Cons iv_ _prIself pv_ prange_ proveOccs_ _tlIself) of
                                                                             { _self ->
                                                                             (case (_self) of
                                                                              { _lhsOself ->
                                                                              ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                     in  sem_Impls_Cons_1)) of
                              { ( sem_Impls_1) ->
                              ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Impls_1) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case ([]) of
          { _lhsOfrTvSL ->
          (case (Map.empty) of
           { _lhsOgathFxTvM ->
           (case (Set.empty) of
            { _lhsOqInsideTvS ->
            (case (Set.empty) of
             { _lhsOqOrphanTvS ->
             (case ((let sem_Impls_Nil_1 :: T_Impls_1 
                         sem_Impls_Nil_1  =
                             (\ _lhsIbaseQu
                                _lhsIfxTvM
                                _lhsIisRow
                                _lhsIqSurrTvS
                                _lhsIrank
                                _lhsItvIsBound
                                _lhsItyCtxt ->
                                  (case (Impls_Nil) of
                                   { _quTy ->
                                   (case (_quTy) of
                                    { _lhsOquTy ->
                                    (case (Impls_Nil) of
                                     { _self ->
                                     (case (_self) of
                                      { _lhsOself ->
                                      ( _lhsOquTy,_lhsOself) }) }) }) }))
                     in  sem_Impls_Nil_1)) of
              { ( sem_Impls_1) ->
              ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Impls_1) }) }) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case ([]) of
          { _lhsOfrTvSL ->
          (case (Map.empty) of
           { _lhsOgathFxTvM ->
           (case (Set.empty) of
            { _lhsOqInsideTvS ->
            (case (Set.empty) of
             { _lhsOqOrphanTvS ->
             (case ((let sem_Impls_Tail_1 :: T_Impls_1 
                         sem_Impls_Tail_1  =
                             (\ _lhsIbaseQu
                                _lhsIfxTvM
                                _lhsIisRow
                                _lhsIqSurrTvS
                                _lhsIrank
                                _lhsItvIsBound
                                _lhsItyCtxt ->
                                  (case (Impls_Tail iv_ proveOccs_) of
                                   { _quTy ->
                                   (case (_quTy) of
                                    { _lhsOquTy ->
                                    (case (Impls_Tail iv_ proveOccs_) of
                                     { _self ->
                                     (case (_self) of
                                      { _lhsOself ->
                                      ( _lhsOquTy,_lhsOself) }) }) }) }))
                     in  sem_Impls_Tail_1)) of
              { ( sem_Impls_1) ->
              ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Impls_1) }) }) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local quTy        : _
            local self        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
            local quTy        : _
            local self        : _
-}
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label (Label_Lab _nm )  =
    (sem_Label_Lab _nm )
sem_Label (Label_Var _lv )  =
    (sem_Label_Var _lv )
-- semantic domain
type T_Label  = ( Label ,Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (case (Label_Lab nm_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (Label_Lab nm_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (case (Label_Var lv_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (Label_Var lv_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
-- LabelAGItf --------------------------------------------------
{-
   alternatives:
      alternative AGItf:
         child lab            : Label 
-}
-- cata
sem_LabelAGItf :: LabelAGItf  ->
                  T_LabelAGItf 
sem_LabelAGItf (LabelAGItf_AGItf _lab )  =
    (sem_LabelAGItf_AGItf (sem_Label _lab ) )
-- semantic domain
type T_LabelAGItf  = ( )
sem_LabelAGItf_AGItf :: T_Label  ->
                        T_LabelAGItf 
sem_LabelAGItf_AGItf lab_  =
    ( )
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isAtTop              : Bool
         opts                 : TyQuOpts
         pol                  : Polarity
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
   visit 1:
      inherited attributes:
         baseQu               : TyQu 
         fxTvM                : TvCatMp
         isRow                : Bool
         qSurrTvS             : TyVarIdS
         rank                 : Int
         tvIsBound            : TvLevIsBound
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 1:
            local appSpinePos : _
            local quTy        : _
            local self        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local appSpinePos : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local appSpinePos : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 1:
            local appSpinePos : _
            local quTy        : _
            local self        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 1:
            local quTy        : _
            local self        : _
-}
-- cata
sem_Pred :: Pred  ->
            T_Pred 
sem_Pred (Pred_Arrow _args _res )  =
    (sem_Pred_Arrow (sem_PredSeq _args ) (sem_Pred _res ) )
sem_Pred (Pred_Class _ty )  =
    (sem_Pred_Class (sem_Ty _ty ) )
sem_Pred (Pred_Eq _tyL _tyR )  =
    (sem_Pred_Eq (sem_Ty _tyL ) (sem_Ty _tyR ) )
sem_Pred (Pred_Lacks _ty _lab )  =
    (sem_Pred_Lacks (sem_Ty _ty ) (sem_Label _lab ) )
sem_Pred (Pred_Pred _ty )  =
    (sem_Pred_Pred (sem_Ty _ty ) )
sem_Pred (Pred_Preds _seq )  =
    (sem_Pred_Preds (sem_PredSeq _seq ) )
sem_Pred (Pred_Var _pv )  =
    (sem_Pred_Var _pv )
-- semantic domain
type T_Pred  = Bool ->
               TyQuOpts ->
               Polarity ->
               ( ([TyVarIdS]),TvCatMp,TyVarIdS,TyVarIdS,T_Pred_1 )
type T_Pred_1  = TyQu  ->
                 TvCatMp ->
                 Bool ->
                 TyVarIdS ->
                 Int ->
                 TvLevIsBound ->
                 TyQuCtxt ->
                 ( Pred ,Pred )
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIpol) of
          { _resOpol ->
          (case (_lhsIopts) of
           { _resOopts ->
           (case (_lhsIisAtTop) of
            { _resOisAtTop ->
            (case (res_ _resOisAtTop _resOopts _resOpol ) of
             { ( _resIfrTvSL,_resIgathFxTvM,_resIqInsideTvS,_resIqOrphanTvS,res_1) ->
                 (case (_lhsIpol) of
                  { _argsOpol ->
                  (case (_lhsIopts) of
                   { _argsOopts ->
                   (case (_lhsIisAtTop) of
                    { _argsOisAtTop ->
                    (case (args_ _argsOisAtTop _argsOopts _argsOpol ) of
                     { ( _argsIfrTvSL,_argsIgathFxTvM,_argsIqInsideTvS,args_1) ->
                         (case (_argsIfrTvSL ++ _resIfrTvSL) of
                          { _lhsOfrTvSL ->
                          (case (_argsIgathFxTvM `Map.union` _resIgathFxTvM) of
                           { _lhsOgathFxTvM ->
                           (case (_argsIqInsideTvS `Set.union` _resIqInsideTvS) of
                            { _lhsOqInsideTvS ->
                            (case (_resIqOrphanTvS) of
                             { _lhsOqOrphanTvS ->
                             (case ((let sem_Pred_Arrow_1 :: T_Pred_1 
                                         sem_Pred_Arrow_1  =
                                             (\ _lhsIbaseQu
                                                _lhsIfxTvM
                                                _lhsIisRow
                                                _lhsIqSurrTvS
                                                _lhsIrank
                                                _lhsItvIsBound
                                                _lhsItyCtxt ->
                                                  (case (_lhsItyCtxt) of
                                                   { _resOtyCtxt ->
                                                   (case (_lhsItvIsBound) of
                                                    { _resOtvIsBound ->
                                                    (case (_lhsIrank) of
                                                     { _resOrank ->
                                                     (case (_lhsIfxTvM) of
                                                      { _resOfxTvM ->
                                                      (case (_lhsIbaseQu) of
                                                       { _resObaseQu ->
                                                       (case (_lhsItyCtxt) of
                                                        { _argsOtyCtxt ->
                                                        (case (_lhsItvIsBound) of
                                                         { _argsOtvIsBound ->
                                                         (case (_lhsIrank) of
                                                          { _argsOrank ->
                                                          (case (_lhsIfxTvM) of
                                                           { _argsOfxTvM ->
                                                           (case (_lhsIbaseQu) of
                                                            { _argsObaseQu ->
                                                            (case (_lhsIqSurrTvS) of
                                                             { _resOqSurrTvS ->
                                                             (case (_lhsIisRow) of
                                                              { _resOisRow ->
                                                              (case (res_1 _resObaseQu _resOfxTvM _resOisRow _resOqSurrTvS _resOrank _resOtvIsBound _resOtyCtxt ) of
                                                               { ( _resIquTy,_resIself) ->
                                                                   (case (_lhsIqSurrTvS) of
                                                                    { _argsOqSurrTvS ->
                                                                    (case (_lhsIisRow) of
                                                                     { _argsOisRow ->
                                                                     (case (0) of
                                                                      { _appSpinePos ->
                                                                      (case (_appSpinePos) of
                                                                       { _argsOappSpinePos ->
                                                                       (case (args_1 _argsOappSpinePos _argsObaseQu _argsOfxTvM _argsOisRow _argsOqSurrTvS _argsOrank _argsOtvIsBound _argsOtyCtxt ) of
                                                                        { ( _argsIquTy,_argsIself) ->
                                                                            (case (Pred_Arrow _argsIquTy _resIquTy) of
                                                                             { _quTy ->
                                                                             (case (_quTy) of
                                                                              { _lhsOquTy ->
                                                                              (case (Pred_Arrow _argsIself _resIself) of
                                                                               { _self ->
                                                                               (case (_self) of
                                                                                { _lhsOself ->
                                                                                ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                     in  sem_Pred_Arrow_1)) of
                              { ( sem_Pred_1) ->
                              ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (0) of
          { _appSpinePos ->
          (case (_appSpinePos) of
           { _tyOappSpinePos ->
           (case (_lhsIopts) of
            { _tyOopts ->
            (case (_lhsIisAtTop) of
             { _tyOisAtTop ->
             (case (polInvariant) of
              { _tyOpol ->
              (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
               { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                   (case (_tyIfrTvSL) of
                    { _lhsOfrTvSL ->
                    (case (_tyIgathFxTvM) of
                     { _lhsOgathFxTvM ->
                     (case (_tyIqInsideTvS) of
                      { _lhsOqInsideTvS ->
                      (case (_tyIqOrphanTvS) of
                       { _lhsOqOrphanTvS ->
                       (case ((let sem_Pred_Class_1 :: T_Pred_1 
                                   sem_Pred_Class_1  =
                                       (\ _lhsIbaseQu
                                          _lhsIfxTvM
                                          _lhsIisRow
                                          _lhsIqSurrTvS
                                          _lhsIrank
                                          _lhsItvIsBound
                                          _lhsItyCtxt ->
                                            (case (_lhsItyCtxt) of
                                             { _tyOtyCtxt ->
                                             (case (_lhsItvIsBound) of
                                              { _tyOtvIsBound ->
                                              (case (_lhsIrank) of
                                               { _tyOrank ->
                                               (case (_lhsIfxTvM) of
                                                { _tyOfxTvM ->
                                                (case (_lhsIbaseQu) of
                                                 { _tyObaseQu ->
                                                 (case (_lhsIqSurrTvS) of
                                                  { _tyOqSurrTvS ->
                                                  (case (_lhsIisRow) of
                                                   { _tyOisRow ->
                                                   (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                    { ( _tyIquTy,_tyIself) ->
                                                        (case (Pred_Class _tyIquTy) of
                                                         { _quTy ->
                                                         (case (_quTy) of
                                                          { _lhsOquTy ->
                                                          (case (Pred_Class _tyIself) of
                                                           { _self ->
                                                           (case (_self) of
                                                            { _lhsOself ->
                                                            ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }))
                               in  sem_Pred_Class_1)) of
                        { ( sem_Pred_1) ->
                        ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (0) of
          { _appSpinePos ->
          (case (_appSpinePos) of
           { _tyROappSpinePos ->
           (case (_appSpinePos) of
            { _tyLOappSpinePos ->
            (case (_lhsIpol) of
             { _tyROpol ->
             (case (_lhsIopts) of
              { _tyROopts ->
              (case (_lhsIisAtTop) of
               { _tyROisAtTop ->
               (case (tyR_ _tyROappSpinePos _tyROisAtTop _tyROopts _tyROpol ) of
                { ( _tyRIappFunNm,_tyRIfrTvSL,_tyRIgathFxTvM,_tyRIisArrow,_tyRIisPred,_tyRIisQuLoc,_tyRIqInsideTvS,_tyRIqOrphanTvS,tyR_1) ->
                    (case (_lhsIpol) of
                     { _tyLOpol ->
                     (case (_lhsIopts) of
                      { _tyLOopts ->
                      (case (_lhsIisAtTop) of
                       { _tyLOisAtTop ->
                       (case (tyL_ _tyLOappSpinePos _tyLOisAtTop _tyLOopts _tyLOpol ) of
                        { ( _tyLIappFunNm,_tyLIfrTvSL,_tyLIgathFxTvM,_tyLIisArrow,_tyLIisPred,_tyLIisQuLoc,_tyLIqInsideTvS,_tyLIqOrphanTvS,tyL_1) ->
                            (case (_tyLIfrTvSL ++ _tyRIfrTvSL) of
                             { _lhsOfrTvSL ->
                             (case (_tyLIgathFxTvM `Map.union` _tyRIgathFxTvM) of
                              { _lhsOgathFxTvM ->
                              (case (_tyLIqInsideTvS `Set.union` _tyRIqInsideTvS) of
                               { _lhsOqInsideTvS ->
                               (case (_tyRIqOrphanTvS) of
                                { _lhsOqOrphanTvS ->
                                (case ((let sem_Pred_Eq_1 :: T_Pred_1 
                                            sem_Pred_Eq_1  =
                                                (\ _lhsIbaseQu
                                                   _lhsIfxTvM
                                                   _lhsIisRow
                                                   _lhsIqSurrTvS
                                                   _lhsIrank
                                                   _lhsItvIsBound
                                                   _lhsItyCtxt ->
                                                     (case (_lhsItyCtxt) of
                                                      { _tyROtyCtxt ->
                                                      (case (_lhsItvIsBound) of
                                                       { _tyROtvIsBound ->
                                                       (case (_lhsIrank) of
                                                        { _tyROrank ->
                                                        (case (_lhsIfxTvM) of
                                                         { _tyROfxTvM ->
                                                         (case (_lhsIbaseQu) of
                                                          { _tyRObaseQu ->
                                                          (case (_lhsItyCtxt) of
                                                           { _tyLOtyCtxt ->
                                                           (case (_lhsItvIsBound) of
                                                            { _tyLOtvIsBound ->
                                                            (case (_lhsIrank) of
                                                             { _tyLOrank ->
                                                             (case (_lhsIfxTvM) of
                                                              { _tyLOfxTvM ->
                                                              (case (_lhsIbaseQu) of
                                                               { _tyLObaseQu ->
                                                               (case (_lhsIqSurrTvS) of
                                                                { _tyROqSurrTvS ->
                                                                (case (_lhsIisRow) of
                                                                 { _tyROisRow ->
                                                                 (case (tyR_1 _tyRObaseQu _tyROfxTvM _tyROisRow _tyROqSurrTvS _tyROrank _tyROtvIsBound _tyROtyCtxt ) of
                                                                  { ( _tyRIquTy,_tyRIself) ->
                                                                      (case (_lhsIqSurrTvS) of
                                                                       { _tyLOqSurrTvS ->
                                                                       (case (_lhsIisRow) of
                                                                        { _tyLOisRow ->
                                                                        (case (tyL_1 _tyLObaseQu _tyLOfxTvM _tyLOisRow _tyLOqSurrTvS _tyLOrank _tyLOtvIsBound _tyLOtyCtxt ) of
                                                                         { ( _tyLIquTy,_tyLIself) ->
                                                                             (case (Pred_Eq _tyLIquTy _tyRIquTy) of
                                                                              { _quTy ->
                                                                              (case (_quTy) of
                                                                               { _lhsOquTy ->
                                                                               (case (Pred_Eq _tyLIself _tyRIself) of
                                                                                { _self ->
                                                                                (case (_self) of
                                                                                 { _lhsOself ->
                                                                                 ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                        in  sem_Pred_Eq_1)) of
                                 { ( sem_Pred_1) ->
                                 ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (0) of
          { _appSpinePos ->
          (case (_appSpinePos) of
           { _tyOappSpinePos ->
           (case (_lhsIpol) of
            { _tyOpol ->
            (case (_lhsIopts) of
             { _tyOopts ->
             (case (_lhsIisAtTop) of
              { _tyOisAtTop ->
              (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
               { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                   (case (_tyIfrTvSL) of
                    { _lhsOfrTvSL ->
                    (case (_tyIgathFxTvM) of
                     { _lhsOgathFxTvM ->
                     (case (_tyIqInsideTvS) of
                      { _lhsOqInsideTvS ->
                      (case (_tyIqOrphanTvS) of
                       { _lhsOqOrphanTvS ->
                       (case ((let sem_Pred_Lacks_1 :: T_Pred_1 
                                   sem_Pred_Lacks_1  =
                                       (\ _lhsIbaseQu
                                          _lhsIfxTvM
                                          _lhsIisRow
                                          _lhsIqSurrTvS
                                          _lhsIrank
                                          _lhsItvIsBound
                                          _lhsItyCtxt ->
                                            (case (_lhsItyCtxt) of
                                             { _tyOtyCtxt ->
                                             (case (_lhsItvIsBound) of
                                              { _tyOtvIsBound ->
                                              (case (_lhsIrank) of
                                               { _tyOrank ->
                                               (case (_lhsIfxTvM) of
                                                { _tyOfxTvM ->
                                                (case (_lhsIbaseQu) of
                                                 { _tyObaseQu ->
                                                 (case (lab_ ) of
                                                  { ( _labIquTy,_labIself) ->
                                                      (case (_lhsIqSurrTvS) of
                                                       { _tyOqSurrTvS ->
                                                       (case (_lhsIisRow) of
                                                        { _tyOisRow ->
                                                        (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                         { ( _tyIquTy,_tyIself) ->
                                                             (case (Pred_Lacks _tyIquTy _labIquTy) of
                                                              { _quTy ->
                                                              (case (_quTy) of
                                                               { _lhsOquTy ->
                                                               (case (Pred_Lacks _tyIself _labIself) of
                                                                { _self ->
                                                                (case (_self) of
                                                                 { _lhsOself ->
                                                                 ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }))
                               in  sem_Pred_Lacks_1)) of
                        { ( sem_Pred_1) ->
                        ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (0) of
          { _appSpinePos ->
          (case (_appSpinePos) of
           { _tyOappSpinePos ->
           (case (_lhsIopts) of
            { _tyOopts ->
            (case (_lhsIisAtTop) of
             { _tyOisAtTop ->
             (case (polCovariant) of
              { _tyOpol ->
              (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
               { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                   (case (_tyIfrTvSL) of
                    { _lhsOfrTvSL ->
                    (case (_tyIgathFxTvM) of
                     { _lhsOgathFxTvM ->
                     (case (_tyIqInsideTvS) of
                      { _lhsOqInsideTvS ->
                      (case (_tyIqOrphanTvS) of
                       { _lhsOqOrphanTvS ->
                       (case ((let sem_Pred_Pred_1 :: T_Pred_1 
                                   sem_Pred_Pred_1  =
                                       (\ _lhsIbaseQu
                                          _lhsIfxTvM
                                          _lhsIisRow
                                          _lhsIqSurrTvS
                                          _lhsIrank
                                          _lhsItvIsBound
                                          _lhsItyCtxt ->
                                            (case (_lhsItyCtxt) of
                                             { _tyOtyCtxt ->
                                             (case (_lhsItvIsBound) of
                                              { _tyOtvIsBound ->
                                              (case (_lhsIrank) of
                                               { _tyOrank ->
                                               (case (_lhsIfxTvM) of
                                                { _tyOfxTvM ->
                                                (case (_lhsIbaseQu) of
                                                 { _tyObaseQu ->
                                                 (case (_lhsIqSurrTvS) of
                                                  { _tyOqSurrTvS ->
                                                  (case (_lhsIisRow) of
                                                   { _tyOisRow ->
                                                   (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                    { ( _tyIquTy,_tyIself) ->
                                                        (case (Pred_Pred _tyIquTy) of
                                                         { _quTy ->
                                                         (case (_quTy) of
                                                          { _lhsOquTy ->
                                                          (case (Pred_Pred _tyIself) of
                                                           { _self ->
                                                           (case (_self) of
                                                            { _lhsOself ->
                                                            ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }))
                               in  sem_Pred_Pred_1)) of
                        { ( sem_Pred_1) ->
                        ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIpol) of
          { _seqOpol ->
          (case (_lhsIopts) of
           { _seqOopts ->
           (case (_lhsIisAtTop) of
            { _seqOisAtTop ->
            (case (seq_ _seqOisAtTop _seqOopts _seqOpol ) of
             { ( _seqIfrTvSL,_seqIgathFxTvM,_seqIqInsideTvS,seq_1) ->
                 (case (_seqIfrTvSL) of
                  { _lhsOfrTvSL ->
                  (case (_seqIgathFxTvM) of
                   { _lhsOgathFxTvM ->
                   (case (_seqIqInsideTvS) of
                    { _lhsOqInsideTvS ->
                    (case (Set.empty) of
                     { _lhsOqOrphanTvS ->
                     (case ((let sem_Pred_Preds_1 :: T_Pred_1 
                                 sem_Pred_Preds_1  =
                                     (\ _lhsIbaseQu
                                        _lhsIfxTvM
                                        _lhsIisRow
                                        _lhsIqSurrTvS
                                        _lhsIrank
                                        _lhsItvIsBound
                                        _lhsItyCtxt ->
                                          (case (_lhsItyCtxt) of
                                           { _seqOtyCtxt ->
                                           (case (_lhsItvIsBound) of
                                            { _seqOtvIsBound ->
                                            (case (_lhsIrank) of
                                             { _seqOrank ->
                                             (case (_lhsIfxTvM) of
                                              { _seqOfxTvM ->
                                              (case (_lhsIbaseQu) of
                                               { _seqObaseQu ->
                                               (case (_lhsIqSurrTvS) of
                                                { _seqOqSurrTvS ->
                                                (case (_lhsIisRow) of
                                                 { _seqOisRow ->
                                                 (case (0) of
                                                  { _appSpinePos ->
                                                  (case (_appSpinePos) of
                                                   { _seqOappSpinePos ->
                                                   (case (seq_1 _seqOappSpinePos _seqObaseQu _seqOfxTvM _seqOisRow _seqOqSurrTvS _seqOrank _seqOtvIsBound _seqOtyCtxt ) of
                                                    { ( _seqIquTy,_seqIself) ->
                                                        (case (Pred_Preds _seqIquTy) of
                                                         { _quTy ->
                                                         (case (_quTy) of
                                                          { _lhsOquTy ->
                                                          (case (Pred_Preds _seqIself) of
                                                           { _self ->
                                                           (case (_self) of
                                                            { _lhsOself ->
                                                            ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                             in  sem_Pred_Preds_1)) of
                      { ( sem_Pred_1) ->
                      ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case ([]) of
          { _lhsOfrTvSL ->
          (case (Map.empty) of
           { _lhsOgathFxTvM ->
           (case (Set.empty) of
            { _lhsOqInsideTvS ->
            (case (Set.empty) of
             { _lhsOqOrphanTvS ->
             (case ((let sem_Pred_Var_1 :: T_Pred_1 
                         sem_Pred_Var_1  =
                             (\ _lhsIbaseQu
                                _lhsIfxTvM
                                _lhsIisRow
                                _lhsIqSurrTvS
                                _lhsIrank
                                _lhsItvIsBound
                                _lhsItyCtxt ->
                                  (case (Pred_Var pv_) of
                                   { _quTy ->
                                   (case (_quTy) of
                                    { _lhsOquTy ->
                                    (case (Pred_Var pv_) of
                                     { _self ->
                                     (case (_self) of
                                      { _lhsOself ->
                                      ( _lhsOquTy,_lhsOself) }) }) }) }))
                     in  sem_Pred_Var_1)) of
              { ( sem_Pred_1) ->
              ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Pred_1) }) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isAtTop              : Bool
         opts                 : TyQuOpts
         pol                  : Polarity
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         qInsideTvS           : TyVarIdS
   visit 1:
      inherited attributes:
         appSpinePos          : Int
         baseQu               : TyQu 
         fxTvM                : TvCatMp
         isRow                : Bool
         qSurrTvS             : TyVarIdS
         rank                 : Int
         tvIsBound            : TvLevIsBound
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 1:
            local quTy        : _
            local self        : _
      alternative Nil:
         visit 1:
            local quTy        : _
            local self        : _
      alternative Var:
         child av             : {TyVarId}
         visit 1:
            local quTy        : _
            local self        : _
-}
-- cata
sem_PredSeq :: PredSeq  ->
               T_PredSeq 
sem_PredSeq (PredSeq_Cons _hd _tl )  =
    (sem_PredSeq_Cons (sem_Pred _hd ) (sem_PredSeq _tl ) )
sem_PredSeq (PredSeq_Nil )  =
    (sem_PredSeq_Nil )
sem_PredSeq (PredSeq_Var _av )  =
    (sem_PredSeq_Var _av )
-- semantic domain
type T_PredSeq  = Bool ->
                  TyQuOpts ->
                  Polarity ->
                  ( ([TyVarIdS]),TvCatMp,TyVarIdS,T_PredSeq_1 )
type T_PredSeq_1  = Int ->
                    TyQu  ->
                    TvCatMp ->
                    Bool ->
                    TyVarIdS ->
                    Int ->
                    TvLevIsBound ->
                    TyQuCtxt ->
                    ( PredSeq ,PredSeq )
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIpol) of
          { _tlOpol ->
          (case (_lhsIopts) of
           { _tlOopts ->
           (case (_lhsIisAtTop) of
            { _tlOisAtTop ->
            (case (tl_ _tlOisAtTop _tlOopts _tlOpol ) of
             { ( _tlIfrTvSL,_tlIgathFxTvM,_tlIqInsideTvS,tl_1) ->
                 (case (_lhsIpol) of
                  { _hdOpol ->
                  (case (_lhsIopts) of
                   { _hdOopts ->
                   (case (_lhsIisAtTop) of
                    { _hdOisAtTop ->
                    (case (hd_ _hdOisAtTop _hdOopts _hdOpol ) of
                     { ( _hdIfrTvSL,_hdIgathFxTvM,_hdIqInsideTvS,_hdIqOrphanTvS,hd_1) ->
                         (case (_hdIfrTvSL ++ _tlIfrTvSL) of
                          { _lhsOfrTvSL ->
                          (case (_hdIgathFxTvM `Map.union` _tlIgathFxTvM) of
                           { _lhsOgathFxTvM ->
                           (case (_hdIqInsideTvS `Set.union` _tlIqInsideTvS) of
                            { _lhsOqInsideTvS ->
                            (case ((let sem_PredSeq_Cons_1 :: T_PredSeq_1 
                                        sem_PredSeq_Cons_1  =
                                            (\ _lhsIappSpinePos
                                               _lhsIbaseQu
                                               _lhsIfxTvM
                                               _lhsIisRow
                                               _lhsIqSurrTvS
                                               _lhsIrank
                                               _lhsItvIsBound
                                               _lhsItyCtxt ->
                                                 (case (_lhsItyCtxt) of
                                                  { _tlOtyCtxt ->
                                                  (case (_lhsItvIsBound) of
                                                   { _tlOtvIsBound ->
                                                   (case (_lhsIrank) of
                                                    { _tlOrank ->
                                                    (case (_lhsIfxTvM) of
                                                     { _tlOfxTvM ->
                                                     (case (_lhsIbaseQu) of
                                                      { _tlObaseQu ->
                                                      (case (_lhsItyCtxt) of
                                                       { _hdOtyCtxt ->
                                                       (case (_lhsItvIsBound) of
                                                        { _hdOtvIsBound ->
                                                        (case (_lhsIrank) of
                                                         { _hdOrank ->
                                                         (case (_lhsIfxTvM) of
                                                          { _hdOfxTvM ->
                                                          (case (_lhsIbaseQu) of
                                                           { _hdObaseQu ->
                                                           (case (_lhsIqSurrTvS) of
                                                            { _tlOqSurrTvS ->
                                                            (case (_lhsIisRow) of
                                                             { _tlOisRow ->
                                                             (case (_lhsIappSpinePos + 1) of
                                                              { _tlOappSpinePos ->
                                                              (case (tl_1 _tlOappSpinePos _tlObaseQu _tlOfxTvM _tlOisRow _tlOqSurrTvS _tlOrank _tlOtvIsBound _tlOtyCtxt ) of
                                                               { ( _tlIquTy,_tlIself) ->
                                                                   (case (_lhsIqSurrTvS) of
                                                                    { _hdOqSurrTvS ->
                                                                    (case (_lhsIisRow) of
                                                                     { _hdOisRow ->
                                                                     (case (hd_1 _hdObaseQu _hdOfxTvM _hdOisRow _hdOqSurrTvS _hdOrank _hdOtvIsBound _hdOtyCtxt ) of
                                                                      { ( _hdIquTy,_hdIself) ->
                                                                          (case (PredSeq_Cons _hdIquTy _tlIquTy) of
                                                                           { _quTy ->
                                                                           (case (_quTy) of
                                                                            { _lhsOquTy ->
                                                                            (case (PredSeq_Cons _hdIself _tlIself) of
                                                                             { _self ->
                                                                             (case (_self) of
                                                                              { _lhsOself ->
                                                                              ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                    in  sem_PredSeq_Cons_1)) of
                             { ( sem_PredSeq_1) ->
                             ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,sem_PredSeq_1) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case ([]) of
          { _lhsOfrTvSL ->
          (case (Map.empty) of
           { _lhsOgathFxTvM ->
           (case (Set.empty) of
            { _lhsOqInsideTvS ->
            (case ((let sem_PredSeq_Nil_1 :: T_PredSeq_1 
                        sem_PredSeq_Nil_1  =
                            (\ _lhsIappSpinePos
                               _lhsIbaseQu
                               _lhsIfxTvM
                               _lhsIisRow
                               _lhsIqSurrTvS
                               _lhsIrank
                               _lhsItvIsBound
                               _lhsItyCtxt ->
                                 (case (PredSeq_Nil) of
                                  { _quTy ->
                                  (case (_quTy) of
                                   { _lhsOquTy ->
                                   (case (PredSeq_Nil) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsOquTy,_lhsOself) }) }) }) }))
                    in  sem_PredSeq_Nil_1)) of
             { ( sem_PredSeq_1) ->
             ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,sem_PredSeq_1) }) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case ([]) of
          { _lhsOfrTvSL ->
          (case (Map.empty) of
           { _lhsOgathFxTvM ->
           (case (Set.empty) of
            { _lhsOqInsideTvS ->
            (case ((let sem_PredSeq_Var_1 :: T_PredSeq_1 
                        sem_PredSeq_Var_1  =
                            (\ _lhsIappSpinePos
                               _lhsIbaseQu
                               _lhsIfxTvM
                               _lhsIisRow
                               _lhsIqSurrTvS
                               _lhsIrank
                               _lhsItvIsBound
                               _lhsItyCtxt ->
                                 (case (PredSeq_Var av_) of
                                  { _quTy ->
                                  (case (_quTy) of
                                   { _lhsOquTy ->
                                   (case (PredSeq_Var av_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsOquTy,_lhsOself) }) }) }) }))
                    in  sem_PredSeq_Var_1)) of
             { ( sem_PredSeq_1) ->
             ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOqInsideTvS,sem_PredSeq_1) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         isAtTop              : Bool
         opts                 : TyQuOpts
         pol                  : Polarity
      synthesized attributes:
         appFunNm             : HsName
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         isArrow              : Bool
         isPred               : Bool
         isQuLoc              : Bool
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
   visit 1:
      inherited attributes:
         baseQu               : TyQu 
         fxTvM                : TvCatMp
         isRow                : Bool
         qSurrTvS             : TyVarIdS
         rank                 : Int
         tvIsBound            : TvLevIsBound
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 0:
            local isAtTop     : _
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local quTy        : _
            local self        : _
      alternative Any:
         visit 0:
            local frTvS       : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local isAtTop     : _
            local appFunNm    : {HsName}
            local isSpineRoot : {Bool}
            local appIsSum    : _
            local appIsRec    : {Bool}
            local appIsRecOrSum : _
            local argIsRow    : {Bool}
            local polArrowRes : _
            local appIsArrow  : {Bool}
            local isArrowArg  : {Bool}
            local isArrowRoot : {Bool}
            local appIsLikeProd : {Bool}
            local _tup3       : {(TyVarIdS,[TyVarIdS])}
            local frTvSL      : {[TyVarIdS]}
            local frRowTvS    : {TyVarIdS}
            local frTvS       : {TyVarIdS}
            local isProdRoot  : _
            local isQuLocExtraHook : _
            local isQuLoc     : {Bool}
            local qOrphanTvS  : {TyVarIdS}
            local qHereTvS    : {TyVarIdS}
         visit 1:
            local tyCtxt      : _
            local qAsExist    : _
            local hereQu      : _
            local isQuFxLoc   : _
            local _tup2       : _
            local tvIsBound   : _
            local qSurrTvS    : _
            local quTyBase    : _
            local qBndL1Tvs   : _
            local qBndExTvs   : _
            local qBndTvs     : _
            local isQuRank    : _
            local self        : _
            intra appIsLikeProd : {Bool}
            intra appIsArrow  : {Bool}
            intra qHereTvS    : {TyVarIdS}
            intra isQuLoc     : {Bool}
            intra isArrowArg  : {Bool}
            intra argIsRow    : {Bool}
            intra frRowTvS    : {TyVarIdS}
      alternative Con:
         child nm             : {HsName}
         visit 0:
            local frTvS       : _
         visit 1:
            local quTy        : _
            local self        : _
      alternative Dbg:
         child info           : {String}
         visit 1:
            local quTy        : _
            local self        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local isAtTop     : _
            local appFunNm    : {HsName}
            local frTvSL      : _
            local qOrphanTvS  : _
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local quTy        : _
            local self        : _
      alternative Impls:
         child impls          : Impls 
         visit 0:
            local isAtTop     : _
            local frTvS       : _
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local quTy        : _
            local self        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 0:
            local introTVarS  : {TyVarIdS}
            local isAtTop     : _
            local frTvS       : _
         visit 1:
            local tyCtxt      : _
            local tvIsBound   : _
            local isRow       : _
            local self        : _
            intra introTVarS  : {TyVarIdS}
      alternative Pred:
         child pr             : Pred 
         visit 0:
            local isAtTop     : _
            local frTvS       : {TyVarIdS}
            local isQuLocExtraHook : _
            local isQuLoc     : {Bool}
         visit 1:
            local tyCtxt      : _
            local qHereTvS    : _
            local _tup4       : _
            local tvIsBound   : _
            local qAsExist    : _
            local hereQu      : _
            local qBndL1Tvs   : _
            local qBndExTvs   : _
            local qBndTvs     : _
            local isQuRank    : _
            local isRow       : _
            local self        : _
            intra frTvS       : {TyVarIdS}
            intra isQuLoc     : {Bool}
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 0:
            local introTVarS  : {TyVarIdS}
            local isAtTop     : _
            local frTvS       : {TyVarIdS}
            local isQuLocExtraHook : _
            local isQuLoc     : {Bool}
         visit 1:
            local tyCtxt      : _
            local qHereTvS    : _
            local hereQu      : _
            local _tup6       : _
            local tvIsBoundQu : _
            local isQuFxLoc   : _
            local _tup5       : _
            local tvIsBound   : _
            local qSurrTvS    : _
            local isRow       : _
            local quTyBase    : _
            local qBndL1TvsQu : _
            local qBndL1TvsOther : _
            local qBndL1Tvs   : _
            local qBndExTvs   : _
            local qBndTvs     : _
            local isQuRank    : _
            local self        : _
            intra frTvS       : {TyVarIdS}
            intra isQuLoc     : {Bool}
            intra introTVarS  : {TyVarIdS}
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 0:
            local frTvS       : {TyVarIdS}
            local isQuLocExtraHook : _
            local isQuLoc     : {Bool}
         visit 1:
            local qHereTvS    : _
            local quTyBase    : _
            local qAsExist    : _
            local hereQu      : _
            local isQuFxLoc   : _
            local _tup8       : _
            local qBndL1Tvs   : _
            local qBndExTvs   : _
            local qBndTvs     : _
            local isQuRank    : _
            local self        : _
            intra frTvS       : {TyVarIdS}
            intra isQuLoc     : {Bool}
-}
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Ty_Ann _ann _ty )  =
    (sem_Ty_Ann (sem_TyAnn _ann ) (sem_Ty _ty ) )
sem_Ty (Ty_Any )  =
    (sem_Ty_Any )
sem_Ty (Ty_App _func _arg )  =
    (sem_Ty_App (sem_Ty _func ) (sem_Ty _arg ) )
sem_Ty (Ty_Con _nm )  =
    (sem_Ty_Con _nm )
sem_Ty (Ty_Dbg _info )  =
    (sem_Ty_Dbg _info )
sem_Ty (Ty_Ext _ty _nm _extTy )  =
    (sem_Ty_Ext (sem_Ty _ty ) _nm (sem_Ty _extTy ) )
sem_Ty (Ty_Impls _impls )  =
    (sem_Ty_Impls (sem_Impls _impls ) )
sem_Ty (Ty_Lam _tv _ty )  =
    (sem_Ty_Lam _tv (sem_Ty _ty ) )
sem_Ty (Ty_Pred _pr )  =
    (sem_Ty_Pred (sem_Pred _pr ) )
sem_Ty (Ty_TBind _qu _tv _l1 _ty )  =
    (sem_Ty_TBind (sem_TyQu _qu ) _tv _l1 (sem_Ty _ty ) )
sem_Ty (Ty_Var _tv _categ )  =
    (sem_Ty_Var _tv (sem_TyVarCateg _categ ) )
-- semantic domain
type T_Ty  = Int ->
             Bool ->
             TyQuOpts ->
             Polarity ->
             ( HsName,([TyVarIdS]),TvCatMp,Bool,Bool,Bool,TyVarIdS,TyVarIdS,T_Ty_1 )
type T_Ty_1  = TyQu  ->
               TvCatMp ->
               Bool ->
               TyVarIdS ->
               Int ->
               TvLevIsBound ->
               TyQuCtxt ->
               ( Ty ,Ty )
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (_lhsIappSpinePos) of
           { _tyOappSpinePos ->
           (case (_lhsIpol) of
            { _tyOpol ->
            (case (_lhsIopts) of
             { _tyOopts ->
             (case (False) of
              { _isAtTop ->
              (case (_isAtTop) of
               { _tyOisAtTop ->
               (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
                { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                    (case (_tyIfrTvSL) of
                     { _lhsOfrTvSL ->
                     (case (_tyIgathFxTvM) of
                      { _lhsOgathFxTvM ->
                      (case (False) of
                       { _lhsOisArrow ->
                       (case (False) of
                        { _lhsOisPred ->
                        (case (_tyIisQuLoc) of
                         { _lhsOisQuLoc ->
                         (case (_tyIqInsideTvS) of
                          { _lhsOqInsideTvS ->
                          (case (Set.empty) of
                           { _lhsOqOrphanTvS ->
                           (case ((let sem_Ty_Ann_1 :: T_Ty_1 
                                       sem_Ty_Ann_1  =
                                           (\ _lhsIbaseQu
                                              _lhsIfxTvM
                                              _lhsIisRow
                                              _lhsIqSurrTvS
                                              _lhsIrank
                                              _lhsItvIsBound
                                              _lhsItyCtxt ->
                                                (case (TyQuCtxtOther) of
                                                 { _tyCtxt ->
                                                 (case (_tyCtxt) of
                                                  { _tyOtyCtxt ->
                                                  (case (_lhsItvIsBound) of
                                                   { _tyOtvIsBound ->
                                                   (case (_lhsIrank) of
                                                    { _tyOrank ->
                                                    (case (_lhsIfxTvM) of
                                                     { _tyOfxTvM ->
                                                     (case (_lhsIbaseQu) of
                                                      { _tyObaseQu ->
                                                      (case (_lhsIqSurrTvS) of
                                                       { _tyOqSurrTvS ->
                                                       (case (False) of
                                                        { _isRow ->
                                                        (case (_isRow) of
                                                         { _tyOisRow ->
                                                         (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                          { ( _tyIquTy,_tyIself) ->
                                                              (case (ann_ ) of
                                                               { ( _annIquTy,_annIself) ->
                                                                   (case (Ty_Ann _annIquTy _tyIquTy) of
                                                                    { _quTy ->
                                                                    (case (_quTy) of
                                                                     { _lhsOquTy ->
                                                                     (case (Ty_Ann _annIself _tyIself) of
                                                                      { _self ->
                                                                      (case (_self) of
                                                                       { _lhsOself ->
                                                                       ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                   in  sem_Ty_Ann_1)) of
                            { ( sem_Ty_1) ->
                            ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (Set.empty) of
           { _frTvS ->
           (case ([_frTvS]) of
            { _lhsOfrTvSL ->
            (case (Map.empty) of
             { _lhsOgathFxTvM ->
             (case (False) of
              { _lhsOisArrow ->
              (case (False) of
               { _lhsOisPred ->
               (case (False) of
                { _lhsOisQuLoc ->
                (case (Set.empty) of
                 { _lhsOqInsideTvS ->
                 (case (Set.empty) of
                  { _lhsOqOrphanTvS ->
                  (case ((let sem_Ty_Any_1 :: T_Ty_1 
                              sem_Ty_Any_1  =
                                  (\ _lhsIbaseQu
                                     _lhsIfxTvM
                                     _lhsIisRow
                                     _lhsIqSurrTvS
                                     _lhsIrank
                                     _lhsItvIsBound
                                     _lhsItyCtxt ->
                                       (case (Ty_Any) of
                                        { _quTy ->
                                        (case (_quTy) of
                                         { _lhsOquTy ->
                                         (case (Ty_Any) of
                                          { _self ->
                                          (case (_self) of
                                           { _lhsOself ->
                                           ( _lhsOquTy,_lhsOself) }) }) }) }))
                          in  sem_Ty_Any_1)) of
                   { ( sem_Ty_1) ->
                   ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIopts) of
          { _funcOopts ->
          (case (False) of
           { _isAtTop ->
           (case (_isAtTop) of
            { _funcOisAtTop ->
            (case (_lhsIpol) of
             { _funcOpol ->
             (case (_lhsIappSpinePos + 1) of
              { _funcOappSpinePos ->
              (case (func_ _funcOappSpinePos _funcOisAtTop _funcOopts _funcOpol ) of
               { ( _funcIappFunNm,_funcIfrTvSL,_funcIgathFxTvM,_funcIisArrow,_funcIisPred,_funcIisQuLoc,_funcIqInsideTvS,_funcIqOrphanTvS,func_1) ->
                   (case (_funcIappFunNm) of
                    { _appFunNm ->
                    (case (_appFunNm) of
                     { _lhsOappFunNm ->
                     (case (_lhsIappSpinePos == 0) of
                      { _isSpineRoot ->
                      (case (0) of
                       { _argOappSpinePos ->
                       (case (hsnIsSum _funcIappFunNm) of
                        { _appIsSum ->
                        (case (hsnIsRec _funcIappFunNm) of
                         { _appIsRec ->
                         (case (_appIsRec || _appIsSum) of
                          { _appIsRecOrSum ->
                          (case (_isSpineRoot && _appIsRecOrSum) of
                           { _argIsRow ->
                           (case (_lhsIopts) of
                            { _argOopts ->
                            (case (_isAtTop) of
                             { _argOisAtTop ->
                             (case (if _funcIisPred then _lhsIpol else polCovariant) of
                              { _polArrowRes ->
                              (case (hsnIsArrow _funcIappFunNm) of
                               { _appIsArrow ->
                               (case (_appIsArrow && _lhsIappSpinePos == 1) of
                                { _isArrowArg ->
                                (case (_appIsArrow && _isSpineRoot) of
                                 { _isArrowRoot ->
                                 (case (hsnIsProd _funcIappFunNm || _appIsRec) of
                                  { _appIsLikeProd ->
                                  (case (if          _appIsLikeProd  then  _lhsIpol
                                         else  if    _isArrowRoot    then  _polArrowRes
                                         else  if    _isArrowArg     then  polContravariant
                                                                     else  polInvariant) of
                                   { _argOpol ->
                                   (case (arg_ _argOappSpinePos _argOisAtTop _argOopts _argOpol ) of
                                    { ( _argIappFunNm,_argIfrTvSL,_argIgathFxTvM,_argIisArrow,_argIisPred,_argIisQuLoc,_argIqInsideTvS,_argIqOrphanTvS,arg_1) ->
                                        (case (if _argIsRow
                                               then hdAndTl (reverse _argIfrTvSL)
                                               else (Set.empty,_argIfrTvSL ++ _funcIfrTvSL)) of
                                         { __tup3 ->
                                         (case (__tup3) of
                                          { (_,_frTvSL) ->
                                          (case (__tup3) of
                                           { (_frRowTvS,_) ->
                                           (case (Set.unions (_frRowTvS : _frTvSL)) of
                                            { _frTvS ->
                                            (case (if _isSpineRoot then [_frTvS] else _frTvSL) of
                                             { _lhsOfrTvSL ->
                                             (case (_funcIgathFxTvM `Map.union` _argIgathFxTvM) of
                                              { _lhsOgathFxTvM ->
                                              (case (_isArrowRoot) of
                                               { _lhsOisArrow ->
                                               (case (if _isArrowArg then _argIisPred else False) of
                                                { _lhsOisPred ->
                                                (case (_appIsLikeProd && _isSpineRoot) of
                                                 { _isProdRoot ->
                                                 (case (_lhsIisAtTop || tqoptAllowInnerQuant _lhsIopts) of
                                                  { _isQuLocExtraHook ->
                                                  (case ((  polIsCovariant _lhsIpol
                                                         || (polIsContravariant _lhsIpol
                                                             && (_isProdRoot
                                                                 || _isArrowRoot
                                                                    && (not _funcIisPred || _argIisQuLoc)
                                                            )   )
                                                         ) && _isQuLocExtraHook) of
                                                   { _isQuLoc ->
                                                   (case (if _isArrowArg then _argIisQuLoc else _isQuLoc) of
                                                    { _lhsOisQuLoc ->
                                                    (case (_argIqOrphanTvS `Set.union` _funcIqOrphanTvS) of
                                                     { _qOrphanTvS ->
                                                     (case (if _isQuLoc
                                                            then  if    _appIsArrow || _appIsLikeProd
                                                                  then  Set.unions [tvarSOccurGE2 _frTvSL, _frRowTvS, _qOrphanTvS]
                                                                  else  _frTvS
                                                            else  Set.empty) of
                                                      { _qHereTvS ->
                                                      (case (_qHereTvS) of
                                                       { _lhsOqInsideTvS ->
                                                       (case (if _isSpineRoot && _isQuLoc then Set.empty else _qOrphanTvS) of
                                                        { _lhsOqOrphanTvS ->
                                                        (case ((let sem_Ty_App_1 :: T_Ty_1 
                                                                    sem_Ty_App_1  =
                                                                        (\ _lhsIbaseQu
                                                                           _lhsIfxTvM
                                                                           _lhsIisRow
                                                                           _lhsIqSurrTvS
                                                                           _lhsIrank
                                                                           _lhsItvIsBound
                                                                           _lhsItyCtxt ->
                                                                             (case (if      _appIsArrow     then TyQuCtxtArrow
                                                                                    else if _appIsLikeProd  then TyQuCtxtProd
                                                                                                            else TyQuCtxtOther) of
                                                                              { _tyCtxt ->
                                                                              (case (_tyCtxt) of
                                                                               { _argOtyCtxt ->
                                                                               (case ((_appIsLikeProd || polIsContravariant _lhsIpol && not _appIsArrow)) of
                                                                                { _qAsExist ->
                                                                                (case (if _qAsExist then tyquExists _lhsIbaseQu else _lhsIbaseQu) of
                                                                                 { _hereQu ->
                                                                                 (case (_lhsIrank > 1
                                                                                        ||  _lhsIrank == 1
                                                                                            &&  (_lhsItyCtxt == TyQuCtxtArrow && not _argIisArrow
                                                                                                || _lhsItyCtxt == TyQuCtxtOther
                                                                                                )) of
                                                                                  { _isQuFxLoc ->
                                                                                  (case (tvarsToQuant _lhsIopts _isQuLoc _lhsIfxTvM (tvMayQuFx _hereQu _lhsIfxTvM _isQuFxLoc) _lhsItvIsBound _qHereTvS) of
                                                                                   { __tup2 ->
                                                                                   (case (__tup2) of
                                                                                    { (_,_,_,_tvIsBound) ->
                                                                                    (case (_tvIsBound) of
                                                                                     { _argOtvIsBound ->
                                                                                     (case (_lhsIfxTvM) of
                                                                                      { _argOfxTvM ->
                                                                                      (case (_lhsIbaseQu) of
                                                                                       { _argObaseQu ->
                                                                                       (case (_tyCtxt) of
                                                                                        { _funcOtyCtxt ->
                                                                                        (case (_tvIsBound) of
                                                                                         { _funcOtvIsBound ->
                                                                                         (case (_lhsIrank) of
                                                                                          { _funcOrank ->
                                                                                          (case (_lhsIfxTvM) of
                                                                                           { _funcOfxTvM ->
                                                                                           (case (_lhsIbaseQu) of
                                                                                            { _funcObaseQu ->
                                                                                            (case (_lhsIrank + (if _isArrowArg then 1 else 0)) of
                                                                                             { _argOrank ->
                                                                                             (case (_qHereTvS `Set.union` _lhsIqSurrTvS) of
                                                                                              { _qSurrTvS ->
                                                                                              (case (_qSurrTvS) of
                                                                                               { _argOqSurrTvS ->
                                                                                               (case (_argIsRow) of
                                                                                                { _argOisRow ->
                                                                                                (case (arg_1 _argObaseQu _argOfxTvM _argOisRow _argOqSurrTvS _argOrank _argOtvIsBound _argOtyCtxt ) of
                                                                                                 { ( _argIquTy,_argIself) ->
                                                                                                     (case (_qSurrTvS) of
                                                                                                      { _funcOqSurrTvS ->
                                                                                                      (case (_lhsIisRow) of
                                                                                                       { _funcOisRow ->
                                                                                                       (case (func_1 _funcObaseQu _funcOfxTvM _funcOisRow _funcOqSurrTvS _funcOrank _funcOtvIsBound _funcOtyCtxt ) of
                                                                                                        { ( _funcIquTy,_funcIself) ->
                                                                                                            (case (Ty_App _funcIquTy _argIquTy) of
                                                                                                             { _quTyBase ->
                                                                                                             (case (__tup2) of
                                                                                                              { (_,_,_qBndL1Tvs,_) ->
                                                                                                              (case (__tup2) of
                                                                                                               { (_,_qBndExTvs,_,_) ->
                                                                                                               (case (__tup2) of
                                                                                                                { (_qBndTvs,_,_,_) ->
                                                                                                                (case (_lhsIrank `elem` tqoptQuRanks _lhsIopts) of
                                                                                                                 { _isQuRank ->
                                                                                                                 (case (let  (rwYTvS,rwNTvS) = partition (\(v,_) -> v `Set.member` _frRowTvS) _qBndTvs
                                                                                                                        in   mkTyQuForRank _lhsIopts _isQuRank _hereQu _qBndL1Tvs rwNTvS _qBndExTvs
                                                                                                                             $  mkTyQuForRank _lhsIopts _isQuRank _lhsIbaseQu [] rwYTvS []
                                                                                                                             $  _quTyBase) of
                                                                                                                  { _lhsOquTy ->
                                                                                                                  (case (Ty_App _funcIself _argIself) of
                                                                                                                   { _self ->
                                                                                                                   (case (_self) of
                                                                                                                    { _lhsOself ->
                                                                                                                    ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                in  sem_Ty_App_1)) of
                                                         { ( sem_Ty_1) ->
                                                         ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (nm_) of
          { _lhsOappFunNm ->
          (case (Set.empty) of
           { _frTvS ->
           (case ([_frTvS]) of
            { _lhsOfrTvSL ->
            (case (Map.empty) of
             { _lhsOgathFxTvM ->
             (case (False) of
              { _lhsOisArrow ->
              (case (False) of
               { _lhsOisPred ->
               (case (False) of
                { _lhsOisQuLoc ->
                (case (Set.empty) of
                 { _lhsOqInsideTvS ->
                 (case (Set.empty) of
                  { _lhsOqOrphanTvS ->
                  (case ((let sem_Ty_Con_1 :: T_Ty_1 
                              sem_Ty_Con_1  =
                                  (\ _lhsIbaseQu
                                     _lhsIfxTvM
                                     _lhsIisRow
                                     _lhsIqSurrTvS
                                     _lhsIrank
                                     _lhsItvIsBound
                                     _lhsItyCtxt ->
                                       (case (Ty_Con nm_) of
                                        { _quTy ->
                                        (case (_quTy) of
                                         { _lhsOquTy ->
                                         (case (Ty_Con nm_) of
                                          { _self ->
                                          (case (_self) of
                                           { _lhsOself ->
                                           ( _lhsOquTy,_lhsOself) }) }) }) }))
                          in  sem_Ty_Con_1)) of
                   { ( sem_Ty_1) ->
                   ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case ([]) of
           { _lhsOfrTvSL ->
           (case (Map.empty) of
            { _lhsOgathFxTvM ->
            (case (False) of
             { _lhsOisArrow ->
             (case (False) of
              { _lhsOisPred ->
              (case (False) of
               { _lhsOisQuLoc ->
               (case (Set.empty) of
                { _lhsOqInsideTvS ->
                (case (Set.empty) of
                 { _lhsOqOrphanTvS ->
                 (case ((let sem_Ty_Dbg_1 :: T_Ty_1 
                             sem_Ty_Dbg_1  =
                                 (\ _lhsIbaseQu
                                    _lhsIfxTvM
                                    _lhsIisRow
                                    _lhsIqSurrTvS
                                    _lhsIrank
                                    _lhsItvIsBound
                                    _lhsItyCtxt ->
                                      (case (Ty_Dbg info_) of
                                       { _quTy ->
                                       (case (_quTy) of
                                        { _lhsOquTy ->
                                        (case (Ty_Dbg info_) of
                                         { _self ->
                                         (case (_self) of
                                          { _lhsOself ->
                                          ( _lhsOquTy,_lhsOself) }) }) }) }))
                         in  sem_Ty_Dbg_1)) of
                  { ( sem_Ty_1) ->
                  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (_lhsIpol) of
          { _tyOpol ->
          (case (_lhsIopts) of
           { _tyOopts ->
           (case (False) of
            { _isAtTop ->
            (case (_isAtTop) of
             { _tyOisAtTop ->
             (case (_lhsIappSpinePos + 1) of
              { _tyOappSpinePos ->
              (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
               { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                   (case (_tyIappFunNm) of
                    { _appFunNm ->
                    (case (_appFunNm) of
                     { _lhsOappFunNm ->
                     (case (0) of
                      { _extTyOappSpinePos ->
                      (case (_lhsIpol) of
                       { _extTyOpol ->
                       (case (_lhsIopts) of
                        { _extTyOopts ->
                        (case (_isAtTop) of
                         { _extTyOisAtTop ->
                         (case (extTy_ _extTyOappSpinePos _extTyOisAtTop _extTyOopts _extTyOpol ) of
                          { ( _extTyIappFunNm,_extTyIfrTvSL,_extTyIgathFxTvM,_extTyIisArrow,_extTyIisPred,_extTyIisQuLoc,_extTyIqInsideTvS,_extTyIqOrphanTvS,extTy_1) ->
                              (case (_extTyIfrTvSL ++ _tyIfrTvSL) of
                               { _frTvSL ->
                               (case (_frTvSL) of
                                { _lhsOfrTvSL ->
                                (case (_tyIgathFxTvM `Map.union` _extTyIgathFxTvM) of
                                 { _lhsOgathFxTvM ->
                                 (case (False) of
                                  { _lhsOisArrow ->
                                  (case (False) of
                                   { _lhsOisPred ->
                                   (case (_tyIisQuLoc && _extTyIisQuLoc) of
                                    { _lhsOisQuLoc ->
                                    (case (_tyIqInsideTvS `Set.union` _extTyIqInsideTvS) of
                                     { _lhsOqInsideTvS ->
                                     (case (_extTyIqOrphanTvS `Set.union` _tyIqOrphanTvS) of
                                      { _qOrphanTvS ->
                                      (case (_qOrphanTvS) of
                                       { _lhsOqOrphanTvS ->
                                       (case ((let sem_Ty_Ext_1 :: T_Ty_1 
                                                   sem_Ty_Ext_1  =
                                                       (\ _lhsIbaseQu
                                                          _lhsIfxTvM
                                                          _lhsIisRow
                                                          _lhsIqSurrTvS
                                                          _lhsIrank
                                                          _lhsItvIsBound
                                                          _lhsItyCtxt ->
                                                            (case (TyQuCtxtOther) of
                                                             { _tyCtxt ->
                                                             (case (_tyCtxt) of
                                                              { _extTyOtyCtxt ->
                                                              (case (_lhsItvIsBound) of
                                                               { _extTyOtvIsBound ->
                                                               (case (_lhsIrank) of
                                                                { _extTyOrank ->
                                                                (case (_lhsIfxTvM) of
                                                                 { _extTyOfxTvM ->
                                                                 (case (_lhsIbaseQu) of
                                                                  { _extTyObaseQu ->
                                                                  (case (_tyCtxt) of
                                                                   { _tyOtyCtxt ->
                                                                   (case (_lhsItvIsBound) of
                                                                    { _tyOtvIsBound ->
                                                                    (case (_lhsIrank) of
                                                                     { _tyOrank ->
                                                                     (case (_lhsIfxTvM) of
                                                                      { _tyOfxTvM ->
                                                                      (case (_lhsIbaseQu) of
                                                                       { _tyObaseQu ->
                                                                       (case (_lhsIqSurrTvS) of
                                                                        { _extTyOqSurrTvS ->
                                                                        (case (False) of
                                                                         { _isRow ->
                                                                         (case (_isRow) of
                                                                          { _extTyOisRow ->
                                                                          (case (extTy_1 _extTyObaseQu _extTyOfxTvM _extTyOisRow _extTyOqSurrTvS _extTyOrank _extTyOtvIsBound _extTyOtyCtxt ) of
                                                                           { ( _extTyIquTy,_extTyIself) ->
                                                                               (case (_lhsIqSurrTvS) of
                                                                                { _tyOqSurrTvS ->
                                                                                (case (_isRow) of
                                                                                 { _tyOisRow ->
                                                                                 (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                                                  { ( _tyIquTy,_tyIself) ->
                                                                                      (case (Ty_Ext _tyIquTy nm_ _extTyIquTy) of
                                                                                       { _quTy ->
                                                                                       (case (_quTy) of
                                                                                        { _lhsOquTy ->
                                                                                        (case (Ty_Ext _tyIself nm_ _extTyIself) of
                                                                                         { _self ->
                                                                                         (case (_self) of
                                                                                          { _lhsOself ->
                                                                                          ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                               in  sem_Ty_Ext_1)) of
                                        { ( sem_Ty_1) ->
                                        ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (_lhsIpol) of
           { _implsOpol ->
           (case (_lhsIopts) of
            { _implsOopts ->
            (case (False) of
             { _isAtTop ->
             (case (_isAtTop) of
              { _implsOisAtTop ->
              (case (impls_ _implsOisAtTop _implsOopts _implsOpol ) of
               { ( _implsIfrTvSL,_implsIgathFxTvM,_implsIqInsideTvS,_implsIqOrphanTvS,impls_1) ->
                   (case (Set.unions _implsIfrTvSL) of
                    { _frTvS ->
                    (case ([_frTvS]) of
                     { _lhsOfrTvSL ->
                     (case (_implsIgathFxTvM) of
                      { _lhsOgathFxTvM ->
                      (case (False) of
                       { _lhsOisArrow ->
                       (case (True) of
                        { _lhsOisPred ->
                        (case (False) of
                         { _lhsOisQuLoc ->
                         (case (_implsIqInsideTvS) of
                          { _lhsOqInsideTvS ->
                          (case (_implsIqOrphanTvS) of
                           { _lhsOqOrphanTvS ->
                           (case ((let sem_Ty_Impls_1 :: T_Ty_1 
                                       sem_Ty_Impls_1  =
                                           (\ _lhsIbaseQu
                                              _lhsIfxTvM
                                              _lhsIisRow
                                              _lhsIqSurrTvS
                                              _lhsIrank
                                              _lhsItvIsBound
                                              _lhsItyCtxt ->
                                                (case (TyQuCtxtOther) of
                                                 { _tyCtxt ->
                                                 (case (_tyCtxt) of
                                                  { _implsOtyCtxt ->
                                                  (case (_lhsItvIsBound) of
                                                   { _implsOtvIsBound ->
                                                   (case (_lhsIrank) of
                                                    { _implsOrank ->
                                                    (case (_lhsIfxTvM) of
                                                     { _implsOfxTvM ->
                                                     (case (_lhsIbaseQu) of
                                                      { _implsObaseQu ->
                                                      (case (_lhsIqSurrTvS) of
                                                       { _implsOqSurrTvS ->
                                                       (case (False) of
                                                        { _isRow ->
                                                        (case (_isRow) of
                                                         { _implsOisRow ->
                                                         (case (impls_1 _implsObaseQu _implsOfxTvM _implsOisRow _implsOqSurrTvS _implsOrank _implsOtvIsBound _implsOtyCtxt ) of
                                                          { ( _implsIquTy,_implsIself) ->
                                                              (case (Ty_Impls _implsIquTy) of
                                                               { _quTy ->
                                                               (case (_quTy) of
                                                                { _lhsOquTy ->
                                                                (case (Ty_Impls _implsIself) of
                                                                 { _self ->
                                                                 (case (_self) of
                                                                  { _lhsOself ->
                                                                  ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                   in  sem_Ty_Impls_1)) of
                            { ( sem_Ty_1) ->
                            ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (_lhsIappSpinePos) of
           { _tyOappSpinePos ->
           (case (Set.singleton tv_) of
            { _introTVarS ->
            (case (_lhsIpol) of
             { _tyOpol ->
             (case (_lhsIopts) of
              { _tyOopts ->
              (case (False) of
               { _isAtTop ->
               (case (_isAtTop) of
                { _tyOisAtTop ->
                (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
                 { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                     (case (head _tyIfrTvSL `Set.difference` _introTVarS) of
                      { _frTvS ->
                      (case ([_frTvS]) of
                       { _lhsOfrTvSL ->
                       (case (_tyIgathFxTvM) of
                        { _lhsOgathFxTvM ->
                        (case (False) of
                         { _lhsOisArrow ->
                         (case (False) of
                          { _lhsOisPred ->
                          (case (_tyIisQuLoc) of
                           { _lhsOisQuLoc ->
                           (case (_tyIqInsideTvS) of
                            { _lhsOqInsideTvS ->
                            (case (Set.empty) of
                             { _lhsOqOrphanTvS ->
                             (case ((let sem_Ty_Lam_1 :: T_Ty_1 
                                         sem_Ty_Lam_1  =
                                             (\ _lhsIbaseQu
                                                _lhsIfxTvM
                                                _lhsIisRow
                                                _lhsIqSurrTvS
                                                _lhsIrank
                                                _lhsItvIsBound
                                                _lhsItyCtxt ->
                                                  (case (TyQuCtxtOther) of
                                                   { _tyCtxt ->
                                                   (case (_tyCtxt) of
                                                    { _tyOtyCtxt ->
                                                    (case (tvBoundAddS 0 _introTVarS _lhsItvIsBound) of
                                                     { _tvIsBound ->
                                                     (case (_tvIsBound) of
                                                      { _tyOtvIsBound ->
                                                      (case (_lhsIrank) of
                                                       { _tyOrank ->
                                                       (case (_lhsIfxTvM) of
                                                        { _tyOfxTvM ->
                                                        (case (_lhsIbaseQu) of
                                                         { _tyObaseQu ->
                                                         (case (_lhsIqSurrTvS) of
                                                          { _tyOqSurrTvS ->
                                                          (case (False) of
                                                           { _isRow ->
                                                           (case (_isRow) of
                                                            { _tyOisRow ->
                                                            (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                             { ( _tyIquTy,_tyIself) ->
                                                                 (case (Ty_Lam tv_ _tyIquTy) of
                                                                  { _lhsOquTy ->
                                                                  (case (Ty_Lam tv_ _tyIself) of
                                                                   { _self ->
                                                                   (case (_self) of
                                                                    { _lhsOself ->
                                                                    ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                     in  sem_Ty_Lam_1)) of
                              { ( sem_Ty_1) ->
                              ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (_lhsIopts) of
           { _prOopts ->
           (case (False) of
            { _isAtTop ->
            (case (_isAtTop) of
             { _prOisAtTop ->
             (case (polInvariant) of
              { _prOpol ->
              (case (pr_ _prOisAtTop _prOopts _prOpol ) of
               { ( _prIfrTvSL,_prIgathFxTvM,_prIqInsideTvS,_prIqOrphanTvS,pr_1) ->
                   (case (Set.unions _prIfrTvSL) of
                    { _frTvS ->
                    (case ([_frTvS]) of
                     { _lhsOfrTvSL ->
                     (case (_prIgathFxTvM) of
                      { _lhsOgathFxTvM ->
                      (case (False) of
                       { _lhsOisArrow ->
                       (case (True) of
                        { _lhsOisPred ->
                        (case (_lhsIisAtTop || tqoptAllowInnerQuant _lhsIopts) of
                         { _isQuLocExtraHook ->
                         (case (polIsCovariant _lhsIpol && _isQuLocExtraHook) of
                          { _isQuLoc ->
                          (case (_isQuLoc) of
                           { _lhsOisQuLoc ->
                           (case (_prIqInsideTvS) of
                            { _lhsOqInsideTvS ->
                            (case (_prIqOrphanTvS) of
                             { _lhsOqOrphanTvS ->
                             (case ((let sem_Ty_Pred_1 :: T_Ty_1 
                                         sem_Ty_Pred_1  =
                                             (\ _lhsIbaseQu
                                                _lhsIfxTvM
                                                _lhsIisRow
                                                _lhsIqSurrTvS
                                                _lhsIrank
                                                _lhsItvIsBound
                                                _lhsItyCtxt ->
                                                  (case (TyQuCtxtOther) of
                                                   { _tyCtxt ->
                                                   (case (_tyCtxt) of
                                                    { _prOtyCtxt ->
                                                    (case (_frTvS `Set.difference` _prIqInsideTvS) of
                                                     { _qHereTvS ->
                                                     (case (tvarsToQuant _lhsIopts _isQuLoc _lhsIfxTvM (const False) _lhsItvIsBound _qHereTvS) of
                                                      { __tup4 ->
                                                      (case (__tup4) of
                                                       { (_,_,_,_tvIsBound) ->
                                                       (case (_tvIsBound) of
                                                        { _prOtvIsBound ->
                                                        (case (_lhsIrank) of
                                                         { _prOrank ->
                                                         (case (_lhsIfxTvM) of
                                                          { _prOfxTvM ->
                                                          (case (_lhsIbaseQu) of
                                                           { _prObaseQu ->
                                                           (case (polIsContravariant _lhsIpol) of
                                                            { _qAsExist ->
                                                            (case (if _qAsExist then tyquExists _lhsIbaseQu else _lhsIbaseQu) of
                                                             { _hereQu ->
                                                             (case (__tup4) of
                                                              { (_,_,_qBndL1Tvs,_) ->
                                                              (case (__tup4) of
                                                               { (_,_qBndExTvs,_,_) ->
                                                               (case (__tup4) of
                                                                { (_qBndTvs,_,_,_) ->
                                                                (case (_lhsIrank `elem` tqoptQuRanks _lhsIopts) of
                                                                 { _isQuRank ->
                                                                 (case (_lhsIqSurrTvS) of
                                                                  { _prOqSurrTvS ->
                                                                  (case (False) of
                                                                   { _isRow ->
                                                                   (case (_isRow) of
                                                                    { _prOisRow ->
                                                                    (case (pr_1 _prObaseQu _prOfxTvM _prOisRow _prOqSurrTvS _prOrank _prOtvIsBound _prOtyCtxt ) of
                                                                     { ( _prIquTy,_prIself) ->
                                                                         (case (mkTyQuForRank _lhsIopts _isQuRank _hereQu _qBndL1Tvs _qBndTvs _qBndExTvs (mkTyPr _prIquTy)) of
                                                                          { _lhsOquTy ->
                                                                          (case (Ty_Pred _prIself) of
                                                                           { _self ->
                                                                           (case (_self) of
                                                                            { _lhsOself ->
                                                                            ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                     in  sem_Ty_Pred_1)) of
                              { ( sem_Ty_1) ->
                              ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (Set.singleton tv_) of
           { _introTVarS ->
           (case (0) of
            { _tyOappSpinePos ->
            (case (_lhsIpol) of
             { _tyOpol ->
             (case (_lhsIopts) of
              { _tyOopts ->
              (case (False) of
               { _isAtTop ->
               (case (_isAtTop) of
                { _tyOisAtTop ->
                (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
                 { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                     (case (head _tyIfrTvSL `Set.difference` _introTVarS) of
                      { _frTvS ->
                      (case ([_frTvS]) of
                       { _lhsOfrTvSL ->
                       (case (_tyIgathFxTvM) of
                        { _lhsOgathFxTvM ->
                        (case (False) of
                         { _lhsOisArrow ->
                         (case (False) of
                          { _lhsOisPred ->
                          (case (_lhsIisAtTop || tqoptAllowInnerQuant _lhsIopts) of
                           { _isQuLocExtraHook ->
                           (case (not (polIsInvariant _lhsIpol) && _isQuLocExtraHook) of
                            { _isQuLoc ->
                            (case (_isQuLoc) of
                             { _lhsOisQuLoc ->
                             (case (_tyIqInsideTvS) of
                              { _lhsOqInsideTvS ->
                              (case (Set.empty) of
                               { _lhsOqOrphanTvS ->
                               (case ((let sem_Ty_TBind_1 :: T_Ty_1 
                                           sem_Ty_TBind_1  =
                                               (\ _lhsIbaseQu
                                                  _lhsIfxTvM
                                                  _lhsIisRow
                                                  _lhsIqSurrTvS
                                                  _lhsIrank
                                                  _lhsItvIsBound
                                                  _lhsItyCtxt ->
                                                    (case (TyQuCtxtOther) of
                                                     { _tyCtxt ->
                                                     (case (_tyCtxt) of
                                                      { _tyOtyCtxt ->
                                                      (case (if _isQuLoc then                                     _frTvS else Set.empty) of
                                                       { _qHereTvS ->
                                                       (case (tyQu_Forall) of
                                                        { _hereQu ->
                                                        (case (let (qBndL1Tvs,_,isB) = tvarsToQuantL1 [(tv_,l1_)] _lhsItvIsBound
                                                               in  (qBndL1Tvs, tvBoundAddS 0 _introTVarS isB)) of
                                                         { __tup6 ->
                                                         (case (__tup6) of
                                                          { (_,_tvIsBoundQu) ->
                                                          (case (False) of
                                                           { _isQuFxLoc ->
                                                           (case (tvarsToQuant _lhsIopts _isQuLoc _lhsIfxTvM
                                                                               (tvMayQuFx _hereQu _lhsIfxTvM _isQuFxLoc)
                                                                               _tvIsBoundQu _qHereTvS) of
                                                            { __tup5 ->
                                                            (case (__tup5) of
                                                             { (_,_,_,_tvIsBound) ->
                                                             (case (_tvIsBound) of
                                                              { _tyOtvIsBound ->
                                                              (case (_lhsIrank) of
                                                               { _tyOrank ->
                                                               (case (_lhsIfxTvM) of
                                                                { _tyOfxTvM ->
                                                                (case (_lhsIbaseQu) of
                                                                 { _tyObaseQu ->
                                                                 (case (_qHereTvS `Set.union` _lhsIqSurrTvS) of
                                                                  { _qSurrTvS ->
                                                                  (case (_qSurrTvS) of
                                                                   { _tyOqSurrTvS ->
                                                                   (case (False) of
                                                                    { _isRow ->
                                                                    (case (_isRow) of
                                                                     { _tyOisRow ->
                                                                     (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                                                      { ( _tyIquTy,_tyIself) ->
                                                                          (case (qu_ ) of
                                                                           { ( _quIquTy,_quIself) ->
                                                                               (case (Ty_TBind _quIself tv_ l1_ _tyIquTy) of
                                                                                { _quTyBase ->
                                                                                (case (__tup6) of
                                                                                 { (_qBndL1TvsQu,_) ->
                                                                                 (case (__tup5) of
                                                                                  { (_,_,_qBndL1TvsOther,_) ->
                                                                                  (case (_qBndL1TvsOther ++ _qBndL1TvsQu) of
                                                                                   { _qBndL1Tvs ->
                                                                                   (case (__tup5) of
                                                                                    { (_,_qBndExTvs,_,_) ->
                                                                                    (case (__tup5) of
                                                                                     { (_qBndTvs,_,_,_) ->
                                                                                     (case (_lhsIrank `elem` tqoptQuRanks _lhsIopts) of
                                                                                      { _isQuRank ->
                                                                                      (case (mkTyQuForRank _lhsIopts _isQuRank _hereQu _qBndL1Tvs _qBndTvs _qBndExTvs _quTyBase) of
                                                                                       { _lhsOquTy ->
                                                                                       (case (Ty_TBind _quIself tv_ l1_ _tyIself) of
                                                                                        { _self ->
                                                                                        (case (_self) of
                                                                                         { _lhsOself ->
                                                                                         ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                       in  sem_Ty_TBind_1)) of
                                { ( sem_Ty_1) ->
                                ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIopts
       _lhsIpol ->
         (case (hsnUnknown) of
          { _lhsOappFunNm ->
          (case (categ_ ) of
           { ( _categIquTy,_categIself) ->
               (case (if tvCatIsPlain _categIself then Set.singleton tv_ else Set.empty) of
                { _frTvS ->
                (case ([_frTvS]) of
                 { _lhsOfrTvSL ->
                 (case (tv_ `Map.singleton` mkTvInfoTy _categIself) of
                  { _lhsOgathFxTvM ->
                  (case (False) of
                   { _lhsOisArrow ->
                   (case (False) of
                    { _lhsOisPred ->
                    (case (_lhsIisAtTop || tqoptAllowInnerQuant _lhsIopts) of
                     { _isQuLocExtraHook ->
                     (case (polIsCovariant _lhsIpol && _isQuLocExtraHook) of
                      { _isQuLoc ->
                      (case (_isQuLoc) of
                       { _lhsOisQuLoc ->
                       (case (Set.empty) of
                        { _lhsOqInsideTvS ->
                        (case (if _isQuLoc then Set.empty else _frTvS) of
                         { _lhsOqOrphanTvS ->
                         (case ((let sem_Ty_Var_1 :: T_Ty_1 
                                     sem_Ty_Var_1  =
                                         (\ _lhsIbaseQu
                                            _lhsIfxTvM
                                            _lhsIisRow
                                            _lhsIqSurrTvS
                                            _lhsIrank
                                            _lhsItvIsBound
                                            _lhsItyCtxt ->
                                              (case (if _isQuLoc then                                     _frTvS else Set.empty) of
                                               { _qHereTvS ->
                                               (case (Ty_Var tv_ _categIquTy) of
                                                { _quTyBase ->
                                                (case (polIsContravariant _lhsIpol) of
                                                 { _qAsExist ->
                                                 (case (if _qAsExist then tyquExists _lhsIbaseQu else _lhsIbaseQu) of
                                                  { _hereQu ->
                                                  (case (False) of
                                                   { _isQuFxLoc ->
                                                   (case (tvarsToQuant _lhsIopts _isQuLoc _lhsIfxTvM (tvMayQuFx _hereQu _lhsIfxTvM _isQuFxLoc) _lhsItvIsBound _qHereTvS) of
                                                    { __tup8 ->
                                                    (case (__tup8) of
                                                     { (_,_,_qBndL1Tvs,_) ->
                                                     (case (__tup8) of
                                                      { (_,_qBndExTvs,_,_) ->
                                                      (case (__tup8) of
                                                       { (_qBndTvs,_,_,_) ->
                                                       (case (_lhsIrank `elem` tqoptQuRanks _lhsIopts) of
                                                        { _isQuRank ->
                                                        (case (mkTyQuForRank _lhsIopts _isQuRank _hereQu _qBndL1Tvs _qBndTvs _qBndExTvs _quTyBase) of
                                                         { _lhsOquTy ->
                                                         (case (Ty_Var tv_ _categIself) of
                                                          { _self ->
                                                          (case (_self) of
                                                           { _lhsOself ->
                                                           ( _lhsOquTy,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                 in  sem_Ty_Var_1)) of
                          { ( sem_Ty_1) ->
                          ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOqInsideTvS,_lhsOqOrphanTvS,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         baseQu               : TyQu 
         opts                 : TyQuOpts
      synthesized attribute:
         quTy                 : Ty 
   alternatives:
      alternative AGItf:
         child ty             : Ty 
         visit 0:
            local opts        : _
            local fxTvM       : _
            local qHereTvS    : _
            local _tup9       : _
            local tvIsBound   : _
            local rank        : _
            local qSurrTvS    : _
            local quTyBase    : _
            local hereQu      : _
            local qBndL1Tvs   : _
            local qBndExTvs   : _
            local qBndTvs     : _
            local isQuRank    : _
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _ty )  =
    (sem_TyAGItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_TyAGItf  = TyQu  ->
                  TyQuOpts ->
                  ( Ty )
data Inh_TyAGItf  = Inh_TyAGItf {baseQu_Inh_TyAGItf :: !(TyQu ),opts_Inh_TyAGItf :: !(TyQuOpts)}
data Syn_TyAGItf  = Syn_TyAGItf {quTy_Syn_TyAGItf :: !(Ty )}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIbaseQu _lhsIopts )  =
    (let ( _lhsOquTy) = sem _lhsIbaseQu _lhsIopts 
     in  (Syn_TyAGItf _lhsOquTy ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIbaseQu
       _lhsIopts ->
         (case (_lhsIopts
                  { tqoptQuRanks = take 5 (tqoptQuRanks _lhsIopts)
                  }) of
          { _opts ->
          (case (_opts) of
           { _tyOopts ->
           (case (0) of
            { _tyOappSpinePos ->
            (case (True) of
             { _tyOisAtTop ->
             (case (polCovariant) of
              { _tyOpol ->
              (case (ty_ _tyOappSpinePos _tyOisAtTop _tyOopts _tyOpol ) of
               { ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyIqInsideTvS,_tyIqOrphanTvS,ty_1) ->
                   (case (_tyIgathFxTvM) of
                    { _fxTvM ->
                    (case (Set.empty) of
                     { _qHereTvS ->
                     (case (tvarsToQuant _lhsIopts True _fxTvM
                                         (const False)
                                         (tvIsBound2L0 $ tqoptTvIsBound _lhsIopts) _qHereTvS) of
                      { __tup9 ->
                      (case (__tup9) of
                       { (_,_,_,_tvIsBound) ->
                       (case (_tvIsBound) of
                        { _tyOtvIsBound ->
                        (case (1) of
                         { _rank ->
                         (case (_rank) of
                          { _tyOrank ->
                          (case (_fxTvM) of
                           { _tyOfxTvM ->
                           (case (_lhsIbaseQu) of
                            { _tyObaseQu ->
                            (case (TyQuCtxtOnTop) of
                             { _tyOtyCtxt ->
                             (case (_qHereTvS) of
                              { _qSurrTvS ->
                              (case (_qSurrTvS) of
                               { _tyOqSurrTvS ->
                               (case (False) of
                                { _tyOisRow ->
                                (case (ty_1 _tyObaseQu _tyOfxTvM _tyOisRow _tyOqSurrTvS _tyOrank _tyOtvIsBound _tyOtyCtxt ) of
                                 { ( _tyIquTy,_tyIself) ->
                                     (case (_tyIquTy) of
                                      { _quTyBase ->
                                      (case (_lhsIbaseQu) of
                                       { _hereQu ->
                                       (case (__tup9) of
                                        { (_,_,_qBndL1Tvs,_) ->
                                        (case (__tup9) of
                                         { (_,_qBndExTvs,_,_) ->
                                         (case (__tup9) of
                                          { (_qBndTvs,_,_,_) ->
                                          (case (_rank `elem` tqoptQuRanks _opts) of
                                           { _isQuRank ->
                                           (case (mkTyQuForRank _lhsIopts _isQuRank _hereQu _qBndL1Tvs _qBndTvs _qBndExTvs _quTyBase) of
                                            { _lhsOquTy ->
                                            ( _lhsOquTy) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local quTy        : _
            local self        : _
      alternative Mono:
         visit 0:
            local quTy        : _
            local self        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local quTy        : _
            local self        : _
-}
-- cata
sem_TyAnn :: TyAnn  ->
             T_TyAnn 
sem_TyAnn (TyAnn_Empty )  =
    (sem_TyAnn_Empty )
sem_TyAnn (TyAnn_Mono )  =
    (sem_TyAnn_Mono )
sem_TyAnn (TyAnn_Strictness _s )  =
    (sem_TyAnn_Strictness _s )
-- semantic domain
type T_TyAnn  = ( TyAnn ,TyAnn )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (case (TyAnn_Empty) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyAnn_Empty) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (case (TyAnn_Mono) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyAnn_Mono) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (case (TyAnn_Strictness s_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyAnn_Strictness s_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
-- TyQu --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local quTy        : _
            local self        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local quTy        : _
            local self        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
            local quTy        : _
            local self        : _
-}
-- cata
sem_TyQu :: TyQu  ->
            T_TyQu 
sem_TyQu (TyQu_Exists _mlev )  =
    (sem_TyQu_Exists _mlev )
sem_TyQu (TyQu_Forall _mlev )  =
    (sem_TyQu_Forall _mlev )
sem_TyQu (TyQu_Plain _mlev )  =
    (sem_TyQu_Plain _mlev )
-- semantic domain
type T_TyQu  = ( TyQu ,TyQu )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (case (TyQu_Exists mlev_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyQu_Exists mlev_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (case (TyQu_Forall mlev_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyQu_Forall mlev_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (case (TyQu_Plain mlev_) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyQu_Plain mlev_) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         quTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local quTy        : _
            local self        : _
      alternative Meta:
         visit 0:
            local quTy        : _
            local self        : _
      alternative Plain:
         visit 0:
            local quTy        : _
            local self        : _
-}
-- cata
sem_TyVarCateg :: TyVarCateg  ->
                  T_TyVarCateg 
sem_TyVarCateg (TyVarCateg_Fixed )  =
    (sem_TyVarCateg_Fixed )
sem_TyVarCateg (TyVarCateg_Meta )  =
    (sem_TyVarCateg_Meta )
sem_TyVarCateg (TyVarCateg_Plain )  =
    (sem_TyVarCateg_Plain )
-- semantic domain
type T_TyVarCateg  = ( TyVarCateg ,TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (case (TyVarCateg_Fixed) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyVarCateg_Fixed) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (case (TyVarCateg_Meta) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyVarCateg_Meta) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (case (TyVarCateg_Plain) of
     { _quTy ->
     (case (_quTy) of
      { _lhsOquTy ->
      (case (TyVarCateg_Plain) of
       { _self ->
       (case (_self) of
        { _lhsOself ->
        ( _lhsOquTy,_lhsOself) }) }) }) })