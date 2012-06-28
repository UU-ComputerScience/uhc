

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag)
module EH101.Ty.Trf.MergePreds(tyMergePreds
, TyMergePredOut (..)
, TQOGam) where

import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable
import EH101.Gam
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List as List
import Data.Maybe
import EH.Util.Pretty











tyMergePreds :: [PredOcc] -> Ty -> TyMergePredOut
tyMergePreds prL ty
  = let  t  =  wrap_TyAGItf
                  (sem_TyAGItf  (TyAGItf_AGItf ty))
                  (Inh_TyAGItf  { prLL_Inh_TyAGItf          = [prL]
                                })
    in   TyMergePredOut
            { tmpoTy                 = prTy_Syn_TyAGItf t
            , tmpoInsPrIdSet         = insPrIdSet_Syn_TyAGItf t
            , tmpoImplsVarMp         = assocImplsLToVarMp (prImpls_Syn_TyAGItf t)
            , tmpoImplsPrvReq        = prvReqs_Syn_TyAGItf t
            }



type InsPrPrvOcc = Assoc PredOcc [ImplsProveOcc]
type PrvReqs = [InsPrPrvOcc]

data TyMergePredOut
  = TyMergePredOut
      { tmpoTy               ::  Ty						-- resulting ty
      , tmpoInsPrIdSet       ::  Set.Set PredOccId		-- idents of merged predicates
      , tmpoImplsVarMp       ::  VarMp					-- additional bindings, for implicits
      , tmpoImplsPrvReq      ::  PrvReqs				-- additional occurrences of predicate occurrences, arising from implicits
      }

instance Show TyMergePredOut where
  show _ = ""

instance PP TyMergePredOut where
  pp tmpo = pp (tmpoImplsVarMp tmpo) >-< ppAssocLV (assocLMapElt ppBracketsCommas $ tmpoImplsPrvReq tmpo)



type TQOGam = Gam HsName TyMergePredOut



prLLArrowSplit :: TyVarIdS -> [[PredOcc]] -> ([PredOcc],[[PredOcc]])
prLLArrowSplit frTvS prLL
  =  let  (h,r) = partition (all (`Set.member` frTvS) . varFree) . concat $ prLL
     in   (h,[r])



toInsPrPrvOcc :: [PredOcc] -> [ImplsProveOcc] -> [InsPrPrvOcc]
toInsPrPrvOcc ps ipos = [ (p,ipos) | p <- ps ]



prOccLImpls :: Maybe Impls -> [InsPrPrvOcc] -> AssocL ImplsVarId Impls
prOccLImpls mbI prL
  = case mbI of
      Just i -> [( implsTailVar i
                 , foldr (\(p,_) im
                             -> Impls_Cons (poId p) (poPr p) (poPoi p)
                                           (poRange p)
                                           [] im
                         ) Impls_Nil prL)]
      _      -> []



prOccLPrvReqs :: Maybe Impls -> [InsPrPrvOcc] -> PrvReqs
prOccLPrvReqs mbI prL = maybe [] (const prL) mbI



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
         fxTvM                : TvCatMp
         isAtTop              : Bool
         isRow                : Bool
         mbPrLoc              : Maybe Impls
         pol                  : Polarity
         prLL                 : [[PredOcc]]
         qSurrTvS             : TyVarIdS
         tyCtxt               : TyQuCtxt
      chained attribute:
         prImpls              : AssocL ImplsVarId Impls
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         insPrIdSet           : Set.Set PredOccId
         insPrL               : [PredOcc]
         prTy                 : SELF 
         prvReqs              : PrvReqs
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
         self                 : SELF 
   alternatives:
      alternative Cons:
         child iv             : {ImplsVarId}
         child pr             : Pred 
         child pv             : {PredOccId}
         child prange         : {Range}
         child proveOccs      : {[ImplsProveOcc]}
         child tl             : Impls 
         visit 0:
            local prTy        : _
            local self        : _
      alternative Nil:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 0:
            local prTy        : _
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
type T_Impls  = TvCatMp ->
                Bool ->
                Bool ->
                (Maybe Impls) ->
                Polarity ->
                (AssocL ImplsVarId Impls) ->
                ([[PredOcc]]) ->
                TyVarIdS ->
                TyQuCtxt ->
                ( ([TyVarIdS]),TvCatMp,(Set.Set PredOccId),([PredOcc]),(AssocL ImplsVarId Impls),Impls ,PrvReqs,TyVarIdS,TyVarIdS,Impls )
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Impls 
              _lhsOself :: Impls 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _prOfxTvM :: TvCatMp
              _prOisAtTop :: Bool
              _prOisRow :: Bool
              _prOmbPrLoc :: (Maybe Impls)
              _prOpol :: Polarity
              _prOprImpls :: (AssocL ImplsVarId Impls)
              _prOprLL :: ([[PredOcc]])
              _prOqSurrTvS :: TyVarIdS
              _prOtyCtxt :: TyQuCtxt
              _tlOfxTvM :: TvCatMp
              _tlOisAtTop :: Bool
              _tlOisRow :: Bool
              _tlOmbPrLoc :: (Maybe Impls)
              _tlOpol :: Polarity
              _tlOprImpls :: (AssocL ImplsVarId Impls)
              _tlOprLL :: ([[PredOcc]])
              _tlOqSurrTvS :: TyVarIdS
              _tlOtyCtxt :: TyQuCtxt
              _prIfrTvSL :: ([TyVarIdS])
              _prIgathFxTvM :: TvCatMp
              _prIinsPrIdSet :: (Set.Set PredOccId)
              _prIinsPrL :: ([PredOcc])
              _prIprImpls :: (AssocL ImplsVarId Impls)
              _prIprTy :: Pred 
              _prIprvReqs :: PrvReqs
              _prIqInsideTvS :: TyVarIdS
              _prIqOrphanTvS :: TyVarIdS
              _prIself :: Pred 
              _tlIfrTvSL :: ([TyVarIdS])
              _tlIgathFxTvM :: TvCatMp
              _tlIinsPrIdSet :: (Set.Set PredOccId)
              _tlIinsPrL :: ([PredOcc])
              _tlIprImpls :: (AssocL ImplsVarId Impls)
              _tlIprTy :: Impls 
              _tlIprvReqs :: PrvReqs
              _tlIqInsideTvS :: TyVarIdS
              _tlIqOrphanTvS :: TyVarIdS
              _tlIself :: Impls 
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 33, column 17)
              _lhsOfrTvSL =
                  _prIfrTvSL ++ _tlIfrTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 132, column 25)
              _lhsOqOrphanTvS =
                  _prIqOrphanTvS `Set.union` _tlIqOrphanTvS
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _prIgathFxTvM `Map.union` _tlIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _prIinsPrIdSet `Set.union` _tlIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _prIinsPrL ++ _tlIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _prIprvReqs ++ _tlIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _prIqInsideTvS `Set.union` _tlIqInsideTvS
              -- self rule
              _prTy =
                  Impls_Cons iv_ _prIprTy pv_ prange_ proveOccs_ _tlIprTy
              -- self rule
              _self =
                  Impls_Cons iv_ _prIself pv_ prange_ proveOccs_ _tlIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tlIprImpls
              -- copy rule (down)
              _prOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _prOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _prOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _prOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _prOpol =
                  _lhsIpol
              -- copy rule (down)
              _prOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _prOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _prOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _prOtyCtxt =
                  _lhsItyCtxt
              -- copy rule (down)
              _tlOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tlOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tlOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tlOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tlOpol =
                  _lhsIpol
              -- copy rule (chain)
              _tlOprImpls =
                  _prIprImpls
              -- copy rule (down)
              _tlOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tlOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tlOtyCtxt =
                  _lhsItyCtxt
              ( _prIfrTvSL,_prIgathFxTvM,_prIinsPrIdSet,_prIinsPrL,_prIprImpls,_prIprTy,_prIprvReqs,_prIqInsideTvS,_prIqOrphanTvS,_prIself) =
                  pr_ _prOfxTvM _prOisAtTop _prOisRow _prOmbPrLoc _prOpol _prOprImpls _prOprLL _prOqSurrTvS _prOtyCtxt 
              ( _tlIfrTvSL,_tlIgathFxTvM,_tlIinsPrIdSet,_tlIinsPrL,_tlIprImpls,_tlIprTy,_tlIprvReqs,_tlIqInsideTvS,_tlIqOrphanTvS,_tlIself) =
                  tl_ _tlOfxTvM _tlOisAtTop _tlOisRow _tlOmbPrLoc _tlOpol _tlOprImpls _tlOprLL _tlOqSurrTvS _tlOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Impls 
              _lhsOself :: Impls 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 32, column 17)
              _lhsOfrTvSL =
                  []
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 133, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Impls_Nil
              -- self rule
              _self =
                  Impls_Nil
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Impls 
              _lhsOself :: Impls 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 32, column 17)
              _lhsOfrTvSL =
                  []
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 133, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Impls_Tail iv_ proveOccs_
              -- self rule
              _self =
                  Impls_Tail iv_ proveOccs_
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
-- Label -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         prTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local prTy        : _
            local self        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
            local prTy        : _
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
    (let _lhsOprTy :: Label 
         _lhsOself :: Label 
         -- self rule
         _prTy =
             Label_Lab nm_
         -- self rule
         _self =
             Label_Lab nm_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (let _lhsOprTy :: Label 
         _lhsOself :: Label 
         -- self rule
         _prTy =
             Label_Var lv_
         -- self rule
         _self =
             Label_Var lv_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
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
    (let _labIprTy :: Label 
         _labIself :: Label 
         ( _labIprTy,_labIself) =
             lab_ 
     in  ( ))
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fxTvM                : TvCatMp
         isAtTop              : Bool
         isRow                : Bool
         mbPrLoc              : Maybe Impls
         pol                  : Polarity
         prLL                 : [[PredOcc]]
         qSurrTvS             : TyVarIdS
         tyCtxt               : TyQuCtxt
      chained attribute:
         prImpls              : AssocL ImplsVarId Impls
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         insPrIdSet           : Set.Set PredOccId
         insPrL               : [PredOcc]
         prTy                 : SELF 
         prvReqs              : PrvReqs
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
         self                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 0:
            local appSpinePos : _
            local prTy        : _
            local self        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 0:
            local appSpinePos : _
            local prTy        : _
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
type T_Pred  = TvCatMp ->
               Bool ->
               Bool ->
               (Maybe Impls) ->
               Polarity ->
               (AssocL ImplsVarId Impls) ->
               ([[PredOcc]]) ->
               TyVarIdS ->
               TyQuCtxt ->
               ( ([TyVarIdS]),TvCatMp,(Set.Set PredOccId),([PredOcc]),(AssocL ImplsVarId Impls),Pred ,PrvReqs,TyVarIdS,TyVarIdS,Pred )
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _argsOappSpinePos :: Int
              _argsOfxTvM :: TvCatMp
              _argsOisAtTop :: Bool
              _argsOisRow :: Bool
              _argsOmbPrLoc :: (Maybe Impls)
              _argsOpol :: Polarity
              _argsOprImpls :: (AssocL ImplsVarId Impls)
              _argsOprLL :: ([[PredOcc]])
              _argsOqSurrTvS :: TyVarIdS
              _argsOtyCtxt :: TyQuCtxt
              _resOfxTvM :: TvCatMp
              _resOisAtTop :: Bool
              _resOisRow :: Bool
              _resOmbPrLoc :: (Maybe Impls)
              _resOpol :: Polarity
              _resOprImpls :: (AssocL ImplsVarId Impls)
              _resOprLL :: ([[PredOcc]])
              _resOqSurrTvS :: TyVarIdS
              _resOtyCtxt :: TyQuCtxt
              _argsIfrTvSL :: ([TyVarIdS])
              _argsIgathFxTvM :: TvCatMp
              _argsIinsPrIdSet :: (Set.Set PredOccId)
              _argsIinsPrL :: ([PredOcc])
              _argsIprImpls :: (AssocL ImplsVarId Impls)
              _argsIprTy :: PredSeq 
              _argsIprvReqs :: PrvReqs
              _argsIqInsideTvS :: TyVarIdS
              _argsIself :: PredSeq 
              _resIfrTvSL :: ([TyVarIdS])
              _resIgathFxTvM :: TvCatMp
              _resIinsPrIdSet :: (Set.Set PredOccId)
              _resIinsPrL :: ([PredOcc])
              _resIprImpls :: (AssocL ImplsVarId Impls)
              _resIprTy :: Pred 
              _resIprvReqs :: PrvReqs
              _resIqInsideTvS :: TyVarIdS
              _resIqOrphanTvS :: TyVarIdS
              _resIself :: Pred 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _argsIfrTvSL ++ _resIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _argsIgathFxTvM `Map.union` _resIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _argsIinsPrIdSet `Set.union` _resIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _argsIinsPrL ++ _resIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _argsIprvReqs ++ _resIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _argsIqInsideTvS `Set.union` _resIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Arrow _argsIprTy _resIprTy
              -- self rule
              _self =
                  Pred_Arrow _argsIself _resIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _resIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _resIqOrphanTvS
              -- copy rule (from local)
              _argsOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _argsOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _argsOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _argsOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _argsOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _argsOpol =
                  _lhsIpol
              -- copy rule (down)
              _argsOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _argsOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _argsOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _argsOtyCtxt =
                  _lhsItyCtxt
              -- copy rule (down)
              _resOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _resOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _resOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _resOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _resOpol =
                  _lhsIpol
              -- copy rule (chain)
              _resOprImpls =
                  _argsIprImpls
              -- copy rule (down)
              _resOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _resOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _resOtyCtxt =
                  _lhsItyCtxt
              ( _argsIfrTvSL,_argsIgathFxTvM,_argsIinsPrIdSet,_argsIinsPrL,_argsIprImpls,_argsIprTy,_argsIprvReqs,_argsIqInsideTvS,_argsIself) =
                  args_ _argsOappSpinePos _argsOfxTvM _argsOisAtTop _argsOisRow _argsOmbPrLoc _argsOpol _argsOprImpls _argsOprLL _argsOqSurrTvS _argsOtyCtxt 
              ( _resIfrTvSL,_resIgathFxTvM,_resIinsPrIdSet,_resIinsPrL,_resIprImpls,_resIprTy,_resIprvReqs,_resIqInsideTvS,_resIqOrphanTvS,_resIself) =
                  res_ _resOfxTvM _resOisAtTop _resOisRow _resOmbPrLoc _resOpol _resOprImpls _resOprLL _resOqSurrTvS _resOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _tyOpol :: Polarity
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _tyOappSpinePos :: Int
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 56, column 17)
              _tyOpol =
                  polInvariant
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _tyIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Class _tyIprTy
              -- self rule
              _self =
                  Pred_Class _tyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _tyIqOrphanTvS
              -- copy rule (from local)
              _tyOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tyOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tyOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tyOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tyOtyCtxt =
                  _lhsItyCtxt
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _tyLOappSpinePos :: Int
              _tyLOfxTvM :: TvCatMp
              _tyLOisAtTop :: Bool
              _tyLOisRow :: Bool
              _tyLOmbPrLoc :: (Maybe Impls)
              _tyLOpol :: Polarity
              _tyLOprImpls :: (AssocL ImplsVarId Impls)
              _tyLOprLL :: ([[PredOcc]])
              _tyLOqSurrTvS :: TyVarIdS
              _tyLOtyCtxt :: TyQuCtxt
              _tyROappSpinePos :: Int
              _tyROfxTvM :: TvCatMp
              _tyROisAtTop :: Bool
              _tyROisRow :: Bool
              _tyROmbPrLoc :: (Maybe Impls)
              _tyROpol :: Polarity
              _tyROprImpls :: (AssocL ImplsVarId Impls)
              _tyROprLL :: ([[PredOcc]])
              _tyROqSurrTvS :: TyVarIdS
              _tyROtyCtxt :: TyQuCtxt
              _tyLIappFunNm :: HsName
              _tyLIfrTvSL :: ([TyVarIdS])
              _tyLIgathFxTvM :: TvCatMp
              _tyLIinsPrIdSet :: (Set.Set PredOccId)
              _tyLIinsPrL :: ([PredOcc])
              _tyLIisArrow :: Bool
              _tyLIisPred :: Bool
              _tyLIisQuLoc :: Bool
              _tyLImbImpls :: (Maybe Impls)
              _tyLIprImpls :: (AssocL ImplsVarId Impls)
              _tyLIprTy :: Ty 
              _tyLIprvReqs :: PrvReqs
              _tyLIqInsideTvS :: TyVarIdS
              _tyLIqOrphanTvS :: TyVarIdS
              _tyLIself :: Ty 
              _tyRIappFunNm :: HsName
              _tyRIfrTvSL :: ([TyVarIdS])
              _tyRIgathFxTvM :: TvCatMp
              _tyRIinsPrIdSet :: (Set.Set PredOccId)
              _tyRIinsPrL :: ([PredOcc])
              _tyRIisArrow :: Bool
              _tyRIisPred :: Bool
              _tyRIisQuLoc :: Bool
              _tyRImbImpls :: (Maybe Impls)
              _tyRIprImpls :: (AssocL ImplsVarId Impls)
              _tyRIprTy :: Ty 
              _tyRIprvReqs :: PrvReqs
              _tyRIqInsideTvS :: TyVarIdS
              _tyRIqOrphanTvS :: TyVarIdS
              _tyRIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _tyLIfrTvSL ++ _tyRIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyLIgathFxTvM `Map.union` _tyRIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyLIinsPrIdSet `Set.union` _tyRIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyLIinsPrL ++ _tyRIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyLIprvReqs ++ _tyRIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyLIqInsideTvS `Set.union` _tyRIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Eq _tyLIprTy _tyRIprTy
              -- self rule
              _self =
                  Pred_Eq _tyLIself _tyRIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyRIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _tyRIqOrphanTvS
              -- copy rule (from local)
              _tyLOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _tyLOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tyLOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tyLOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tyLOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tyLOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyLOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyLOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyLOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tyLOtyCtxt =
                  _lhsItyCtxt
              -- copy rule (from local)
              _tyROappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _tyROfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tyROisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tyROisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tyROmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tyROpol =
                  _lhsIpol
              -- copy rule (chain)
              _tyROprImpls =
                  _tyLIprImpls
              -- copy rule (down)
              _tyROprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyROqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tyROtyCtxt =
                  _lhsItyCtxt
              ( _tyLIappFunNm,_tyLIfrTvSL,_tyLIgathFxTvM,_tyLIinsPrIdSet,_tyLIinsPrL,_tyLIisArrow,_tyLIisPred,_tyLIisQuLoc,_tyLImbImpls,_tyLIprImpls,_tyLIprTy,_tyLIprvReqs,_tyLIqInsideTvS,_tyLIqOrphanTvS,_tyLIself) =
                  tyL_ _tyLOappSpinePos _tyLOfxTvM _tyLOisAtTop _tyLOisRow _tyLOmbPrLoc _tyLOpol _tyLOprImpls _tyLOprLL _tyLOqSurrTvS _tyLOtyCtxt 
              ( _tyRIappFunNm,_tyRIfrTvSL,_tyRIgathFxTvM,_tyRIinsPrIdSet,_tyRIinsPrL,_tyRIisArrow,_tyRIisPred,_tyRIisQuLoc,_tyRImbImpls,_tyRIprImpls,_tyRIprTy,_tyRIprvReqs,_tyRIqInsideTvS,_tyRIqOrphanTvS,_tyRIself) =
                  tyR_ _tyROappSpinePos _tyROfxTvM _tyROisAtTop _tyROisRow _tyROmbPrLoc _tyROpol _tyROprImpls _tyROprLL _tyROqSurrTvS _tyROtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _tyOappSpinePos :: Int
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOpol :: Polarity
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              _labIprTy :: Label 
              _labIself :: Label 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _tyIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Lacks _tyIprTy _labIprTy
              -- self rule
              _self =
                  Pred_Lacks _tyIself _labIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _tyIqOrphanTvS
              -- copy rule (from local)
              _tyOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tyOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tyOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tyOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tyOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tyOtyCtxt =
                  _lhsItyCtxt
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
              ( _labIprTy,_labIself) =
                  lab_ 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _tyOpol :: Polarity
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _tyOappSpinePos :: Int
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 55, column 17)
              _tyOpol =
                  polCovariant
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _tyIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Pred _tyIprTy
              -- self rule
              _self =
                  Pred_Pred _tyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _tyIqOrphanTvS
              -- copy rule (from local)
              _tyOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tyOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tyOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tyOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tyOtyCtxt =
                  _lhsItyCtxt
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOqOrphanTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _seqOappSpinePos :: Int
              _seqOfxTvM :: TvCatMp
              _seqOisAtTop :: Bool
              _seqOisRow :: Bool
              _seqOmbPrLoc :: (Maybe Impls)
              _seqOpol :: Polarity
              _seqOprImpls :: (AssocL ImplsVarId Impls)
              _seqOprLL :: ([[PredOcc]])
              _seqOqSurrTvS :: TyVarIdS
              _seqOtyCtxt :: TyQuCtxt
              _seqIfrTvSL :: ([TyVarIdS])
              _seqIgathFxTvM :: TvCatMp
              _seqIinsPrIdSet :: (Set.Set PredOccId)
              _seqIinsPrL :: ([PredOcc])
              _seqIprImpls :: (AssocL ImplsVarId Impls)
              _seqIprTy :: PredSeq 
              _seqIprvReqs :: PrvReqs
              _seqIqInsideTvS :: TyVarIdS
              _seqIself :: PredSeq 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 138, column 33)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _seqIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _seqIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _seqIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _seqIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _seqIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _seqIqInsideTvS
              -- self rule
              _prTy =
                  Pred_Preds _seqIprTy
              -- self rule
              _self =
                  Pred_Preds _seqIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _seqIprImpls
              -- copy rule (from local)
              _seqOappSpinePos =
                  _appSpinePos
              -- copy rule (down)
              _seqOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _seqOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _seqOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _seqOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _seqOpol =
                  _lhsIpol
              -- copy rule (down)
              _seqOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _seqOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _seqOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _seqOtyCtxt =
                  _lhsItyCtxt
              ( _seqIfrTvSL,_seqIgathFxTvM,_seqIinsPrIdSet,_seqIinsPrL,_seqIprImpls,_seqIprTy,_seqIprvReqs,_seqIqInsideTvS,_seqIself) =
                  seq_ _seqOappSpinePos _seqOfxTvM _seqOisAtTop _seqOisRow _seqOmbPrLoc _seqOpol _seqOprImpls _seqOprLL _seqOqSurrTvS _seqOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOqOrphanTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Pred 
              _lhsOself :: Pred 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 72, column 17)
              _appSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 138, column 33)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Pred_Var pv_
              -- self rule
              _self =
                  Pred_Var pv_
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         fxTvM                : TvCatMp
         isAtTop              : Bool
         isRow                : Bool
         mbPrLoc              : Maybe Impls
         pol                  : Polarity
         prLL                 : [[PredOcc]]
         qSurrTvS             : TyVarIdS
         tyCtxt               : TyQuCtxt
      chained attribute:
         prImpls              : AssocL ImplsVarId Impls
      synthesized attributes:
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         insPrIdSet           : Set.Set PredOccId
         insPrL               : [PredOcc]
         prTy                 : SELF 
         prvReqs              : PrvReqs
         qInsideTvS           : TyVarIdS
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 0:
            local isSpineRoot : {Bool}
            local prTy        : _
            local self        : _
      alternative Nil:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Var:
         child av             : {TyVarId}
         visit 0:
            local prTy        : _
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
type T_PredSeq  = Int ->
                  TvCatMp ->
                  Bool ->
                  Bool ->
                  (Maybe Impls) ->
                  Polarity ->
                  (AssocL ImplsVarId Impls) ->
                  ([[PredOcc]]) ->
                  TyVarIdS ->
                  TyQuCtxt ->
                  ( ([TyVarIdS]),TvCatMp,(Set.Set PredOccId),([PredOcc]),(AssocL ImplsVarId Impls),PredSeq ,PrvReqs,TyVarIdS,PredSeq )
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _tlOappSpinePos :: Int
              _isSpineRoot :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: PredSeq 
              _lhsOself :: PredSeq 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _hdOfxTvM :: TvCatMp
              _hdOisAtTop :: Bool
              _hdOisRow :: Bool
              _hdOmbPrLoc :: (Maybe Impls)
              _hdOpol :: Polarity
              _hdOprImpls :: (AssocL ImplsVarId Impls)
              _hdOprLL :: ([[PredOcc]])
              _hdOqSurrTvS :: TyVarIdS
              _hdOtyCtxt :: TyQuCtxt
              _tlOfxTvM :: TvCatMp
              _tlOisAtTop :: Bool
              _tlOisRow :: Bool
              _tlOmbPrLoc :: (Maybe Impls)
              _tlOpol :: Polarity
              _tlOprImpls :: (AssocL ImplsVarId Impls)
              _tlOprLL :: ([[PredOcc]])
              _tlOqSurrTvS :: TyVarIdS
              _tlOtyCtxt :: TyQuCtxt
              _hdIfrTvSL :: ([TyVarIdS])
              _hdIgathFxTvM :: TvCatMp
              _hdIinsPrIdSet :: (Set.Set PredOccId)
              _hdIinsPrL :: ([PredOcc])
              _hdIprImpls :: (AssocL ImplsVarId Impls)
              _hdIprTy :: Pred 
              _hdIprvReqs :: PrvReqs
              _hdIqInsideTvS :: TyVarIdS
              _hdIqOrphanTvS :: TyVarIdS
              _hdIself :: Pred 
              _tlIfrTvSL :: ([TyVarIdS])
              _tlIgathFxTvM :: TvCatMp
              _tlIinsPrIdSet :: (Set.Set PredOccId)
              _tlIinsPrL :: ([PredOcc])
              _tlIprImpls :: (AssocL ImplsVarId Impls)
              _tlIprTy :: PredSeq 
              _tlIprvReqs :: PrvReqs
              _tlIqInsideTvS :: TyVarIdS
              _tlIself :: PredSeq 
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 75, column 17)
              _tlOappSpinePos =
                  _lhsIappSpinePos + 1
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 87, column 17)
              _isSpineRoot =
                  _lhsIappSpinePos == 0
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _hdIfrTvSL ++ _tlIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _hdIgathFxTvM `Map.union` _tlIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _hdIinsPrIdSet `Set.union` _tlIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _hdIinsPrL ++ _tlIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _hdIprvReqs ++ _tlIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _hdIqInsideTvS `Set.union` _tlIqInsideTvS
              -- self rule
              _prTy =
                  PredSeq_Cons _hdIprTy _tlIprTy
              -- self rule
              _self =
                  PredSeq_Cons _hdIself _tlIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tlIprImpls
              -- copy rule (down)
              _hdOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _hdOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _hdOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _hdOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _hdOpol =
                  _lhsIpol
              -- copy rule (down)
              _hdOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _hdOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _hdOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _hdOtyCtxt =
                  _lhsItyCtxt
              -- copy rule (down)
              _tlOfxTvM =
                  _lhsIfxTvM
              -- copy rule (down)
              _tlOisAtTop =
                  _lhsIisAtTop
              -- copy rule (down)
              _tlOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _tlOmbPrLoc =
                  _lhsImbPrLoc
              -- copy rule (down)
              _tlOpol =
                  _lhsIpol
              -- copy rule (chain)
              _tlOprImpls =
                  _hdIprImpls
              -- copy rule (down)
              _tlOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tlOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (down)
              _tlOtyCtxt =
                  _lhsItyCtxt
              ( _hdIfrTvSL,_hdIgathFxTvM,_hdIinsPrIdSet,_hdIinsPrL,_hdIprImpls,_hdIprTy,_hdIprvReqs,_hdIqInsideTvS,_hdIqOrphanTvS,_hdIself) =
                  hd_ _hdOfxTvM _hdOisAtTop _hdOisRow _hdOmbPrLoc _hdOpol _hdOprImpls _hdOprLL _hdOqSurrTvS _hdOtyCtxt 
              ( _tlIfrTvSL,_tlIgathFxTvM,_tlIinsPrIdSet,_tlIinsPrL,_tlIprImpls,_tlIprTy,_tlIprvReqs,_tlIqInsideTvS,_tlIself) =
                  tl_ _tlOappSpinePos _tlOfxTvM _tlOisAtTop _tlOisRow _tlOmbPrLoc _tlOpol _tlOprImpls _tlOprLL _tlOqSurrTvS _tlOtyCtxt 
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOself)))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: PredSeq 
              _lhsOself :: PredSeq 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  PredSeq_Nil
              -- self rule
              _self =
                  PredSeq_Nil
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOself)))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: PredSeq 
              _lhsOself :: PredSeq 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  PredSeq_Var av_
              -- self rule
              _self =
                  PredSeq_Var av_
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOself)))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         fxTvM                : TvCatMp
         isAtTop              : Bool
         isRow                : Bool
         mbPrLoc              : Maybe Impls
         pol                  : Polarity
         prLL                 : [[PredOcc]]
         qSurrTvS             : TyVarIdS
         tyCtxt               : TyQuCtxt
      chained attribute:
         prImpls              : AssocL ImplsVarId Impls
      synthesized attributes:
         appFunNm             : HsName
         frTvSL               : [TyVarIdS]
         gathFxTvM            : TvCatMp
         insPrIdSet           : Set.Set PredOccId
         insPrL               : [PredOcc]
         isArrow              : Bool
         isPred               : Bool
         isQuLoc              : Bool
         mbImpls              : Maybe Impls
         prTy                 : SELF 
         prvReqs              : PrvReqs
         qInsideTvS           : TyVarIdS
         qOrphanTvS           : TyVarIdS
         self                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local prTy        : _
            local self        : _
      alternative Any:
         visit 0:
            local isQuLocExtraHook : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local frTvS       : _
            local prTy        : _
            local self        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local isQuLocExtraHook : _
            local _tup1       : _
            local implsPrL    : _
            local mbTail      : _
            local _tup2       : _
            local herePrL     : _
            local candPrL     : _
            local _tup3       : _
            local insHerePrL  : _
            local forPrTyOccL : _
            local insPrIdSet  : _
            local prTy        : _
            local appFunNm    : {HsName}
            local appIsArrow  : {Bool}
            local appIsOther  : _
            local appIsRec    : {Bool}
            local appIsSum    : _
            local appIsRecOrSum : _
            local appIsLikeProd : {Bool}
            local isAtTop     : _
            local argIsRow    : {Bool}
            local isSpineRoot : {Bool}
            local isArrowRoot : {Bool}
            local isArrowArg  : {Bool}
            local isProdRoot  : _
            local tyCtxt      : _
            local _tup4       : {(TyVarIdS,[TyVarIdS])}
            local frRowTvS    : {TyVarIdS}
            local frTvSL      : {[TyVarIdS]}
            local frTvS       : {TyVarIdS}
            local polArrowRes : _
            local isQuLoc     : {Bool}
            local qHereTvS    : {TyVarIdS}
            local qOrphanTvS  : {TyVarIdS}
            local qSurrTvS    : _
            local self        : _
      alternative Con:
         child nm             : {HsName}
         visit 0:
            local isQuLocExtraHook : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local frTvS       : _
            local prTy        : _
            local self        : _
      alternative Dbg:
         child info           : {String}
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local prTy        : _
            local self        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local appFunNm    : {HsName}
            local isAtTop     : _
            local isRow       : _
            local isSpineRoot : _
            local tyCtxt      : _
            local frTvSL      : _
            local qOrphanTvS  : _
            local prTy        : _
            local self        : _
      alternative Impls:
         child impls          : Impls 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local frTvS       : _
            local prTy        : _
            local self        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local introTVarL  : _
            local introTVarS  : {TyVarIdS}
            local frTvS       : _
            local prTy        : _
            local self        : _
      alternative Pred:
         child pr             : Pred 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local frTvS       : {TyVarIdS}
            local isQuLoc     : {Bool}
            local qHereTvS    : _
            local prTy        : _
            local self        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 0:
            local isQuLocExtraHook : _
            local mbPrLoc     : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local introTVarL  : _
            local introTVarS  : {TyVarIdS}
            local frTvS       : {TyVarIdS}
            local isQuLoc     : {Bool}
            local qHereTvS    : _
            local qSurrTvS    : _
            local prTy        : _
            local self        : _
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 0:
            local isQuLocExtraHook : _
            local implsPrL    : _
            local _tup5       : _
            local herePrL     : _
            local candPrL     : _
            local _tup6       : _
            local insHerePrL  : _
            local forPrTyOccL : _
            local insPrIdSet  : _
            local isAtTop     : _
            local isRow       : _
            local tyCtxt      : _
            local frTvS       : {TyVarIdS}
            local isQuLoc     : {Bool}
            local qHereTvS    : _
            local qSurrTvS    : _
            local prTy        : _
            local self        : _
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
             TvCatMp ->
             Bool ->
             Bool ->
             (Maybe Impls) ->
             Polarity ->
             (AssocL ImplsVarId Impls) ->
             ([[PredOcc]]) ->
             TyVarIdS ->
             TyQuCtxt ->
             ( HsName,([TyVarIdS]),TvCatMp,(Set.Set PredOccId),([PredOcc]),Bool,Bool,Bool,(Maybe Impls),(AssocL ImplsVarId Impls),Ty ,PrvReqs,TyVarIdS,TyVarIdS,Ty )
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _tyOappSpinePos :: Int
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOpol :: Polarity
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _annIprTy :: TyAnn 
              _annIself :: TyAnn 
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _tyIfrTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _tyIisQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Ty_Ann _annIprTy _tyIprTy
              -- self rule
              _self =
                  Ty_Ann _annIself _tyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (down)
              _tyOappSpinePos =
                  _lhsIappSpinePos
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _tyOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _tyOisRow =
                  _isRow
              -- copy rule (from local)
              _tyOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _tyOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _tyOtyCtxt =
                  _tyCtxt
              ( _annIprTy,_annIself) =
                  ann_ 
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 14, column 17)
              _frTvS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 17, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  False
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Ty_Any
              -- self rule
              _self =
                  Ty_Any
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _argOmbPrLoc :: (Maybe Impls)
              _funcOmbPrLoc :: (Maybe Impls)
              _funcOprLL :: ([[PredOcc]])
              _argOprLL :: ([[PredOcc]])
              _lhsOinsPrL :: ([PredOcc])
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOprvReqs :: PrvReqs
              _appFunNm :: HsName
              _appIsArrow :: Bool
              _appIsRec :: Bool
              _appIsLikeProd :: Bool
              _argIsRow :: Bool
              _argOisRow :: Bool
              _funcOappSpinePos :: Int
              _argOappSpinePos :: Int
              _isSpineRoot :: Bool
              _isArrowRoot :: Bool
              _isArrowArg :: Bool
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              __tup4 :: ((TyVarIdS,[TyVarIdS]))
              _frRowTvS :: TyVarIdS
              _frTvSL :: ([TyVarIdS])
              _frTvS :: TyVarIdS
              _funcOpol :: Polarity
              _argOpol :: Polarity
              _isQuLoc :: Bool
              _lhsOisQuLoc :: Bool
              _qHereTvS :: TyVarIdS
              _qOrphanTvS :: TyVarIdS
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOappFunNm :: HsName
              _funcOfxTvM :: TvCatMp
              _funcOisAtTop :: Bool
              _funcOisRow :: Bool
              _funcOprImpls :: (AssocL ImplsVarId Impls)
              _funcOqSurrTvS :: TyVarIdS
              _funcOtyCtxt :: TyQuCtxt
              _argOfxTvM :: TvCatMp
              _argOisAtTop :: Bool
              _argOprImpls :: (AssocL ImplsVarId Impls)
              _argOqSurrTvS :: TyVarIdS
              _argOtyCtxt :: TyQuCtxt
              _funcIappFunNm :: HsName
              _funcIfrTvSL :: ([TyVarIdS])
              _funcIgathFxTvM :: TvCatMp
              _funcIinsPrIdSet :: (Set.Set PredOccId)
              _funcIinsPrL :: ([PredOcc])
              _funcIisArrow :: Bool
              _funcIisPred :: Bool
              _funcIisQuLoc :: Bool
              _funcImbImpls :: (Maybe Impls)
              _funcIprImpls :: (AssocL ImplsVarId Impls)
              _funcIprTy :: Ty 
              _funcIprvReqs :: PrvReqs
              _funcIqInsideTvS :: TyVarIdS
              _funcIqOrphanTvS :: TyVarIdS
              _funcIself :: Ty 
              _argIappFunNm :: HsName
              _argIfrTvSL :: ([TyVarIdS])
              _argIgathFxTvM :: TvCatMp
              _argIinsPrIdSet :: (Set.Set PredOccId)
              _argIinsPrL :: ([PredOcc])
              _argIisArrow :: Bool
              _argIisPred :: Bool
              _argIisQuLoc :: Bool
              _argImbImpls :: (Maybe Impls)
              _argIprImpls :: (AssocL ImplsVarId Impls)
              _argIprTy :: Ty 
              _argIprvReqs :: PrvReqs
              _argIqInsideTvS :: TyVarIdS
              _argIqOrphanTvS :: TyVarIdS
              _argIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 84, column 17)
              _lhsOmbImpls =
                  if _isArrowArg then _argImbImpls else Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 94, column 33)
              __tup1 =
                  maybe ([],Nothing) implsPredsMbTail _funcImbImpls
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 94, column 33)
              (_implsPrL,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 94, column 33)
              (_,_mbTail) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 95, column 17)
              _argOmbPrLoc =
                  _mbTail
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 96, column 17)
              _funcOmbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 112, column 17)
              __tup2 =
                  if _isSpineRoot
                  then  if _appIsArrow
                        then  let  (h,r) = prLLArrowSplit (_frTvSL !! 1 `Set.union` _lhsIqSurrTvS) _lhsIprLL
                              in   (h,[],r)
                        else  let  (h,_) = prLLArrowSplit (_frTvS `Set.union` _lhsIqSurrTvS) _lhsIprLL
                              in   (h,[],[])
                  else  ([],[],[])
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 112, column 17)
              (_herePrL,_,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 112, column 17)
              (_,_funcOprLL,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 112, column 17)
              (_,_,_argOprLL) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 128, column 17)
              _candPrL =
                  _herePrL ++ _funcIinsPrL ++ _argIinsPrL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              __tup3 =
                  case _lhsImbPrLoc of
                    Just (Impls_Tail _ ipos)
                      -> (toInsPrPrvOcc _candPrL ipos,[])
                    _ -> ([],_candPrL)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              (_insHerePrL,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              (_,_lhsOinsPrL) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 134, column 17)
              _forPrTyOccL =
                  _implsPrL ++ _insHerePrL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 139, column 17)
              _insPrIdSet =
                  Set.fromList (map (poPoi . fst) _insHerePrL)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 140, column 17)
              _lhsOinsPrIdSet =
                  _insPrIdSet `Set.union` _funcIinsPrIdSet `Set.union` _argIinsPrIdSet
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 162, column 17)
              _lhsOprImpls =
                  prOccLImpls _lhsImbPrLoc _insHerePrL ++ _argIprImpls
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 173, column 17)
              _lhsOprvReqs =
                  prOccLPrvReqs _lhsImbPrLoc _insHerePrL ++ _funcIprvReqs ++ _argIprvReqs
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 181, column 17)
              _prTy =
                  mkTyImpls (map (poPr . fst) _forPrTyOccL) $ maybe (Ty_App _funcIprTy _argIprTy) (const _argIprTy) _funcImbImpls
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 5, column 17)
              _appFunNm =
                  _funcIappFunNm
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 16, column 17)
              _appIsArrow =
                  hsnIsArrow _funcIappFunNm
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 16, column 17)
              _appIsOther =
                  not (_appIsArrow || _appIsLikeProd)
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 21, column 17)
              _appIsRec =
                  hsnIsRec _funcIappFunNm
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 21, column 17)
              _appIsSum =
                  hsnIsSum _funcIappFunNm
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 21, column 17)
              _appIsRecOrSum =
                  _appIsRec || _appIsSum
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 21, column 17)
              _appIsLikeProd =
                  hsnIsProd _funcIappFunNm || _appIsRec
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 45, column 17)
              _argIsRow =
                  _isSpineRoot && _appIsRecOrSum
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 46, column 17)
              _argOisRow =
                  _argIsRow
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 61, column 17)
              _funcOappSpinePos =
                  _lhsIappSpinePos + 1
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 62, column 17)
              _argOappSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 84, column 17)
              _isSpineRoot =
                  _lhsIappSpinePos == 0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 90, column 17)
              _isArrowRoot =
                  _appIsArrow && _isSpineRoot
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 90, column 17)
              _isArrowArg =
                  _appIsArrow && _lhsIappSpinePos == 1
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 90, column 17)
              _isProdRoot =
                  _appIsLikeProd && _isSpineRoot
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 97, column 17)
              _lhsOisArrow =
                  _isArrowRoot
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 103, column 17)
              _tyCtxt =
                  if      _appIsArrow     then TyQuCtxtArrow
                  else if _appIsLikeProd  then TyQuCtxtProd
                                          else TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 115, column 17)
              _lhsOisPred =
                  if _isArrowArg then _argIisPred else False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 10, column 17)
              _lhsOfrTvSL =
                  if _isSpineRoot then [_frTvS] else _frTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 23, column 17)
              __tup4 =
                  if _argIsRow
                  then hdAndTl (reverse _argIfrTvSL)
                  else (Set.empty,_argIfrTvSL ++ _funcIfrTvSL)
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 23, column 17)
              (_frRowTvS,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 23, column 17)
              (_,_frTvSL) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 26, column 17)
              _frTvS =
                  Set.unions (_frRowTvS : _frTvSL)
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 44, column 17)
              _funcOpol =
                  _lhsIpol
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 45, column 17)
              _argOpol =
                  if          _appIsLikeProd  then  _lhsIpol
                  else  if    _isArrowRoot    then  _polArrowRes
                  else  if    _isArrowArg     then  polContravariant
                                              else  polInvariant
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 49, column 17)
              _polArrowRes =
                  if _funcIisPred then _lhsIpol else polCovariant
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 64, column 17)
              _isQuLoc =
                  (  polIsCovariant _lhsIpol
                  || (polIsContravariant _lhsIpol
                      && (_isProdRoot
                          || _isArrowRoot
                             && (not _funcIisPred || _argIisQuLoc)
                     )   )
                  ) && _isQuLocExtraHook
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 71, column 17)
              _lhsOisQuLoc =
                  if _isArrowArg then _argIisQuLoc else _isQuLoc
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 103, column 17)
              _qHereTvS =
                  if _isQuLoc
                  then  if    _appIsArrow || _appIsLikeProd
                        then  Set.unions [tvarSOccurGE2 _frTvSL, _frRowTvS, _qOrphanTvS]
                        else  _frTvS
                  else  Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 120, column 17)
              _qOrphanTvS =
                  _argIqOrphanTvS `Set.union` _funcIqOrphanTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 124, column 17)
              _lhsOqOrphanTvS =
                  if _isSpineRoot && _isQuLoc then Set.empty else _qOrphanTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 143, column 17)
              _lhsOqInsideTvS =
                  _qHereTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 152, column 17)
              _qSurrTvS =
                  _qHereTvS `Set.union` _lhsIqSurrTvS
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _funcIgathFxTvM `Map.union` _argIgathFxTvM
              -- self rule
              _self =
                  Ty_App _funcIself _argIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOappFunNm =
                  _appFunNm
              -- copy rule (down)
              _funcOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _funcOisAtTop =
                  _isAtTop
              -- copy rule (down)
              _funcOisRow =
                  _lhsIisRow
              -- copy rule (down)
              _funcOprImpls =
                  _lhsIprImpls
              -- copy rule (from local)
              _funcOqSurrTvS =
                  _qSurrTvS
              -- copy rule (from local)
              _funcOtyCtxt =
                  _tyCtxt
              -- copy rule (down)
              _argOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _argOisAtTop =
                  _isAtTop
              -- copy rule (chain)
              _argOprImpls =
                  _funcIprImpls
              -- copy rule (from local)
              _argOqSurrTvS =
                  _qSurrTvS
              -- copy rule (from local)
              _argOtyCtxt =
                  _tyCtxt
              ( _funcIappFunNm,_funcIfrTvSL,_funcIgathFxTvM,_funcIinsPrIdSet,_funcIinsPrL,_funcIisArrow,_funcIisPred,_funcIisQuLoc,_funcImbImpls,_funcIprImpls,_funcIprTy,_funcIprvReqs,_funcIqInsideTvS,_funcIqOrphanTvS,_funcIself) =
                  func_ _funcOappSpinePos _funcOfxTvM _funcOisAtTop _funcOisRow _funcOmbPrLoc _funcOpol _funcOprImpls _funcOprLL _funcOqSurrTvS _funcOtyCtxt 
              ( _argIappFunNm,_argIfrTvSL,_argIgathFxTvM,_argIinsPrIdSet,_argIinsPrL,_argIisArrow,_argIisPred,_argIisQuLoc,_argImbImpls,_argIprImpls,_argIprTy,_argIprvReqs,_argIqInsideTvS,_argIqOrphanTvS,_argIself) =
                  arg_ _argOappSpinePos _argOfxTvM _argOisAtTop _argOisRow _argOmbPrLoc _argOpol _argOprImpls _argOprLL _argOqSurrTvS _argOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 4, column 17)
              _lhsOappFunNm =
                  nm_
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 14, column 17)
              _frTvS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 17, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  False
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Ty_Con nm_
              -- self rule
              _self =
                  Ty_Con nm_
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  False
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  []
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Ty_Dbg info_
              -- self rule
              _self =
                  Ty_Dbg info_
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOprImpls =
                  _lhsIprImpls
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _appFunNm :: HsName
              _tyOappSpinePos :: Int
              _extTyOappSpinePos :: Int
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOappFunNm :: HsName
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOpol :: Polarity
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _extTyOfxTvM :: TvCatMp
              _extTyOisAtTop :: Bool
              _extTyOisRow :: Bool
              _extTyOmbPrLoc :: (Maybe Impls)
              _extTyOpol :: Polarity
              _extTyOprImpls :: (AssocL ImplsVarId Impls)
              _extTyOprLL :: ([[PredOcc]])
              _extTyOqSurrTvS :: TyVarIdS
              _extTyOtyCtxt :: TyQuCtxt
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              _extTyIappFunNm :: HsName
              _extTyIfrTvSL :: ([TyVarIdS])
              _extTyIgathFxTvM :: TvCatMp
              _extTyIinsPrIdSet :: (Set.Set PredOccId)
              _extTyIinsPrL :: ([PredOcc])
              _extTyIisArrow :: Bool
              _extTyIisPred :: Bool
              _extTyIisQuLoc :: Bool
              _extTyImbImpls :: (Maybe Impls)
              _extTyIprImpls :: (AssocL ImplsVarId Impls)
              _extTyIprTy :: Ty 
              _extTyIprvReqs :: PrvReqs
              _extTyIqInsideTvS :: TyVarIdS
              _extTyIqOrphanTvS :: TyVarIdS
              _extTyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 8, column 17)
              _appFunNm =
                  _tyIappFunNm
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 68, column 17)
              _tyOappSpinePos =
                  _lhsIappSpinePos + 1
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 69, column 17)
              _extTyOappSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 84, column 17)
              _isSpineRoot =
                  _lhsIappSpinePos == 0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 20, column 17)
              _frTvSL =
                  _extTyIfrTvSL ++ _tyIfrTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 121, column 17)
              _qOrphanTvS =
                  _extTyIqOrphanTvS `Set.union` _tyIqOrphanTvS
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 1, column 25)
              _lhsOfrTvSL =
                  _frTvSL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM `Map.union` _extTyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet `Set.union` _extTyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL ++ _extTyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _tyIisQuLoc && _extTyIisQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs ++ _extTyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS `Set.union` _extTyIqInsideTvS
              -- self rule
              _prTy =
                  Ty_Ext _tyIprTy nm_ _extTyIprTy
              -- self rule
              _self =
                  Ty_Ext _tyIself nm_ _extTyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOappFunNm =
                  _appFunNm
              -- copy rule (up)
              _lhsOprImpls =
                  _extTyIprImpls
              -- copy rule (from local)
              _lhsOqOrphanTvS =
                  _qOrphanTvS
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _tyOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _tyOisRow =
                  _isRow
              -- copy rule (from local)
              _tyOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _tyOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _tyOtyCtxt =
                  _tyCtxt
              -- copy rule (down)
              _extTyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _extTyOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _extTyOisRow =
                  _isRow
              -- copy rule (from local)
              _extTyOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _extTyOpol =
                  _lhsIpol
              -- copy rule (chain)
              _extTyOprImpls =
                  _tyIprImpls
              -- copy rule (down)
              _extTyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _extTyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _extTyOtyCtxt =
                  _tyCtxt
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
              ( _extTyIappFunNm,_extTyIfrTvSL,_extTyIgathFxTvM,_extTyIinsPrIdSet,_extTyIinsPrL,_extTyIisArrow,_extTyIisPred,_extTyIisQuLoc,_extTyImbImpls,_extTyIprImpls,_extTyIprTy,_extTyIprvReqs,_extTyIqInsideTvS,_extTyIqOrphanTvS,_extTyIself) =
                  extTy_ _extTyOappSpinePos _extTyOfxTvM _extTyOisAtTop _extTyOisRow _extTyOmbPrLoc _extTyOpol _extTyOprImpls _extTyOprLL _extTyOqSurrTvS _extTyOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _implsOfxTvM :: TvCatMp
              _implsOisAtTop :: Bool
              _implsOisRow :: Bool
              _implsOmbPrLoc :: (Maybe Impls)
              _implsOpol :: Polarity
              _implsOprImpls :: (AssocL ImplsVarId Impls)
              _implsOprLL :: ([[PredOcc]])
              _implsOqSurrTvS :: TyVarIdS
              _implsOtyCtxt :: TyQuCtxt
              _implsIfrTvSL :: ([TyVarIdS])
              _implsIgathFxTvM :: TvCatMp
              _implsIinsPrIdSet :: (Set.Set PredOccId)
              _implsIinsPrL :: ([PredOcc])
              _implsIprImpls :: (AssocL ImplsVarId Impls)
              _implsIprTy :: Impls 
              _implsIprvReqs :: PrvReqs
              _implsIqInsideTvS :: TyVarIdS
              _implsIqOrphanTvS :: TyVarIdS
              _implsIself :: Impls 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 83, column 17)
              _lhsOmbImpls =
                  Just _implsIprTy
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 114, column 17)
              _lhsOisPred =
                  True
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 28, column 17)
              _frTvS =
                  Set.unions _implsIfrTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 29, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _implsIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _implsIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _implsIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  False
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _implsIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _implsIqInsideTvS
              -- self rule
              _prTy =
                  Ty_Impls _implsIprTy
              -- self rule
              _self =
                  Ty_Impls _implsIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _implsIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _implsIqOrphanTvS
              -- copy rule (down)
              _implsOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _implsOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _implsOisRow =
                  _isRow
              -- copy rule (from local)
              _implsOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _implsOpol =
                  _lhsIpol
              -- copy rule (down)
              _implsOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _implsOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _implsOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _implsOtyCtxt =
                  _tyCtxt
              ( _implsIfrTvSL,_implsIgathFxTvM,_implsIinsPrIdSet,_implsIinsPrL,_implsIprImpls,_implsIprTy,_implsIprvReqs,_implsIqInsideTvS,_implsIqOrphanTvS,_implsIself) =
                  impls_ _implsOfxTvM _implsOisAtTop _implsOisRow _implsOmbPrLoc _implsOpol _implsOprImpls _implsOprLL _implsOqSurrTvS _implsOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _introTVarS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _tyOappSpinePos :: Int
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOpol :: Polarity
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 130, column 17)
              _introTVarL =
                  [tv_]
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 130, column 17)
              _introTVarS =
                  Set.singleton tv_
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 13, column 17)
              _frTvS =
                  head _tyIfrTvSL `Set.difference` _introTVarS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 17, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _tyIisQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Ty_Lam tv_ _tyIprTy
              -- self rule
              _self =
                  Ty_Lam tv_ _tyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (down)
              _tyOappSpinePos =
                  _lhsIappSpinePos
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _tyOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _tyOisRow =
                  _isRow
              -- copy rule (from local)
              _tyOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _tyOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _tyOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _tyOtyCtxt =
                  _tyCtxt
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _frTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _prOpol :: Polarity
              _isQuLoc :: Bool
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOqOrphanTvS :: TyVarIdS
              _prOfxTvM :: TvCatMp
              _prOisAtTop :: Bool
              _prOisRow :: Bool
              _prOmbPrLoc :: (Maybe Impls)
              _prOprImpls :: (AssocL ImplsVarId Impls)
              _prOprLL :: ([[PredOcc]])
              _prOqSurrTvS :: TyVarIdS
              _prOtyCtxt :: TyQuCtxt
              _prIfrTvSL :: ([TyVarIdS])
              _prIgathFxTvM :: TvCatMp
              _prIinsPrIdSet :: (Set.Set PredOccId)
              _prIinsPrL :: ([PredOcc])
              _prIprImpls :: (AssocL ImplsVarId Impls)
              _prIprTy :: Pred 
              _prIprvReqs :: PrvReqs
              _prIqInsideTvS :: TyVarIdS
              _prIqOrphanTvS :: TyVarIdS
              _prIself :: Pred 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 114, column 17)
              _lhsOisPred =
                  True
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 27, column 17)
              _frTvS =
                  Set.unions _prIfrTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 29, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 52, column 17)
              _prOpol =
                  polInvariant
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 74, column 17)
              _isQuLoc =
                  polIsCovariant _lhsIpol && _isQuLocExtraHook
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 108, column 17)
              _qHereTvS =
                  _frTvS `Set.difference` _prIqInsideTvS
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _prIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _prIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _prIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _isQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _prIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _prIqInsideTvS
              -- self rule
              _prTy =
                  Ty_Pred _prIprTy
              -- self rule
              _self =
                  Ty_Pred _prIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _prIprImpls
              -- copy rule (up)
              _lhsOqOrphanTvS =
                  _prIqOrphanTvS
              -- copy rule (down)
              _prOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _prOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _prOisRow =
                  _isRow
              -- copy rule (from local)
              _prOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _prOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _prOprLL =
                  _lhsIprLL
              -- copy rule (down)
              _prOqSurrTvS =
                  _lhsIqSurrTvS
              -- copy rule (from local)
              _prOtyCtxt =
                  _tyCtxt
              ( _prIfrTvSL,_prIgathFxTvM,_prIinsPrIdSet,_prIinsPrL,_prIprImpls,_prIprTy,_prIprvReqs,_prIqInsideTvS,_prIqOrphanTvS,_prIself) =
                  pr_ _prOfxTvM _prOisAtTop _prOisRow _prOmbPrLoc _prOpol _prOprImpls _prOprLL _prOqSurrTvS _prOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOappFunNm :: HsName
              _tyOappSpinePos :: Int
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _introTVarS :: TyVarIdS
              _frTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _isQuLoc :: Bool
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOisQuLoc :: Bool
              _lhsOprvReqs :: PrvReqs
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOprTy :: Ty 
              _lhsOself :: Ty 
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _tyOfxTvM :: TvCatMp
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOpol :: Polarity
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyOtyCtxt :: TyQuCtxt
              _quIprTy :: TyQu 
              _quIself :: TyQu 
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 99, column 17)
              _mbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 65, column 17)
              _tyOappSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 130, column 17)
              _introTVarL =
                  [tv_]
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 130, column 17)
              _introTVarS =
                  Set.singleton tv_
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 13, column 17)
              _frTvS =
                  head _tyIfrTvSL `Set.difference` _introTVarS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 17, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 63, column 17)
              _isQuLoc =
                  not (polIsInvariant _lhsIpol) && _isQuLocExtraHook
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 92, column 17)
              _qHereTvS =
                  if _isQuLoc then                                     _frTvS else Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 129, column 17)
              _lhsOqOrphanTvS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 152, column 17)
              _qSurrTvS =
                  _qHereTvS `Set.union` _lhsIqSurrTvS
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 154, column 28)
              _lhsOgathFxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 125, column 25)
              _lhsOinsPrL =
                  _tyIinsPrL
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _isQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  _tyIqInsideTvS
              -- self rule
              _prTy =
                  Ty_TBind _quIprTy tv_ l1_ _tyIprTy
              -- self rule
              _self =
                  Ty_TBind _quIself tv_ l1_ _tyIself
              -- self rule
              _lhsOprTy =
                  _prTy
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (down)
              _tyOfxTvM =
                  _lhsIfxTvM
              -- copy rule (from local)
              _tyOisAtTop =
                  _isAtTop
              -- copy rule (from local)
              _tyOisRow =
                  _isRow
              -- copy rule (from local)
              _tyOmbPrLoc =
                  _mbPrLoc
              -- copy rule (down)
              _tyOpol =
                  _lhsIpol
              -- copy rule (down)
              _tyOprImpls =
                  _lhsIprImpls
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (from local)
              _tyOqSurrTvS =
                  _qSurrTvS
              -- copy rule (from local)
              _tyOtyCtxt =
                  _tyCtxt
              ( _quIprTy,_quIself) =
                  qu_ 
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsIappSpinePos
       _lhsIfxTvM
       _lhsIisAtTop
       _lhsIisRow
       _lhsImbPrLoc
       _lhsIpol
       _lhsIprImpls
       _lhsIprLL
       _lhsIqSurrTvS
       _lhsItyCtxt ->
         (let _lhsOmbImpls :: (Maybe Impls)
              _lhsOinsPrL :: ([PredOcc])
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOprvReqs :: PrvReqs
              _lhsOprTy :: Ty 
              _lhsOappFunNm :: HsName
              _lhsOisArrow :: Bool
              _lhsOisPred :: Bool
              _frTvS :: TyVarIdS
              _lhsOfrTvSL :: ([TyVarIdS])
              _isQuLoc :: Bool
              _lhsOqOrphanTvS :: TyVarIdS
              _lhsOgathFxTvM :: TvCatMp
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOisQuLoc :: Bool
              _lhsOqInsideTvS :: TyVarIdS
              _lhsOself :: Ty 
              _categIprTy :: TyVarCateg 
              _categIself :: TyVarCateg 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 73, column 17)
              _isQuLocExtraHook =
                  True
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 86, column 17)
              _lhsOmbImpls =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 97, column 17)
              _implsPrL =
                  []
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 111, column 33)
              __tup5 =
                  prLLArrowSplit _qSurrTvS _lhsIprLL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 111, column 33)
              (_herePrL,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 129, column 17)
              _candPrL =
                  _herePrL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              __tup6 =
                  case _lhsImbPrLoc of
                    Just (Impls_Tail _ ipos)
                      -> (toInsPrPrvOcc _candPrL ipos,[])
                    _ -> ([],_candPrL)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              (_insHerePrL,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 130, column 17)
              (_,_lhsOinsPrL) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 134, column 17)
              _forPrTyOccL =
                  _implsPrL ++ _insHerePrL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 139, column 17)
              _insPrIdSet =
                  Set.fromList (map (poPoi . fst) _insHerePrL)
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 163, column 17)
              _lhsOprImpls =
                  prOccLImpls _lhsImbPrLoc _insHerePrL ++ _lhsIprImpls
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 174, column 17)
              _lhsOprvReqs =
                  prOccLPrvReqs _lhsImbPrLoc _insHerePrL
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 180, column 17)
              _lhsOprTy =
                  mkTyImpls (map (poPr . fst) _forPrTyOccL) $ Ty_Var tv_ _categIprTy
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 10, column 17)
              _lhsOappFunNm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 37, column 17)
              _isAtTop =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 47, column 17)
              _isRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 98, column 17)
              _lhsOisArrow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 106, column 17)
              _tyCtxt =
                  TyQuCtxtOther
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 117, column 17)
              _lhsOisPred =
                  False
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 7, column 17)
              _frTvS =
                  if tvCatIsPlain _categIself then Set.singleton tv_ else Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 17, column 17)
              _lhsOfrTvSL =
                  [_frTvS]
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 62, column 17)
              _isQuLoc =
                  polIsCovariant _lhsIpol && _isQuLocExtraHook
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 92, column 17)
              _qHereTvS =
                  if _isQuLoc then                                     _frTvS else Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 125, column 17)
              _lhsOqOrphanTvS =
                  if _isQuLoc then Set.empty else _frTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 152, column 17)
              _qSurrTvS =
                  _qHereTvS `Set.union` _lhsIqSurrTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 158, column 17)
              _lhsOgathFxTvM =
                  tv_ `Map.singleton` mkTvInfoTy _categIself
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _insPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 59, column 23)
              _lhsOisQuLoc =
                  _isQuLoc
              -- use rule "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 140, column 29)
              _lhsOqInsideTvS =
                  Set.empty
              -- self rule
              _prTy =
                  Ty_Var tv_ _categIprTy
              -- self rule
              _self =
                  Ty_Var tv_ _categIself
              -- self rule
              _lhsOself =
                  _self
              ( _categIprTy,_categIself) =
                  categ_ 
          in  ( _lhsOappFunNm,_lhsOfrTvSL,_lhsOgathFxTvM,_lhsOinsPrIdSet,_lhsOinsPrL,_lhsOisArrow,_lhsOisPred,_lhsOisQuLoc,_lhsOmbImpls,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs,_lhsOqInsideTvS,_lhsOqOrphanTvS,_lhsOself)))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         prLL                 : [[PredOcc]]
      synthesized attributes:
         insPrIdSet           : Set.Set PredOccId
         prImpls              : AssocL ImplsVarId Impls
         prTy                 : Ty 
         prvReqs              : PrvReqs
   alternatives:
      alternative AGItf:
         child ty             : Ty 
         visit 0:
            local frTvS       : _
            local qHereTvS    : _
            local qSurrTvS    : _
            local fxTvM       : _
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _ty )  =
    (sem_TyAGItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_TyAGItf  = ([[PredOcc]]) ->
                  ( (Set.Set PredOccId),(AssocL ImplsVarId Impls),Ty ,PrvReqs)
data Inh_TyAGItf  = Inh_TyAGItf {prLL_Inh_TyAGItf :: !(([[PredOcc]]))}
data Syn_TyAGItf  = Syn_TyAGItf {insPrIdSet_Syn_TyAGItf :: !((Set.Set PredOccId)),prImpls_Syn_TyAGItf :: !((AssocL ImplsVarId Impls)),prTy_Syn_TyAGItf :: !(Ty ),prvReqs_Syn_TyAGItf :: !(PrvReqs)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIprLL )  =
    (let ( _lhsOinsPrIdSet,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs) = sem _lhsIprLL 
     in  (Syn_TyAGItf _lhsOinsPrIdSet _lhsOprImpls _lhsOprTy _lhsOprvReqs ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIprLL ->
         (let _tyOpol :: Polarity
              _tyOmbPrLoc :: (Maybe Impls)
              _tyOprImpls :: (AssocL ImplsVarId Impls)
              _tyOisAtTop :: Bool
              _tyOisRow :: Bool
              _tyOappSpinePos :: Int
              _tyOtyCtxt :: TyQuCtxt
              _lhsOinsPrIdSet :: (Set.Set PredOccId)
              _lhsOprvReqs :: PrvReqs
              _lhsOprImpls :: (AssocL ImplsVarId Impls)
              _lhsOprTy :: Ty 
              _tyOfxTvM :: TvCatMp
              _tyOprLL :: ([[PredOcc]])
              _tyOqSurrTvS :: TyVarIdS
              _tyIappFunNm :: HsName
              _tyIfrTvSL :: ([TyVarIdS])
              _tyIgathFxTvM :: TvCatMp
              _tyIinsPrIdSet :: (Set.Set PredOccId)
              _tyIinsPrL :: ([PredOcc])
              _tyIisArrow :: Bool
              _tyIisPred :: Bool
              _tyIisQuLoc :: Bool
              _tyImbImpls :: (Maybe Impls)
              _tyIprImpls :: (AssocL ImplsVarId Impls)
              _tyIprTy :: Ty 
              _tyIprvReqs :: PrvReqs
              _tyIqInsideTvS :: TyVarIdS
              _tyIqOrphanTvS :: TyVarIdS
              _tyIself :: Ty 
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 77, column 17)
              _tyOpol =
                  polCovariant
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 91, column 17)
              _tyOmbPrLoc =
                  Nothing
              -- "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 159, column 17)
              _tyOprImpls =
                  []
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 34, column 17)
              _tyOisAtTop =
                  True
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 42, column 17)
              _tyOisRow =
                  False
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 58, column 17)
              _tyOappSpinePos =
                  0
              -- "build/101/lib-ehc/EH101/Ty/CommonAG.ag"(line 109, column 17)
              _tyOtyCtxt =
                  TyQuCtxtOnTop
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 4, column 17)
              _frTvS =
                  head _tyIfrTvSL
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 89, column 17)
              _qHereTvS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 148, column 17)
              _qSurrTvS =
                  _qHereTvS
              -- "build/101/lib-ehc/EH101/Ty/TyVarCommon.ag"(line 161, column 17)
              _fxTvM =
                  _tyIgathFxTvM
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 136, column 37)
              _lhsOinsPrIdSet =
                  _tyIinsPrIdSet
              -- use rule "build/101/lib-ehc/EH101/Ty/Trf/MergePreds.ag"(line 170, column 34)
              _lhsOprvReqs =
                  _tyIprvReqs
              -- copy rule (up)
              _lhsOprImpls =
                  _tyIprImpls
              -- copy rule (up)
              _lhsOprTy =
                  _tyIprTy
              -- copy rule (from local)
              _tyOfxTvM =
                  _fxTvM
              -- copy rule (down)
              _tyOprLL =
                  _lhsIprLL
              -- copy rule (from local)
              _tyOqSurrTvS =
                  _qSurrTvS
              ( _tyIappFunNm,_tyIfrTvSL,_tyIgathFxTvM,_tyIinsPrIdSet,_tyIinsPrL,_tyIisArrow,_tyIisPred,_tyIisQuLoc,_tyImbImpls,_tyIprImpls,_tyIprTy,_tyIprvReqs,_tyIqInsideTvS,_tyIqOrphanTvS,_tyIself) =
                  ty_ _tyOappSpinePos _tyOfxTvM _tyOisAtTop _tyOisRow _tyOmbPrLoc _tyOpol _tyOprImpls _tyOprLL _tyOqSurrTvS _tyOtyCtxt 
          in  ( _lhsOinsPrIdSet,_lhsOprImpls,_lhsOprTy,_lhsOprvReqs)))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         prTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Mono:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local prTy        : _
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
    (let _lhsOprTy :: TyAnn 
         _lhsOself :: TyAnn 
         -- self rule
         _prTy =
             TyAnn_Empty
         -- self rule
         _self =
             TyAnn_Empty
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (let _lhsOprTy :: TyAnn 
         _lhsOself :: TyAnn 
         -- self rule
         _prTy =
             TyAnn_Mono
         -- self rule
         _self =
             TyAnn_Mono
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (let _lhsOprTy :: TyAnn 
         _lhsOself :: TyAnn 
         -- self rule
         _prTy =
             TyAnn_Strictness s_
         -- self rule
         _self =
             TyAnn_Strictness s_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
-- TyQu --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         prTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local prTy        : _
            local self        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local prTy        : _
            local self        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
            local prTy        : _
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
    (let _lhsOprTy :: TyQu 
         _lhsOself :: TyQu 
         -- self rule
         _prTy =
             TyQu_Exists mlev_
         -- self rule
         _self =
             TyQu_Exists mlev_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (let _lhsOprTy :: TyQu 
         _lhsOself :: TyQu 
         -- self rule
         _prTy =
             TyQu_Forall mlev_
         -- self rule
         _self =
             TyQu_Forall mlev_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (let _lhsOprTy :: TyQu 
         _lhsOself :: TyQu 
         -- self rule
         _prTy =
             TyQu_Plain mlev_
         -- self rule
         _self =
             TyQu_Plain mlev_
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         prTy                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Meta:
         visit 0:
            local prTy        : _
            local self        : _
      alternative Plain:
         visit 0:
            local prTy        : _
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
    (let _lhsOprTy :: TyVarCateg 
         _lhsOself :: TyVarCateg 
         -- self rule
         _prTy =
             TyVarCateg_Fixed
         -- self rule
         _self =
             TyVarCateg_Fixed
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (let _lhsOprTy :: TyVarCateg 
         _lhsOself :: TyVarCateg 
         -- self rule
         _prTy =
             TyVarCateg_Meta
         -- self rule
         _self =
             TyVarCateg_Meta
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (let _lhsOprTy :: TyVarCateg 
         _lhsOself :: TyVarCateg 
         -- self rule
         _prTy =
             TyVarCateg_Plain
         -- self rule
         _self =
             TyVarCateg_Plain
         -- self rule
         _lhsOprTy =
             _prTy
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOprTy,_lhsOself))