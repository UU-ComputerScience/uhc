

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Pretty.ag)
module EH101.Ty.Pretty(ppTyWithCfg', ppTyWithCfg, ppTy
, CfgPPTy
, ppTyPr
, ppImplsWithCfg
, ppPredOccId'
, ppTyDt
, cfgPPTyDT) where

import EH.Util.Utils
import EH.Util.Pretty
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Ty
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import EH101.Scanner.Common(hiScanOpts)
import EH101.Base.CfgPP















ppTyWithCfg' :: CfgPP x => x -> Ty -> PP_Doc
ppTyWithCfg' x ty = pp_Syn_TyAGItf $ synTyWithCfg (cfgPPTy x) ty

synTyWithCfg :: CfgPPTy -> Ty -> Syn_TyAGItf
synTyWithCfg c ty
  =  wrap_TyAGItf
       (sem_TyAGItf (TyAGItf_AGItf ty))
       (Inh_TyAGItf {cfg_Inh_TyAGItf = c})

ppTyWithCfg :: CfgPPTy -> Ty -> PP_Doc
ppTyWithCfg c ty = pp_Syn_TyAGItf $ synTyWithCfg c ty

ppTy :: Ty -> PP_Doc
ppTy = ppTyWithCfg cfgPPTyDflt



ppImplsWithCfg :: CfgPPTy -> Impls -> PP_Doc
ppImplsWithCfg c i = ppTyWithCfg c (Ty_Impls i)



ppTyDt :: Ty -> PP_Doc
ppTyDt = ppTyWithCfg cfgPPTyDT



instance PP Ty where
  pp t = ppTy t



ppTyPr :: Ty -> PP_Doc
ppTyPr = ppTyWithCfg cfgPPTyPred




instance PP FIMode where
  pp m = pp (show m)



instance PP InstTo where
  pp  InstTo_Plain       = pp "_"
  pp (InstTo_Qu q v t k) = t >|< "/" >|< v >|< "@" >|< tyquMetaLev q



ppPredOccId' :: CfgPP x => x -> PredOccId -> PP_Doc
ppPredOccId' x poi
  = ppi (poiId poi)
  where ppi = cfgppUID x



ppPredOccId :: PredOccId -> PP_Doc
ppPredOccId poi = "Poi" >|< ppPredOccId' CfgPP_Plain poi



instance PP TyKiKey where
  pp = pp . show



instance PP Pred where
  pp p = ppTyWithCfg cfgPPTyPred (Ty_Pred p)

instance PP Impls where
  pp i = ppTy (Ty_Impls i)

instance PP PredOccId where
  pp = ppPredOccId

instance PP PredOcc where
  pp po = pp (poPr po) >|< "/" >|< pp (poPoi po) >|< "/" >|< pp (poScope po)

instance PP CHRPredOccCxt where
  pp (CHRPredOccCxt_Scope1 sc) = pp sc

instance PP CHRPredOcc where
  pp po = ppParensCommas
            [ pp (cpoPr po), pp (cpoCxt po)
            , pp (cpoRange po)
            ]

instance PP PredScope where
  pp (PredScope_Lev l) = ppListSep "<" ">" "," $ rllToList l
  pp (PredScope_Var v) = "<sc_" >|< v >|< ">"

instance PP ImplsProveOcc where
  pp o = ipoId o >|< "/" >|< ipoScope o



instance PP PredSeq where
  pp (PredSeq_Cons hd tl) = pp hd >#< ":" >#< pp tl
  pp (PredSeq_Nil       ) = pp "[]"
  pp (PredSeq_Var u     ) = pp u



instance PP LabelOffset where
  pp = pp . show



instance PP Label where
  pp l = pp_Syn_LabelAGItf t
       where t =  wrap_LabelAGItf
                    (sem_LabelAGItf (LabelAGItf_AGItf l))
                    (Inh_LabelAGItf)



data CfgPPTy
  = CfgPPTy
      { cfgPPTyPPHsn                :: HsName -> PP_Doc
      , cfgPPTyPPCon                :: CfgPPTy -> HsName -> PP_Doc
      , cfgPPTyPPVar                :: CfgPPTy -> UID -> Int -> PP_Doc
      , cfgPPTyPPVarDflt            :: CfgPPTy -> String -> UID -> PP_Doc
      , cfgPPTyPPImplsPred          :: PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
      , cfgPPTyPPImplsTailCons      :: PP_Doc -> PP_Doc -> PP_Doc
      , cfgPPTyPPImplsAppWrap       :: (PP_Doc -> PP_Doc) -> PP_Doc -> PP_Doc
      , cfgPPTyCtxt                 :: TyCtxt
      , cfgPPTyFollowAST            :: Bool
      , cfgPPTyLhsSafe              :: String -> String
      , cfgPPTyElimEmptyImpls       :: Bool
      }



cfgPPTy' :: CfgPP x => x -> CfgPPTy -> CfgPPTy
cfgPPTy' x c
  = c { cfgPPTyPPHsn        = cfgppHsName x
      , cfgPPTyPPVar        = \c u i -> cfgppVarHsName x Nothing (Just u) (Just i)
      , cfgPPTyPPCon        = \c n -> cfgppConHsName x n
      , cfgPPTyFollowAST    = cfgppFollowAST x
      }

cfgPPTy :: CfgPP x => x -> CfgPPTy
cfgPPTy x = cfgPPTy' x cfgPPTyDflt



cfgPPTyDflt :: CfgPPTy
cfgPPTyDflt
  = CfgPPTy
      { cfgPPTyPPHsn                = pp
      , cfgPPTyPPCon                = \_ -> ppCon
      , cfgPPTyPPVar                = \c _ i -> ppTnUniq c i
      , cfgPPTyPPVarDflt            = \cfg pre tv -> cfgPPTyPPHsn cfg $ mkHNm $ pre ++ "_" ++ show tv
      , cfgPPTyPPImplsPred          = \iv pr pv -> iv >|< "=" >|< pr >|< "/" >|< pv
      , cfgPPTyPPImplsTailCons      = \pr occs -> pr >|< occs
      , cfgPPTyPPImplsAppWrap       = ($)
      , cfgPPTyCtxt                 = TyCtxt_Ty
      , cfgPPTyFollowAST            = False
      , cfgPPTyLhsSafe              = id
      , cfgPPTyElimEmptyImpls       = False
      }




cfgPPTyDT :: CfgPPTy
cfgPPTyDT
  = cfgPPTyDflt
      { cfgPPTyPPHsn                = pp . hsnQualified
      , cfgPPTyPPCon                = \_ -> ppCon . hsnQualified
      , cfgPPTyPPImplsPred          = \_ pr _ -> pr
      , cfgPPTyPPImplsTailCons      = \pr _ -> pr
      , cfgPPTyPPImplsAppWrap       = \_ pr -> pr
      , cfgPPTyElimEmptyImpls       = True
      , cfgPPTyLhsSafe              = let safe '|' = "||"
                                          safe '@' = "@@"
                                          safe x   = [x]
                                      in  concatMap safe
      }



cfgPPTyPred :: CfgPPTy
cfgPPTyPred
  = cfgPPTyDflt
      { cfgPPTyCtxt         = TyCtxt_Pred
      }



cfgPPTyExtraForHI :: CfgPPTy -> CfgPPTy
cfgPPTyExtraForHI c
  = c { cfgPPTyPPVarDflt    = \cfg pre tv -> cfgPPTyPPVar cfg cfg tv 0
      }



cfgppAside :: (PP a, PP b) => CfgPPTy -> a -> b -> PP_Doc
cfgppAside c a b = if cfgPPTyFollowAST c then a >-< b else a >#< b



ppExts :: CfgPPTy -> HsName -> PP_Doc -> [PP_Doc] -> (PP_Doc,PP_Doc)
ppExts cfg appFunNm appFunPP appArgPPL
  = (ppNice,ppCfg)
  where ppNice = ppAppTop (hsnRow,mkPPAppFun' sep appFunNm appFunPP)
                          appArgPPL empty
        ppCfg  = if cfgPPTyFollowAST cfg
                 then let (e:es) = appArgPPL
                      in  hv ([hsnORow >#< appFunPP,sep >#< e] ++ map ("," >#<) es ++ [pp hsnCRow])
                 else ppNice
        sep = cfgPPTyLhsSafe cfg "|"



type TVarNameMap
  = Map.Map TyVarId
            PP_Doc



tnLookupPP :: TyVarId -> TVarNameMap -> Maybe PP_Doc
tnLookupPP = Map.lookup



tnMapInsert = Map.insert



tnUniqRepr :: Int -> String
tnUniqRepr
  = lrepr
  where lrepr i     =  if i <= 26
                       then  [repr i]
                       else  let  (d,r) = i `divMod` 26
                             in   (repr d : lrepr r)
        repr        =  (chr . (97+))



ppTnUniq :: CfgPPTy -> Int -> PP_Doc
ppTnUniq c = cfgPPTyPPHsn c . mkHNm . tnUniqRepr



mkDefaultTvNmPP :: CfgPPTy -> String -> TyVarId -> PP_Doc
mkDefaultTvNmPP cfg pre tv = cfgPPTyPPHsn cfg $ mkHNm $ pre ++ "_" ++ show tv



tvCategPrefix :: TyVarCateg -> String
tvCategPrefix TyVarCateg_Fixed = "c"
tvCategPrefix TyVarCateg_Plain = "v"
tvCategPrefix TyVarCateg_Meta  = "m"



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

-- Impls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cfg                  : CfgPPTy
         isAtTop              : Bool
         isRow                : Bool
         tyCtxt               : TyQuCtxt
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         isEmptyImpls         : Bool
         pp                   : PP_Doc
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
            local ppPv        : _
            local ppIv        : _
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 0:
            local ppIv        : _
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
type T_Impls  = CfgPPTy ->
                Bool ->
                Bool ->
                TVarNameMap ->
                Int ->
                TyQuCtxt ->
                ( Bool,PP_Doc,Impls ,TVarNameMap,Int)
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (False) of
          { _lhsOisEmptyImpls ->
          (case (_lhsItnUniq) of
           { _prOtnUniq ->
           (case (_lhsItyCtxt) of
            { _prOtyCtxt ->
            (case (_lhsItnMap) of
             { _prOtnMap ->
             (case (_lhsIisRow) of
              { _prOisRow ->
              (case (_lhsIisAtTop) of
               { _prOisAtTop ->
               (case (_lhsIcfg) of
                { _prOcfg ->
                (case (pr_ _prOcfg _prOisAtTop _prOisRow _prOtnMap _prOtnUniq _prOtyCtxt ) of
                 { ( _prIpp,_prIself,_prItnMap,_prItnUniq) ->
                     (case (_prItnUniq) of
                      { _tlOtnUniq ->
                      (case (_prItnMap) of
                       { _tlOtnMap ->
                       (case (_lhsIcfg) of
                        { _tlOcfg ->
                        (case (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "p" (poiId pv_)) id (tnLookupPP iv_ _lhsItnMap)) of
                         { _ppPv ->
                         (case (cfgPPTyPPImplsTailCons _lhsIcfg
                                  (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "i" iv_) id (tnLookupPP iv_ _lhsItnMap))
                                  (ppBracketsCommas proveOccs_)) of
                          { _ppIv ->
                          (case (_lhsItyCtxt) of
                           { _tlOtyCtxt ->
                           (case (_lhsIisRow) of
                            { _tlOisRow ->
                            (case (_lhsIisAtTop) of
                             { _tlOisAtTop ->
                             (case (tl_ _tlOcfg _tlOisAtTop _tlOisRow _tlOtnMap _tlOtnUniq _tlOtyCtxt ) of
                              { ( _tlIisEmptyImpls,_tlIpp,_tlIself,_tlItnMap,_tlItnUniq) ->
                                  (case (cfgPPTyPPImplsPred _lhsIcfg _ppIv _prIpp _ppPv >|< "," >|< _tlIpp) of
                                   { _lhsOpp ->
                                   (case (Impls_Cons iv_ _prIself pv_ prange_ proveOccs_ _tlIself) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_tlItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_tlItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOisEmptyImpls,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (cfgPPTyElimEmptyImpls _lhsIcfg) of
          { _lhsOisEmptyImpls ->
          (case (pp "_") of
           { _lhsOpp ->
           (case (Impls_Nil) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOisEmptyImpls,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (False) of
          { _lhsOisEmptyImpls ->
          (case (cfgPPTyPPImplsTailCons _lhsIcfg
                   (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "i" iv_) id (tnLookupPP iv_ _lhsItnMap))
                   (ppBracketsCommas proveOccs_)) of
           { _ppIv ->
           (case (_ppIv) of
            { _lhsOpp ->
            (case (Impls_Tail iv_ proveOccs_) of
             { _self ->
             (case (_self) of
              { _lhsOself ->
              (case (_lhsItnMap) of
               { _lhsOtnMap ->
               (case (_lhsItnUniq) of
                { _lhsOtnUniq ->
                ( _lhsOisEmptyImpls,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cfg                  : CfgPPTy
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local self        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
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
type T_Label  = CfgPPTy ->
                TVarNameMap ->
                Int ->
                ( PP_Doc,Label ,TVarNameMap,Int)
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (cfgPPTyPPHsn _lhsIcfg nm_) of
          { _lhsOpp ->
          (case (Label_Lab nm_) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "l" lv_) id (tnLookupPP lv_ _lhsItnMap)) of
          { _lhsOpp ->
          (case (Label_Var lv_) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
-- LabelAGItf --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child lab            : Label 
         visit 0:
            local tnMap       : _
            local cfg         : _
            local tnUniq      : _
-}
-- cata
sem_LabelAGItf :: LabelAGItf  ->
                  T_LabelAGItf 
sem_LabelAGItf (LabelAGItf_AGItf _lab )  =
    (sem_LabelAGItf_AGItf (sem_Label _lab ) )
-- semantic domain
type T_LabelAGItf  = ( PP_Doc)
data Inh_LabelAGItf  = Inh_LabelAGItf {}
data Syn_LabelAGItf  = Syn_LabelAGItf {pp_Syn_LabelAGItf :: !(PP_Doc)}
wrap_LabelAGItf :: T_LabelAGItf  ->
                   Inh_LabelAGItf  ->
                   Syn_LabelAGItf 
wrap_LabelAGItf sem (Inh_LabelAGItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_LabelAGItf _lhsOpp ))
sem_LabelAGItf_AGItf :: T_Label  ->
                        T_LabelAGItf 
sem_LabelAGItf_AGItf lab_  =
    (case (Map.empty) of
     { _tnMap ->
     (case (_tnMap) of
      { _labOtnMap ->
      (case (cfgPPTyDflt) of
       { _cfg ->
       (case (_cfg) of
        { _labOcfg ->
        (case (0) of
         { _tnUniq ->
         (case (_tnUniq) of
          { _labOtnUniq ->
          (case (lab_ _labOcfg _labOtnMap _labOtnUniq ) of
           { ( _labIpp,_labIself,_labItnMap,_labItnUniq) ->
               (case (_labIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) }) }) }) }) })
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cfg                  : CfgPPTy
         isAtTop              : Bool
         isRow                : Bool
         tyCtxt               : TyQuCtxt
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 0:
            local cfg         : _
            local appSpinePos : _
            local self        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local positionalFldNmL : _
            local parNeedL    : _
            local parNeed     : _
            local cfg         : _
            local appSpinePos : _
            local self        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local positionalFldNmL : _
            local parNeedL    : _
            local parNeed     : _
            local cfg         : _
            local appSpinePos : _
            local self        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local cfg         : _
            local positionalFldNmL : _
            local parNeedL    : _
            local parNeed     : _
            local appSpinePos : _
            local self        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local positionalFldNmL : _
            local parNeedL    : _
            local parNeed     : _
            local cfg         : _
            local appSpinePos : _
            local self        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 0:
            local cfg         : _
            local appSpinePos : _
            local self        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 0:
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
type T_Pred  = CfgPPTy ->
               Bool ->
               Bool ->
               TVarNameMap ->
               Int ->
               TyQuCtxt ->
               ( PP_Doc,Pred ,TVarNameMap,Int)
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _argsOtnUniq ->
          (case (_lhsItyCtxt) of
           { _argsOtyCtxt ->
           (case (_lhsItnMap) of
            { _argsOtnMap ->
            (case (_lhsIisRow) of
             { _argsOisRow ->
             (case (_lhsIisAtTop) of
              { _argsOisAtTop ->
              (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Ty   }) of
               { _cfg ->
               (case (_cfg) of
                { _argsOcfg ->
                (case (0) of
                 { _appSpinePos ->
                 (case (_appSpinePos) of
                  { _argsOappSpinePos ->
                  (case (args_ _argsOappSpinePos _argsOcfg _argsOisAtTop _argsOisRow _argsOtnMap _argsOtnUniq _argsOtyCtxt ) of
                   { ( _argsIpp,_argsIppL,_argsIself,_argsItnMap,_argsItnUniq) ->
                       (case (_argsItnUniq) of
                        { _resOtnUniq ->
                        (case (_argsItnMap) of
                         { _resOtnMap ->
                         (case (_cfg) of
                          { _resOcfg ->
                          (case (_lhsItyCtxt) of
                           { _resOtyCtxt ->
                           (case (_lhsIisRow) of
                            { _resOisRow ->
                            (case (_lhsIisAtTop) of
                             { _resOisAtTop ->
                             (case (res_ _resOcfg _resOisAtTop _resOisRow _resOtnMap _resOtnUniq _resOtyCtxt ) of
                              { ( _resIpp,_resIself,_resItnMap,_resItnUniq) ->
                                  (case (ppParensCommas _argsIppL >#< "=>" >#< _resIpp) of
                                   { _lhsOpp ->
                                   (case (Pred_Arrow _argsIself _resIself) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_resItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_resItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _tyOtnUniq ->
          (case (_lhsItnMap) of
           { _tyOtnMap ->
           (case (positionalFldNames) of
            { _positionalFldNmL ->
            (case (_positionalFldNmL) of
             { _tyOpositionalFldNmL ->
             (case ([]) of
              { _parNeedL ->
              (case (_parNeedL) of
               { _tyOparNeedL ->
               (case (ParNotNeeded) of
                { _parNeed ->
                (case (_parNeed) of
                 { _tyOparNeed ->
                 (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Ty   }) of
                  { _cfg ->
                  (case (_cfg) of
                   { _tyOcfg ->
                   (case (0) of
                    { _appSpinePos ->
                    (case (_appSpinePos) of
                     { _tyOappSpinePos ->
                     (case (ty_ ) of
                      { ( _tyIappFunNm,ty_1) ->
                          (case (_lhsItyCtxt) of
                           { _tyOtyCtxt ->
                           (case (_lhsIisRow) of
                            { _tyOisRow ->
                            (case (_lhsIisAtTop) of
                             { _tyOisAtTop ->
                             (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                              { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                  (case (_tyIpp) of
                                   { _lhsOpp ->
                                   (case (Pred_Class _tyIself) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_tyItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_tyItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _tyLOtnUniq ->
          (case (tyL_ ) of
           { ( _tyLIappFunNm,tyL_1) ->
               (case (_lhsItyCtxt) of
                { _tyLOtyCtxt ->
                (case (_lhsItnMap) of
                 { _tyLOtnMap ->
                 (case (positionalFldNames) of
                  { _positionalFldNmL ->
                  (case (_positionalFldNmL) of
                   { _tyLOpositionalFldNmL ->
                   (case ([]) of
                    { _parNeedL ->
                    (case (_parNeedL) of
                     { _tyLOparNeedL ->
                     (case (ParNotNeeded) of
                      { _parNeed ->
                      (case (_parNeed) of
                       { _tyLOparNeed ->
                       (case (_lhsIisRow) of
                        { _tyLOisRow ->
                        (case (_lhsIisAtTop) of
                         { _tyLOisAtTop ->
                         (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Ty   }) of
                          { _cfg ->
                          (case (_cfg) of
                           { _tyLOcfg ->
                           (case (0) of
                            { _appSpinePos ->
                            (case (_appSpinePos) of
                             { _tyLOappSpinePos ->
                             (case (tyL_1 _tyLOappSpinePos _tyLOcfg _tyLOisAtTop _tyLOisRow _tyLOparNeed _tyLOparNeedL _tyLOpositionalFldNmL _tyLOtnMap _tyLOtnUniq _tyLOtyCtxt ) of
                              { ( _tyLIappArgIsEmptyImplsL,_tyLIappArgPPL,_tyLIappFunPP,_tyLIisArrow,_tyLIisEmptyImpls,_tyLIisPred,_tyLIpositionalFldNmL,_tyLIpp,_tyLIself,_tyLItnMap,_tyLItnUniq) ->
                                  (case (_tyLItnUniq) of
                                   { _tyROtnUniq ->
                                   (case (_tyLItnMap) of
                                    { _tyROtnMap ->
                                    (case (_positionalFldNmL) of
                                     { _tyROpositionalFldNmL ->
                                     (case (_parNeedL) of
                                      { _tyROparNeedL ->
                                      (case (_parNeed) of
                                       { _tyROparNeed ->
                                       (case (_cfg) of
                                        { _tyROcfg ->
                                        (case (_appSpinePos) of
                                         { _tyROappSpinePos ->
                                         (case (tyR_ ) of
                                          { ( _tyRIappFunNm,tyR_1) ->
                                              (case (_lhsItyCtxt) of
                                               { _tyROtyCtxt ->
                                               (case (_lhsIisRow) of
                                                { _tyROisRow ->
                                                (case (_lhsIisAtTop) of
                                                 { _tyROisAtTop ->
                                                 (case (tyR_1 _tyROappSpinePos _tyROcfg _tyROisAtTop _tyROisRow _tyROparNeed _tyROparNeedL _tyROpositionalFldNmL _tyROtnMap _tyROtnUniq _tyROtyCtxt ) of
                                                  { ( _tyRIappArgIsEmptyImplsL,_tyRIappArgPPL,_tyRIappFunPP,_tyRIisArrow,_tyRIisEmptyImpls,_tyRIisPred,_tyRIpositionalFldNmL,_tyRIpp,_tyRIself,_tyRItnMap,_tyRItnUniq) ->
                                                      (case (_tyLIpp >#< hsnEqTilde >#< _tyRIpp) of
                                                       { _lhsOpp ->
                                                       (case (Pred_Eq _tyLIself _tyRIself) of
                                                        { _self ->
                                                        (case (_self) of
                                                         { _lhsOself ->
                                                         (case (_tyRItnMap) of
                                                          { _lhsOtnMap ->
                                                          (case (_tyRItnUniq) of
                                                           { _lhsOtnUniq ->
                                                           ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _tyOtnUniq ->
          (case (_lhsItnMap) of
           { _tyOtnMap ->
           (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Ty   }) of
            { _cfg ->
            (case (_cfg) of
             { _tyOcfg ->
             (case (ty_ ) of
              { ( _tyIappFunNm,ty_1) ->
                  (case (_lhsItyCtxt) of
                   { _tyOtyCtxt ->
                   (case (positionalFldNames) of
                    { _positionalFldNmL ->
                    (case (_positionalFldNmL) of
                     { _tyOpositionalFldNmL ->
                     (case ([]) of
                      { _parNeedL ->
                      (case (_parNeedL) of
                       { _tyOparNeedL ->
                       (case (ParNotNeeded) of
                        { _parNeed ->
                        (case (_parNeed) of
                         { _tyOparNeed ->
                         (case (_lhsIisRow) of
                          { _tyOisRow ->
                          (case (_lhsIisAtTop) of
                           { _tyOisAtTop ->
                           (case (0) of
                            { _appSpinePos ->
                            (case (_appSpinePos) of
                             { _tyOappSpinePos ->
                             (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                              { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                  (case (_tyItnMap) of
                                   { _labOtnMap ->
                                   (case (_cfg) of
                                    { _labOcfg ->
                                    (case (_tyItnUniq) of
                                     { _labOtnUniq ->
                                     (case (lab_ _labOcfg _labOtnMap _labOtnUniq ) of
                                      { ( _labIpp,_labIself,_labItnMap,_labItnUniq) ->
                                          (case (_tyIpp >|< "\\" >|< _labIpp) of
                                           { _lhsOpp ->
                                           (case (Pred_Lacks _tyIself _labIself) of
                                            { _self ->
                                            (case (_self) of
                                             { _lhsOself ->
                                             (case (_labItnMap) of
                                              { _lhsOtnMap ->
                                              (case (_labItnUniq) of
                                               { _lhsOtnUniq ->
                                               ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _tyOtnUniq ->
          (case (_lhsItnMap) of
           { _tyOtnMap ->
           (case (positionalFldNames) of
            { _positionalFldNmL ->
            (case (_positionalFldNmL) of
             { _tyOpositionalFldNmL ->
             (case ([]) of
              { _parNeedL ->
              (case (_parNeedL) of
               { _tyOparNeedL ->
               (case (ParNeeded) of
                { _parNeed ->
                (case (_parNeed) of
                 { _tyOparNeed ->
                 (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Pred }) of
                  { _cfg ->
                  (case (_cfg) of
                   { _tyOcfg ->
                   (case (0) of
                    { _appSpinePos ->
                    (case (_appSpinePos) of
                     { _tyOappSpinePos ->
                     (case (ty_ ) of
                      { ( _tyIappFunNm,ty_1) ->
                          (case (_lhsItyCtxt) of
                           { _tyOtyCtxt ->
                           (case (_lhsIisRow) of
                            { _tyOisRow ->
                            (case (_lhsIisAtTop) of
                             { _tyOisAtTop ->
                             (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                              { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                  (case (_tyIpp) of
                                   { _lhsOpp ->
                                   (case (Pred_Pred _tyIself) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_tyItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_tyItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _seqOtnUniq ->
          (case (_lhsItnMap) of
           { _seqOtnMap ->
           (case (_lhsIcfg { cfgPPTyCtxt = TyCtxt_Ty   }) of
            { _cfg ->
            (case (_cfg) of
             { _seqOcfg ->
             (case (_lhsItyCtxt) of
              { _seqOtyCtxt ->
              (case (_lhsIisRow) of
               { _seqOisRow ->
               (case (_lhsIisAtTop) of
                { _seqOisAtTop ->
                (case (0) of
                 { _appSpinePos ->
                 (case (_appSpinePos) of
                  { _seqOappSpinePos ->
                  (case (seq_ _seqOappSpinePos _seqOcfg _seqOisAtTop _seqOisRow _seqOtnMap _seqOtnUniq _seqOtyCtxt ) of
                   { ( _seqIpp,_seqIppL,_seqIself,_seqItnMap,_seqItnUniq) ->
                       (case (ppParensCommas _seqIppL) of
                        { _lhsOpp ->
                        (case (Pred_Preds _seqIself) of
                         { _self ->
                         (case (_self) of
                          { _lhsOself ->
                          (case (_seqItnMap) of
                           { _lhsOtnMap ->
                           (case (_seqItnUniq) of
                            { _lhsOtnUniq ->
                            ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "p" pv_) id (tnLookupPP pv_ _lhsItnMap)) of
          { _lhsOpp ->
          (case (Pred_Var pv_) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         cfg                  : CfgPPTy
         isAtTop              : Bool
         isRow                : Bool
         tyCtxt               : TyQuCtxt
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
      alternative Var:
         child av             : {TyVarId}
         visit 0:
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
                  CfgPPTy ->
                  Bool ->
                  Bool ->
                  TVarNameMap ->
                  Int ->
                  TyQuCtxt ->
                  ( PP_Doc,([PP_Doc]),PredSeq ,TVarNameMap,Int)
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIappSpinePos
       _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (_lhsItnUniq) of
          { _hdOtnUniq ->
          (case (_lhsItyCtxt) of
           { _hdOtyCtxt ->
           (case (_lhsItnMap) of
            { _hdOtnMap ->
            (case (_lhsIisRow) of
             { _hdOisRow ->
             (case (_lhsIisAtTop) of
              { _hdOisAtTop ->
              (case (_lhsIcfg) of
               { _hdOcfg ->
               (case (hd_ _hdOcfg _hdOisAtTop _hdOisRow _hdOtnMap _hdOtnUniq _hdOtyCtxt ) of
                { ( _hdIpp,_hdIself,_hdItnMap,_hdItnUniq) ->
                    (case (_hdItnUniq) of
                     { _tlOtnUniq ->
                     (case (_hdItnMap) of
                      { _tlOtnMap ->
                      (case (_lhsIcfg) of
                       { _tlOcfg ->
                       (case (_lhsItyCtxt) of
                        { _tlOtyCtxt ->
                        (case (_lhsIisRow) of
                         { _tlOisRow ->
                         (case (_lhsIisAtTop) of
                          { _tlOisAtTop ->
                          (case (_lhsIappSpinePos + 1) of
                           { _tlOappSpinePos ->
                           (case (tl_ _tlOappSpinePos _tlOcfg _tlOisAtTop _tlOisRow _tlOtnMap _tlOtnUniq _tlOtyCtxt ) of
                            { ( _tlIpp,_tlIppL,_tlIself,_tlItnMap,_tlItnUniq) ->
                                (case (_hdIpp >#< _tlIpp) of
                                 { _lhsOpp ->
                                 (case (_hdIpp :  _tlIppL) of
                                  { _lhsOppL ->
                                  (case (PredSeq_Cons _hdIself _tlIself) of
                                   { _self ->
                                   (case (_self) of
                                    { _lhsOself ->
                                    (case (_tlItnMap) of
                                     { _lhsOtnMap ->
                                     (case (_tlItnUniq) of
                                      { _lhsOtnUniq ->
                                      ( _lhsOpp,_lhsOppL,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIappSpinePos
       _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           (case (PredSeq_Nil) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOpp,_lhsOppL,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIappSpinePos
       _lhsIcfg
       _lhsIisAtTop
       _lhsIisRow
       _lhsItnMap
       _lhsItnUniq
       _lhsItyCtxt ->
         (case (empty) of
          { _lhsOpp ->
          (case ([maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg "prsq" av_) id (tnLookupPP av_ _lhsItnMap)]) of
           { _lhsOppL ->
           (case (PredSeq_Var av_) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOpp,_lhsOppL,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         appFunNm             : HsName
   visit 1:
      inherited attributes:
         appSpinePos          : Int
         cfg                  : CfgPPTy
         isAtTop              : Bool
         isRow                : Bool
         parNeed              : ParNeed
         parNeedL             : ParNeedL
         tyCtxt               : TyQuCtxt
      chained attributes:
         positionalFldNmL     : [HsName]
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         appArgIsEmptyImplsL  : [Bool]
         appArgPPL            : [PP_Doc]
         appFunPP             : PP_Doc
         isArrow              : Bool
         isEmptyImpls         : Bool
         isPred               : Bool
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local pp          : _
            local self        : _
      alternative Any:
         visit 1:
            local pp          : _
            local self        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local appFunNm    : {HsName}
         visit 1:
            local appIsRec    : {Bool}
            local appIsLikeProd : {Bool}
            local appIsArrow  : {Bool}
            local tyCtxt      : _
            local isSpineRoot : {Bool}
            local _tup3       : _
            local parNeed     : _
            local isAtTop     : _
            local argsParNeedL : _
            local _tup4       : {(ParNeed,ParNeedL)}
            local appIsSum    : _
            local appIsRecOrSum : _
            local argIsRow    : {Bool}
            local appArgIsEmptyImplsL : _
            local recSep      : _
            local _tup2       : _
            local appArgPPL   : _
            local appFunPP    : _
            local isArrowRoot : {Bool}
            local isArrowArg  : {Bool}
            local ppDflt      : _
            local ppAppTop    : _
            local ppAST       : _
            local ppNice      : _
            local pp          : _
            local self        : _
            intra appFunNm    : {HsName}
      alternative Con:
         child nm             : {HsName}
         visit 1:
            local pp          : _
            local self        : _
      alternative Dbg:
         child info           : {String}
         visit 1:
            local pp          : _
            local self        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local appFunNm    : {HsName}
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local _tup6       : _
            local positionalNm : _
            local fldPP       : _
            local appArgPPL   : _
            local appFunPP    : _
            local isSpineRoot : _
            local _tup5       : _
            local ppCfg       : _
            local ppNice      : _
            local pp          : _
            local self        : _
            intra appFunNm    : {HsName}
      alternative Impls:
         child impls          : Impls 
         visit 1:
            local wrapPP      : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local pp          : _
            local self        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 1:
            local tnPP        : _
            local tnMap       : _
            local parNeed     : _
            local tnUniq      : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local pp          : _
            local self        : _
      alternative Pred:
         child pr             : Pred 
         visit 1:
            local wrapPP      : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local pp          : _
            local self        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 1:
            inst  l1'         : Ty 
            local tnUniq      : _
            local tnPP        : _
            local tnMap       : _
            local tyCtxt      : _
            local parNeed     : _
            local isRow       : _
            local isAtTop     : _
            local ppL1        : _
            local ppTvPost    : _
            local ppQu        : _
            local ppTvPre     : _
            local ppTv        : _
            local pp          : _
            local self        : _
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 1:
            local pp          : _
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
type T_Ty  = ( HsName,T_Ty_1 )
type T_Ty_1  = Int ->
               CfgPPTy ->
               Bool ->
               Bool ->
               ParNeed ->
               ParNeedL ->
               ([HsName]) ->
               TVarNameMap ->
               Int ->
               TyQuCtxt ->
               ( ([Bool]),([PP_Doc]),PP_Doc,Bool,Bool,Bool,([HsName]),PP_Doc,Ty ,TVarNameMap,Int)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Ann_1 :: T_Ty_1 
                 sem_Ty_Ann_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (_lhsItnUniq) of
                             { _annOtnUniq ->
                             (case (_lhsItnMap) of
                              { _annOtnMap ->
                              (case (_lhsIcfg) of
                               { _annOcfg ->
                               (case (ann_ _annOcfg _annOtnMap _annOtnUniq ) of
                                { ( _annIpp,_annIself,_annItnMap,_annItnUniq) ->
                                    (case (_annItnUniq) of
                                     { _tyOtnUniq ->
                                     (case (_annItnMap) of
                                      { _tyOtnMap ->
                                      (case (_lhsIpositionalFldNmL) of
                                       { _tyOpositionalFldNmL ->
                                       (case (_lhsIparNeedL) of
                                        { _tyOparNeedL ->
                                        (case (_lhsIparNeed) of
                                         { _tyOparNeed ->
                                         (case (_lhsIcfg) of
                                          { _tyOcfg ->
                                          (case (_lhsIappSpinePos) of
                                           { _tyOappSpinePos ->
                                           (case (ty_ ) of
                                            { ( _tyIappFunNm,ty_1) ->
                                                (case (TyQuCtxtOther) of
                                                 { _tyCtxt ->
                                                 (case (_tyCtxt) of
                                                  { _tyOtyCtxt ->
                                                  (case (False) of
                                                   { _isRow ->
                                                   (case (_isRow) of
                                                    { _tyOisRow ->
                                                    (case (False) of
                                                     { _isAtTop ->
                                                     (case (_isAtTop) of
                                                      { _tyOisAtTop ->
                                                      (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                                                       { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                                           (case (_annIpp >#< _tyIpp) of
                                                            { _pp ->
                                                            (case (_pp) of
                                                             { _lhsOappFunPP ->
                                                             (case (False) of
                                                              { _lhsOisArrow ->
                                                              (case (False) of
                                                               { _lhsOisEmptyImpls ->
                                                               (case (False) of
                                                                { _lhsOisPred ->
                                                                (case (_lhsIpositionalFldNmL) of
                                                                 { _lhsOpositionalFldNmL ->
                                                                 (case (_pp) of
                                                                  { _lhsOpp ->
                                                                  (case (Ty_Ann _annIself _tyIself) of
                                                                   { _self ->
                                                                   (case (_self) of
                                                                    { _lhsOself ->
                                                                    (case (_tyItnMap) of
                                                                     { _lhsOtnMap ->
                                                                     (case (_tyItnUniq) of
                                                                      { _lhsOtnUniq ->
                                                                      ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Ann_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Any_1 :: T_Ty_1 
                 sem_Ty_Any_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (pp hsnUnknown) of
                             { _pp ->
                             (case (_pp) of
                              { _lhsOappFunPP ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (False) of
                                { _lhsOisEmptyImpls ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (_lhsIpositionalFldNmL) of
                                  { _lhsOpositionalFldNmL ->
                                  (case (_pp) of
                                   { _lhsOpp ->
                                   (case (Ty_Any) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_lhsItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_lhsItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Any_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (case (func_ ) of
     { ( _funcIappFunNm,func_1) ->
         (case (_funcIappFunNm) of
          { _appFunNm ->
          (case (_appFunNm) of
           { _lhsOappFunNm ->
           (case ((let sem_Ty_App_1 :: T_Ty_1 
                       sem_Ty_App_1  =
                           (\ _lhsIappSpinePos
                              _lhsIcfg
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIparNeed
                              _lhsIparNeedL
                              _lhsIpositionalFldNmL
                              _lhsItnMap
                              _lhsItnUniq
                              _lhsItyCtxt ->
                                (case (_lhsIcfg) of
                                 { _argOcfg ->
                                 (case (_lhsIcfg) of
                                  { _funcOcfg ->
                                  (case (arg_ ) of
                                   { ( _argIappFunNm,arg_1) ->
                                       (case (hsnIsRec _funcIappFunNm) of
                                        { _appIsRec ->
                                        (case (hsnIsProd _funcIappFunNm || _appIsRec) of
                                         { _appIsLikeProd ->
                                         (case (hsnIsArrow _funcIappFunNm) of
                                          { _appIsArrow ->
                                          (case (if      _appIsArrow     then TyQuCtxtArrow
                                                 else if _appIsLikeProd  then TyQuCtxtProd
                                                                         else TyQuCtxtOther) of
                                           { _tyCtxt ->
                                           (case (_tyCtxt) of
                                            { _argOtyCtxt ->
                                            (case (_lhsItnUniq) of
                                             { _funcOtnUniq ->
                                             (case (_tyCtxt) of
                                              { _funcOtyCtxt ->
                                              (case (_lhsItnMap) of
                                               { _funcOtnMap ->
                                               (case (_lhsIpositionalFldNmL) of
                                                { _funcOpositionalFldNmL ->
                                                (case (_lhsIappSpinePos == 0) of
                                                 { _isSpineRoot ->
                                                 (case (if _isSpineRoot
                                                        then  parNeedApp _appFunNm
                                                        else  (ParNotNeeded,_lhsIparNeedL)) of
                                                  { __tup3 ->
                                                  (case (__tup3) of
                                                   { (_parNeed,_) ->
                                                   (case (_parNeed) of
                                                    { _funcOparNeed ->
                                                    (case (_lhsIisRow) of
                                                     { _funcOisRow ->
                                                     (case (False) of
                                                      { _isAtTop ->
                                                      (case (_isAtTop) of
                                                       { _funcOisAtTop ->
                                                       (case (_lhsIappSpinePos + 1) of
                                                        { _funcOappSpinePos ->
                                                        (case (__tup3) of
                                                         { (_,_argsParNeedL) ->
                                                         (case (hdAndTl _argsParNeedL) of
                                                          { __tup4 ->
                                                          (case (__tup4) of
                                                           { (_,_funcOparNeedL) ->
                                                           (case (func_1 _funcOappSpinePos _funcOcfg _funcOisAtTop _funcOisRow _funcOparNeed _funcOparNeedL _funcOpositionalFldNmL _funcOtnMap _funcOtnUniq _funcOtyCtxt ) of
                                                            { ( _funcIappArgIsEmptyImplsL,_funcIappArgPPL,_funcIappFunPP,_funcIisArrow,_funcIisEmptyImpls,_funcIisPred,_funcIpositionalFldNmL,_funcIpp,_funcIself,_funcItnMap,_funcItnUniq) ->
                                                                (case (_funcItnUniq) of
                                                                 { _argOtnUniq ->
                                                                 (case (_funcItnMap) of
                                                                  { _argOtnMap ->
                                                                  (case (_funcIpositionalFldNmL) of
                                                                   { _argOpositionalFldNmL ->
                                                                   (case (_lhsIparNeedL) of
                                                                    { _argOparNeedL ->
                                                                    (case (_isAtTop) of
                                                                     { _argOisAtTop ->
                                                                     (case (0) of
                                                                      { _argOappSpinePos ->
                                                                      (case (hsnIsSum _funcIappFunNm) of
                                                                       { _appIsSum ->
                                                                       (case (_appIsRec || _appIsSum) of
                                                                        { _appIsRecOrSum ->
                                                                        (case (_isSpineRoot && _appIsRecOrSum) of
                                                                         { _argIsRow ->
                                                                         (case (_argIsRow) of
                                                                          { _argOisRow ->
                                                                          (case (__tup4) of
                                                                           { (_argOparNeed,_) ->
                                                                           (case (arg_1 _argOappSpinePos _argOcfg _argOisAtTop _argOisRow _argOparNeed _argOparNeedL _argOpositionalFldNmL _argOtnMap _argOtnUniq _argOtyCtxt ) of
                                                                            { ( _argIappArgIsEmptyImplsL,_argIappArgPPL,_argIappFunPP,_argIisArrow,_argIisEmptyImpls,_argIisPred,_argIpositionalFldNmL,_argIpp,_argIself,_argItnMap,_argItnUniq) ->
                                                                                (case (_funcIappArgIsEmptyImplsL ++ [_argIisEmptyImpls]) of
                                                                                 { _appArgIsEmptyImplsL ->
                                                                                 (case (_appArgIsEmptyImplsL) of
                                                                                  { _lhsOappArgIsEmptyImplsL ->
                                                                                  (case (cfgPPTyLhsSafe _lhsIcfg "|") of
                                                                                   { _recSep ->
                                                                                   (case (if cfgPPTyFollowAST _lhsIcfg
                                                                                          then (_funcIappFunPP,_funcIappArgPPL ++ [_argIpp])
                                                                                          else mkExtAppPP' _recSep
                                                                                                           (_appFunNm,_funcIappFunPP,_funcIappArgPPL)
                                                                                                           (_argIappFunNm,_argIappFunPP,_argIappArgPPL,_argIpp)) of
                                                                                    { __tup2 ->
                                                                                    (case (__tup2) of
                                                                                     { (_,_appArgPPL) ->
                                                                                     (case (_appArgPPL) of
                                                                                      { _lhsOappArgPPL ->
                                                                                      (case (__tup2) of
                                                                                       { (_appFunPP,_) ->
                                                                                       (case (_appFunPP) of
                                                                                        { _lhsOappFunPP ->
                                                                                        (case (_appIsArrow && _isSpineRoot) of
                                                                                         { _isArrowRoot ->
                                                                                         (case (_isArrowRoot) of
                                                                                          { _lhsOisArrow ->
                                                                                          (case (False) of
                                                                                           { _lhsOisEmptyImpls ->
                                                                                           (case (_appIsArrow && _lhsIappSpinePos == 1) of
                                                                                            { _isArrowArg ->
                                                                                            (case (if _isArrowArg then _argIisPred else False) of
                                                                                             { _lhsOisPred ->
                                                                                             (case (_lhsIpositionalFldNmL) of
                                                                                              { _lhsOpositionalFldNmL ->
                                                                                              (case (if cfgPPTyFollowAST _lhsIcfg then ppParens _funcIpp >#< ppParens _argIpp else _funcIpp >#< _argIpp) of
                                                                                               { _ppDflt ->
                                                                                               (case (ppAppTop' (_appFunNm,_appFunPP) _appArgPPL _appArgIsEmptyImplsL _ppDflt) of
                                                                                                { _ppAppTop ->
                                                                                                (case (hv ([ppParens _appFunPP] ++ map ((" " >|<) . ppParens) _appArgPPL)) of
                                                                                                 { _ppAST ->
                                                                                                 (case (ppParNeed  _parNeed _lhsIparNeed _ppAppTop) of
                                                                                                  { _ppNice ->
                                                                                                  (case (if _isSpineRoot
                                                                                                         then if cfgPPTyFollowAST _lhsIcfg
                                                                                                              then _ppAST
                                                                                                              else _ppNice
                                                                                                         else _ppDflt) of
                                                                                                   { _pp ->
                                                                                                   (case (_pp) of
                                                                                                    { _lhsOpp ->
                                                                                                    (case (Ty_App _funcIself _argIself) of
                                                                                                     { _self ->
                                                                                                     (case (_self) of
                                                                                                      { _lhsOself ->
                                                                                                      (case (_argItnMap) of
                                                                                                       { _lhsOtnMap ->
                                                                                                       (case (_argItnUniq) of
                                                                                                        { _lhsOtnUniq ->
                                                                                                        ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_App_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (case (nm_) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Con_1 :: T_Ty_1 
                 sem_Ty_Con_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (if cfgPPTyCtxt _lhsIcfg == TyCtxt_Pred && hsnIsArrow nm_
                                   then cfgPPTyPPCon _lhsIcfg _lhsIcfg hsnPrArrow
                                   else cfgPPTyPPCon _lhsIcfg _lhsIcfg nm_) of
                             { _pp ->
                             (case (_pp) of
                              { _lhsOappFunPP ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (False) of
                                { _lhsOisEmptyImpls ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (_lhsIpositionalFldNmL) of
                                  { _lhsOpositionalFldNmL ->
                                  (case (_pp) of
                                   { _lhsOpp ->
                                   (case (Ty_Con nm_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_lhsItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_lhsItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Con_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Dbg_1 :: T_Ty_1 
                 sem_Ty_Dbg_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case ("DBG<<<" >#< info_ >#< ">>>") of
                             { _pp ->
                             (case (_pp) of
                              { _lhsOappFunPP ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (False) of
                                { _lhsOisEmptyImpls ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (_lhsIpositionalFldNmL) of
                                  { _lhsOpositionalFldNmL ->
                                  (case (_pp) of
                                   { _lhsOpp ->
                                   (case (Ty_Dbg info_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     (case (_lhsItnMap) of
                                      { _lhsOtnMap ->
                                      (case (_lhsItnUniq) of
                                       { _lhsOtnUniq ->
                                       ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Dbg_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (case (ty_ ) of
     { ( _tyIappFunNm,ty_1) ->
         (case (_tyIappFunNm) of
          { _appFunNm ->
          (case (_appFunNm) of
           { _lhsOappFunNm ->
           (case ((let sem_Ty_Ext_1 :: T_Ty_1 
                       sem_Ty_Ext_1  =
                           (\ _lhsIappSpinePos
                              _lhsIcfg
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIparNeed
                              _lhsIparNeedL
                              _lhsIpositionalFldNmL
                              _lhsItnMap
                              _lhsItnUniq
                              _lhsItyCtxt ->
                                (case ([]) of
                                 { _lhsOappArgIsEmptyImplsL ->
                                 (case (_lhsItnUniq) of
                                  { _tyOtnUniq ->
                                  (case (TyQuCtxtOther) of
                                   { _tyCtxt ->
                                   (case (_tyCtxt) of
                                    { _tyOtyCtxt ->
                                    (case (_lhsItnMap) of
                                     { _tyOtnMap ->
                                     (case (_lhsIpositionalFldNmL) of
                                      { _tyOpositionalFldNmL ->
                                      (case (_lhsIparNeedL) of
                                       { _tyOparNeedL ->
                                       (case (False) of
                                        { _isRow ->
                                        (case (_isRow) of
                                         { _tyOisRow ->
                                         (case (False) of
                                          { _isAtTop ->
                                          (case (_isAtTop) of
                                           { _tyOisAtTop ->
                                           (case (_lhsIcfg) of
                                            { _tyOcfg ->
                                            (case (_lhsIappSpinePos + 1) of
                                             { _tyOappSpinePos ->
                                             (case (ParNotNeeded) of
                                              { _tyOparNeed ->
                                              (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                                               { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                                   (case (_tyItnUniq) of
                                                    { _extTyOtnUniq ->
                                                    (case (_tyItnMap) of
                                                     { _extTyOtnMap ->
                                                     (case (_lhsIparNeedL) of
                                                      { _extTyOparNeedL ->
                                                      (case (_lhsIcfg) of
                                                       { _extTyOcfg ->
                                                       (case (0) of
                                                        { _extTyOappSpinePos ->
                                                        (case (ParNotNeeded) of
                                                         { _extTyOparNeed ->
                                                         (case (positionalFldNames) of
                                                          { _extTyOpositionalFldNmL ->
                                                          (case (hdAndTl _tyIpositionalFldNmL) of
                                                           { __tup6 ->
                                                           (case (__tup6) of
                                                            { (_positionalNm,_) ->
                                                            (case (extTy_ ) of
                                                             { ( _extTyIappFunNm,extTy_1) ->
                                                                 (case (_tyCtxt) of
                                                                  { _extTyOtyCtxt ->
                                                                  (case (_isRow) of
                                                                   { _extTyOisRow ->
                                                                   (case (_isAtTop) of
                                                                    { _extTyOisAtTop ->
                                                                    (case (extTy_1 _extTyOappSpinePos _extTyOcfg _extTyOisAtTop _extTyOisRow _extTyOparNeed _extTyOparNeedL _extTyOpositionalFldNmL _extTyOtnMap _extTyOtnUniq _extTyOtyCtxt ) of
                                                                     { ( _extTyIappArgIsEmptyImplsL,_extTyIappArgPPL,_extTyIappFunPP,_extTyIisArrow,_extTyIisEmptyImpls,_extTyIisPred,_extTyIpositionalFldNmL,_extTyIpp,_extTyIself,_extTyItnMap,_extTyItnUniq) ->
                                                                         (case (let pn = if cfgPPTyFollowAST _lhsIcfg then Nothing else Just _positionalNm
                                                                                in  ppFld "::" pn nm_ (cfgPPTyPPHsn _lhsIcfg nm_) _extTyIpp) of
                                                                          { _fldPP ->
                                                                          (case (_tyIappArgPPL ++ [_fldPP]) of
                                                                           { _appArgPPL ->
                                                                           (case (_appArgPPL) of
                                                                            { _lhsOappArgPPL ->
                                                                            (case (_tyIappFunPP) of
                                                                             { _appFunPP ->
                                                                             (case (_appFunPP) of
                                                                              { _lhsOappFunPP ->
                                                                              (case (False) of
                                                                               { _lhsOisArrow ->
                                                                               (case (False) of
                                                                                { _lhsOisEmptyImpls ->
                                                                                (case (False) of
                                                                                 { _lhsOisPred ->
                                                                                 (case (__tup6) of
                                                                                  { (_,_lhsOpositionalFldNmL) ->
                                                                                  (case (_lhsIappSpinePos == 0) of
                                                                                   { _isSpineRoot ->
                                                                                   (case (ppExts _lhsIcfg _appFunNm _appFunPP _appArgPPL) of
                                                                                    { __tup5 ->
                                                                                    (case (__tup5) of
                                                                                     { (_,_ppCfg) ->
                                                                                     (case (__tup5) of
                                                                                      { (_ppNice,_) ->
                                                                                      (case (if _isSpineRoot then _ppCfg else _ppNice) of
                                                                                       { _pp ->
                                                                                       (case (_pp) of
                                                                                        { _lhsOpp ->
                                                                                        (case (Ty_Ext _tyIself nm_ _extTyIself) of
                                                                                         { _self ->
                                                                                         (case (_self) of
                                                                                          { _lhsOself ->
                                                                                          (case (_extTyItnMap) of
                                                                                           { _lhsOtnMap ->
                                                                                           (case (_extTyItnUniq) of
                                                                                            { _lhsOtnUniq ->
                                                                                            ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_Ext_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Impls_1 :: T_Ty_1 
                 sem_Ty_Impls_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (_lhsItnUniq) of
                             { _implsOtnUniq ->
                             (case (_lhsItnMap) of
                              { _implsOtnMap ->
                              (case (_lhsIcfg) of
                               { _implsOcfg ->
                               (case (if cfgPPTyCtxt _lhsIcfg == TyCtxt_Pred then id else (\pp -> hsnOImpl >#< pp >#< hsnCImpl)) of
                                { _wrapPP ->
                                (case (TyQuCtxtOther) of
                                 { _tyCtxt ->
                                 (case (_tyCtxt) of
                                  { _implsOtyCtxt ->
                                  (case (False) of
                                   { _isRow ->
                                   (case (_isRow) of
                                    { _implsOisRow ->
                                    (case (False) of
                                     { _isAtTop ->
                                     (case (_isAtTop) of
                                      { _implsOisAtTop ->
                                      (case (impls_ _implsOcfg _implsOisAtTop _implsOisRow _implsOtnMap _implsOtnUniq _implsOtyCtxt ) of
                                       { ( _implsIisEmptyImpls,_implsIpp,_implsIself,_implsItnMap,_implsItnUniq) ->
                                           (case (cfgPPTyPPImplsAppWrap _lhsIcfg _wrapPP _implsIpp) of
                                            { _pp ->
                                            (case (_pp) of
                                             { _lhsOappFunPP ->
                                             (case (False) of
                                              { _lhsOisArrow ->
                                              (case (_implsIisEmptyImpls) of
                                               { _lhsOisEmptyImpls ->
                                               (case (True) of
                                                { _lhsOisPred ->
                                                (case (_lhsIpositionalFldNmL) of
                                                 { _lhsOpositionalFldNmL ->
                                                 (case (_pp) of
                                                  { _lhsOpp ->
                                                  (case (Ty_Impls _implsIself) of
                                                   { _self ->
                                                   (case (_self) of
                                                    { _lhsOself ->
                                                    (case (_implsItnMap) of
                                                     { _lhsOtnMap ->
                                                     (case (_implsItnUniq) of
                                                      { _lhsOtnUniq ->
                                                      ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Impls_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Lam_1 :: T_Ty_1 
                 sem_Ty_Lam_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (cfgPPTyPPVar _lhsIcfg _lhsIcfg tv_ _lhsItnUniq) of
                             { _tnPP ->
                             (case (tnMapInsert tv_ _tnPP _lhsItnMap) of
                              { _tnMap ->
                              (case (_tnMap) of
                               { _tyOtnMap ->
                               (case (_lhsIpositionalFldNmL) of
                                { _tyOpositionalFldNmL ->
                                (case (_lhsIparNeedL) of
                                 { _tyOparNeedL ->
                                 (case (ParNotNeeded) of
                                  { _parNeed ->
                                  (case (_parNeed) of
                                   { _tyOparNeed ->
                                   (case (_lhsIcfg) of
                                    { _tyOcfg ->
                                    (case (_lhsIappSpinePos) of
                                     { _tyOappSpinePos ->
                                     (case (_lhsItnUniq + 1) of
                                      { _tnUniq ->
                                      (case (_tnUniq) of
                                       { _tyOtnUniq ->
                                       (case (ty_ ) of
                                        { ( _tyIappFunNm,ty_1) ->
                                            (case (TyQuCtxtOther) of
                                             { _tyCtxt ->
                                             (case (_tyCtxt) of
                                              { _tyOtyCtxt ->
                                              (case (False) of
                                               { _isRow ->
                                               (case (_isRow) of
                                                { _tyOisRow ->
                                                (case (False) of
                                                 { _isAtTop ->
                                                 (case (_isAtTop) of
                                                  { _tyOisAtTop ->
                                                  (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                                                   { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                                       (case (ppParNeed  _parNeed _lhsIparNeed
                                                                         (cfgppAside _lhsIcfg ("\\" >|< _tnPP) ("->" >#< _tyIpp))) of
                                                        { _pp ->
                                                        (case (_pp) of
                                                         { _lhsOappFunPP ->
                                                         (case (False) of
                                                          { _lhsOisArrow ->
                                                          (case (False) of
                                                           { _lhsOisEmptyImpls ->
                                                           (case (False) of
                                                            { _lhsOisPred ->
                                                            (case (_lhsIpositionalFldNmL) of
                                                             { _lhsOpositionalFldNmL ->
                                                             (case (_pp) of
                                                              { _lhsOpp ->
                                                              (case (Ty_Lam tv_ _tyIself) of
                                                               { _self ->
                                                               (case (_self) of
                                                                { _lhsOself ->
                                                                (case (Map.filterWithKey (\v _ -> v /= tv_) _tyItnMap) of
                                                                 { _lhsOtnMap ->
                                                                 (case (_tnUniq) of
                                                                  { _lhsOtnUniq ->
                                                                  ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Lam_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Pred_1 :: T_Ty_1 
                 sem_Ty_Pred_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (_lhsItnUniq) of
                             { _prOtnUniq ->
                             (case (_lhsItnMap) of
                              { _prOtnMap ->
                              (case (_lhsIcfg) of
                               { _prOcfg ->
                               (case (if cfgPPTyCtxt _lhsIcfg == TyCtxt_Pred then id else (\pp -> hsnOImpl >#< pp >#< hsnCImpl)) of
                                { _wrapPP ->
                                (case (TyQuCtxtOther) of
                                 { _tyCtxt ->
                                 (case (_tyCtxt) of
                                  { _prOtyCtxt ->
                                  (case (False) of
                                   { _isRow ->
                                   (case (_isRow) of
                                    { _prOisRow ->
                                    (case (False) of
                                     { _isAtTop ->
                                     (case (_isAtTop) of
                                      { _prOisAtTop ->
                                      (case (pr_ _prOcfg _prOisAtTop _prOisRow _prOtnMap _prOtnUniq _prOtyCtxt ) of
                                       { ( _prIpp,_prIself,_prItnMap,_prItnUniq) ->
                                           (case (cfgPPTyPPImplsAppWrap _lhsIcfg _wrapPP _prIpp) of
                                            { _pp ->
                                            (case (_pp) of
                                             { _lhsOappFunPP ->
                                             (case (False) of
                                              { _lhsOisArrow ->
                                              (case (False) of
                                               { _lhsOisEmptyImpls ->
                                               (case (True) of
                                                { _lhsOisPred ->
                                                (case (_lhsIpositionalFldNmL) of
                                                 { _lhsOpositionalFldNmL ->
                                                 (case (_pp) of
                                                  { _lhsOpp ->
                                                  (case (Ty_Pred _prIself) of
                                                   { _self ->
                                                   (case (_self) of
                                                    { _lhsOself ->
                                                    (case (_prItnMap) of
                                                     { _lhsOtnMap ->
                                                     (case (_prItnUniq) of
                                                      { _lhsOtnUniq ->
                                                      ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Pred_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_TBind_1 :: T_Ty_1 
                 sem_Ty_TBind_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (l1_) of
                             { l1'_val_ ->
                             (case ((sem_Ty l1'_val_ )) of
                              { l1'_inst_ ->
                              (case (_lhsItnUniq + 1) of
                               { _tnUniq ->
                               (case (_tnUniq) of
                                { _l1'OtnUniq ->
                                (case (cfgPPTyPPVar _lhsIcfg _lhsIcfg tv_ _lhsItnUniq) of
                                 { _tnPP ->
                                 (case (tnMapInsert tv_ _tnPP _lhsItnMap) of
                                  { _tnMap ->
                                  (case (_tnMap) of
                                   { _l1'OtnMap ->
                                   (case (_lhsIpositionalFldNmL) of
                                    { _tyOpositionalFldNmL ->
                                    (case (ty_ ) of
                                     { ( _tyIappFunNm,ty_1) ->
                                         (case (TyQuCtxtOther) of
                                          { _tyCtxt ->
                                          (case (_tyCtxt) of
                                           { _tyOtyCtxt ->
                                           (case (_tnUniq) of
                                            { _tyOtnUniq ->
                                            (case (_tnMap) of
                                             { _tyOtnMap ->
                                             (case (_lhsIparNeedL) of
                                              { _tyOparNeedL ->
                                              (case (ParNotNeeded) of
                                               { _parNeed ->
                                               (case (_parNeed) of
                                                { _tyOparNeed ->
                                                (case (False) of
                                                 { _isRow ->
                                                 (case (_isRow) of
                                                  { _tyOisRow ->
                                                  (case (False) of
                                                   { _isAtTop ->
                                                   (case (_isAtTop) of
                                                    { _tyOisAtTop ->
                                                    (case (_lhsIcfg) of
                                                     { _tyOcfg ->
                                                     (case (0) of
                                                      { _tyOappSpinePos ->
                                                      (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                                                       { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                                                           (case (_tyIpositionalFldNmL) of
                                                            { _l1'OpositionalFldNmL ->
                                                            (case (_lhsIparNeedL) of
                                                             { _l1'OparNeedL ->
                                                             (case (_parNeed) of
                                                              { _l1'OparNeed ->
                                                              (case (_lhsIcfg) of
                                                               { _l1'Ocfg ->
                                                               (case (_lhsIappSpinePos) of
                                                                { _l1'OappSpinePos ->
                                                                (case (l1'_inst_ ) of
                                                                 { ( _l1'IappFunNm,l1'_1) ->
                                                                     (case (_tyCtxt) of
                                                                      { _l1'OtyCtxt ->
                                                                      (case (_isRow) of
                                                                       { _l1'OisRow ->
                                                                       (case (_isAtTop) of
                                                                        { _l1'OisAtTop ->
                                                                        (case (l1'_1 _l1'OappSpinePos _l1'Ocfg _l1'OisAtTop _l1'OisRow _l1'OparNeed _l1'OparNeedL _l1'OpositionalFldNmL _l1'OtnMap _l1'OtnUniq _l1'OtyCtxt ) of
                                                                         { ( _l1'IappArgIsEmptyImplsL,_l1'IappArgPPL,_l1'IappFunPP,_l1'IisArrow,_l1'IisEmptyImpls,_l1'IisPred,_l1'IpositionalFldNmL,_l1'Ipp,_l1'Iself,_l1'ItnMap,_l1'ItnUniq) ->
                                                                             (case (_l1'Ipp) of
                                                                              { _ppL1 ->
                                                                              (case (if maybe False (== hsnKindStar) $ tyMbCon l1_ then empty else ppParens _ppL1) of
                                                                               { _ppTvPost ->
                                                                               (case (_tnUniq) of
                                                                                { _quOtnUniq ->
                                                                                (case (_tnMap) of
                                                                                 { _quOtnMap ->
                                                                                 (case (_lhsIcfg) of
                                                                                  { _quOcfg ->
                                                                                  (case (qu_ _quOcfg _quOtnMap _quOtnUniq ) of
                                                                                   { ( _quIpp,_quIself,_quItnMap,_quItnUniq) ->
                                                                                       (case (_quIpp) of
                                                                                        { _ppQu ->
                                                                                        (case (empty) of
                                                                                         { _ppTvPre ->
                                                                                         (case (_tnPP) of
                                                                                          { _ppTv ->
                                                                                          (case (ppParNeed  _parNeed _lhsIparNeed
                                                                                                            (cfgppAside _lhsIcfg (_ppQu >#< _ppTvPre >|< _ppTv >|< _ppTvPost) ("." >#< _tyIpp))) of
                                                                                           { _pp ->
                                                                                           (case (_pp) of
                                                                                            { _lhsOappFunPP ->
                                                                                            (case (False) of
                                                                                             { _lhsOisArrow ->
                                                                                             (case (False) of
                                                                                              { _lhsOisEmptyImpls ->
                                                                                              (case (False) of
                                                                                               { _lhsOisPred ->
                                                                                               (case (_lhsIpositionalFldNmL) of
                                                                                                { _lhsOpositionalFldNmL ->
                                                                                                (case (_pp) of
                                                                                                 { _lhsOpp ->
                                                                                                 (case (Ty_TBind _quIself tv_ l1_ _tyIself) of
                                                                                                  { _self ->
                                                                                                  (case (_self) of
                                                                                                   { _lhsOself ->
                                                                                                   (case (Map.filterWithKey (\v _ -> v /= tv_) _tyItnMap) of
                                                                                                    { _lhsOtnMap ->
                                                                                                    (case (_tnUniq) of
                                                                                                     { _lhsOtnUniq ->
                                                                                                     ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_TBind_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Var_1 :: T_Ty_1 
                 sem_Ty_Var_1  =
                     (\ _lhsIappSpinePos
                        _lhsIcfg
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIparNeed
                        _lhsIparNeedL
                        _lhsIpositionalFldNmL
                        _lhsItnMap
                        _lhsItnUniq
                        _lhsItyCtxt ->
                          (case ([]) of
                           { _lhsOappArgIsEmptyImplsL ->
                           (case ([]) of
                            { _lhsOappArgPPL ->
                            (case (_lhsItnUniq) of
                             { _categOtnUniq ->
                             (case (_lhsItnMap) of
                              { _categOtnMap ->
                              (case (_lhsIcfg) of
                               { _categOcfg ->
                               (case (categ_ _categOcfg _categOtnMap _categOtnUniq ) of
                                { ( _categIpp,_categIself,_categItnMap,_categItnUniq) ->
                                    (case (maybe (cfgPPTyPPVarDflt _lhsIcfg _lhsIcfg (tvCategPrefix _categIself) tv_) id (tnLookupPP tv_ _lhsItnMap)) of
                                     { _pp ->
                                     (case (_pp) of
                                      { _lhsOappFunPP ->
                                      (case (False) of
                                       { _lhsOisArrow ->
                                       (case (False) of
                                        { _lhsOisEmptyImpls ->
                                        (case (False) of
                                         { _lhsOisPred ->
                                         (case (_lhsIpositionalFldNmL) of
                                          { _lhsOpositionalFldNmL ->
                                          (case (_pp) of
                                           { _lhsOpp ->
                                           (case (Ty_Var tv_ _categIself) of
                                            { _self ->
                                            (case (_self) of
                                             { _lhsOself ->
                                             (case (_categItnMap) of
                                              { _lhsOtnMap ->
                                              (case (_categItnUniq) of
                                               { _lhsOtnUniq ->
                                               ( _lhsOappArgIsEmptyImplsL,_lhsOappArgPPL,_lhsOappFunPP,_lhsOisArrow,_lhsOisEmptyImpls,_lhsOisPred,_lhsOpositionalFldNmL,_lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Var_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cfg                  : CfgPPTy
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child ty             : Ty 
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _ty )  =
    (sem_TyAGItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_TyAGItf  = CfgPPTy ->
                  ( PP_Doc)
data Inh_TyAGItf  = Inh_TyAGItf {cfg_Inh_TyAGItf :: !(CfgPPTy)}
data Syn_TyAGItf  = Syn_TyAGItf {pp_Syn_TyAGItf :: !(PP_Doc)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIcfg )  =
    (let ( _lhsOpp) = sem _lhsIcfg 
     in  (Syn_TyAGItf _lhsOpp ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIcfg ->
         (case (_lhsIcfg) of
          { _tyOcfg ->
          (case (0) of
           { _tyOappSpinePos ->
           (case ([]) of
            { _tyOparNeedL ->
            (case (ParNotNeeded) of
             { _tyOparNeed ->
             (case (positionalFldNames) of
              { _tyOpositionalFldNmL ->
              (case (Map.empty) of
               { _tyOtnMap ->
               (case (0) of
                { _tyOtnUniq ->
                (case (ty_ ) of
                 { ( _tyIappFunNm,ty_1) ->
                     (case (TyQuCtxtOnTop) of
                      { _tyOtyCtxt ->
                      (case (False) of
                       { _tyOisRow ->
                       (case (True) of
                        { _tyOisAtTop ->
                        (case (ty_1 _tyOappSpinePos _tyOcfg _tyOisAtTop _tyOisRow _tyOparNeed _tyOparNeedL _tyOpositionalFldNmL _tyOtnMap _tyOtnUniq _tyOtyCtxt ) of
                         { ( _tyIappArgIsEmptyImplsL,_tyIappArgPPL,_tyIappFunPP,_tyIisArrow,_tyIisEmptyImpls,_tyIisPred,_tyIpositionalFldNmL,_tyIpp,_tyIself,_tyItnMap,_tyItnUniq) ->
                             (case (_tyIpp) of
                              { _lhsOpp ->
                              ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cfg                  : CfgPPTy
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local pp          : _
            local self        : _
      alternative Mono:
         visit 0:
            local pp          : _
            local self        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local pp          : _
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
type T_TyAnn  = CfgPPTy ->
                TVarNameMap ->
                Int ->
                ( PP_Doc,TyAnn ,TVarNameMap,Int)
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (empty) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           (case (TyAnn_Empty) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (pp "MONO ") of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           (case (TyAnn_Mono) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case ("@" >|< show s_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           (case (TyAnn_Strictness s_) of
            { _self ->
            (case (_self) of
             { _lhsOself ->
             (case (_lhsItnMap) of
              { _lhsOtnMap ->
              (case (_lhsItnUniq) of
               { _lhsOtnUniq ->
               ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }) }))
-- TyQu --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cfg                  : CfgPPTy
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local self        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local self        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
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
type T_TyQu  = CfgPPTy ->
               TVarNameMap ->
               Int ->
               ( PP_Doc,TyQu ,TVarNameMap,Int)
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (TyQu_Exists mlev_) of
          { _self ->
          (case (text (showTyQu _self)) of
           { _lhsOpp ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (TyQu_Forall mlev_) of
          { _self ->
          (case (text (showTyQu _self)) of
           { _lhsOpp ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (TyQu_Plain mlev_) of
          { _self ->
          (case (text (showTyQu _self)) of
           { _lhsOpp ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cfg                  : CfgPPTy
      chained attributes:
         tnMap                : TVarNameMap
         tnUniq               : Int
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local self        : _
      alternative Meta:
         visit 0:
            local self        : _
      alternative Plain:
         visit 0:
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
type T_TyVarCateg  = CfgPPTy ->
                     TVarNameMap ->
                     Int ->
                     ( PP_Doc,TyVarCateg ,TVarNameMap,Int)
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (empty) of
          { _lhsOpp ->
          (case (TyVarCateg_Fixed) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (empty) of
          { _lhsOpp ->
          (case (TyVarCateg_Meta) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (\ _lhsIcfg
       _lhsItnMap
       _lhsItnUniq ->
         (case (empty) of
          { _lhsOpp ->
          (case (TyVarCateg_Plain) of
           { _self ->
           (case (_self) of
            { _lhsOself ->
            (case (_lhsItnMap) of
             { _lhsOtnMap ->
             (case (_lhsItnUniq) of
              { _lhsOtnUniq ->
              ( _lhsOpp,_lhsOself,_lhsOtnMap,_lhsOtnUniq) }) }) }) }) }))