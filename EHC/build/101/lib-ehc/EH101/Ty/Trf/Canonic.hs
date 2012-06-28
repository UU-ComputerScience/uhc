

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/Canonic.ag)
module EH101.Ty.Trf.Canonic(TyCanonicOut
, TyCanonicOpts (..), emptyTyCanonicOpts
, tyCanonic
, tyCanonicFFI'
, tyCanonicFFI
, predCanonic
, tyCanonic'
, predCanonic') where

import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.FitsInCommon2
import Data.Maybe
import qualified Data.Set as Set
import EH101.Opts
import EH101.Ty.Trf.BetaReduce
import EH.Util.Utils
import EH101.Gam.Full
import EH.Util.Debug
import EH.Util.Pretty
import EH101.Base.Builtin
import Control.Applicative


















type TyCanonicOut' x = TyBetaRedOut' x
type TyCanonicOut    = TyBetaRedOut

mkCanonicOut x y = emptyTyBetaRedOut {tbroutRes = x, tbroutVarMp = y}

mkDfltTyCanonicOut :: x -> TyCanonicOut' x
mkDfltTyCanonicOut = mkDfltTyBetaRedOut



data TyCanonicOpts
  = TyCanonicOpts
      { tcoTyBetaRedFullMb  :: Ty -> Maybe TyCanonicOut
      }

emptyTyCanonicOpts :: TyCanonicOpts
emptyTyCanonicOpts
  = TyCanonicOpts
      { tcoTyBetaRedFullMb  = \t -> Nothing
      }



tcoTyBetaRedFull :: TyCanonicOpts -> Ty -> TyCanonicOut
tcoTyBetaRedFull tco ty = maybe (mkDfltTyCanonicOut ty) id $ tcoTyBetaRedFullMb tco ty



tyCanonicMb' :: TyCanonicOpts -> Ty -> Maybe TyCanonicOut
tyCanonicMb' opts ty
  =  let  t =  wrap_TyAGItf
                 (sem_TyAGItf (TyAGItf_AGItf ty))
                 (Inh_TyAGItf
                   { opts_Inh_TyAGItf = opts
                   }
                 )
     in   if isReplaced_Syn_TyAGItf t then Just (mkCanonicOut (repl_Syn_TyAGItf t) (varMp_Syn_TyAGItf t)) else Nothing

tyCanonic' :: TyCanonicOpts -> Ty -> TyCanonicOut
tyCanonic' opts ty = maybe (mkDfltTyCanonicOut ty) id $ tyCanonicMb' opts ty



predCanonic' :: TyCanonicOpts -> Pred -> (Pred,VarMp)
predCanonic' opts pr
  = case tyCanonicMb' opts $ mkTyPr pr of
      -- Just (Ty_Pred pr',m) -> (pr', m)
      Just (r@(TyBetaRedOut {tbroutRes = Ty_Pred pr'})) -> (pr', tbroutVarMp r)
      _                    -> (pr, emptyVarMp)



tyCanonic
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> Ty
     -> Ty
tyCanonic fi
  = tbroutRes . tyCanonic' opts
  where opts = emptyTyCanonicOpts
                  {tcoTyBetaRedFullMb = tyBetaRedFullMb fi canonLkupTy (tyCanonicMb' opts)}



tyCanonicFFI'
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => Bool			-- expand newtypes
     -> TyBetaRedEnv gm
     -> Ty
     -> Ty
tyCanonicFFI' expNewtype renv
  = tbroutRes . tyCanonic' opts
  where opts = emptyTyCanonicOpts
                  {tcoTyBetaRedFullMb = tyBetaRedFullMb renv canonLkupTy' (tyCanonicMb' opts)}
        canonLkupTy' renv nm = -- tr "tyCanonicFFI" (pp nm) $
                             newtypeLamLkup <|> canonLkupTy renv nm
                     where newtypeLamLkup | not expNewtype = Nothing
                                          | ehcOptBuiltin ehcopts ehbnIO == nm
                                                           = Nothing
                                          | otherwise      = fmap mkDfltTyCanonicOut (dgiMbNewtype $? dataGamLookup nm $ feDataGam fe)
        fe      = fiEnv fi
        fi      = tbredFI renv
        ehcopts = feEHCOpts fe



tyCanonicFFI
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm
     -> Ty
     -> Ty
tyCanonicFFI = tyCanonicFFI' False



predCanonic
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> Pred
     -> (Pred,VarMp)
predCanonic renv
  = predCanonic' opts
  where opts = emptyTyCanonicOpts
                  {tcoTyBetaRedFullMb = tyBetaRedFullMb renv canonLkupTy (tyCanonicMb' opts)}



canonLkupTy :: TyBetaRedLkup gm
canonLkupTy = betaRedTyLookup



type MbPredL = Maybe [Pred]

mbPredLCmb :: MbPredL -> MbPredL -> MbPredL
mbPredLCmb Nothing Nothing = Nothing
mbPredLCmb m1      m2      = Just $ concat $ maybeToList m1 ++ maybeToList m2



extr :: TyCanonicOut -> (Ty,VarMp)
extr o = (tbroutRes o, tbroutVarMp o)



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
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TyCanonicOpts
         tyCtxt               : TyQuCtxt
      chained attribute:
         varMp                : VarMp
      synthesized attributes:
         isReplaced           : Bool
         mbPredL              : MbPredL
         repl                 : SELF 
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
            local repl        : _
            local self        : _
      alternative Nil:
         visit 0:
            local repl        : _
            local self        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 0:
            local repl        : _
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
                Bool ->
                TyCanonicOpts ->
                TyQuCtxt ->
                VarMp ->
                ( Bool,MbPredL,Impls ,Impls ,VarMp)
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _prOopts | _prOopts `seq` (True) ->
           (case (_lhsIvarMp) of
            { _prOvarMp | _prOvarMp `seq` (True) ->
            (case (_lhsItyCtxt) of
             { _prOtyCtxt | _prOtyCtxt `seq` (True) ->
             (case (_lhsIisRow) of
              { _prOisRow | _prOisRow `seq` (True) ->
              (case (_lhsIisAtTop) of
               { _prOisAtTop | _prOisAtTop `seq` (True) ->
               (case (pr_ _prOisAtTop _prOisRow _prOopts _prOtyCtxt _prOvarMp ) of
                { ( _prIisReplaced,_prImbPredL,_prIrepl,_prIself,_prIvarMp) | True ->
                    (case (_prIvarMp) of
                     { _tlOvarMp | _tlOvarMp `seq` (True) ->
                     (case (_lhsItyCtxt) of
                      { _tlOtyCtxt | _tlOtyCtxt `seq` (True) ->
                      (case (_lhsIisRow) of
                       { _tlOisRow | _tlOisRow `seq` (True) ->
                       (case (_lhsIisAtTop) of
                        { _tlOisAtTop | _tlOisAtTop `seq` (True) ->
                        (case (tl_ _tlOisAtTop _tlOisRow _tlOopts _tlOtyCtxt _tlOvarMp ) of
                         { ( _tlIisReplaced,_tlImbPredL,_tlIrepl,_tlIself,_tlIvarMp) | True ->
                             (case (_prIisReplaced || _tlIisReplaced) of
                              { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                              (case (_prImbPredL `mbPredLCmb` _tlImbPredL) of
                               { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                               (case (Impls_Cons iv_ _prIrepl pv_ prange_ proveOccs_ _tlIrepl) of
                                { _repl | _repl `seq` (True) ->
                                (case (_repl) of
                                 { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                 (case (Impls_Cons iv_ _prIself pv_ prange_ proveOccs_ _tlIself) of
                                  { _self | _self `seq` (True) ->
                                  (case (_self) of
                                   { _lhsOself | _lhsOself `seq` (True) ->
                                   (case (_tlIvarMp) of
                                    { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                    ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (False) of
          { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
          (case (Nothing) of
           { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
           (case (Impls_Nil) of
            { _repl | _repl `seq` (True) ->
            (case (_repl) of
             { _lhsOrepl | _lhsOrepl `seq` (True) ->
             (case (Impls_Nil) of
              { _self | _self `seq` (True) ->
              (case (_self) of
               { _lhsOself | _lhsOself `seq` (True) ->
               (case (_lhsIvarMp) of
                { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (False) of
          { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
          (case (Just []) of
           { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
           (case (Impls_Tail iv_ proveOccs_) of
            { _repl | _repl `seq` (True) ->
            (case (_repl) of
             { _lhsOrepl | _lhsOrepl `seq` (True) ->
             (case (Impls_Tail iv_ proveOccs_) of
              { _self | _self `seq` (True) ->
              (case (_self) of
               { _lhsOself | _lhsOself `seq` (True) ->
               (case (varmpImplsUnit iv_ Impls_Nil `varUpd` _lhsIvarMp) of
                { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         isReplaced           : Bool
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local repl        : _
            local self        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
            local repl        : _
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
type T_Label  = ( Bool,Label ,Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (Label_Lab nm_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (Label_Lab nm_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (Label_Var lv_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (Label_Var lv_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
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
         isRow                : Bool
         opts                 : TyCanonicOpts
         tyCtxt               : TyQuCtxt
      chained attribute:
         varMp                : VarMp
      synthesized attributes:
         isReplaced           : Bool
         mbPredL              : MbPredL
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 0:
            local appSpinePos : _
            local repl        : _
            local self        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 0:
            local repl        : _
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
               Bool ->
               TyCanonicOpts ->
               TyQuCtxt ->
               VarMp ->
               ( Bool,MbPredL,Pred ,Pred ,VarMp)
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _resOopts | _resOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _argsOopts | _argsOopts `seq` (True) ->
           (case (_lhsIvarMp) of
            { _argsOvarMp | _argsOvarMp `seq` (True) ->
            (case (_lhsItyCtxt) of
             { _argsOtyCtxt | _argsOtyCtxt `seq` (True) ->
             (case (_lhsIisRow) of
              { _argsOisRow | _argsOisRow `seq` (True) ->
              (case (_lhsIisAtTop) of
               { _argsOisAtTop | _argsOisAtTop `seq` (True) ->
               (case (0) of
                { _appSpinePos | _appSpinePos `seq` (True) ->
                (case (_appSpinePos) of
                 { _argsOappSpinePos | _argsOappSpinePos `seq` (True) ->
                 (case (args_ _argsOappSpinePos _argsOisAtTop _argsOisRow _argsOopts _argsOtyCtxt _argsOvarMp ) of
                  { ( _argsIisReplaced,_argsIrepl,_argsIself,_argsIvarMp) | True ->
                      (case (_argsIvarMp) of
                       { _resOvarMp | _resOvarMp `seq` (True) ->
                       (case (_lhsItyCtxt) of
                        { _resOtyCtxt | _resOtyCtxt `seq` (True) ->
                        (case (_lhsIisRow) of
                         { _resOisRow | _resOisRow `seq` (True) ->
                         (case (_lhsIisAtTop) of
                          { _resOisAtTop | _resOisAtTop `seq` (True) ->
                          (case (res_ _resOisAtTop _resOisRow _resOopts _resOtyCtxt _resOvarMp ) of
                           { ( _resIisReplaced,_resImbPredL,_resIrepl,_resIself,_resIvarMp) | True ->
                               (case (_argsIisReplaced || _resIisReplaced) of
                                { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                (case (Pred_Arrow _argsIrepl _resIrepl) of
                                 { _repl | _repl `seq` (True) ->
                                 (case (Just [_repl]) of
                                  { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                  (case (_repl) of
                                   { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                   (case (Pred_Arrow _argsIself _resIself) of
                                    { _self | _self `seq` (True) ->
                                    (case (_self) of
                                     { _lhsOself | _lhsOself `seq` (True) ->
                                     (case (_resIvarMp) of
                                      { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                      ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tyOopts | _tyOopts `seq` (True) ->
          (case (0) of
           { _appSpinePos | _appSpinePos `seq` (True) ->
           (case (_appSpinePos) of
            { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
            (case (ty_ ) of
             { ( _tyIappFunNm,ty_1) | True ->
                 (case (_lhsIvarMp) of
                  { _tyOvarMp | _tyOvarMp `seq` (True) ->
                  (case (_lhsItyCtxt) of
                   { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                   (case (_lhsIisRow) of
                    { _tyOisRow | _tyOisRow `seq` (True) ->
                    (case (_lhsIisAtTop) of
                     { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                     (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                      { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                          (case (_tyIisReplaced) of
                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                           (case (Pred_Class _tyIrepl) of
                            { _repl | _repl `seq` (True) ->
                            (case (Just [_repl]) of
                             { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                             (case (_repl) of
                              { _lhsOrepl | _lhsOrepl `seq` (True) ->
                              (case (Pred_Class _tyIself) of
                               { _self | _self `seq` (True) ->
                               (case (_self) of
                                { _lhsOself | _lhsOself `seq` (True) ->
                                (case (_tyIvarMp) of
                                 { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                 ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tyROopts | _tyROopts `seq` (True) ->
          (case (0) of
           { _appSpinePos | _appSpinePos `seq` (True) ->
           (case (_appSpinePos) of
            { _tyROappSpinePos | _tyROappSpinePos `seq` (True) ->
            (case (_lhsIopts) of
             { _tyLOopts | _tyLOopts `seq` (True) ->
             (case (_appSpinePos) of
              { _tyLOappSpinePos | _tyLOappSpinePos `seq` (True) ->
              (case (tyR_ ) of
               { ( _tyRIappFunNm,tyR_1) | True ->
                   (case (_lhsIvarMp) of
                    { _tyLOvarMp | _tyLOvarMp `seq` (True) ->
                    (case (tyL_ ) of
                     { ( _tyLIappFunNm,tyL_1) | True ->
                         (case (_lhsItyCtxt) of
                          { _tyLOtyCtxt | _tyLOtyCtxt `seq` (True) ->
                          (case (_lhsIisRow) of
                           { _tyLOisRow | _tyLOisRow `seq` (True) ->
                           (case (_lhsIisAtTop) of
                            { _tyLOisAtTop | _tyLOisAtTop `seq` (True) ->
                            (case (tyL_1 _tyLOappSpinePos _tyLOisAtTop _tyLOisRow _tyLOopts _tyLOtyCtxt _tyLOvarMp ) of
                             { ( _tyLIappArgReplL,_tyLIisArrow,_tyLIisPred,_tyLIisReplaced,_tyLImbPredL,_tyLIrepl,_tyLIself,_tyLIvarMp) | True ->
                                 (case (_tyLIvarMp) of
                                  { _tyROvarMp | _tyROvarMp `seq` (True) ->
                                  (case (_lhsItyCtxt) of
                                   { _tyROtyCtxt | _tyROtyCtxt `seq` (True) ->
                                   (case (_lhsIisRow) of
                                    { _tyROisRow | _tyROisRow `seq` (True) ->
                                    (case (_lhsIisAtTop) of
                                     { _tyROisAtTop | _tyROisAtTop `seq` (True) ->
                                     (case (tyR_1 _tyROappSpinePos _tyROisAtTop _tyROisRow _tyROopts _tyROtyCtxt _tyROvarMp ) of
                                      { ( _tyRIappArgReplL,_tyRIisArrow,_tyRIisPred,_tyRIisReplaced,_tyRImbPredL,_tyRIrepl,_tyRIself,_tyRIvarMp) | True ->
                                          (case (_tyLIisReplaced || _tyRIisReplaced) of
                                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                           (case (Pred_Eq _tyLIrepl _tyRIrepl) of
                                            { _repl | _repl `seq` (True) ->
                                            (case (Just [_repl]) of
                                             { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                             (case (_repl) of
                                              { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                              (case (Pred_Eq _tyLIself _tyRIself) of
                                               { _self | _self `seq` (True) ->
                                               (case (_self) of
                                                { _lhsOself | _lhsOself `seq` (True) ->
                                                (case (_tyRIvarMp) of
                                                 { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                 ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tyOopts | _tyOopts `seq` (True) ->
          (case (0) of
           { _appSpinePos | _appSpinePos `seq` (True) ->
           (case (_appSpinePos) of
            { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
            (case (lab_ ) of
             { ( _labIisReplaced,_labIrepl,_labIself) | True ->
                 (case (ty_ ) of
                  { ( _tyIappFunNm,ty_1) | True ->
                      (case (_lhsIvarMp) of
                       { _tyOvarMp | _tyOvarMp `seq` (True) ->
                       (case (_lhsItyCtxt) of
                        { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                        (case (_lhsIisRow) of
                         { _tyOisRow | _tyOisRow `seq` (True) ->
                         (case (_lhsIisAtTop) of
                          { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                          (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                           { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                               (case (_tyIisReplaced || _labIisReplaced) of
                                { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                (case (Pred_Lacks _tyIrepl _labIrepl) of
                                 { _repl | _repl `seq` (True) ->
                                 (case (Just [_repl]) of
                                  { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                  (case (_repl) of
                                   { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                   (case (Pred_Lacks _tyIself _labIself) of
                                    { _self | _self `seq` (True) ->
                                    (case (_self) of
                                     { _lhsOself | _lhsOself `seq` (True) ->
                                     (case (_tyIvarMp) of
                                      { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                      ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tyOopts | _tyOopts `seq` (True) ->
          (case (0) of
           { _appSpinePos | _appSpinePos `seq` (True) ->
           (case (_appSpinePos) of
            { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
            (case (ty_ ) of
             { ( _tyIappFunNm,ty_1) | True ->
                 (case (_lhsIvarMp) of
                  { _tyOvarMp | _tyOvarMp `seq` (True) ->
                  (case (_lhsItyCtxt) of
                   { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                   (case (_lhsIisRow) of
                    { _tyOisRow | _tyOisRow `seq` (True) ->
                    (case (_lhsIisAtTop) of
                     { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                     (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                      { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                          (case (_tyIisReplaced) of
                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                           (case (Pred_Pred _tyIrepl) of
                            { _repl | _repl `seq` (True) ->
                            (case (Just [_repl]) of
                             { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                             (case (_repl) of
                              { _lhsOrepl | _lhsOrepl `seq` (True) ->
                              (case (Pred_Pred _tyIself) of
                               { _self | _self `seq` (True) ->
                               (case (_self) of
                                { _lhsOself | _lhsOself `seq` (True) ->
                                (case (_tyIvarMp) of
                                 { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                 ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _seqOopts | _seqOopts `seq` (True) ->
          (case (_lhsIvarMp) of
           { _seqOvarMp | _seqOvarMp `seq` (True) ->
           (case (_lhsItyCtxt) of
            { _seqOtyCtxt | _seqOtyCtxt `seq` (True) ->
            (case (_lhsIisRow) of
             { _seqOisRow | _seqOisRow `seq` (True) ->
             (case (_lhsIisAtTop) of
              { _seqOisAtTop | _seqOisAtTop `seq` (True) ->
              (case (0) of
               { _appSpinePos | _appSpinePos `seq` (True) ->
               (case (_appSpinePos) of
                { _seqOappSpinePos | _seqOappSpinePos `seq` (True) ->
                (case (seq_ _seqOappSpinePos _seqOisAtTop _seqOisRow _seqOopts _seqOtyCtxt _seqOvarMp ) of
                 { ( _seqIisReplaced,_seqIrepl,_seqIself,_seqIvarMp) | True ->
                     (case (_seqIisReplaced) of
                      { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                      (case (Pred_Preds _seqIrepl) of
                       { _repl | _repl `seq` (True) ->
                       (case (Just [_repl]) of
                        { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                        (case (_repl) of
                         { _lhsOrepl | _lhsOrepl `seq` (True) ->
                         (case (Pred_Preds _seqIself) of
                          { _self | _self `seq` (True) ->
                          (case (_self) of
                           { _lhsOself | _lhsOself `seq` (True) ->
                           (case (_seqIvarMp) of
                            { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                            ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (False) of
          { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
          (case (Pred_Var pv_) of
           { _repl | _repl `seq` (True) ->
           (case (Just [_repl]) of
            { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
            (case (_repl) of
             { _lhsOrepl | _lhsOrepl `seq` (True) ->
             (case (Pred_Var pv_) of
              { _self | _self `seq` (True) ->
              (case (_self) of
               { _lhsOself | _lhsOself `seq` (True) ->
               (case (_lhsIvarMp) of
                { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                ( _lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TyCanonicOpts
         tyCtxt               : TyQuCtxt
      chained attribute:
         varMp                : VarMp
      synthesized attributes:
         isReplaced           : Bool
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 0:
            local repl        : _
            local self        : _
      alternative Nil:
         visit 0:
            local repl        : _
            local self        : _
      alternative Var:
         child av             : {TyVarId}
         visit 0:
            local repl        : _
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
                  Bool ->
                  Bool ->
                  TyCanonicOpts ->
                  TyQuCtxt ->
                  VarMp ->
                  ( Bool,PredSeq ,PredSeq ,VarMp)
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (_lhsIvarMp) of
            { _hdOvarMp | _hdOvarMp `seq` (True) ->
            (case (_lhsItyCtxt) of
             { _hdOtyCtxt | _hdOtyCtxt `seq` (True) ->
             (case (_lhsIisRow) of
              { _hdOisRow | _hdOisRow `seq` (True) ->
              (case (_lhsIisAtTop) of
               { _hdOisAtTop | _hdOisAtTop `seq` (True) ->
               (case (hd_ _hdOisAtTop _hdOisRow _hdOopts _hdOtyCtxt _hdOvarMp ) of
                { ( _hdIisReplaced,_hdImbPredL,_hdIrepl,_hdIself,_hdIvarMp) | True ->
                    (case (_hdIvarMp) of
                     { _tlOvarMp | _tlOvarMp `seq` (True) ->
                     (case (_lhsItyCtxt) of
                      { _tlOtyCtxt | _tlOtyCtxt `seq` (True) ->
                      (case (_lhsIisRow) of
                       { _tlOisRow | _tlOisRow `seq` (True) ->
                       (case (_lhsIisAtTop) of
                        { _tlOisAtTop | _tlOisAtTop `seq` (True) ->
                        (case (_lhsIappSpinePos + 1) of
                         { _tlOappSpinePos | _tlOappSpinePos `seq` (True) ->
                         (case (tl_ _tlOappSpinePos _tlOisAtTop _tlOisRow _tlOopts _tlOtyCtxt _tlOvarMp ) of
                          { ( _tlIisReplaced,_tlIrepl,_tlIself,_tlIvarMp) | True ->
                              (case (_hdIisReplaced || _tlIisReplaced) of
                               { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                               (case (PredSeq_Cons _hdIrepl _tlIrepl) of
                                { _repl | _repl `seq` (True) ->
                                (case (_repl) of
                                 { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                 (case (PredSeq_Cons _hdIself _tlIself) of
                                  { _self | _self `seq` (True) ->
                                  (case (_self) of
                                   { _lhsOself | _lhsOself `seq` (True) ->
                                   (case (_tlIvarMp) of
                                    { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                    ( _lhsOisReplaced,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (False) of
          { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
          (case (PredSeq_Nil) of
           { _repl | _repl `seq` (True) ->
           (case (_repl) of
            { _lhsOrepl | _lhsOrepl `seq` (True) ->
            (case (PredSeq_Nil) of
             { _self | _self `seq` (True) ->
             (case (_self) of
              { _lhsOself | _lhsOself `seq` (True) ->
              (case (_lhsIvarMp) of
               { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
               ( _lhsOisReplaced,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt
       _lhsIvarMp ->
         (case (False) of
          { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
          (case (PredSeq_Var av_) of
           { _repl | _repl `seq` (True) ->
           (case (_repl) of
            { _lhsOrepl | _lhsOrepl `seq` (True) ->
            (case (PredSeq_Var av_) of
             { _self | _self `seq` (True) ->
             (case (_self) of
              { _lhsOself | _lhsOself `seq` (True) ->
              (case (_lhsIvarMp) of
               { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
               ( _lhsOisReplaced,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         appFunNm             : HsName
   visit 1:
      inherited attributes:
         appSpinePos          : Int
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TyCanonicOpts
         tyCtxt               : TyQuCtxt
      chained attribute:
         varMp                : VarMp
      synthesized attributes:
         appArgReplL          : [Ty]
         isArrow              : Bool
         isPred               : Bool
         isReplaced           : Bool
         mbPredL              : MbPredL
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local isReplaced  : _
            local self        : _
      alternative Any:
         visit 1:
            local repl        : _
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
            local isAtTop     : _
            local isSpineRoot : {Bool}
            local appIsSum    : _
            local appIsRecOrSum : _
            local argIsRow    : {Bool}
            local appArgReplL : _
            local isArrowRoot : {Bool}
            local isArrowArg  : {Bool}
            local repl        : _
            local mbReplNoImplNil : _
            local replNoImplNil : _
            local mbRepl      : _
            local isReplaced  : _
            local _tup1       : _
            local self        : _
            local replVarMp   : _
      alternative Con:
         child nm             : {HsName}
         visit 1:
            local repl        : _
            local mbRepl      : _
            local isReplaced  : _
            local _tup2       : _
            local self        : _
            local replVarMp   : _
      alternative Dbg:
         child info           : {String}
         visit 1:
            local repl        : _
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
            local repl        : _
            local isSpineRoot : _
            local mbRepl      : _
            local isReplaced  : _
            local self        : _
      alternative Impls:
         child impls          : Impls 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local repl        : _
            local self        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local repl        : _
            local self        : _
      alternative Pred:
         child pr             : Pred 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local repl        : _
            local self        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local repl        : _
            local self        : _
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 1:
            local repl        : _
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
               Bool ->
               Bool ->
               TyCanonicOpts ->
               TyQuCtxt ->
               VarMp ->
               ( ([Ty]),Bool,Bool,Bool,MbPredL,Ty ,Ty ,VarMp)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Ann_1 :: T_Ty_1 
                 sem_Ty_Ann_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case (_lhsIopts) of
                           { _tyOopts | _tyOopts `seq` (True) ->
                           (case (ty_ ) of
                            { ( _tyIappFunNm,ty_1) | True ->
                                (case (_lhsIvarMp) of
                                 { _tyOvarMp | _tyOvarMp `seq` (True) ->
                                 (case (TyQuCtxtOther) of
                                  { _tyCtxt | _tyCtxt `seq` (True) ->
                                  (case (_tyCtxt) of
                                   { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                                   (case (False) of
                                    { _isRow | _isRow `seq` (True) ->
                                    (case (_isRow) of
                                     { _tyOisRow | _tyOisRow `seq` (True) ->
                                     (case (False) of
                                      { _isAtTop | _isAtTop `seq` (True) ->
                                      (case (_isAtTop) of
                                       { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                                       (case (_lhsIappSpinePos) of
                                        { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
                                        (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                                         { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                                             (case (_tyIappArgReplL) of
                                              { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                                              (case (False) of
                                               { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                                               (case (False) of
                                                { _lhsOisPred | _lhsOisPred `seq` (True) ->
                                                (case (True) of
                                                 { _isReplaced | _isReplaced `seq` (True) ->
                                                 (case (_isReplaced) of
                                                  { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                                  (case (_tyImbPredL) of
                                                   { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                                   (case (_tyIrepl) of
                                                    { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                                    (case (ann_ ) of
                                                     { ( _annIisReplaced,_annIrepl,_annIself) | True ->
                                                         (case (Ty_Ann _annIself _tyIself) of
                                                          { _self | _self `seq` (True) ->
                                                          (case (_self) of
                                                           { _lhsOself | _lhsOself `seq` (True) ->
                                                           (case (_tyIvarMp) of
                                                            { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                            ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Ann_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Any_1 :: T_Ty_1 
                 sem_Ty_Any_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (False) of
                              { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                              (case (Nothing) of
                               { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                               (case (Ty_Any) of
                                { _repl | _repl `seq` (True) ->
                                (case (_repl) of
                                 { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                 (case (Ty_Any) of
                                  { _self | _self `seq` (True) ->
                                  (case (_self) of
                                   { _lhsOself | _lhsOself `seq` (True) ->
                                   (case (_lhsIvarMp) of
                                    { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                    ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Any_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (case (func_ ) of
     { ( _funcIappFunNm,func_1) | True ->
         (case (_funcIappFunNm) of
          { _appFunNm | _appFunNm `seq` (True) ->
          (case (_appFunNm) of
           { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
           (case ((let sem_Ty_App_1 :: T_Ty_1 
                       sem_Ty_App_1  =
                           (\ _lhsIappSpinePos
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIopts
                              _lhsItyCtxt
                              _lhsIvarMp ->
                                (case (_lhsIopts) of
                                 { _argOopts | _argOopts `seq` (True) ->
                                 (case (_lhsIopts) of
                                  { _funcOopts | _funcOopts `seq` (True) ->
                                  (case (0) of
                                   { _argOappSpinePos | _argOappSpinePos `seq` (True) ->
                                   (case (arg_ ) of
                                    { ( _argIappFunNm,arg_1) | True ->
                                        (case (_lhsIvarMp) of
                                         { _funcOvarMp | _funcOvarMp `seq` (True) ->
                                         (case (_lhsIappSpinePos + 1) of
                                          { _funcOappSpinePos | _funcOappSpinePos `seq` (True) ->
                                          (case (hsnIsRec _funcIappFunNm) of
                                           { _appIsRec | _appIsRec `seq` (True) ->
                                           (case (hsnIsProd _funcIappFunNm || _appIsRec) of
                                            { _appIsLikeProd | _appIsLikeProd `seq` (True) ->
                                            (case (hsnIsArrow _funcIappFunNm) of
                                             { _appIsArrow | _appIsArrow `seq` (True) ->
                                             (case (if      _appIsArrow     then TyQuCtxtArrow
                                                    else if _appIsLikeProd  then TyQuCtxtProd
                                                                            else TyQuCtxtOther) of
                                              { _tyCtxt | _tyCtxt `seq` (True) ->
                                              (case (_tyCtxt) of
                                               { _funcOtyCtxt | _funcOtyCtxt `seq` (True) ->
                                               (case (_lhsIisRow) of
                                                { _funcOisRow | _funcOisRow `seq` (True) ->
                                                (case (False) of
                                                 { _isAtTop | _isAtTop `seq` (True) ->
                                                 (case (_isAtTop) of
                                                  { _funcOisAtTop | _funcOisAtTop `seq` (True) ->
                                                  (case (func_1 _funcOappSpinePos _funcOisAtTop _funcOisRow _funcOopts _funcOtyCtxt _funcOvarMp ) of
                                                   { ( _funcIappArgReplL,_funcIisArrow,_funcIisPred,_funcIisReplaced,_funcImbPredL,_funcIrepl,_funcIself,_funcIvarMp) | True ->
                                                       (case (_funcIvarMp) of
                                                        { _argOvarMp | _argOvarMp `seq` (True) ->
                                                        (case (_tyCtxt) of
                                                         { _argOtyCtxt | _argOtyCtxt `seq` (True) ->
                                                         (case (_isAtTop) of
                                                          { _argOisAtTop | _argOisAtTop `seq` (True) ->
                                                          (case (_lhsIappSpinePos == 0) of
                                                           { _isSpineRoot | _isSpineRoot `seq` (True) ->
                                                           (case (hsnIsSum _funcIappFunNm) of
                                                            { _appIsSum | _appIsSum `seq` (True) ->
                                                            (case (_appIsRec || _appIsSum) of
                                                             { _appIsRecOrSum | _appIsRecOrSum `seq` (True) ->
                                                             (case (_isSpineRoot && _appIsRecOrSum) of
                                                              { _argIsRow | _argIsRow `seq` (True) ->
                                                              (case (_argIsRow) of
                                                               { _argOisRow | _argOisRow `seq` (True) ->
                                                               (case (arg_1 _argOappSpinePos _argOisAtTop _argOisRow _argOopts _argOtyCtxt _argOvarMp ) of
                                                                { ( _argIappArgReplL,_argIisArrow,_argIisPred,_argIisReplaced,_argImbPredL,_argIrepl,_argIself,_argIvarMp) | True ->
                                                                    (case (_argIrepl : _funcIappArgReplL) of
                                                                     { _appArgReplL | _appArgReplL `seq` (True) ->
                                                                     (case (_appArgReplL) of
                                                                      { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                                                                      (case (_appIsArrow && _isSpineRoot) of
                                                                       { _isArrowRoot | _isArrowRoot `seq` (True) ->
                                                                       (case (_isArrowRoot) of
                                                                        { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                                                                        (case (_appIsArrow && _lhsIappSpinePos == 1) of
                                                                         { _isArrowArg | _isArrowArg `seq` (True) ->
                                                                         (case (if _isArrowArg then _argIisPred else False) of
                                                                          { _lhsOisPred | _lhsOisPred `seq` (True) ->
                                                                          (case (Ty_App _funcIrepl _argIrepl) of
                                                                           { _repl | _repl `seq` (True) ->
                                                                           (case (let r =                     _argIrepl
                                                                                  in  case _funcImbPredL of
                                                                                        Nothing | _funcIisPred -> Just r
                                                                                        Just [] | _funcIisPred -> Just r
                                                                                        _
                                                                                          | otherwise          -> Nothing) of
                                                                            { _mbReplNoImplNil | _mbReplNoImplNil `seq` (True) ->
                                                                            (case (maybe _repl id _mbReplNoImplNil) of
                                                                             { _replNoImplNil | _replNoImplNil `seq` (True) ->
                                                                             (case (if _isSpineRoot
                                                                                    then tcoTyBetaRedFullMb _lhsIopts _replNoImplNil
                                                                                    else Nothing) of
                                                                              { _mbRepl | _mbRepl `seq` (True) ->
                                                                              (case (isJust _mbReplNoImplNil || isJust _mbRepl) of
                                                                               { _isReplaced | _isReplaced `seq` (True) ->
                                                                               (case (_isReplaced || _funcIisReplaced || _argIisReplaced) of
                                                                                { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                                                                (case (_funcImbPredL `mbPredLCmb` _argImbPredL) of
                                                                                 { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                                                                 (case (maybe (_replNoImplNil,emptyVarMp) extr _mbRepl) of
                                                                                  { __tup1 | __tup1 `seq` (True) ->
                                                                                  (case (__tup1) of
                                                                                   { (_lhsOrepl,_) | _lhsOrepl `seq` (True) ->
                                                                                   (case (Ty_App _funcIself _argIself) of
                                                                                    { _self | _self `seq` (True) ->
                                                                                    (case (_self) of
                                                                                     { _lhsOself | _lhsOself `seq` (True) ->
                                                                                     (case (__tup1) of
                                                                                      { (_,_replVarMp) | _replVarMp `seq` (True) ->
                                                                                      (case (_replVarMp `varUpd` _argIvarMp) of
                                                                                       { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                                                       ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_App_1)) of
            { ( sem_Ty_1) | True ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (case (nm_) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Con_1 :: T_Ty_1 
                 sem_Ty_Con_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (Ty_Con nm_) of
                              { _repl | _repl `seq` (True) ->
                              (case (tcoTyBetaRedFullMb _lhsIopts _repl) of
                               { _mbRepl | _mbRepl `seq` (True) ->
                               (case (isJust _mbRepl) of
                                { _isReplaced | _isReplaced `seq` (True) ->
                                (case (_isReplaced) of
                                 { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                  (case (maybe (_repl,emptyVarMp) extr _mbRepl) of
                                   { __tup2 | __tup2 `seq` (True) ->
                                   (case (__tup2) of
                                    { (_lhsOrepl,_) | _lhsOrepl `seq` (True) ->
                                    (case (Ty_Con nm_) of
                                     { _self | _self `seq` (True) ->
                                     (case (_self) of
                                      { _lhsOself | _lhsOself `seq` (True) ->
                                      (case (__tup2) of
                                       { (_,_replVarMp) | _replVarMp `seq` (True) ->
                                       (case (_replVarMp `varUpd` _lhsIvarMp) of
                                        { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                        ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Con_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Dbg_1 :: T_Ty_1 
                 sem_Ty_Dbg_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (False) of
                              { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                              (case (Nothing) of
                               { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                               (case (Ty_Dbg info_) of
                                { _repl | _repl `seq` (True) ->
                                (case (_repl) of
                                 { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                 (case (Ty_Dbg info_) of
                                  { _self | _self `seq` (True) ->
                                  (case (_self) of
                                   { _lhsOself | _lhsOself `seq` (True) ->
                                   (case (_lhsIvarMp) of
                                    { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                    ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Dbg_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (case (ty_ ) of
     { ( _tyIappFunNm,ty_1) | True ->
         (case (_tyIappFunNm) of
          { _appFunNm | _appFunNm `seq` (True) ->
          (case (_appFunNm) of
           { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
           (case ((let sem_Ty_Ext_1 :: T_Ty_1 
                       sem_Ty_Ext_1  =
                           (\ _lhsIappSpinePos
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIopts
                              _lhsItyCtxt
                              _lhsIvarMp ->
                                (case ([]) of
                                 { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                                 (case (False) of
                                  { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                                  (case (False) of
                                   { _lhsOisPred | _lhsOisPred `seq` (True) ->
                                   (case (_lhsIopts) of
                                    { _extTyOopts | _extTyOopts `seq` (True) ->
                                    (case (_lhsIopts) of
                                     { _tyOopts | _tyOopts `seq` (True) ->
                                     (case (0) of
                                      { _extTyOappSpinePos | _extTyOappSpinePos `seq` (True) ->
                                      (case (_lhsIappSpinePos + 1) of
                                       { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
                                       (case (extTy_ ) of
                                        { ( _extTyIappFunNm,extTy_1) | True ->
                                            (case (_lhsIvarMp) of
                                             { _tyOvarMp | _tyOvarMp `seq` (True) ->
                                             (case (TyQuCtxtOther) of
                                              { _tyCtxt | _tyCtxt `seq` (True) ->
                                              (case (_tyCtxt) of
                                               { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                                               (case (False) of
                                                { _isRow | _isRow `seq` (True) ->
                                                (case (_isRow) of
                                                 { _tyOisRow | _tyOisRow `seq` (True) ->
                                                 (case (False) of
                                                  { _isAtTop | _isAtTop `seq` (True) ->
                                                  (case (_isAtTop) of
                                                   { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                                                   (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                                                    { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                                                        (case (_tyIvarMp) of
                                                         { _extTyOvarMp | _extTyOvarMp `seq` (True) ->
                                                         (case (_tyCtxt) of
                                                          { _extTyOtyCtxt | _extTyOtyCtxt `seq` (True) ->
                                                          (case (_isRow) of
                                                           { _extTyOisRow | _extTyOisRow `seq` (True) ->
                                                           (case (_isAtTop) of
                                                            { _extTyOisAtTop | _extTyOisAtTop `seq` (True) ->
                                                            (case (extTy_1 _extTyOappSpinePos _extTyOisAtTop _extTyOisRow _extTyOopts _extTyOtyCtxt _extTyOvarMp ) of
                                                             { ( _extTyIappArgReplL,_extTyIisArrow,_extTyIisPred,_extTyIisReplaced,_extTyImbPredL,_extTyIrepl,_extTyIself,_extTyIvarMp) | True ->
                                                                 (case (Ty_Ext _tyIrepl nm_ _extTyIrepl) of
                                                                  { _repl | _repl `seq` (True) ->
                                                                  (case (_lhsIappSpinePos == 0) of
                                                                   { _isSpineRoot | _isSpineRoot `seq` (True) ->
                                                                   (case (if _isSpineRoot
                                                                          then let (row,exts) = tyRowExtsUnAnn $ tyRowExts _repl
                                                                               in  if tyRowIsCanonOrdered exts
                                                                                   then Nothing
                                                                                   else Just $ mkDfltTyCanonicOut $ mkTyRow row $ tyRowCanonOrder exts
                                                                          else Nothing) of
                                                                    { _mbRepl | _mbRepl `seq` (True) ->
                                                                    (case (isJust _mbRepl) of
                                                                     { _isReplaced | _isReplaced `seq` (True) ->
                                                                     (case (_isReplaced || _tyIisReplaced || _extTyIisReplaced) of
                                                                      { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                                                      (case (_tyImbPredL `mbPredLCmb` _extTyImbPredL) of
                                                                       { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                                                       (case (maybe _repl tbroutRes _mbRepl) of
                                                                        { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                                                        (case (Ty_Ext _tyIself nm_ _extTyIself) of
                                                                         { _self | _self `seq` (True) ->
                                                                         (case (_self) of
                                                                          { _lhsOself | _lhsOself `seq` (True) ->
                                                                          (case (_extTyIvarMp) of
                                                                           { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                                           ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_Ext_1)) of
            { ( sem_Ty_1) | True ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Impls_1 :: T_Ty_1 
                 sem_Ty_Impls_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (True) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (_lhsIopts) of
                              { _implsOopts | _implsOopts `seq` (True) ->
                              (case (_lhsIvarMp) of
                               { _implsOvarMp | _implsOvarMp `seq` (True) ->
                               (case (TyQuCtxtOther) of
                                { _tyCtxt | _tyCtxt `seq` (True) ->
                                (case (_tyCtxt) of
                                 { _implsOtyCtxt | _implsOtyCtxt `seq` (True) ->
                                 (case (False) of
                                  { _isRow | _isRow `seq` (True) ->
                                  (case (_isRow) of
                                   { _implsOisRow | _implsOisRow `seq` (True) ->
                                   (case (False) of
                                    { _isAtTop | _isAtTop `seq` (True) ->
                                    (case (_isAtTop) of
                                     { _implsOisAtTop | _implsOisAtTop `seq` (True) ->
                                     (case (impls_ _implsOisAtTop _implsOisRow _implsOopts _implsOtyCtxt _implsOvarMp ) of
                                      { ( _implsIisReplaced,_implsImbPredL,_implsIrepl,_implsIself,_implsIvarMp) | True ->
                                          (case (_implsIisReplaced) of
                                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                           (case (_implsImbPredL) of
                                            { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                            (case (Ty_Impls _implsIrepl) of
                                             { _repl | _repl `seq` (True) ->
                                             (case (_repl) of
                                              { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                              (case (Ty_Impls _implsIself) of
                                               { _self | _self `seq` (True) ->
                                               (case (_self) of
                                                { _lhsOself | _lhsOself `seq` (True) ->
                                                (case (_implsIvarMp) of
                                                 { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                 ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Impls_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Lam_1 :: T_Ty_1 
                 sem_Ty_Lam_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (_lhsIopts) of
                              { _tyOopts | _tyOopts `seq` (True) ->
                              (case (_lhsIappSpinePos) of
                               { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
                               (case (ty_ ) of
                                { ( _tyIappFunNm,ty_1) | True ->
                                    (case (_lhsIvarMp) of
                                     { _tyOvarMp | _tyOvarMp `seq` (True) ->
                                     (case (TyQuCtxtOther) of
                                      { _tyCtxt | _tyCtxt `seq` (True) ->
                                      (case (_tyCtxt) of
                                       { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                                       (case (False) of
                                        { _isRow | _isRow `seq` (True) ->
                                        (case (_isRow) of
                                         { _tyOisRow | _tyOisRow `seq` (True) ->
                                         (case (False) of
                                          { _isAtTop | _isAtTop `seq` (True) ->
                                          (case (_isAtTop) of
                                           { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                                           (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                                            { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                                                (case (_tyIisReplaced) of
                                                 { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                                 (case (_tyImbPredL) of
                                                  { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                                  (case (Ty_Lam tv_ _tyIrepl) of
                                                   { _repl | _repl `seq` (True) ->
                                                   (case (_repl) of
                                                    { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                                    (case (Ty_Lam tv_ _tyIself) of
                                                     { _self | _self `seq` (True) ->
                                                     (case (_self) of
                                                      { _lhsOself | _lhsOself `seq` (True) ->
                                                      (case (_tyIvarMp) of
                                                       { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                       ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Lam_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Pred_1 :: T_Ty_1 
                 sem_Ty_Pred_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (True) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (_lhsIopts) of
                              { _prOopts | _prOopts `seq` (True) ->
                              (case (_lhsIvarMp) of
                               { _prOvarMp | _prOvarMp `seq` (True) ->
                               (case (TyQuCtxtOther) of
                                { _tyCtxt | _tyCtxt `seq` (True) ->
                                (case (_tyCtxt) of
                                 { _prOtyCtxt | _prOtyCtxt `seq` (True) ->
                                 (case (False) of
                                  { _isRow | _isRow `seq` (True) ->
                                  (case (_isRow) of
                                   { _prOisRow | _prOisRow `seq` (True) ->
                                   (case (False) of
                                    { _isAtTop | _isAtTop `seq` (True) ->
                                    (case (_isAtTop) of
                                     { _prOisAtTop | _prOisAtTop `seq` (True) ->
                                     (case (pr_ _prOisAtTop _prOisRow _prOopts _prOtyCtxt _prOvarMp ) of
                                      { ( _prIisReplaced,_prImbPredL,_prIrepl,_prIself,_prIvarMp) | True ->
                                          (case (_prIisReplaced) of
                                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                           (case (_prImbPredL) of
                                            { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                            (case (Ty_Pred _prIrepl) of
                                             { _repl | _repl `seq` (True) ->
                                             (case (_repl) of
                                              { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                              (case (Ty_Pred _prIself) of
                                               { _self | _self `seq` (True) ->
                                               (case (_self) of
                                                { _lhsOself | _lhsOself `seq` (True) ->
                                                (case (_prIvarMp) of
                                                 { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                 ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Pred_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_TBind_1 :: T_Ty_1 
                 sem_Ty_TBind_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (_lhsIopts) of
                              { _tyOopts | _tyOopts `seq` (True) ->
                              (case (0) of
                               { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
                               (case (ty_ ) of
                                { ( _tyIappFunNm,ty_1) | True ->
                                    (case (_lhsIvarMp) of
                                     { _tyOvarMp | _tyOvarMp `seq` (True) ->
                                     (case (TyQuCtxtOther) of
                                      { _tyCtxt | _tyCtxt `seq` (True) ->
                                      (case (_tyCtxt) of
                                       { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                                       (case (False) of
                                        { _isRow | _isRow `seq` (True) ->
                                        (case (_isRow) of
                                         { _tyOisRow | _tyOisRow `seq` (True) ->
                                         (case (False) of
                                          { _isAtTop | _isAtTop `seq` (True) ->
                                          (case (_isAtTop) of
                                           { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                                           (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                                            { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                                                (case (qu_ ) of
                                                 { ( _quIisReplaced,_quIrepl,_quIself) | True ->
                                                     (case (_quIisReplaced || _tyIisReplaced) of
                                                      { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                                      (case (_tyImbPredL) of
                                                       { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                                       (case (Ty_TBind _quIrepl tv_ l1_ _tyIrepl) of
                                                        { _repl | _repl `seq` (True) ->
                                                        (case (_repl) of
                                                         { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                                         (case (Ty_TBind _quIself tv_ l1_ _tyIself) of
                                                          { _self | _self `seq` (True) ->
                                                          (case (_self) of
                                                           { _lhsOself | _lhsOself `seq` (True) ->
                                                           (case (_tyIvarMp) of
                                                            { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                                            ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_TBind_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
     (case ((let sem_Ty_Var_1 :: T_Ty_1 
                 sem_Ty_Var_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt
                        _lhsIvarMp ->
                          (case ([]) of
                           { _lhsOappArgReplL | _lhsOappArgReplL `seq` (True) ->
                           (case (False) of
                            { _lhsOisArrow | _lhsOisArrow `seq` (True) ->
                            (case (False) of
                             { _lhsOisPred | _lhsOisPred `seq` (True) ->
                             (case (categ_ ) of
                              { ( _categIisReplaced,_categIrepl,_categIself) | True ->
                                  (case (_categIisReplaced) of
                                   { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                                   (case (Nothing) of
                                    { _lhsOmbPredL | _lhsOmbPredL `seq` (True) ->
                                    (case (Ty_Var tv_ _categIrepl) of
                                     { _repl | _repl `seq` (True) ->
                                     (case (_repl) of
                                      { _lhsOrepl | _lhsOrepl `seq` (True) ->
                                      (case (Ty_Var tv_ _categIself) of
                                       { _self | _self `seq` (True) ->
                                       (case (_self) of
                                        { _lhsOself | _lhsOself `seq` (True) ->
                                        (case (_lhsIvarMp) of
                                         { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                                         ( _lhsOappArgReplL,_lhsOisArrow,_lhsOisPred,_lhsOisReplaced,_lhsOmbPredL,_lhsOrepl,_lhsOself,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Var_1)) of
      { ( sem_Ty_1) | True ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : TyCanonicOpts
      synthesized attributes:
         isReplaced           : Bool
         repl                 : Ty 
         varMp                : VarMp
   alternatives:
      alternative AGItf:
         child ty             : Ty 
         visit 0:
            local varMp       : _
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _ty )  =
    (sem_TyAGItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_TyAGItf  = TyCanonicOpts ->
                  ( Bool,Ty ,VarMp)
data Inh_TyAGItf  = Inh_TyAGItf {opts_Inh_TyAGItf :: !(TyCanonicOpts)}
data Syn_TyAGItf  = Syn_TyAGItf {isReplaced_Syn_TyAGItf :: !(Bool),repl_Syn_TyAGItf :: !(Ty ),varMp_Syn_TyAGItf :: !(VarMp)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIopts )  =
    (let ( _lhsOisReplaced,_lhsOrepl,_lhsOvarMp) | True = sem _lhsIopts 
     in  (Syn_TyAGItf _lhsOisReplaced _lhsOrepl _lhsOvarMp ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _tyOopts | _tyOopts `seq` (True) ->
          (case (0) of
           { _tyOappSpinePos | _tyOappSpinePos `seq` (True) ->
           (case (ty_ ) of
            { ( _tyIappFunNm,ty_1) | True ->
                (case (emptyVarMp) of
                 { _varMp | _varMp `seq` (True) ->
                 (case (_varMp) of
                  { _tyOvarMp | _tyOvarMp `seq` (True) ->
                  (case (TyQuCtxtOnTop) of
                   { _tyOtyCtxt | _tyOtyCtxt `seq` (True) ->
                   (case (False) of
                    { _tyOisRow | _tyOisRow `seq` (True) ->
                    (case (True) of
                     { _tyOisAtTop | _tyOisAtTop `seq` (True) ->
                     (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt _tyOvarMp ) of
                      { ( _tyIappArgReplL,_tyIisArrow,_tyIisPred,_tyIisReplaced,_tyImbPredL,_tyIrepl,_tyIself,_tyIvarMp) | True ->
                          (case (_tyIisReplaced) of
                           { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
                           (case (_tyIrepl) of
                            { _lhsOrepl | _lhsOrepl `seq` (True) ->
                            (case (_varMp) of
                             { _lhsOvarMp | _lhsOvarMp `seq` (True) ->
                             ( _lhsOisReplaced,_lhsOrepl,_lhsOvarMp) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         isReplaced           : Bool
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local repl        : _
            local self        : _
      alternative Mono:
         visit 0:
            local repl        : _
            local self        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local repl        : _
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
type T_TyAnn  = ( Bool,TyAnn ,TyAnn )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyAnn_Empty) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyAnn_Empty) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyAnn_Mono) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyAnn_Mono) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyAnn_Strictness s_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyAnn_Strictness s_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
-- TyQu --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         isReplaced           : Bool
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
            local self        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
            local self        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
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
type T_TyQu  = ( Bool,TyQu ,TyQu )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyQu_Exists mlev_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyQu_Exists mlev_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyQu_Forall mlev_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyQu_Forall mlev_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyQu_Plain mlev_) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyQu_Plain mlev_) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         isReplaced           : Bool
         repl                 : SELF 
         self                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local repl        : _
            local self        : _
      alternative Meta:
         visit 0:
            local repl        : _
            local self        : _
      alternative Plain:
         visit 0:
            local repl        : _
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
type T_TyVarCateg  = ( Bool,TyVarCateg ,TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyVarCateg_Fixed) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyVarCateg_Fixed) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyVarCateg_Meta) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyVarCateg_Meta) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (case (False) of
     { _lhsOisReplaced | _lhsOisReplaced `seq` (True) ->
     (case (TyVarCateg_Plain) of
      { _repl | _repl `seq` (True) ->
      (case (_repl) of
       { _lhsOrepl | _lhsOrepl `seq` (True) ->
       (case (TyVarCateg_Plain) of
        { _self | _self `seq` (True) ->
        (case (_self) of
         { _lhsOself | _lhsOself `seq` (True) ->
         ( _lhsOisReplaced,_lhsOrepl,_lhsOself) }) }) }) }) })