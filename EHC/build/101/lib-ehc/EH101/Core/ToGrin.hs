

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/ToGrin.ag)
module EH101.Core.ToGrin(Inh_CodeAGItf (..), Syn_CodeAGItf (..), wrap_CodeAGItf, sem_CodeAGItf) where

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Utils
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Opts
import qualified EH101.Config as Cfg
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.LamInfo
import EH101.Core
import EH101.Ty
import EH101.Gam.Full
import EH101.GrinCode
import EH101.Core.FFI
import Debug.Trace
import EH101.Foreign.Extract
import EH101.BuiltinPrims





















primNmList2GrTagMap :: [HsName] -> Map.Map HsName [GrTag]
primNmList2GrTagMap = Map.fromList . map (\tn -> (tn, [tr tn]))
                where tr nm = let arity = if hsnIsProd nm then hsnProdArity nm else 1
                              in  GrTag_Con (mkGrTagAnn arity arity) 0 nm

cTagsMp2GrTagMap :: CTagsMp -> Map.Map HsName [GrTag]
cTagsMp2GrTagMap = Map.fromList . map (\(tn,ts) -> (tn, tr ts))
             where  tr = map (\(_,(CTag _ nm i a ma)) -> GrTag_Con (mkGrTagAnn a ma) i nm)




grE2V :: GrExpr -> Maybe GrVal
grE2V e = case e of {GrExpr_Unit v _ -> Just v; _ -> Nothing}

grV2HNm :: GrVal -> Maybe HsName
grV2HNm v = case v of {GrVal_Var n -> Just n; _ -> Nothing}

emptyGrExpr = GrExpr_Unit GrVal_Empty GrType_None

mbMkStrict :: HsName -> UID -> Bool -> GrVal -> GrExpr
mbMkStrict modNm u isStrict v
  = case v of
      GrVal_Node _ _ -> dflt
      _ | isStrict   -> GrExpr_Seq (GrExpr_Unit v GrType_None) (GrPatLam_Var n) (GrExpr_Eval n)
        | otherwise  -> dflt
        where n = uidQualHNm modNm u
  where dflt = GrExpr_Unit v GrType_None

retStrict :: Bool -> GrVal -> GrExpr
retStrict isStrict = if isStrict then (\v->GrExpr_Unit v GrType_None) else GrExpr_Store



mkNdPApp :: HsName -> Int -> GrValL -> GrVal
mkNdPApp f nMiss argL = GrVal_Node (GrTag_PApp nMiss f) argL

mkNdApp :: HsName -> GrValL -> GrVal
mkNdApp f argL = GrVal_Node (GrTag_App hsnWild) (GrVal_Var f : argL)

mkSq :: GrExpr -> HsName -> GrExpr -> GrExpr
mkSq e1 p e2 = GrExpr_Seq e1 (GrPatLam_Var p) e2



simplArgL :: HsName -> UID -> LamMp -> GrValL -> (GrValL,GrExpr->GrExpr)
simplArgL modNm uniq lamMp vL
  =  let  nL = map (uidQualHNm modNm) . mkNewUIDL (length vL) $ uniq
     in   foldr  (\(n,a) (aL,wG)
                    ->  case a of
                            GrVal_Var v | isJust mbArity
                                -> (GrVal_Var n : aL,mkSq a' n . wG)
                                where mbArity = lamMpLookupLam v lamMp
                                      a' = GrExpr_Store (mkNdPApp v (fromJust mbArity) [])
                            GrVal_Node _ _
                                -> (GrVal_Var n : aL,mkSq (GrExpr_Store a) n . wG)
                            _   -> (a:aL,wG)
                 )
                 ([],id)
          $ zip nL vL

saturateAltL :: UID -> GrExpr -> GrAltL -> GrAltL
saturateAltL uniq dflt altL
  =  case altL of
       (GrAlt_Alt _ (GrPatAlt_LitInt _) _ : _)
         | null [ a | a@(GrAlt_Alt _ (GrPatAlt_Otherwise) _) <- altL ]
           -> altL ++ [GrAlt_Alt GrAltAnnNormal (GrPatAlt_Otherwise) dflt]
       _   -> altL



unboxArg :: HsName -> UID -> HsName -> GrVal -> (GrVal,GrExpr->GrExpr)
unboxArg modNm uniq tyNm v
  =  case v of
       GrVal_LitInt _
           -> (v,id)
       _   -> (GrVal_Var n,GrExpr_Seq (GrExpr_Unit v GrType_None) (mkGrUnbox tyNm n))
  where n = uidQualHNm modNm uniq

unboxArgL :: HsName -> UID -> HsName -> GrValL -> (GrValL,GrExpr->GrExpr)
unboxArgL modNm uniq tyNm vL
  =  let  uL = mkNewUIDL (length vL) $ uniq
     in   foldr  (\(u,a) (aL,wG)
                    ->  let (a',w) = unboxArg modNm u tyNm a
                        in  (a' : aL, w . wG)
                 )
                 ([],id)
          $ zip uL vL




mkHole    (GrBind_Bind nm _ _ _)    rest  = GrExpr_Seq (GrExpr_Store (GrVal_Node GrTag_Hole []))
                                                       (GrPatLam_Var nm)
                                                       rest

mkVarBind (GrBind_Bind nm _ _ body) rest  = GrExpr_Seq body (GrPatLam_Var nm) rest

mkVarBindWithUpdate
          (GrBind_Bind nm _ _ body) rest  = let nm2 = hsnUniqify HsNameUniqifier_GrinUpdated nm
                                            in  GrExpr_Seq body
                                                           (GrPatLam_Var nm2)
                                                           (GrExpr_Seq (GrExpr_FetchUpdate nm2 nm)
                                                                       GrPatLam_Empty
                                                                       rest
                                                           )

mkHoles               bindL rest  = foldr mkHole              rest bindL
mkVarBindWithUpdates  bindL rest  = foldr mkVarBindWithUpdate rest bindL
mkVarBinds            bindL rest  = foldr mkVarBind           rest bindL






type TupAdaptFldL = [(GrVal,GrVal,GrVal->GrVal->GrAdapt)]

mkGrAdapt :: HsName -> UID -> LamMp -> Bool -> TupAdaptFldL -> HsName -> GrExpr
mkGrAdapt modNm u lamMp isStrict tupFldL tupRecNm
  =  let  [u1,u2] = mkNewUIDL 2 (uidChild u)
          (oL,vL,mkAL) = unzip3 tupFldL
          (oL',wrO) = unboxArgL modNm u1 hsnInt oL
          (vL',wrV) = simplArgL modNm u2 lamMp vL
     in   wrV . wrO . retStrict isStrict
          . GrVal_NodeAdapt tupRecNm
          . zipWith3 ($) mkAL oL' $ vL'



idGrWrapCaseSel :: GrVal -> (GrVal,GrExpr->GrExpr)
idGrWrapCaseSel s = (s,id)



-- CAlt --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grAlt                : GrAlt
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 1:
            local whatAbove   : {WhatExpr}
            local lev         : _
            local fvS         : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = UID ->
               ( UID,T_CAlt_1 )
type T_CAlt_1  = DataGam ->
                 EvalCtx ->
                 Bool ->
                 Bool ->
                 LamMp ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 ( FvS,GrAlt,(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)))
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _patOgUniq ->
          (case (pat_ _patOgUniq ) of
           { ( _patIgUniq,pat_1) ->
               (case (_patIgUniq) of
                { _exprOgUniq ->
                (case (expr_ ) of
                 { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                     (case (expr_1 _exprOgUniq ) of
                      { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                          (case (_exprIgUniq) of
                           { _lhsOgUniq ->
                           (case ((let sem_CAlt_Alt_1 :: T_CAlt_1 
                                       sem_CAlt_Alt_1  =
                                           (\ _lhsIdataGam
                                              _lhsIevalCtx
                                              _lhsIisLamBody
                                              _lhsIisStrict
                                              _lhsIlamMp
                                              _lhsIlev
                                              _lhsImodNm
                                              _lhsIopts ->
                                                (case (ExprIsOther) of
                                                 { _whatAbove ->
                                                 (case (_whatAbove) of
                                                  { _exprOwhatAbove ->
                                                  (case (_lhsIopts) of
                                                   { _exprOopts ->
                                                   (case (_lhsImodNm) of
                                                    { _exprOmodNm ->
                                                    (case (_lhsIlev + 1) of
                                                     { _lev ->
                                                     (case (_lev) of
                                                      { _exprOlev ->
                                                      (case (_lhsIlamMp) of
                                                       { _exprOlamMp ->
                                                       (case (_lhsIisStrict) of
                                                        { _exprOisStrict ->
                                                        (case (_lhsIisLamBody) of
                                                         { _exprOisLamBody ->
                                                         (case (_lhsIevalCtx) of
                                                          { _exprOevalCtx ->
                                                          (case (_lhsIdataGam) of
                                                           { _exprOdataGam ->
                                                           (case (True) of
                                                            { _exprOisTopTup ->
                                                            (case (True) of
                                                             { _exprOisTopApp ->
                                                             (case (True) of
                                                              { _exprOdoBox ->
                                                              (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                               { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                   (case (_lhsIopts) of
                                                                    { _patOopts ->
                                                                    (case (_lhsImodNm) of
                                                                     { _patOmodNm ->
                                                                     (case (_lev) of
                                                                      { _patOlev ->
                                                                      (case (_lhsIlamMp) of
                                                                       { _patOlamMp ->
                                                                       (case (_lhsIdataGam) of
                                                                        { _patOdataGam ->
                                                                        (case (pat_1 _patOdataGam _patOlamMp _patOlev _patOmodNm _patOopts ) of
                                                                         { ( _patIfldNmL,_patIfvS,_patIgrPat,_patIgrWrapCase,_patIgrWrapCaseSel,_patInmL) ->
                                                                             (case (_exprIfvS `Set.difference` Set.fromList _patInmL) of
                                                                              { _fvS ->
                                                                              (case (_fvS) of
                                                                               { _lhsOfvS ->
                                                                               (case (GrAlt_Alt GrAltAnnNormal _patIgrPat _exprIgrExpr) of
                                                                                { _lhsOgrAlt ->
                                                                                (case (_patIgrWrapCase) of
                                                                                 { _lhsOgrWrapCase ->
                                                                                 (case (_patIgrWrapCaseSel) of
                                                                                  { _lhsOgrWrapCaseSel ->
                                                                                  ( _lhsOfvS,_lhsOgrAlt,_lhsOgrWrapCase,_lhsOgrWrapCaseSel) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                   in  sem_CAlt_Alt_1)) of
                            { ( sem_CAlt_1) ->
                            ( _lhsOgUniq,sem_CAlt_1) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grAltL               : GrAltL
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
      alternative Nil:
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = UID ->
                ( UID,T_CAltL_1 )
type T_CAltL_1  = DataGam ->
                  EvalCtx ->
                  Bool ->
                  Bool ->
                  LamMp ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  ( FvS,GrAltL,(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)))
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (hd_ _hdOgUniq ) of
           { ( _hdIgUniq,hd_1) ->
               (case (_hdIgUniq) of
                { _tlOgUniq ->
                (case (tl_ _tlOgUniq ) of
                 { ( _tlIgUniq,tl_1) ->
                     (case (_tlIgUniq) of
                      { _lhsOgUniq ->
                      (case ((let sem_CAltL_Cons_1 :: T_CAltL_1 
                                  sem_CAltL_Cons_1  =
                                      (\ _lhsIdataGam
                                         _lhsIevalCtx
                                         _lhsIisLamBody
                                         _lhsIisStrict
                                         _lhsIlamMp
                                         _lhsIlev
                                         _lhsImodNm
                                         _lhsIopts ->
                                           (case (_lhsIopts) of
                                            { _tlOopts ->
                                            (case (_lhsImodNm) of
                                             { _tlOmodNm ->
                                             (case (_lhsIlev) of
                                              { _tlOlev ->
                                              (case (_lhsIlamMp) of
                                               { _tlOlamMp ->
                                               (case (_lhsIisStrict) of
                                                { _tlOisStrict ->
                                                (case (_lhsIisLamBody) of
                                                 { _tlOisLamBody ->
                                                 (case (_lhsIevalCtx) of
                                                  { _tlOevalCtx ->
                                                  (case (_lhsIdataGam) of
                                                   { _tlOdataGam ->
                                                   (case (tl_1 _tlOdataGam _tlOevalCtx _tlOisLamBody _tlOisStrict _tlOlamMp _tlOlev _tlOmodNm _tlOopts ) of
                                                    { ( _tlIfvS,_tlIgrAltL,_tlIgrWrapCase,_tlIgrWrapCaseSel) ->
                                                        (case (_lhsIopts) of
                                                         { _hdOopts ->
                                                         (case (_lhsImodNm) of
                                                          { _hdOmodNm ->
                                                          (case (_lhsIlev) of
                                                           { _hdOlev ->
                                                           (case (_lhsIlamMp) of
                                                            { _hdOlamMp ->
                                                            (case (_lhsIisStrict) of
                                                             { _hdOisStrict ->
                                                             (case (_lhsIisLamBody) of
                                                              { _hdOisLamBody ->
                                                              (case (_lhsIevalCtx) of
                                                               { _hdOevalCtx ->
                                                               (case (_lhsIdataGam) of
                                                                { _hdOdataGam ->
                                                                (case (hd_1 _hdOdataGam _hdOevalCtx _hdOisLamBody _hdOisStrict _hdOlamMp _hdOlev _hdOmodNm _hdOopts ) of
                                                                 { ( _hdIfvS,_hdIgrAlt,_hdIgrWrapCase,_hdIgrWrapCaseSel) ->
                                                                     (case (_hdIfvS `Set.union` _tlIfvS) of
                                                                      { _lhsOfvS ->
                                                                      (case (_hdIgrAlt : _tlIgrAltL) of
                                                                       { _lhsOgrAltL ->
                                                                       (case (_hdIgrWrapCase `const` _tlIgrWrapCase) of
                                                                        { _lhsOgrWrapCase ->
                                                                        (case (_hdIgrWrapCaseSel `const` _tlIgrWrapCaseSel) of
                                                                         { _lhsOgrWrapCaseSel ->
                                                                         ( _lhsOfvS,_lhsOgrAltL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                              in  sem_CAltL_Cons_1)) of
                       { ( sem_CAltL_1) ->
                       ( _lhsOgUniq,sem_CAltL_1) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CAltL_Nil_1 :: T_CAltL_1 
                      sem_CAltL_Nil_1  =
                          (\ _lhsIdataGam
                             _lhsIevalCtx
                             _lhsIisLamBody
                             _lhsIisStrict
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                (case ([]) of
                                 { _lhsOgrAltL ->
                                 (case (id) of
                                  { _lhsOgrWrapCase ->
                                  (case (idGrWrapCaseSel) of
                                   { _lhsOgrWrapCaseSel ->
                                   ( _lhsOfvS,_lhsOgrAltL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel) }) }) }) }))
                  in  sem_CAltL_Nil_1)) of
           { ( sem_CAltL_1) ->
           ( _lhsOgUniq,sem_CAltL_1) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         bindLamMp            : LamMp
         nm                   : HsName
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         fvSMp                : FvSMp
         grBindL              : GrBindL
         grGlobalL            : GrGlobalL
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = ( LamMp,HsName,T_CBind_1 )
type T_CBind_1  = UID ->
                  ( UID,T_CBind_2 )
type T_CBind_2  = DataGam ->
                  EvalCtx ->
                  Bool ->
                  Bool ->
                  Bool ->
                  LamMp ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  ( FvS,FvSMp,GrBindL,GrGlobalL,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (case (nm_) of
     { _bindAspectsOnm ->
     (case (bindAspects_ _bindAspectsOnm ) of
      { ( _bindAspectsIbindLamMp,bindAspects_1) ->
          (case (_bindAspectsIbindLamMp) of
           { _lhsObindLamMp ->
           (case (nm_) of
            { _lhsOnm ->
            (case ((let sem_CBind_Bind_1 :: T_CBind_1 
                        sem_CBind_Bind_1  =
                            (\ _lhsIgUniq ->
                                 (case (_lhsIgUniq) of
                                  { _bindAspectsOgUniq ->
                                  (case (bindAspects_1 _bindAspectsOgUniq ) of
                                   { ( _bindAspectsIgUniq,bindAspects_2) ->
                                       (case (_bindAspectsIgUniq) of
                                        { _lhsOgUniq ->
                                        (case ((let sem_CBind_Bind_2 :: T_CBind_2 
                                                    sem_CBind_Bind_2  =
                                                        (\ _lhsIdataGam
                                                           _lhsIevalCtx
                                                           _lhsIisGlobal
                                                           _lhsIisLamBody
                                                           _lhsIisStrict
                                                           _lhsIlamMp
                                                           _lhsIletBindingsCateg
                                                           _lhsIlev
                                                           _lhsImodNm
                                                           _lhsIopts ->
                                                             (case (_lhsIopts) of
                                                              { _bindAspectsOopts ->
                                                              (case (_lhsImodNm) of
                                                               { _bindAspectsOmodNm ->
                                                               (case (_lhsIlev) of
                                                                { _bindAspectsOlev ->
                                                                (case (_lhsIletBindingsCateg) of
                                                                 { _bindAspectsOletBindingsCateg ->
                                                                 (case (_lhsIlamMp) of
                                                                  { _bindAspectsOlamMp ->
                                                                  (case (_lhsIisStrict) of
                                                                   { _bindAspectsOisStrict ->
                                                                   (case (_lhsIisLamBody) of
                                                                    { _bindAspectsOisLamBody ->
                                                                    (case (_lhsIisGlobal) of
                                                                     { _bindAspectsOisGlobal ->
                                                                     (case (_lhsIevalCtx) of
                                                                      { _bindAspectsOevalCtx ->
                                                                      (case (_lhsIdataGam) of
                                                                       { _bindAspectsOdataGam ->
                                                                       (case (bindAspects_2 _bindAspectsOdataGam _bindAspectsOevalCtx _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOlamMp _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOmodNm _bindAspectsOopts ) of
                                                                        { ( _bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsIgrBindL,_bindAspectsIgrGlobalL,_bindAspectsInmL) ->
                                                                            (case (_bindAspectsIfvS) of
                                                                             { _lhsOfvS ->
                                                                             (case (Map.singleton nm_ _bindAspectsIfvS) of
                                                                              { _lhsOfvSMp ->
                                                                              (case (_bindAspectsIgrBindL) of
                                                                               { _lhsOgrBindL ->
                                                                               (case (_bindAspectsIgrGlobalL) of
                                                                                { _lhsOgrGlobalL ->
                                                                                (case ([nm_]) of
                                                                                 { _lhsOnmL ->
                                                                                 ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrGlobalL,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                in  sem_CBind_Bind_2)) of
                                         { ( sem_CBind_2) ->
                                         ( _lhsOgUniq,sem_CBind_2) }) }) }) }))
                    in  sem_CBind_Bind_1)) of
             { ( sem_CBind_1) ->
             ( _lhsObindLamMp,_lhsOnm,sem_CBind_1) }) }) }) }) })
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grTupFldL            : [(GrVal,GrVal->GrSplit)]
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = UID ->
                   ( UID,T_CBindAnn_1 )
type T_CBindAnn_1  = DataGam ->
                     LamMp ->
                     Int ->
                     HsName ->
                     EHCOpts ->
                     ( FvS,([(GrVal,GrVal->GrSplit)]),(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CBindAnn_Coe_1 :: T_CBindAnn_1 
                      sem_CBindAnn_Coe_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                (case ([]) of
                                 { _lhsOgrTupFldL ->
                                 (case (id) of
                                  { _lhsOgrWrapCase ->
                                  (case (idGrWrapCaseSel) of
                                   { _lhsOgrWrapCaseSel ->
                                   (case ([]) of
                                    { _lhsOnmL ->
                                    ( _lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }))
                  in  sem_CBindAnn_Coe_1)) of
           { ( sem_CBindAnn_1) ->
           ( _lhsOgUniq,sem_CBindAnn_1) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grTupFldL            : [(GrVal,GrVal->GrSplit)]
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
      alternative Nil:
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = UID ->
                    ( UID,T_CBindAnnL_1 )
type T_CBindAnnL_1  = DataGam ->
                      LamMp ->
                      Int ->
                      HsName ->
                      EHCOpts ->
                      ( FvS,([(GrVal,GrVal->GrSplit)]),(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (hd_ _hdOgUniq ) of
           { ( _hdIgUniq,hd_1) ->
               (case (_hdIgUniq) of
                { _tlOgUniq ->
                (case (tl_ _tlOgUniq ) of
                 { ( _tlIgUniq,tl_1) ->
                     (case (_tlIgUniq) of
                      { _lhsOgUniq ->
                      (case ((let sem_CBindAnnL_Cons_1 :: T_CBindAnnL_1 
                                  sem_CBindAnnL_Cons_1  =
                                      (\ _lhsIdataGam
                                         _lhsIlamMp
                                         _lhsIlev
                                         _lhsImodNm
                                         _lhsIopts ->
                                           (case (_lhsIopts) of
                                            { _tlOopts ->
                                            (case (_lhsImodNm) of
                                             { _tlOmodNm ->
                                             (case (_lhsIlev) of
                                              { _tlOlev ->
                                              (case (_lhsIlamMp) of
                                               { _tlOlamMp ->
                                               (case (_lhsIdataGam) of
                                                { _tlOdataGam ->
                                                (case (tl_1 _tlOdataGam _tlOlamMp _tlOlev _tlOmodNm _tlOopts ) of
                                                 { ( _tlIfvS,_tlIgrTupFldL,_tlIgrWrapCase,_tlIgrWrapCaseSel,_tlInmL) ->
                                                     (case (_lhsIopts) of
                                                      { _hdOopts ->
                                                      (case (_lhsImodNm) of
                                                       { _hdOmodNm ->
                                                       (case (_lhsIlev) of
                                                        { _hdOlev ->
                                                        (case (_lhsIlamMp) of
                                                         { _hdOlamMp ->
                                                         (case (_lhsIdataGam) of
                                                          { _hdOdataGam ->
                                                          (case (hd_1 _hdOdataGam _hdOlamMp _hdOlev _hdOmodNm _hdOopts ) of
                                                           { ( _hdIfvS,_hdIgrTupFldL,_hdIgrWrapCase,_hdIgrWrapCaseSel,_hdInmL) ->
                                                               (case (_hdIfvS `Set.union` _tlIfvS) of
                                                                { _lhsOfvS ->
                                                                (case (_hdIgrTupFldL ++ _tlIgrTupFldL) of
                                                                 { _lhsOgrTupFldL ->
                                                                 (case (_hdIgrWrapCase `const` _tlIgrWrapCase) of
                                                                  { _lhsOgrWrapCase ->
                                                                  (case (_hdIgrWrapCaseSel `const` _tlIgrWrapCaseSel) of
                                                                   { _lhsOgrWrapCaseSel ->
                                                                   (case (_hdInmL ++ _tlInmL) of
                                                                    { _lhsOnmL ->
                                                                    ( _lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                              in  sem_CBindAnnL_Cons_1)) of
                       { ( sem_CBindAnnL_1) ->
                       ( _lhsOgUniq,sem_CBindAnnL_1) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CBindAnnL_Nil_1 :: T_CBindAnnL_1 
                      sem_CBindAnnL_Nil_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                (case ([]) of
                                 { _lhsOgrTupFldL ->
                                 (case (id) of
                                  { _lhsOgrWrapCase ->
                                  (case (idGrWrapCaseSel) of
                                   { _lhsOgrWrapCaseSel ->
                                   (case ([]) of
                                    { _lhsOnmL ->
                                    ( _lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }))
                  in  sem_CBindAnnL_Nil_1)) of
           { ( sem_CBindAnnL_1) ->
           ( _lhsOgUniq,sem_CBindAnnL_1) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         bindLamMp            : LamMp
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         fvSMp                : FvSMp
         grBindL              : GrBindL
         grGlobalL            : GrGlobalL
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
      alternative Nil:
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ( LamMp,T_CBindL_1 )
type T_CBindL_1  = UID ->
                   ( UID,T_CBindL_2 )
type T_CBindL_2  = DataGam ->
                   EvalCtx ->
                   Bool ->
                   Bool ->
                   Bool ->
                   LamMp ->
                   CBindCateg ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   ( FvS,FvSMp,GrBindL,GrGlobalL,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIbindLamMp,tl_1) ->
         (case (hd_ ) of
          { ( _hdIbindLamMp,_hdInm,hd_1) ->
              (case (_hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp) of
               { _lhsObindLamMp ->
               (case ((let sem_CBindL_Cons_1 :: T_CBindL_1 
                           sem_CBindL_Cons_1  =
                               (\ _lhsIgUniq ->
                                    (case (_lhsIgUniq) of
                                     { _hdOgUniq ->
                                     (case (hd_1 _hdOgUniq ) of
                                      { ( _hdIgUniq,hd_2) ->
                                          (case (_hdIgUniq) of
                                           { _tlOgUniq ->
                                           (case (tl_1 _tlOgUniq ) of
                                            { ( _tlIgUniq,tl_2) ->
                                                (case (_tlIgUniq) of
                                                 { _lhsOgUniq ->
                                                 (case ((let sem_CBindL_Cons_2 :: T_CBindL_2 
                                                             sem_CBindL_Cons_2  =
                                                                 (\ _lhsIdataGam
                                                                    _lhsIevalCtx
                                                                    _lhsIisGlobal
                                                                    _lhsIisLamBody
                                                                    _lhsIisStrict
                                                                    _lhsIlamMp
                                                                    _lhsIletBindingsCateg
                                                                    _lhsIlev
                                                                    _lhsImodNm
                                                                    _lhsIopts ->
                                                                      (case (_lhsIopts) of
                                                                       { _tlOopts ->
                                                                       (case (_lhsImodNm) of
                                                                        { _tlOmodNm ->
                                                                        (case (_lhsIlev) of
                                                                         { _tlOlev ->
                                                                         (case (_lhsIletBindingsCateg) of
                                                                          { _tlOletBindingsCateg ->
                                                                          (case (_lhsIlamMp) of
                                                                           { _tlOlamMp ->
                                                                           (case (_lhsIisStrict) of
                                                                            { _tlOisStrict ->
                                                                            (case (_lhsIisLamBody) of
                                                                             { _tlOisLamBody ->
                                                                             (case (_lhsIisGlobal) of
                                                                              { _tlOisGlobal ->
                                                                              (case (_lhsIevalCtx) of
                                                                               { _tlOevalCtx ->
                                                                               (case (_lhsIdataGam) of
                                                                                { _tlOdataGam ->
                                                                                (case (tl_2 _tlOdataGam _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOmodNm _tlOopts ) of
                                                                                 { ( _tlIfvS,_tlIfvSMp,_tlIgrBindL,_tlIgrGlobalL,_tlInmL) ->
                                                                                     (case (_lhsIopts) of
                                                                                      { _hdOopts ->
                                                                                      (case (_lhsImodNm) of
                                                                                       { _hdOmodNm ->
                                                                                       (case (_lhsIlev) of
                                                                                        { _hdOlev ->
                                                                                        (case (_lhsIletBindingsCateg) of
                                                                                         { _hdOletBindingsCateg ->
                                                                                         (case (_lhsIlamMp) of
                                                                                          { _hdOlamMp ->
                                                                                          (case (_lhsIisStrict) of
                                                                                           { _hdOisStrict ->
                                                                                           (case (_lhsIisLamBody) of
                                                                                            { _hdOisLamBody ->
                                                                                            (case (_lhsIisGlobal) of
                                                                                             { _hdOisGlobal ->
                                                                                             (case (_lhsIevalCtx) of
                                                                                              { _hdOevalCtx ->
                                                                                              (case (_lhsIdataGam) of
                                                                                               { _hdOdataGam ->
                                                                                               (case (hd_2 _hdOdataGam _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOmodNm _hdOopts ) of
                                                                                                { ( _hdIfvS,_hdIfvSMp,_hdIgrBindL,_hdIgrGlobalL,_hdInmL) ->
                                                                                                    (case (_hdIfvS `Set.union` _tlIfvS) of
                                                                                                     { _lhsOfvS ->
                                                                                                     (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                                                                                                      { _lhsOfvSMp ->
                                                                                                      (case (_hdIgrBindL ++ _tlIgrBindL) of
                                                                                                       { _lhsOgrBindL ->
                                                                                                       (case (_hdIgrGlobalL ++ _tlIgrGlobalL) of
                                                                                                        { _lhsOgrGlobalL ->
                                                                                                        (case (_hdInmL ++ _tlInmL) of
                                                                                                         { _lhsOnmL ->
                                                                                                         ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrGlobalL,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                         in  sem_CBindL_Cons_2)) of
                                                  { ( sem_CBindL_2) ->
                                                  ( _lhsOgUniq,sem_CBindL_2) }) }) }) }) }) }))
                       in  sem_CBindL_Cons_1)) of
                { ( sem_CBindL_1) ->
                ( _lhsObindLamMp,sem_CBindL_1) }) }) }) })
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (case (Map.empty) of
     { _lhsObindLamMp ->
     (case ((let sem_CBindL_Nil_1 :: T_CBindL_1 
                 sem_CBindL_Nil_1  =
                     (\ _lhsIgUniq ->
                          (case (_lhsIgUniq) of
                           { _lhsOgUniq ->
                           (case ((let sem_CBindL_Nil_2 :: T_CBindL_2 
                                       sem_CBindL_Nil_2  =
                                           (\ _lhsIdataGam
                                              _lhsIevalCtx
                                              _lhsIisGlobal
                                              _lhsIisLamBody
                                              _lhsIisStrict
                                              _lhsIlamMp
                                              _lhsIletBindingsCateg
                                              _lhsIlev
                                              _lhsImodNm
                                              _lhsIopts ->
                                                (case (Set.empty) of
                                                 { _lhsOfvS ->
                                                 (case (Map.empty) of
                                                  { _lhsOfvSMp ->
                                                  (case ([]) of
                                                   { _lhsOgrBindL ->
                                                   (case ([]) of
                                                    { _lhsOgrGlobalL ->
                                                    (case ([]) of
                                                     { _lhsOnmL ->
                                                     ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrGlobalL,_lhsOnmL) }) }) }) }) }))
                                   in  sem_CBindL_Nil_2)) of
                            { ( sem_CBindL_2) ->
                            ( _lhsOgUniq,sem_CBindL_2) }) }))
             in  sem_CBindL_Nil_1)) of
      { ( sem_CBindL_1) ->
      ( _lhsObindLamMp,sem_CBindL_1) }) })
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nm                   : HsName
      synthesized attribute:
         bindLamMp            : LamMp
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataGam              : DataGam
         doBox                : Bool
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         fvSMp                : FvSMp
         grBindL              : GrBindL
         grExpr               : GrExpr
         grGlobalL            : GrGlobalL
         grVal                : GrVal
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 2:
            local whatAbove   : {WhatExpr}
            local aspectKeyS  : _
            local nmAsp       : _
            local grBindMeta  : _
            local isApply0    : _
            local _tup1       : {(GrBindL,GrGlobalL)}
            local grExpr      : _
            local grVal       : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 2:
            local whatAbove   : {WhatExpr}
            local grExpr      : _
            local grVal       : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 2:
            local grExpr      : _
            local grVal       : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 2:
            local grExpr      : _
            local grVal       : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 2:
            local grExpr      : _
            local grVal       : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 2:
            local whatAbove   : {WhatExpr}
            local nmAsp       : _
            local grBindMeta  : _
            local isApply0    : _
            local _tup2       : {(GrBindL,GrGlobalL)}
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
type T_CBound  = HsName ->
                 ( LamMp,T_CBound_1 )
type T_CBound_1  = UID ->
                   ( UID,T_CBound_2 )
type T_CBound_2  = DataGam ->
                   Bool ->
                   EvalCtx ->
                   Bool ->
                   Bool ->
                   Bool ->
                   Bool ->
                   Bool ->
                   LamMp ->
                   CBindCateg ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   ( FvS,FvSMp,GrBindL,GrExpr,GrGlobalL,GrVal,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsInm ->
         (case (expr_ ) of
          { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
              (case (Map.singleton _lhsInm
                                   (emptyLamInfo {laminfoArity = length _exprIgrLamArgL})) of
               { _lhsObindLamMp ->
               (case ((let sem_CBound_Bind_1 :: T_CBound_1 
                           sem_CBound_Bind_1  =
                               (\ _lhsIgUniq ->
                                    (case (_lhsIgUniq) of
                                     { _exprOgUniq ->
                                     (case (expr_1 _exprOgUniq ) of
                                      { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                          (case (_exprIgUniq) of
                                           { _lhsOgUniq ->
                                           (case ((let sem_CBound_Bind_2 :: T_CBound_2 
                                                       sem_CBound_Bind_2  =
                                                           (\ _lhsIdataGam
                                                              _lhsIdoBox
                                                              _lhsIevalCtx
                                                              _lhsIisGlobal
                                                              _lhsIisLamBody
                                                              _lhsIisStrict
                                                              _lhsIisTopApp
                                                              _lhsIisTopTup
                                                              _lhsIlamMp
                                                              _lhsIletBindingsCateg
                                                              _lhsIlev
                                                              _lhsImodNm
                                                              _lhsIopts ->
                                                                (case (ExprIsBind) of
                                                                 { _whatAbove ->
                                                                 (case (_whatAbove) of
                                                                  { _exprOwhatAbove ->
                                                                  (case (_lhsIopts) of
                                                                   { _exprOopts ->
                                                                   (case (_lhsImodNm) of
                                                                    { _exprOmodNm ->
                                                                    (case (_lhsIlev) of
                                                                     { _exprOlev ->
                                                                     (case (_lhsIlamMp) of
                                                                      { _exprOlamMp ->
                                                                      (case (_lhsIisLamBody) of
                                                                       { _exprOisLamBody ->
                                                                       (case (_lhsIevalCtx) of
                                                                        { _exprOevalCtx ->
                                                                        (case (_lhsIdataGam) of
                                                                         { _exprOdataGam ->
                                                                         (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                                                                          { _exprOisStrict ->
                                                                          (case (True) of
                                                                           { _exprOisTopTup ->
                                                                           (case (True) of
                                                                            { _exprOisTopApp ->
                                                                            (case (True) of
                                                                             { _exprOdoBox ->
                                                                             (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                              { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                  (case (_lhsIopts) of
                                                                                   { _bindMetaOopts ->
                                                                                   (case (_lhsImodNm) of
                                                                                    { _bindMetaOmodNm ->
                                                                                    (case (_lhsIlev) of
                                                                                     { _bindMetaOlev ->
                                                                                     (case (_lhsIlamMp) of
                                                                                      { _bindMetaOlamMp ->
                                                                                      (case (_lhsIdataGam) of
                                                                                       { _bindMetaOdataGam ->
                                                                                       (case (bindMeta_ _bindMetaOdataGam _bindMetaOlamMp _bindMetaOlev _bindMetaOmodNm _bindMetaOopts ) of
                                                                                        { ( _bindMetaIfvS,_bindMetaIgrBindAnn,_bindMetaIisApply0) ->
                                                                                            (case (_bindMetaIfvS `Set.union` _exprIfvS) of
                                                                                             { _lhsOfvS ->
                                                                                             (case (Map.empty) of
                                                                                              { _lhsOfvSMp ->
                                                                                              (case (acbaspkeyNone) of
                                                                                               { _aspectKeyS ->
                                                                                               (case (hsnUniqifyACoreBindAspectKeyS _aspectKeyS _lhsInm) of
                                                                                                { _nmAsp ->
                                                                                                (case (_bindMetaIgrBindAnn) of
                                                                                                 { _grBindMeta ->
                                                                                                 (case (_bindMetaIisApply0) of
                                                                                                  { _isApply0 ->
                                                                                                  (case (if   _isApply0
                                                                                                         then let nm2 = case _exprIwhatBelow of
                                                                                                                          ExprIsVar n -> n
                                                                                                                          _           -> error "ToGrin: RHS of Apply0-bind is not a variable"
                                                                                                              in  ( []
                                                                                                                  , [GrGlobal_Global _nmAsp (GrVal_Node (GrTag_Fun nm2) [])]
                                                                                                                  )
                                                                                                         else     ( [GrBind_Bind _nmAsp _grBindMeta _exprIgrLamArgL _exprIgrLamBody]
                                                                                                                  , []
                                                                                                                  )) of
                                                                                                   { __tup1 ->
                                                                                                   (case (__tup1) of
                                                                                                    { (_lhsOgrBindL,_) ->
                                                                                                    (case (GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                                     { _grExpr ->
                                                                                                     (case (_grExpr) of
                                                                                                      { _lhsOgrExpr ->
                                                                                                      (case (__tup1) of
                                                                                                       { (_,_lhsOgrGlobalL) ->
                                                                                                       (case (GrVal_Empty) of
                                                                                                        { _grVal ->
                                                                                                        (case (_grVal) of
                                                                                                         { _lhsOgrVal ->
                                                                                                         (case ([]) of
                                                                                                          { _lhsOnmL ->
                                                                                                          ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                   in  sem_CBound_Bind_2)) of
                                            { ( sem_CBound_2) ->
                                            ( _lhsOgUniq,sem_CBound_2) }) }) }) }))
                       in  sem_CBound_Bind_1)) of
                { ( sem_CBound_1) ->
                ( _lhsObindLamMp,sem_CBound_1) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBound_FFE_1 :: T_CBound_1 
                      sem_CBound_FFE_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _exprOgUniq ->
                                (case (expr_ ) of
                                 { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                     (case (expr_1 _exprOgUniq ) of
                                      { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                          (case (_exprIgUniq) of
                                           { _lhsOgUniq ->
                                           (case ((let sem_CBound_FFE_2 :: T_CBound_2 
                                                       sem_CBound_FFE_2  =
                                                           (\ _lhsIdataGam
                                                              _lhsIdoBox
                                                              _lhsIevalCtx
                                                              _lhsIisGlobal
                                                              _lhsIisLamBody
                                                              _lhsIisStrict
                                                              _lhsIisTopApp
                                                              _lhsIisTopTup
                                                              _lhsIlamMp
                                                              _lhsIletBindingsCateg
                                                              _lhsIlev
                                                              _lhsImodNm
                                                              _lhsIopts ->
                                                                (case (ExprIsLam) of
                                                                 { _whatAbove ->
                                                                 (case (_whatAbove) of
                                                                  { _exprOwhatAbove ->
                                                                  (case (_lhsIopts) of
                                                                   { _exprOopts ->
                                                                   (case (_lhsImodNm) of
                                                                    { _exprOmodNm ->
                                                                    (case (_lhsIlev) of
                                                                     { _exprOlev ->
                                                                     (case (_lhsIlamMp) of
                                                                      { _exprOlamMp ->
                                                                      (case (_lhsIisLamBody) of
                                                                       { _exprOisLamBody ->
                                                                       (case (_lhsIevalCtx) of
                                                                        { _exprOevalCtx ->
                                                                        (case (_lhsIdataGam) of
                                                                         { _exprOdataGam ->
                                                                         (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                                                                          { _exprOisStrict ->
                                                                          (case (True) of
                                                                           { _exprOisTopTup ->
                                                                           (case (True) of
                                                                            { _exprOisTopApp ->
                                                                            (case (True) of
                                                                             { _exprOdoBox ->
                                                                             (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                              { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                  (case (_exprIfvS) of
                                                                                   { _lhsOfvS ->
                                                                                   (case (Map.empty) of
                                                                                    { _lhsOfvSMp ->
                                                                                    (case ([]) of
                                                                                     { _lhsOgrBindL ->
                                                                                     (case (GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                      { _grExpr ->
                                                                                      (case (_grExpr) of
                                                                                       { _lhsOgrExpr ->
                                                                                       (case (_exprIgrGlobalL) of
                                                                                        { _lhsOgrGlobalL ->
                                                                                        (case (GrVal_Empty) of
                                                                                         { _grVal ->
                                                                                         (case (_grVal) of
                                                                                          { _lhsOgrVal ->
                                                                                          (case ([]) of
                                                                                           { _lhsOnmL ->
                                                                                           ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                   in  sem_CBound_FFE_2)) of
                                            { ( sem_CBound_2) ->
                                            ( _lhsOgUniq,sem_CBound_2) }) }) }) }) }))
                  in  sem_CBound_FFE_1)) of
           { ( sem_CBound_1) ->
           ( _lhsObindLamMp,sem_CBound_1) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBound_Meta_1 :: T_CBound_1 
                      sem_CBound_Meta_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq ->
                                (case ((let sem_CBound_Meta_2 :: T_CBound_2 
                                            sem_CBound_Meta_2  =
                                                (\ _lhsIdataGam
                                                   _lhsIdoBox
                                                   _lhsIevalCtx
                                                   _lhsIisGlobal
                                                   _lhsIisLamBody
                                                   _lhsIisStrict
                                                   _lhsIisTopApp
                                                   _lhsIisTopTup
                                                   _lhsIlamMp
                                                   _lhsIletBindingsCateg
                                                   _lhsIlev
                                                   _lhsImodNm
                                                   _lhsIopts ->
                                                     (case (_lhsIopts) of
                                                      { _cmetasOopts ->
                                                      (case (_lhsImodNm) of
                                                       { _cmetasOmodNm ->
                                                       (case (_lhsIlev) of
                                                        { _cmetasOlev ->
                                                        (case (_lhsIlamMp) of
                                                         { _cmetasOlamMp ->
                                                         (case (_lhsIdataGam) of
                                                          { _cmetasOdataGam ->
                                                          (case (cmetas_ _cmetasOdataGam _cmetasOlamMp _cmetasOlev _cmetasOmodNm _cmetasOopts ) of
                                                           { ( _cmetasIfvS,_cmetasIgrBindAnn,_cmetasIisApply0) ->
                                                               (case (_cmetasIfvS) of
                                                                { _lhsOfvS ->
                                                                (case (Map.empty) of
                                                                 { _lhsOfvSMp ->
                                                                 (case ([]) of
                                                                  { _lhsOgrBindL ->
                                                                  (case (GrExpr_Unit GrVal_Empty GrType_None) of
                                                                   { _grExpr ->
                                                                   (case (_grExpr) of
                                                                    { _lhsOgrExpr ->
                                                                    (case ([]) of
                                                                     { _lhsOgrGlobalL ->
                                                                     (case (GrVal_Empty) of
                                                                      { _grVal ->
                                                                      (case (_grVal) of
                                                                       { _lhsOgrVal ->
                                                                       (case ([]) of
                                                                        { _lhsOnmL ->
                                                                        ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                        in  sem_CBound_Meta_2)) of
                                 { ( sem_CBound_2) ->
                                 ( _lhsOgUniq,sem_CBound_2) }) }))
                  in  sem_CBound_Meta_1)) of
           { ( sem_CBound_1) ->
           ( _lhsObindLamMp,sem_CBound_1) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBound_RelevTy_1 :: T_CBound_1 
                      sem_CBound_RelevTy_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq ->
                                (case ((let sem_CBound_RelevTy_2 :: T_CBound_2 
                                            sem_CBound_RelevTy_2  =
                                                (\ _lhsIdataGam
                                                   _lhsIdoBox
                                                   _lhsIevalCtx
                                                   _lhsIisGlobal
                                                   _lhsIisLamBody
                                                   _lhsIisStrict
                                                   _lhsIisTopApp
                                                   _lhsIisTopTup
                                                   _lhsIlamMp
                                                   _lhsIletBindingsCateg
                                                   _lhsIlev
                                                   _lhsImodNm
                                                   _lhsIopts ->
                                                     (case (Set.empty) of
                                                      { _lhsOfvS ->
                                                      (case (Map.empty) of
                                                       { _lhsOfvSMp ->
                                                       (case ([]) of
                                                        { _lhsOgrBindL ->
                                                        (case (GrExpr_Unit GrVal_Empty GrType_None) of
                                                         { _grExpr ->
                                                         (case (_grExpr) of
                                                          { _lhsOgrExpr ->
                                                          (case ([]) of
                                                           { _lhsOgrGlobalL ->
                                                           (case (GrVal_Empty) of
                                                            { _grVal ->
                                                            (case (_grVal) of
                                                             { _lhsOgrVal ->
                                                             (case ([]) of
                                                              { _lhsOnmL ->
                                                              ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }))
                                        in  sem_CBound_RelevTy_2)) of
                                 { ( sem_CBound_2) ->
                                 ( _lhsOgUniq,sem_CBound_2) }) }))
                  in  sem_CBound_RelevTy_1)) of
           { ( sem_CBound_1) ->
           ( _lhsObindLamMp,sem_CBound_1) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBound_Ty_1 :: T_CBound_1 
                      sem_CBound_Ty_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq ->
                                (case ((let sem_CBound_Ty_2 :: T_CBound_2 
                                            sem_CBound_Ty_2  =
                                                (\ _lhsIdataGam
                                                   _lhsIdoBox
                                                   _lhsIevalCtx
                                                   _lhsIisGlobal
                                                   _lhsIisLamBody
                                                   _lhsIisStrict
                                                   _lhsIisTopApp
                                                   _lhsIisTopTup
                                                   _lhsIlamMp
                                                   _lhsIletBindingsCateg
                                                   _lhsIlev
                                                   _lhsImodNm
                                                   _lhsIopts ->
                                                     (case (Set.empty) of
                                                      { _lhsOfvS ->
                                                      (case (Map.empty) of
                                                       { _lhsOfvSMp ->
                                                       (case ([]) of
                                                        { _lhsOgrBindL ->
                                                        (case (GrExpr_Unit GrVal_Empty GrType_None) of
                                                         { _grExpr ->
                                                         (case (_grExpr) of
                                                          { _lhsOgrExpr ->
                                                          (case ([]) of
                                                           { _lhsOgrGlobalL ->
                                                           (case (GrVal_Empty) of
                                                            { _grVal ->
                                                            (case (_grVal) of
                                                             { _lhsOgrVal ->
                                                             (case ([]) of
                                                              { _lhsOnmL ->
                                                              ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }))
                                        in  sem_CBound_Ty_2)) of
                                 { ( sem_CBound_2) ->
                                 ( _lhsOgUniq,sem_CBound_2) }) }))
                  in  sem_CBound_Ty_1)) of
           { ( sem_CBound_1) ->
           ( _lhsObindLamMp,sem_CBound_1) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBound_Val_1 :: T_CBound_1 
                      sem_CBound_Val_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _exprOgUniq ->
                                (case (expr_ ) of
                                 { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                     (case (expr_1 _exprOgUniq ) of
                                      { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                          (case (_exprIgUniq) of
                                           { _lhsOgUniq ->
                                           (case ((let sem_CBound_Val_2 :: T_CBound_2 
                                                       sem_CBound_Val_2  =
                                                           (\ _lhsIdataGam
                                                              _lhsIdoBox
                                                              _lhsIevalCtx
                                                              _lhsIisGlobal
                                                              _lhsIisLamBody
                                                              _lhsIisStrict
                                                              _lhsIisTopApp
                                                              _lhsIisTopTup
                                                              _lhsIlamMp
                                                              _lhsIletBindingsCateg
                                                              _lhsIlev
                                                              _lhsImodNm
                                                              _lhsIopts ->
                                                                (case (ExprIsBind) of
                                                                 { _whatAbove ->
                                                                 (case (_whatAbove) of
                                                                  { _exprOwhatAbove ->
                                                                  (case (_lhsIopts) of
                                                                   { _exprOopts ->
                                                                   (case (_lhsImodNm) of
                                                                    { _exprOmodNm ->
                                                                    (case (_lhsIlev) of
                                                                     { _exprOlev ->
                                                                     (case (_lhsIlamMp) of
                                                                      { _exprOlamMp ->
                                                                      (case (_lhsIisTopTup) of
                                                                       { _exprOisTopTup ->
                                                                       (case (_lhsIisTopApp) of
                                                                        { _exprOisTopApp ->
                                                                        (case (_lhsIisLamBody) of
                                                                         { _exprOisLamBody ->
                                                                         (case (_lhsIevalCtx) of
                                                                          { _exprOevalCtx ->
                                                                          (case (_lhsIdataGam) of
                                                                           { _exprOdataGam ->
                                                                           (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                                                                            { _exprOisStrict ->
                                                                            (case (True) of
                                                                             { _exprOdoBox ->
                                                                             (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                              { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                  (case (_exprIfvS) of
                                                                                   { _lhsOfvS ->
                                                                                   (case (Map.empty) of
                                                                                    { _lhsOfvSMp ->
                                                                                    (case (hsnUniqifyACoreBindAspectKeyS aspectKeyS_ _lhsInm) of
                                                                                     { _nmAsp ->
                                                                                     (case (GrBindAnnNormal) of
                                                                                      { _grBindMeta ->
                                                                                      (case (False) of
                                                                                       { _isApply0 ->
                                                                                       (case (if   _isApply0
                                                                                              then let nm2 = case _exprIwhatBelow of
                                                                                                               ExprIsVar n -> n
                                                                                                               _           -> error "ToGrin: RHS of Apply0-bind is not a variable"
                                                                                                   in  ( []
                                                                                                       , [GrGlobal_Global _nmAsp (GrVal_Node (GrTag_Fun nm2) [])]
                                                                                                       )
                                                                                              else     ( [GrBind_Bind _nmAsp _grBindMeta _exprIgrLamArgL _exprIgrLamBody]
                                                                                                       , []
                                                                                                       )) of
                                                                                        { __tup2 ->
                                                                                        (case (__tup2) of
                                                                                         { (_lhsOgrBindL,_) ->
                                                                                         (case (_exprIgrExpr) of
                                                                                          { _lhsOgrExpr ->
                                                                                          (case (__tup2) of
                                                                                           { (_,_lhsOgrGlobalL) ->
                                                                                           (case (_exprIgrVal) of
                                                                                            { _lhsOgrVal ->
                                                                                            (case ([]) of
                                                                                             { _lhsOnmL ->
                                                                                             ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrVal,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                   in  sem_CBound_Val_2)) of
                                            { ( sem_CBound_2) ->
                                            ( _lhsOgUniq,sem_CBound_2) }) }) }) }) }))
                  in  sem_CBound_Val_1)) of
           { ( sem_CBound_1) ->
           ( _lhsObindLamMp,sem_CBound_1) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nm                   : HsName
      synthesized attribute:
         bindLamMp            : LamMp
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         fvSMp                : FvSMp
         grBindL              : GrBindL
         grGlobalL            : GrGlobalL
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
      alternative Nil:
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = HsName ->
                  ( LamMp,T_CBoundL_1 )
type T_CBoundL_1  = UID ->
                    ( UID,T_CBoundL_2 )
type T_CBoundL_2  = DataGam ->
                    EvalCtx ->
                    Bool ->
                    Bool ->
                    Bool ->
                    LamMp ->
                    CBindCateg ->
                    Int ->
                    HsName ->
                    EHCOpts ->
                    ( FvS,FvSMp,GrBindL,GrGlobalL,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsInm ->
         (case (_lhsInm) of
          { _tlOnm ->
          (case (_lhsInm) of
           { _hdOnm ->
           (case (tl_ _tlOnm ) of
            { ( _tlIbindLamMp,tl_1) ->
                (case (hd_ _hdOnm ) of
                 { ( _hdIbindLamMp,hd_1) ->
                     (case (_hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp) of
                      { _lhsObindLamMp ->
                      (case ((let sem_CBoundL_Cons_1 :: T_CBoundL_1 
                                  sem_CBoundL_Cons_1  =
                                      (\ _lhsIgUniq ->
                                           (case (_lhsIgUniq) of
                                            { _hdOgUniq ->
                                            (case (hd_1 _hdOgUniq ) of
                                             { ( _hdIgUniq,hd_2) ->
                                                 (case (_hdIgUniq) of
                                                  { _tlOgUniq ->
                                                  (case (tl_1 _tlOgUniq ) of
                                                   { ( _tlIgUniq,tl_2) ->
                                                       (case (_tlIgUniq) of
                                                        { _lhsOgUniq ->
                                                        (case ((let sem_CBoundL_Cons_2 :: T_CBoundL_2 
                                                                    sem_CBoundL_Cons_2  =
                                                                        (\ _lhsIdataGam
                                                                           _lhsIevalCtx
                                                                           _lhsIisGlobal
                                                                           _lhsIisLamBody
                                                                           _lhsIisStrict
                                                                           _lhsIlamMp
                                                                           _lhsIletBindingsCateg
                                                                           _lhsIlev
                                                                           _lhsImodNm
                                                                           _lhsIopts ->
                                                                             (case (_lhsIopts) of
                                                                              { _tlOopts ->
                                                                              (case (_lhsImodNm) of
                                                                               { _tlOmodNm ->
                                                                               (case (_lhsIlev) of
                                                                                { _tlOlev ->
                                                                                (case (_lhsIletBindingsCateg) of
                                                                                 { _tlOletBindingsCateg ->
                                                                                 (case (_lhsIlamMp) of
                                                                                  { _tlOlamMp ->
                                                                                  (case (_lhsIisStrict) of
                                                                                   { _tlOisStrict ->
                                                                                   (case (_lhsIisLamBody) of
                                                                                    { _tlOisLamBody ->
                                                                                    (case (_lhsIisGlobal) of
                                                                                     { _tlOisGlobal ->
                                                                                     (case (_lhsIevalCtx) of
                                                                                      { _tlOevalCtx ->
                                                                                      (case (_lhsIdataGam) of
                                                                                       { _tlOdataGam ->
                                                                                       (case (tl_2 _tlOdataGam _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOmodNm _tlOopts ) of
                                                                                        { ( _tlIfvS,_tlIfvSMp,_tlIgrBindL,_tlIgrGlobalL,_tlInmL) ->
                                                                                            (case (_lhsIopts) of
                                                                                             { _hdOopts ->
                                                                                             (case (_lhsImodNm) of
                                                                                              { _hdOmodNm ->
                                                                                              (case (_lhsIlev) of
                                                                                               { _hdOlev ->
                                                                                               (case (_lhsIletBindingsCateg) of
                                                                                                { _hdOletBindingsCateg ->
                                                                                                (case (_lhsIlamMp) of
                                                                                                 { _hdOlamMp ->
                                                                                                 (case (_lhsIisStrict) of
                                                                                                  { _hdOisStrict ->
                                                                                                  (case (_lhsIisLamBody) of
                                                                                                   { _hdOisLamBody ->
                                                                                                   (case (_lhsIisGlobal) of
                                                                                                    { _hdOisGlobal ->
                                                                                                    (case (_lhsIevalCtx) of
                                                                                                     { _hdOevalCtx ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _hdOdataGam ->
                                                                                                      (case (True) of
                                                                                                       { _hdOisTopTup ->
                                                                                                       (case (True) of
                                                                                                        { _hdOisTopApp ->
                                                                                                        (case (True) of
                                                                                                         { _hdOdoBox ->
                                                                                                         (case (hd_2 _hdOdataGam _hdOdoBox _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOmodNm _hdOopts ) of
                                                                                                          { ( _hdIfvS,_hdIfvSMp,_hdIgrBindL,_hdIgrExpr,_hdIgrGlobalL,_hdIgrVal,_hdInmL) ->
                                                                                                              (case (_hdIfvS `Set.union` _tlIfvS) of
                                                                                                               { _lhsOfvS ->
                                                                                                               (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                                                                                                                { _lhsOfvSMp ->
                                                                                                                (case (_hdIgrBindL ++ _tlIgrBindL) of
                                                                                                                 { _lhsOgrBindL ->
                                                                                                                 (case (_hdIgrGlobalL ++ _tlIgrGlobalL) of
                                                                                                                  { _lhsOgrGlobalL ->
                                                                                                                  (case (_hdInmL ++ _tlInmL) of
                                                                                                                   { _lhsOnmL ->
                                                                                                                   ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrGlobalL,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                in  sem_CBoundL_Cons_2)) of
                                                         { ( sem_CBoundL_2) ->
                                                         ( _lhsOgUniq,sem_CBoundL_2) }) }) }) }) }) }))
                              in  sem_CBoundL_Cons_1)) of
                       { ( sem_CBoundL_1) ->
                       ( _lhsObindLamMp,sem_CBoundL_1) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsInm ->
         (case (Map.empty) of
          { _lhsObindLamMp ->
          (case ((let sem_CBoundL_Nil_1 :: T_CBoundL_1 
                      sem_CBoundL_Nil_1  =
                          (\ _lhsIgUniq ->
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq ->
                                (case ((let sem_CBoundL_Nil_2 :: T_CBoundL_2 
                                            sem_CBoundL_Nil_2  =
                                                (\ _lhsIdataGam
                                                   _lhsIevalCtx
                                                   _lhsIisGlobal
                                                   _lhsIisLamBody
                                                   _lhsIisStrict
                                                   _lhsIlamMp
                                                   _lhsIletBindingsCateg
                                                   _lhsIlev
                                                   _lhsImodNm
                                                   _lhsIopts ->
                                                     (case (Set.empty) of
                                                      { _lhsOfvS ->
                                                      (case (Map.empty) of
                                                       { _lhsOfvSMp ->
                                                       (case ([]) of
                                                        { _lhsOgrBindL ->
                                                        (case ([]) of
                                                         { _lhsOgrGlobalL ->
                                                         (case ([]) of
                                                          { _lhsOnmL ->
                                                          ( _lhsOfvS,_lhsOfvSMp,_lhsOgrBindL,_lhsOgrGlobalL,_lhsOnmL) }) }) }) }) }))
                                        in  sem_CBoundL_Nil_2)) of
                                 { ( sem_CBoundL_2) ->
                                 ( _lhsOgUniq,sem_CBoundL_2) }) }))
                  in  sem_CBoundL_Nil_1)) of
           { ( sem_CBoundL_1) ->
           ( _lhsObindLamMp,sem_CBoundL_1) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathLamMp            : LamMp
         grLamArgL            : [HsName]
   visit 1:
      chained attribute:
         gUniq                : UID
      synthesized attribute:
         whatBelow            : WhatExpr
   visit 2:
      inherited attributes:
         dataGam              : DataGam
         doBox                : Bool
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
         whatAbove            : WhatExpr
      synthesized attributes:
         appFunKind           : AppFunKind
         fvS                  : FvS
         grAppArgL            : GrValL
         grAppFun             : HsName
         grBindL              : GrBindL
         grExpr               : GrExpr
         grGlobalL            : GrGlobalL
         grLamBody            : GrExpr
         grLetBody            : GrExpr
         grTupFldL            : TupAdaptFldL
         grTupRec             : GrVal
         grVal                : GrVal
         mbFFIApp             : Maybe ( Ty
                                  , Bool
                                  , FFIWay
                                  , ForeignEnt
                                  , [Ty]
                                  )
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 2:
            local grExpr      : _
            local grVal       : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 1:
            local _tup3       : {(UID,UID,UID)}
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local doBox       : _
            local letBindingsCateg : _
            local isGlobal    : _
            local fvS         : _
            local grAppArgL   : _
            local grAppFun    : _
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local appArity    : _
            local mbLam       : _
            local grExpr      : _
            local grTupFldL   : _
            local grVal       : _
            local grTupRec    : _
            intra _tup3       : {(UID,UID,UID)}
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 1:
            local _tup4       : {(UID,UID)}
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local lUniq       : {UID}
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
            intra _tup4       : {(UID,UID)}
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative Char:
         child char           : {Char}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative CoeArg:
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 1:
            local _tup5       : {(UID,UID)}
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local lUniq       : {UID}
            local foreignEntInfo : _
            local mbPrimNeedEval : {Maybe PrimitiveNeedsEval}
            local primResNeedsEval : {Bool}
            local argTyLresTy : {( TyL, Ty )}
            local resTy       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
            local argTyL      : {TyL}
            intra _tup5       : {(UID,UID)}
      alternative Hole:
         child uid            : {UID}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 1:
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 1:
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 1:
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative Int:
         child int            : {Int}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative Integer:
         child integer        : {Integer}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local argNm       : _
         visit 1:
            local whatBelow   : _
            intra argNm       : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local lev         : _
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local fvS         : _
            local grVal       : _
            local letBindingsCateg : _
            local isGlobal    : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
            intra argNm       : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 1:
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local evalCtx     : _
            local doBox       : _
            local letBindingsCateg : _
            local isGlobal    : _
            local fvS         : _
            local grVal       : _
            local _tup6       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative String:
         child str            : {String}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative Tup:
         child tag            : {CTag}
         visit 1:
            local whatBelow   : _
         visit 2:
            local grVal       : _
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 1:
            local _tup7       : {(UID,UID)}
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local lUniq       : {UID}
            local grTupFldL   : _
            local grTupRec    : _
            local grExpr      : _
            intra _tup7       : {(UID,UID)}
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 1:
            local _tup8       : {(UID,UID)}
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local lUniq       : {UID}
            local grTupFldL   : _
            local grTupRec    : _
            local grExpr      : _
            intra _tup8       : {(UID,UID)}
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 1:
            local _tup9       : {(UID,UID)}
            local whatBelow   : _
         visit 2:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local doBox       : _
            local grVal       : _
            local lUniq       : {UID}
            local grTupFldL   : _
            local grTupRec    : _
            local grExpr      : _
            intra _tup9       : {(UID,UID)}
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 1:
            local _tup10      : {(UID,UID)}
            local nm          : {HsName}
            local whatBelow   : _
         visit 2:
            local nmAsp       : {HsName}
            local mbLam       : _
            local grVal       : _
            local lUniq       : {UID}
            local grExpr      : _
            local grTupFldL   : _
            local grTupRec    : _
            local mbVar       : {Maybe HsName}
            intra nm          : {HsName}
            intra _tup10      : {(UID,UID)}
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
type T_CExpr  = ( LamMp,([HsName]),T_CExpr_1 )
type T_CExpr_1  = UID ->
                  ( UID,WhatExpr,T_CExpr_2 )
type T_CExpr_2  = DataGam ->
                  Bool ->
                  EvalCtx ->
                  Bool ->
                  Bool ->
                  Bool ->
                  Bool ->
                  LamMp ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  WhatExpr ->
                  ( AppFunKind,FvS,GrValL,HsName,GrBindL,GrExpr,GrGlobalL,GrExpr,GrExpr,TupAdaptFldL,GrVal,GrVal,(Maybe ( Ty
                                                                                                                                                    , Bool
                                                                                                                                                    , FFIWay
                                                                                                                                                    , ForeignEnt
                                                                                                                                                    , [Ty]
                                                                                                                                                    )),(Maybe [HsName]),(Maybe HsName))
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (case (expr_ ) of
     { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
         (case (_exprIgathLamMp) of
          { _lhsOgathLamMp ->
          (case (_exprIgrLamArgL) of
           { _lhsOgrLamArgL ->
           (case ((let sem_CExpr_Ann_1 :: T_CExpr_1 
                       sem_CExpr_Ann_1  =
                           (\ _lhsIgUniq ->
                                (case (_lhsIgUniq) of
                                 { _annOgUniq ->
                                 (case (ann_ _annOgUniq ) of
                                  { ( _annIgUniq,ann_1) ->
                                      (case (_annIgUniq) of
                                       { _exprOgUniq ->
                                       (case (expr_1 _exprOgUniq ) of
                                        { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                            (case (_exprIgUniq) of
                                             { _lhsOgUniq ->
                                             (case (_exprIwhatBelow) of
                                              { _lhsOwhatBelow ->
                                              (case ((let sem_CExpr_Ann_2 :: T_CExpr_2 
                                                          sem_CExpr_Ann_2  =
                                                              (\ _lhsIdataGam
                                                                 _lhsIdoBox
                                                                 _lhsIevalCtx
                                                                 _lhsIisLamBody
                                                                 _lhsIisStrict
                                                                 _lhsIisTopApp
                                                                 _lhsIisTopTup
                                                                 _lhsIlamMp
                                                                 _lhsIlev
                                                                 _lhsImodNm
                                                                 _lhsIopts
                                                                 _lhsIwhatAbove ->
                                                                   (case (_lhsIwhatAbove) of
                                                                    { _exprOwhatAbove ->
                                                                    (case (_lhsIopts) of
                                                                     { _exprOopts ->
                                                                     (case (_lhsImodNm) of
                                                                      { _exprOmodNm ->
                                                                      (case (_lhsIlev) of
                                                                       { _exprOlev ->
                                                                       (case (_lhsIlamMp) of
                                                                        { _exprOlamMp ->
                                                                        (case (_lhsIisTopTup) of
                                                                         { _exprOisTopTup ->
                                                                         (case (_lhsIisTopApp) of
                                                                          { _exprOisTopApp ->
                                                                          (case (_lhsIisStrict) of
                                                                           { _exprOisStrict ->
                                                                           (case (_lhsIisLamBody) of
                                                                            { _exprOisLamBody ->
                                                                            (case (_lhsIevalCtx) of
                                                                             { _exprOevalCtx ->
                                                                             (case (_lhsIdoBox) of
                                                                              { _exprOdoBox ->
                                                                              (case (_lhsIdataGam) of
                                                                               { _exprOdataGam ->
                                                                               (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                                { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                    (case (_exprIappFunKind) of
                                                                                     { _lhsOappFunKind ->
                                                                                     (case (_lhsIopts) of
                                                                                      { _annOopts ->
                                                                                      (case (_lhsImodNm) of
                                                                                       { _annOmodNm ->
                                                                                       (case (_lhsIlev) of
                                                                                        { _annOlev ->
                                                                                        (case (_lhsIlamMp) of
                                                                                         { _annOlamMp ->
                                                                                         (case (_lhsIdataGam) of
                                                                                          { _annOdataGam ->
                                                                                          (case (ann_1 _annOdataGam _annOlamMp _annOlev _annOmodNm _annOopts ) of
                                                                                           { ( _annIfvS) ->
                                                                                               (case (_annIfvS `Set.union` _exprIfvS) of
                                                                                                { _lhsOfvS ->
                                                                                                (case (_exprIgrAppArgL) of
                                                                                                 { _lhsOgrAppArgL ->
                                                                                                 (case (_exprIgrAppFun) of
                                                                                                  { _lhsOgrAppFun ->
                                                                                                  (case (_exprIgrBindL) of
                                                                                                   { _lhsOgrBindL ->
                                                                                                   (case (_exprIgrExpr) of
                                                                                                    { _grExpr ->
                                                                                                    (case (_grExpr) of
                                                                                                     { _lhsOgrExpr ->
                                                                                                     (case (_exprIgrGlobalL) of
                                                                                                      { _lhsOgrGlobalL ->
                                                                                                      (case (_exprIgrLamBody) of
                                                                                                       { _lhsOgrLamBody ->
                                                                                                       (case (_exprIgrLetBody) of
                                                                                                        { _lhsOgrLetBody ->
                                                                                                        (case (_exprIgrTupFldL) of
                                                                                                         { _lhsOgrTupFldL ->
                                                                                                         (case (_exprIgrTupRec) of
                                                                                                          { _lhsOgrTupRec ->
                                                                                                          (case (_exprIgrVal) of
                                                                                                           { _grVal ->
                                                                                                           (case (_grVal) of
                                                                                                            { _lhsOgrVal ->
                                                                                                            (case (_exprImbFFIApp) of
                                                                                                             { _lhsOmbFFIApp ->
                                                                                                             (case (_exprImbLam) of
                                                                                                              { _lhsOmbLam ->
                                                                                                              (case (_exprImbVar) of
                                                                                                               { _lhsOmbVar ->
                                                                                                               ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                      in  sem_CExpr_Ann_2)) of
                                               { ( sem_CExpr_2) ->
                                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }))
                   in  sem_CExpr_Ann_1)) of
            { ( sem_CExpr_1) ->
            ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) }) })
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_App_1 :: T_CExpr_1 
                  sem_CExpr_App_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )) of
                            { __tup3 ->
                            (case (__tup3) of
                             { (_funcOgUniq,_,_) ->
                             (case (func_ ) of
                              { ( _funcIgathLamMp,_funcIgrLamArgL,func_1) ->
                                  (case (func_1 _funcOgUniq ) of
                                   { ( _funcIgUniq,_funcIwhatBelow,func_2) ->
                                       (case (_funcIgUniq) of
                                        { _argOgUniq ->
                                        (case (hsnUnknown) of
                                         { _argOnm ->
                                         (case (arg_ _argOnm ) of
                                          { ( _argIbindLamMp,arg_1) ->
                                              (case (arg_1 _argOgUniq ) of
                                               { ( _argIgUniq,arg_2) ->
                                                   (case (_argIgUniq) of
                                                    { _lhsOgUniq ->
                                                    (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow) of
                                                     { _whatBelow ->
                                                     (case (_whatBelow) of
                                                      { _lhsOwhatBelow ->
                                                      (case ((let sem_CExpr_App_2 :: T_CExpr_2 
                                                                  sem_CExpr_App_2  =
                                                                      (\ _lhsIdataGam
                                                                         _lhsIdoBox
                                                                         _lhsIevalCtx
                                                                         _lhsIisLamBody
                                                                         _lhsIisStrict
                                                                         _lhsIisTopApp
                                                                         _lhsIisTopTup
                                                                         _lhsIlamMp
                                                                         _lhsIlev
                                                                         _lhsImodNm
                                                                         _lhsIopts
                                                                         _lhsIwhatAbove ->
                                                                           (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove) of
                                                                            { _whatAbove ->
                                                                            (case (_whatAbove) of
                                                                             { _funcOwhatAbove ->
                                                                             (case (_lhsIopts) of
                                                                              { _funcOopts ->
                                                                              (case (_lhsImodNm) of
                                                                               { _funcOmodNm ->
                                                                               (case (_lhsIlev) of
                                                                                { _funcOlev ->
                                                                                (case (_lhsIlamMp) of
                                                                                 { _funcOlamMp ->
                                                                                 (case (True) of
                                                                                  { _isTopTup ->
                                                                                  (case (_isTopTup) of
                                                                                   { _funcOisTopTup ->
                                                                                   (case (_lhsIisStrict) of
                                                                                    { _funcOisStrict ->
                                                                                    (case (_lhsIisLamBody) of
                                                                                     { _funcOisLamBody ->
                                                                                     (case (_lhsIevalCtx) of
                                                                                      { _funcOevalCtx ->
                                                                                      (case (True) of
                                                                                       { _doBox ->
                                                                                       (case (_doBox) of
                                                                                        { _funcOdoBox ->
                                                                                        (case (_lhsIdataGam) of
                                                                                         { _funcOdataGam ->
                                                                                         (case (False) of
                                                                                          { _funcOisTopApp ->
                                                                                          (case (func_2 _funcOdataGam _funcOdoBox _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamMp _funcOlev _funcOmodNm _funcOopts _funcOwhatAbove ) of
                                                                                           { ( _funcIappFunKind,_funcIfvS,_funcIgrAppArgL,_funcIgrAppFun,_funcIgrBindL,_funcIgrExpr,_funcIgrGlobalL,_funcIgrLamBody,_funcIgrLetBody,_funcIgrTupFldL,_funcIgrTupRec,_funcIgrVal,_funcImbFFIApp,_funcImbLam,_funcImbVar) ->
                                                                                               (case (_funcIappFunKind) of
                                                                                                { _lhsOappFunKind ->
                                                                                                (case (_lhsIopts) of
                                                                                                 { _argOopts ->
                                                                                                 (case (_lhsImodNm) of
                                                                                                  { _argOmodNm ->
                                                                                                  (case (_lhsIlev) of
                                                                                                   { _argOlev ->
                                                                                                   (case (acoreBindcategPlain) of
                                                                                                    { _letBindingsCateg ->
                                                                                                    (case (_letBindingsCateg) of
                                                                                                     { _argOletBindingsCateg ->
                                                                                                     (case (_lhsIlamMp) of
                                                                                                      { _argOlamMp ->
                                                                                                      (case (_isTopTup) of
                                                                                                       { _argOisTopTup ->
                                                                                                       (case (_lhsIisStrict) of
                                                                                                        { _argOisStrict ->
                                                                                                        (case (_lhsIisLamBody) of
                                                                                                         { _argOisLamBody ->
                                                                                                         (case (False) of
                                                                                                          { _isGlobal ->
                                                                                                          (case (_isGlobal) of
                                                                                                           { _argOisGlobal ->
                                                                                                           (case (_lhsIevalCtx) of
                                                                                                            { _argOevalCtx ->
                                                                                                            (case (_doBox) of
                                                                                                             { _argOdoBox ->
                                                                                                             (case (_lhsIdataGam) of
                                                                                                              { _argOdataGam ->
                                                                                                              (case (True) of
                                                                                                               { _argOisTopApp ->
                                                                                                               (case (arg_2 _argOdataGam _argOdoBox _argOevalCtx _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOlamMp _argOletBindingsCateg _argOlev _argOmodNm _argOopts ) of
                                                                                                                { ( _argIfvS,_argIfvSMp,_argIgrBindL,_argIgrExpr,_argIgrGlobalL,_argIgrVal,_argInmL) ->
                                                                                                                    (case (_funcIfvS `Set.union` _argIfvS) of
                                                                                                                     { _fvS ->
                                                                                                                     (case (_fvS) of
                                                                                                                      { _lhsOfvS ->
                                                                                                                      (case (_argIgrVal : _funcIgrAppArgL) of
                                                                                                                       { _grAppArgL ->
                                                                                                                       (case (_grAppArgL) of
                                                                                                                        { _lhsOgrAppArgL ->
                                                                                                                        (case (_funcIgrAppFun) of
                                                                                                                         { _grAppFun ->
                                                                                                                         (case (_grAppFun) of
                                                                                                                          { _lhsOgrAppFun ->
                                                                                                                          (case (_funcIgrBindL ++ _argIgrBindL) of
                                                                                                                           { _lhsOgrBindL ->
                                                                                                                           (case (__tup3) of
                                                                                                                            { (_,_,_lUniq2) ->
                                                                                                                            (case (__tup3) of
                                                                                                                             { (_,_lUniq,_) ->
                                                                                                                             (case (length _grAppArgL) of
                                                                                                                              { _appArity ->
                                                                                                                              (case (lamMpLookupLam _grAppFun _lhsIlamMp) of
                                                                                                                               { _mbLam ->
                                                                                                                               (case (if _lhsIisTopApp
                                                                                                                                      then
                                                                                                                                           let  argL = reverse _grAppArgL
                                                                                                                                                funNm = _grAppFun
                                                                                                                                                mkE  = retStrict _lhsIisStrict
                                                                                                                                                n = uidQualHNm _lhsImodNm _lUniq
                                                                                                                                                (argL',wrapGr) = simplArgL _lhsImodNm _lUniq2 _lhsIlamMp argL
                                                                                                                                           in   case _funcIappFunKind of
                                                                                                                                                  AppFunKind_Tag t
                                                                                                                                                    -> wrapGr (mkE v)
                                                                                                                                                    where v = case t of
                                                                                                                                                                CTagRec         -> mkGrRecNode argL'
                                                                                                                                                                CTag _ l t a ma -> mkGrConNode (mkGrTagAnn a ma) t l argL'
                                                                                                                                                  _ -> case _funcImbFFIApp of
                                                                                                                                                         Just ( resTy
                                                                                                                                                              , primResNeedsEval
                                                                                                                                                              , callconv
                                                                                                                                                              , impEnt
                                                                                                                                                              , argTyL
                                                                                                                                                              )
                                                                                                                                                           -> wrapGr ffi
                                                                                                                                                           where ffi = ffiGrinMk _lhsIopts _lhsIdataGam _lUniq _lhsImodNm
                                                                                                                                                                                 callconv
                                                                                                                                                                                 impEnt
                                                                                                                                                                                 (not primResNeedsEval)
                                                                                                                                                                                 (zip argL' argTyL)
                                                                                                                                                                                 resTy
                                                                                                                                                         _ -> wrapGr ap
                                                                                                                                                           where ap = case _mbLam of
                                                                                                                                                                         Just arity
                                                                                                                                                                           | arity == _appArity && _lhsIisStrict
                                                                                                                                                                               -> GrExpr_Call funNm argL'
                                                                                                                                                                           | arity == _appArity
                                                                                                                                                                               -> GrExpr_Store (GrVal_Node (GrTag_Fun funNm) argL')
                                                                                                                                                                           | arity > _appArity
                                                                                                                                                                               -> mkE (mkNdPApp funNm (arity - _appArity) argL')
                                                                                                                                                                           | arity < _appArity && _lhsIisStrict
                                                                                                                                                                               -> mkSq (GrExpr_Call funNm (take arity argL')) n (GrExpr_App n (drop arity argL'))
                                                                                                                                                                           | otherwise
                                                                                                                                                                               -> mkSq (GrExpr_Store (GrVal_Node (GrTag_Fun funNm) (take arity argL'))) n (GrExpr_Store (mkNdApp n (drop arity argL')))
                                                                                                                                                                         Nothing
                                                                                                                                                                           | _lhsIisStrict
                                                                                                                                                                               -> mkSq (GrExpr_Eval funNm) n (GrExpr_App n argL')
                                                                                                                                                                           | otherwise
                                                                                                                                                                               -> GrExpr_Store (mkNdApp funNm argL')
                                                                                                                                      else
                                                                                                                                           GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                                                                { _grExpr ->
                                                                                                                                (case (_grExpr) of
                                                                                                                                 { _lhsOgrExpr ->
                                                                                                                                 (case (_funcIgrGlobalL ++ _argIgrGlobalL) of
                                                                                                                                  { _lhsOgrGlobalL ->
                                                                                                                                  (case (_grExpr) of
                                                                                                                                   { _lhsOgrLamBody ->
                                                                                                                                   (case (_grExpr) of
                                                                                                                                    { _lhsOgrLetBody ->
                                                                                                                                    (case ([]) of
                                                                                                                                     { _grTupFldL ->
                                                                                                                                     (case (_grTupFldL) of
                                                                                                                                      { _lhsOgrTupFldL ->
                                                                                                                                      (case (GrVal_Empty) of
                                                                                                                                       { _grVal ->
                                                                                                                                       (case (_grVal) of
                                                                                                                                        { _grTupRec ->
                                                                                                                                        (case (_grTupRec) of
                                                                                                                                         { _lhsOgrTupRec ->
                                                                                                                                         (case (_grVal) of
                                                                                                                                          { _lhsOgrVal ->
                                                                                                                                          (case (_funcImbFFIApp) of
                                                                                                                                           { _lhsOmbFFIApp ->
                                                                                                                                           (case (Nothing) of
                                                                                                                                            { _lhsOmbLam ->
                                                                                                                                            (case (Nothing) of
                                                                                                                                             { _lhsOmbVar ->
                                                                                                                                             ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                              in  sem_CExpr_App_2)) of
                                                       { ( sem_CExpr_2) ->
                                                       ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_App_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Case_1 :: T_CExpr_1 
                  sem_CExpr_Case_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup4 ->
                            (case (__tup4) of
                             { (_exprOgUniq,_) ->
                             (case (expr_ ) of
                              { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                  (case (expr_1 _exprOgUniq ) of
                                   { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                       (case (_exprIgUniq) of
                                        { _altsOgUniq ->
                                        (case (alts_ _altsOgUniq ) of
                                         { ( _altsIgUniq,alts_1) ->
                                             (case (_altsIgUniq) of
                                              { _dfltOgUniq ->
                                              (case (dflt_ ) of
                                               { ( _dfltIgathLamMp,_dfltIgrLamArgL,dflt_1) ->
                                                   (case (dflt_1 _dfltOgUniq ) of
                                                    { ( _dfltIgUniq,_dfltIwhatBelow,dflt_2) ->
                                                        (case (_dfltIgUniq) of
                                                         { _lhsOgUniq ->
                                                         (case (ExprIsOther) of
                                                          { _whatBelow ->
                                                          (case (_whatBelow) of
                                                           { _lhsOwhatBelow ->
                                                           (case ((let sem_CExpr_Case_2 :: T_CExpr_2 
                                                                       sem_CExpr_Case_2  =
                                                                           (\ _lhsIdataGam
                                                                              _lhsIdoBox
                                                                              _lhsIevalCtx
                                                                              _lhsIisLamBody
                                                                              _lhsIisStrict
                                                                              _lhsIisTopApp
                                                                              _lhsIisTopTup
                                                                              _lhsIlamMp
                                                                              _lhsIlev
                                                                              _lhsImodNm
                                                                              _lhsIopts
                                                                              _lhsIwhatAbove ->
                                                                                (case (AppFunKind_NoApp) of
                                                                                 { _lhsOappFunKind ->
                                                                                 (case (ExprIsOther) of
                                                                                  { _whatAbove ->
                                                                                  (case (_whatAbove) of
                                                                                   { _dfltOwhatAbove ->
                                                                                   (case (_lhsIopts) of
                                                                                    { _dfltOopts ->
                                                                                    (case (_lhsImodNm) of
                                                                                     { _dfltOmodNm ->
                                                                                     (case (_lhsIlev) of
                                                                                      { _dfltOlev ->
                                                                                      (case (_lhsIlamMp) of
                                                                                       { _dfltOlamMp ->
                                                                                       (case (True) of
                                                                                        { _isTopTup ->
                                                                                        (case (_isTopTup) of
                                                                                         { _dfltOisTopTup ->
                                                                                         (case (True) of
                                                                                          { _isTopApp ->
                                                                                          (case (_isTopApp) of
                                                                                           { _dfltOisTopApp ->
                                                                                           (case (_lhsIisStrict) of
                                                                                            { _dfltOisStrict ->
                                                                                            (case (_lhsIisLamBody) of
                                                                                             { _dfltOisLamBody ->
                                                                                             (case (_lhsIevalCtx) of
                                                                                              { _dfltOevalCtx ->
                                                                                              (case (True) of
                                                                                               { _doBox ->
                                                                                               (case (_doBox) of
                                                                                                { _dfltOdoBox ->
                                                                                                (case (_lhsIdataGam) of
                                                                                                 { _dfltOdataGam ->
                                                                                                 (case (dflt_2 _dfltOdataGam _dfltOdoBox _dfltOevalCtx _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlamMp _dfltOlev _dfltOmodNm _dfltOopts _dfltOwhatAbove ) of
                                                                                                  { ( _dfltIappFunKind,_dfltIfvS,_dfltIgrAppArgL,_dfltIgrAppFun,_dfltIgrBindL,_dfltIgrExpr,_dfltIgrGlobalL,_dfltIgrLamBody,_dfltIgrLetBody,_dfltIgrTupFldL,_dfltIgrTupRec,_dfltIgrVal,_dfltImbFFIApp,_dfltImbLam,_dfltImbVar) ->
                                                                                                      (case (_lhsIopts) of
                                                                                                       { _altsOopts ->
                                                                                                       (case (_lhsImodNm) of
                                                                                                        { _altsOmodNm ->
                                                                                                        (case (_lhsIlev) of
                                                                                                         { _altsOlev ->
                                                                                                         (case (_lhsIlamMp) of
                                                                                                          { _altsOlamMp ->
                                                                                                          (case (_lhsIisStrict) of
                                                                                                           { _altsOisStrict ->
                                                                                                           (case (_lhsIisLamBody) of
                                                                                                            { _altsOisLamBody ->
                                                                                                            (case (_lhsIevalCtx) of
                                                                                                             { _altsOevalCtx ->
                                                                                                             (case (_lhsIdataGam) of
                                                                                                              { _altsOdataGam ->
                                                                                                              (case (alts_1 _altsOdataGam _altsOevalCtx _altsOisLamBody _altsOisStrict _altsOlamMp _altsOlev _altsOmodNm _altsOopts ) of
                                                                                                               { ( _altsIfvS,_altsIgrAltL,_altsIgrWrapCase,_altsIgrWrapCaseSel) ->
                                                                                                                   (case (_whatAbove) of
                                                                                                                    { _exprOwhatAbove ->
                                                                                                                    (case (_lhsIopts) of
                                                                                                                     { _exprOopts ->
                                                                                                                     (case (_lhsImodNm) of
                                                                                                                      { _exprOmodNm ->
                                                                                                                      (case (_lhsIlev) of
                                                                                                                       { _exprOlev ->
                                                                                                                       (case (_lhsIlamMp) of
                                                                                                                        { _exprOlamMp ->
                                                                                                                        (case (_isTopTup) of
                                                                                                                         { _exprOisTopTup ->
                                                                                                                         (case (_isTopApp) of
                                                                                                                          { _exprOisTopApp ->
                                                                                                                          (case (_lhsIisStrict) of
                                                                                                                           { _exprOisStrict ->
                                                                                                                           (case (_lhsIisLamBody) of
                                                                                                                            { _exprOisLamBody ->
                                                                                                                            (case (_lhsIevalCtx) of
                                                                                                                             { _exprOevalCtx ->
                                                                                                                             (case (_doBox) of
                                                                                                                              { _exprOdoBox ->
                                                                                                                              (case (_lhsIdataGam) of
                                                                                                                               { _exprOdataGam ->
                                                                                                                               (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                                                                                { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                                                                    (case (_exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS) of
                                                                                                                                     { _lhsOfvS ->
                                                                                                                                     (case ([]) of
                                                                                                                                      { _lhsOgrAppArgL ->
                                                                                                                                      (case (GrVal_Empty) of
                                                                                                                                       { _grVal ->
                                                                                                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                                                        { _lhsOgrAppFun ->
                                                                                                                                        (case (_exprIgrBindL ++ _dfltIgrBindL) of
                                                                                                                                         { _lhsOgrBindL ->
                                                                                                                                         (case (__tup4) of
                                                                                                                                          { (_,_lUniq) ->
                                                                                                                                          (case (let  w1 = _altsIgrWrapCase
                                                                                                                                                      (sel,w2) = _altsIgrWrapCaseSel _exprIgrVal
                                                                                                                                                 in   w1 . w2 . GrExpr_Case sel . saturateAltL _lUniq _dfltIgrExpr $ _altsIgrAltL) of
                                                                                                                                           { _grExpr ->
                                                                                                                                           (case (_grExpr) of
                                                                                                                                            { _lhsOgrExpr ->
                                                                                                                                            (case (_exprIgrGlobalL ++ _dfltIgrGlobalL) of
                                                                                                                                             { _lhsOgrGlobalL ->
                                                                                                                                             (case (_grExpr) of
                                                                                                                                              { _lhsOgrLamBody ->
                                                                                                                                              (case (_grExpr) of
                                                                                                                                               { _lhsOgrLetBody ->
                                                                                                                                               (case ([]) of
                                                                                                                                                { _grTupFldL ->
                                                                                                                                                (case (_grTupFldL) of
                                                                                                                                                 { _lhsOgrTupFldL ->
                                                                                                                                                 (case (_grVal) of
                                                                                                                                                  { _grTupRec ->
                                                                                                                                                  (case (_grTupRec) of
                                                                                                                                                   { _lhsOgrTupRec ->
                                                                                                                                                   (case (_grVal) of
                                                                                                                                                    { _lhsOgrVal ->
                                                                                                                                                    (case (Nothing) of
                                                                                                                                                     { _lhsOmbFFIApp ->
                                                                                                                                                     (case (Nothing) of
                                                                                                                                                      { _lhsOmbLam ->
                                                                                                                                                      (case (Nothing) of
                                                                                                                                                       { _lhsOmbVar ->
                                                                                                                                                       ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                   in  sem_CExpr_Case_2)) of
                                                            { ( sem_CExpr_2) ->
                                                            ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_Case_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (case (errorExpr_ ) of
     { ( _errorExprIgathLamMp,_errorExprIgrLamArgL,errorExpr_1) ->
         (case (_errorExprIgathLamMp) of
          { _lhsOgathLamMp ->
          (case (_errorExprIgrLamArgL) of
           { _lhsOgrLamArgL ->
           (case ((let sem_CExpr_CaseAltFail_1 :: T_CExpr_1 
                       sem_CExpr_CaseAltFail_1  =
                           (\ _lhsIgUniq ->
                                (case (_lhsIgUniq) of
                                 { _errorExprOgUniq ->
                                 (case (errorExpr_1 _errorExprOgUniq ) of
                                  { ( _errorExprIgUniq,_errorExprIwhatBelow,errorExpr_2) ->
                                      (case (_errorExprIgUniq) of
                                       { _lhsOgUniq ->
                                       (case (_errorExprIwhatBelow) of
                                        { _lhsOwhatBelow ->
                                        (case ((let sem_CExpr_CaseAltFail_2 :: T_CExpr_2 
                                                    sem_CExpr_CaseAltFail_2  =
                                                        (\ _lhsIdataGam
                                                           _lhsIdoBox
                                                           _lhsIevalCtx
                                                           _lhsIisLamBody
                                                           _lhsIisStrict
                                                           _lhsIisTopApp
                                                           _lhsIisTopTup
                                                           _lhsIlamMp
                                                           _lhsIlev
                                                           _lhsImodNm
                                                           _lhsIopts
                                                           _lhsIwhatAbove ->
                                                             (case (ExprIsOther) of
                                                              { _whatAbove ->
                                                              (case (_whatAbove) of
                                                               { _errorExprOwhatAbove ->
                                                               (case (_lhsIopts) of
                                                                { _errorExprOopts ->
                                                                (case (_lhsImodNm) of
                                                                 { _errorExprOmodNm ->
                                                                 (case (_lhsIlev) of
                                                                  { _errorExprOlev ->
                                                                  (case (_lhsIlamMp) of
                                                                   { _errorExprOlamMp ->
                                                                   (case (True) of
                                                                    { _isTopTup ->
                                                                    (case (_isTopTup) of
                                                                     { _errorExprOisTopTup ->
                                                                     (case (True) of
                                                                      { _isTopApp ->
                                                                      (case (_isTopApp) of
                                                                       { _errorExprOisTopApp ->
                                                                       (case (_lhsIisStrict) of
                                                                        { _errorExprOisStrict ->
                                                                        (case (_lhsIisLamBody) of
                                                                         { _errorExprOisLamBody ->
                                                                         (case (_lhsIevalCtx) of
                                                                          { _errorExprOevalCtx ->
                                                                          (case (True) of
                                                                           { _doBox ->
                                                                           (case (_doBox) of
                                                                            { _errorExprOdoBox ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _errorExprOdataGam ->
                                                                             (case (errorExpr_2 _errorExprOdataGam _errorExprOdoBox _errorExprOevalCtx _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlamMp _errorExprOlev _errorExprOmodNm _errorExprOopts _errorExprOwhatAbove ) of
                                                                              { ( _errorExprIappFunKind,_errorExprIfvS,_errorExprIgrAppArgL,_errorExprIgrAppFun,_errorExprIgrBindL,_errorExprIgrExpr,_errorExprIgrGlobalL,_errorExprIgrLamBody,_errorExprIgrLetBody,_errorExprIgrTupFldL,_errorExprIgrTupRec,_errorExprIgrVal,_errorExprImbFFIApp,_errorExprImbLam,_errorExprImbVar) ->
                                                                                  (case (_errorExprIappFunKind) of
                                                                                   { _lhsOappFunKind ->
                                                                                   (case (_errorExprIfvS) of
                                                                                    { _lhsOfvS ->
                                                                                    (case ([]) of
                                                                                     { _lhsOgrAppArgL ->
                                                                                     (case (GrVal_Empty) of
                                                                                      { _grVal ->
                                                                                      (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                       { _lhsOgrAppFun ->
                                                                                       (case (_errorExprIgrBindL) of
                                                                                        { _lhsOgrBindL ->
                                                                                        (case (_errorExprIgrExpr) of
                                                                                         { _grExpr ->
                                                                                         (case (_grExpr) of
                                                                                          { _lhsOgrExpr ->
                                                                                          (case (_errorExprIgrGlobalL) of
                                                                                           { _lhsOgrGlobalL ->
                                                                                           (case (_errorExprIgrLamBody) of
                                                                                            { _lhsOgrLamBody ->
                                                                                            (case (_errorExprIgrLetBody) of
                                                                                             { _lhsOgrLetBody ->
                                                                                             (case ([]) of
                                                                                              { _grTupFldL ->
                                                                                              (case (_grTupFldL) of
                                                                                               { _lhsOgrTupFldL ->
                                                                                               (case (_grVal) of
                                                                                                { _grTupRec ->
                                                                                                (case (_grTupRec) of
                                                                                                 { _lhsOgrTupRec ->
                                                                                                 (case (_grVal) of
                                                                                                  { _lhsOgrVal ->
                                                                                                  (case (Nothing) of
                                                                                                   { _lhsOmbFFIApp ->
                                                                                                   (case (_errorExprImbLam) of
                                                                                                    { _lhsOmbLam ->
                                                                                                    (case (_errorExprImbVar) of
                                                                                                     { _lhsOmbVar ->
                                                                                                     ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                in  sem_CExpr_CaseAltFail_2)) of
                                         { ( sem_CExpr_2) ->
                                         ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }))
                   in  sem_CExpr_CaseAltFail_1)) of
            { ( sem_CExpr_1) ->
            ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) }) })
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Char_1 :: T_CExpr_1 
                  sem_CExpr_Char_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_Char_2 :: T_CExpr_2 
                                          sem_CExpr_Char_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case ((if _lhsIdoBox then mkGrBox hsnChar else id)  (GrVal_LitInt (ord char_))) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (retStrict _lhsIisStrict _grVal) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_Char_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_Char_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_CoeArg_1 :: T_CExpr_1 
                  sem_CExpr_CoeArg_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_CoeArg_2 :: T_CExpr_2 
                                          sem_CExpr_CoeArg_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case (GrVal_Empty) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_CoeArg_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_CoeArg_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_FFI_1 :: T_CExpr_1 
                  sem_CExpr_FFI_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup5 ->
                            (case (__tup5) of
                             { (_lhsOgUniq,_) ->
                             (case (ExprIsOther) of
                              { _whatBelow ->
                              (case (_whatBelow) of
                               { _lhsOwhatBelow ->
                               (case ((let sem_CExpr_FFI_2 :: T_CExpr_2 
                                           sem_CExpr_FFI_2  =
                                               (\ _lhsIdataGam
                                                  _lhsIdoBox
                                                  _lhsIevalCtx
                                                  _lhsIisLamBody
                                                  _lhsIisStrict
                                                  _lhsIisTopApp
                                                  _lhsIisTopTup
                                                  _lhsIlamMp
                                                  _lhsIlev
                                                  _lhsImodNm
                                                  _lhsIopts
                                                  _lhsIwhatAbove ->
                                                    (case (AppFunKind_FFI) of
                                                     { _lhsOappFunKind ->
                                                     (case (Set.empty) of
                                                      { _lhsOfvS ->
                                                      (case ([]) of
                                                       { _lhsOgrAppArgL ->
                                                       (case (GrVal_Empty) of
                                                        { _grVal ->
                                                        (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                         { _lhsOgrAppFun ->
                                                         (case ([]) of
                                                          { _lhsOgrBindL ->
                                                          (case (__tup5) of
                                                           { (_,_lUniq) ->
                                                           (case (foreignEntExtract impEnt_) of
                                                            { _foreignEntInfo ->
                                                            (case (maybe Nothing lookupPrimNeedsEval $ forextractMbEnt _foreignEntInfo) of
                                                             { _mbPrimNeedEval ->
                                                             (case (maybe False primResNeedEval _mbPrimNeedEval) of
                                                              { _primResNeedsEval ->
                                                              (case (tyArrowArgsRes ty_) of
                                                               { _argTyLresTy ->
                                                               (case (snd _argTyLresTy) of
                                                                { _resTy ->
                                                                (case (ffiGrinMk _lhsIopts _lhsIdataGam _lUniq _lhsImodNm
                                                                                 callconv_
                                                                                 impEnt_
                                                                                 (not _primResNeedsEval)
                                                                                 []
                                                                                 _resTy) of
                                                                 { _grExpr ->
                                                                 (case (_grExpr) of
                                                                  { _lhsOgrExpr ->
                                                                  (case ([]) of
                                                                   { _lhsOgrGlobalL ->
                                                                   (case (_grExpr) of
                                                                    { _lhsOgrLamBody ->
                                                                    (case (_grExpr) of
                                                                     { _lhsOgrLetBody ->
                                                                     (case ([]) of
                                                                      { _grTupFldL ->
                                                                      (case (_grTupFldL) of
                                                                       { _lhsOgrTupFldL ->
                                                                       (case (_grVal) of
                                                                        { _grTupRec ->
                                                                        (case (_grTupRec) of
                                                                         { _lhsOgrTupRec ->
                                                                         (case (_grVal) of
                                                                          { _lhsOgrVal ->
                                                                          (case (fst _argTyLresTy) of
                                                                           { _argTyL ->
                                                                           (case (Just ( _resTy
                                                                                       , _primResNeedsEval
                                                                                       , callconv_
                                                                                       , impEnt_
                                                                                       , _argTyL
                                                                                       )) of
                                                                            { _lhsOmbFFIApp ->
                                                                            (case (Nothing) of
                                                                             { _lhsOmbLam ->
                                                                             (case (Nothing) of
                                                                              { _lhsOmbVar ->
                                                                              ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                       in  sem_CExpr_FFI_2)) of
                                { ( sem_CExpr_2) ->
                                ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }))
              in  sem_CExpr_FFI_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Hole_1 :: T_CExpr_1 
                  sem_CExpr_Hole_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_Hole_2 :: T_CExpr_2 
                                          sem_CExpr_Hole_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case (GrVal_Empty) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_Hole_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_Hole_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_HoleLet_1 :: T_CExpr_1 
                  sem_CExpr_HoleLet_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _bodyOgUniq ->
                            (case (body_ ) of
                             { ( _bodyIgathLamMp,_bodyIgrLamArgL,body_1) ->
                                 (case (body_1 _bodyOgUniq ) of
                                  { ( _bodyIgUniq,_bodyIwhatBelow,body_2) ->
                                      (case (_bodyIgUniq) of
                                       { _lhsOgUniq ->
                                       (case (ExprIsOther) of
                                        { _whatBelow ->
                                        (case (_whatBelow) of
                                         { _lhsOwhatBelow ->
                                         (case ((let sem_CExpr_HoleLet_2 :: T_CExpr_2 
                                                     sem_CExpr_HoleLet_2  =
                                                         (\ _lhsIdataGam
                                                            _lhsIdoBox
                                                            _lhsIevalCtx
                                                            _lhsIisLamBody
                                                            _lhsIisStrict
                                                            _lhsIisTopApp
                                                            _lhsIisTopTup
                                                            _lhsIlamMp
                                                            _lhsIlev
                                                            _lhsImodNm
                                                            _lhsIopts
                                                            _lhsIwhatAbove ->
                                                              (case (AppFunKind_NoApp) of
                                                               { _lhsOappFunKind ->
                                                               (case (ExprIsOther) of
                                                                { _whatAbove ->
                                                                (case (_whatAbove) of
                                                                 { _bodyOwhatAbove ->
                                                                 (case (_lhsIopts) of
                                                                  { _bodyOopts ->
                                                                  (case (_lhsImodNm) of
                                                                   { _bodyOmodNm ->
                                                                   (case (_lhsIlev) of
                                                                    { _bodyOlev ->
                                                                    (case (_lhsIlamMp) of
                                                                     { _bodyOlamMp ->
                                                                     (case (True) of
                                                                      { _isTopTup ->
                                                                      (case (_isTopTup) of
                                                                       { _bodyOisTopTup ->
                                                                       (case (True) of
                                                                        { _isTopApp ->
                                                                        (case (_isTopApp) of
                                                                         { _bodyOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _bodyOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _bodyOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _bodyOevalCtx ->
                                                                            (case (True) of
                                                                             { _doBox ->
                                                                             (case (_doBox) of
                                                                              { _bodyOdoBox ->
                                                                              (case (_lhsIdataGam) of
                                                                               { _bodyOdataGam ->
                                                                               (case (body_2 _bodyOdataGam _bodyOdoBox _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmodNm _bodyOopts _bodyOwhatAbove ) of
                                                                                { ( _bodyIappFunKind,_bodyIfvS,_bodyIgrAppArgL,_bodyIgrAppFun,_bodyIgrBindL,_bodyIgrExpr,_bodyIgrGlobalL,_bodyIgrLamBody,_bodyIgrLetBody,_bodyIgrTupFldL,_bodyIgrTupRec,_bodyIgrVal,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar) ->
                                                                                    (case (_bodyIfvS) of
                                                                                     { _lhsOfvS ->
                                                                                     (case ([]) of
                                                                                      { _lhsOgrAppArgL ->
                                                                                      (case (GrVal_Empty) of
                                                                                       { _grVal ->
                                                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                        { _lhsOgrAppFun ->
                                                                                        (case (_bodyIgrBindL) of
                                                                                         { _lhsOgrBindL ->
                                                                                         (case (_bodyIgrExpr) of
                                                                                          { _grExpr ->
                                                                                          (case (_grExpr) of
                                                                                           { _lhsOgrExpr ->
                                                                                           (case (_bodyIgrGlobalL) of
                                                                                            { _lhsOgrGlobalL ->
                                                                                            (case (_grExpr) of
                                                                                             { _lhsOgrLamBody ->
                                                                                             (case (_grExpr) of
                                                                                              { _lhsOgrLetBody ->
                                                                                              (case ([]) of
                                                                                               { _grTupFldL ->
                                                                                               (case (_grTupFldL) of
                                                                                                { _lhsOgrTupFldL ->
                                                                                                (case (_grVal) of
                                                                                                 { _grTupRec ->
                                                                                                 (case (_grTupRec) of
                                                                                                  { _lhsOgrTupRec ->
                                                                                                  (case (_grVal) of
                                                                                                   { _lhsOgrVal ->
                                                                                                   (case (Nothing) of
                                                                                                    { _lhsOmbFFIApp ->
                                                                                                    (case (Nothing) of
                                                                                                     { _lhsOmbLam ->
                                                                                                     (case (Nothing) of
                                                                                                      { _lhsOmbVar ->
                                                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_HoleLet_2)) of
                                          { ( sem_CExpr_2) ->
                                          ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }))
              in  sem_CExpr_HoleLet_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_ImplsApp_1 :: T_CExpr_1 
                  sem_CExpr_ImplsApp_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _funcOgUniq ->
                            (case (func_ ) of
                             { ( _funcIgathLamMp,_funcIgrLamArgL,func_1) ->
                                 (case (func_1 _funcOgUniq ) of
                                  { ( _funcIgUniq,_funcIwhatBelow,func_2) ->
                                      (case (_funcIgUniq) of
                                       { _lhsOgUniq ->
                                       (case (ExprIsOther) of
                                        { _whatBelow ->
                                        (case (_whatBelow) of
                                         { _lhsOwhatBelow ->
                                         (case ((let sem_CExpr_ImplsApp_2 :: T_CExpr_2 
                                                     sem_CExpr_ImplsApp_2  =
                                                         (\ _lhsIdataGam
                                                            _lhsIdoBox
                                                            _lhsIevalCtx
                                                            _lhsIisLamBody
                                                            _lhsIisStrict
                                                            _lhsIisTopApp
                                                            _lhsIisTopTup
                                                            _lhsIlamMp
                                                            _lhsIlev
                                                            _lhsImodNm
                                                            _lhsIopts
                                                            _lhsIwhatAbove ->
                                                              (case (AppFunKind_NoApp) of
                                                               { _lhsOappFunKind ->
                                                               (case (ExprIsOther) of
                                                                { _whatAbove ->
                                                                (case (_whatAbove) of
                                                                 { _funcOwhatAbove ->
                                                                 (case (_lhsIopts) of
                                                                  { _funcOopts ->
                                                                  (case (_lhsImodNm) of
                                                                   { _funcOmodNm ->
                                                                   (case (_lhsIlev) of
                                                                    { _funcOlev ->
                                                                    (case (_lhsIlamMp) of
                                                                     { _funcOlamMp ->
                                                                     (case (True) of
                                                                      { _isTopTup ->
                                                                      (case (_isTopTup) of
                                                                       { _funcOisTopTup ->
                                                                       (case (True) of
                                                                        { _isTopApp ->
                                                                        (case (_isTopApp) of
                                                                         { _funcOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _funcOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _funcOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _funcOevalCtx ->
                                                                            (case (True) of
                                                                             { _doBox ->
                                                                             (case (_doBox) of
                                                                              { _funcOdoBox ->
                                                                              (case (_lhsIdataGam) of
                                                                               { _funcOdataGam ->
                                                                               (case (func_2 _funcOdataGam _funcOdoBox _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamMp _funcOlev _funcOmodNm _funcOopts _funcOwhatAbove ) of
                                                                                { ( _funcIappFunKind,_funcIfvS,_funcIgrAppArgL,_funcIgrAppFun,_funcIgrBindL,_funcIgrExpr,_funcIgrGlobalL,_funcIgrLamBody,_funcIgrLetBody,_funcIgrTupFldL,_funcIgrTupRec,_funcIgrVal,_funcImbFFIApp,_funcImbLam,_funcImbVar) ->
                                                                                    (case (_funcIfvS) of
                                                                                     { _lhsOfvS ->
                                                                                     (case ([]) of
                                                                                      { _lhsOgrAppArgL ->
                                                                                      (case (GrVal_Empty) of
                                                                                       { _grVal ->
                                                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                        { _lhsOgrAppFun ->
                                                                                        (case (_funcIgrBindL) of
                                                                                         { _lhsOgrBindL ->
                                                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                                                          { _grExpr ->
                                                                                          (case (_grExpr) of
                                                                                           { _lhsOgrExpr ->
                                                                                           (case (_funcIgrGlobalL) of
                                                                                            { _lhsOgrGlobalL ->
                                                                                            (case (_grExpr) of
                                                                                             { _lhsOgrLamBody ->
                                                                                             (case (_grExpr) of
                                                                                              { _lhsOgrLetBody ->
                                                                                              (case ([]) of
                                                                                               { _grTupFldL ->
                                                                                               (case (_grTupFldL) of
                                                                                                { _lhsOgrTupFldL ->
                                                                                                (case (_grVal) of
                                                                                                 { _grTupRec ->
                                                                                                 (case (_grTupRec) of
                                                                                                  { _lhsOgrTupRec ->
                                                                                                  (case (_grVal) of
                                                                                                   { _lhsOgrVal ->
                                                                                                   (case (Nothing) of
                                                                                                    { _lhsOmbFFIApp ->
                                                                                                    (case (Nothing) of
                                                                                                     { _lhsOmbLam ->
                                                                                                     (case (Nothing) of
                                                                                                      { _lhsOmbVar ->
                                                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_ImplsApp_2)) of
                                          { ( sem_CExpr_2) ->
                                          ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }))
              in  sem_CExpr_ImplsApp_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_ImplsLam_1 :: T_CExpr_1 
                  sem_CExpr_ImplsLam_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _bodyOgUniq ->
                            (case (body_ ) of
                             { ( _bodyIgathLamMp,_bodyIgrLamArgL,body_1) ->
                                 (case (body_1 _bodyOgUniq ) of
                                  { ( _bodyIgUniq,_bodyIwhatBelow,body_2) ->
                                      (case (_bodyIgUniq) of
                                       { _lhsOgUniq ->
                                       (case (ExprIsOther) of
                                        { _whatBelow ->
                                        (case (_whatBelow) of
                                         { _lhsOwhatBelow ->
                                         (case ((let sem_CExpr_ImplsLam_2 :: T_CExpr_2 
                                                     sem_CExpr_ImplsLam_2  =
                                                         (\ _lhsIdataGam
                                                            _lhsIdoBox
                                                            _lhsIevalCtx
                                                            _lhsIisLamBody
                                                            _lhsIisStrict
                                                            _lhsIisTopApp
                                                            _lhsIisTopTup
                                                            _lhsIlamMp
                                                            _lhsIlev
                                                            _lhsImodNm
                                                            _lhsIopts
                                                            _lhsIwhatAbove ->
                                                              (case (AppFunKind_NoApp) of
                                                               { _lhsOappFunKind ->
                                                               (case (ExprIsOther) of
                                                                { _whatAbove ->
                                                                (case (_whatAbove) of
                                                                 { _bodyOwhatAbove ->
                                                                 (case (_lhsIopts) of
                                                                  { _bodyOopts ->
                                                                  (case (_lhsImodNm) of
                                                                   { _bodyOmodNm ->
                                                                   (case (_lhsIlev) of
                                                                    { _bodyOlev ->
                                                                    (case (_lhsIlamMp) of
                                                                     { _bodyOlamMp ->
                                                                     (case (True) of
                                                                      { _isTopTup ->
                                                                      (case (_isTopTup) of
                                                                       { _bodyOisTopTup ->
                                                                       (case (True) of
                                                                        { _isTopApp ->
                                                                        (case (_isTopApp) of
                                                                         { _bodyOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _bodyOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _bodyOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _bodyOevalCtx ->
                                                                            (case (True) of
                                                                             { _doBox ->
                                                                             (case (_doBox) of
                                                                              { _bodyOdoBox ->
                                                                              (case (_lhsIdataGam) of
                                                                               { _bodyOdataGam ->
                                                                               (case (body_2 _bodyOdataGam _bodyOdoBox _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmodNm _bodyOopts _bodyOwhatAbove ) of
                                                                                { ( _bodyIappFunKind,_bodyIfvS,_bodyIgrAppArgL,_bodyIgrAppFun,_bodyIgrBindL,_bodyIgrExpr,_bodyIgrGlobalL,_bodyIgrLamBody,_bodyIgrLetBody,_bodyIgrTupFldL,_bodyIgrTupRec,_bodyIgrVal,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar) ->
                                                                                    (case (_bodyIfvS) of
                                                                                     { _lhsOfvS ->
                                                                                     (case ([]) of
                                                                                      { _lhsOgrAppArgL ->
                                                                                      (case (GrVal_Empty) of
                                                                                       { _grVal ->
                                                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                        { _lhsOgrAppFun ->
                                                                                        (case (_bodyIgrBindL) of
                                                                                         { _lhsOgrBindL ->
                                                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                                                          { _grExpr ->
                                                                                          (case (_grExpr) of
                                                                                           { _lhsOgrExpr ->
                                                                                           (case (_bodyIgrGlobalL) of
                                                                                            { _lhsOgrGlobalL ->
                                                                                            (case (_grExpr) of
                                                                                             { _lhsOgrLamBody ->
                                                                                             (case (_grExpr) of
                                                                                              { _lhsOgrLetBody ->
                                                                                              (case ([]) of
                                                                                               { _grTupFldL ->
                                                                                               (case (_grTupFldL) of
                                                                                                { _lhsOgrTupFldL ->
                                                                                                (case (_grVal) of
                                                                                                 { _grTupRec ->
                                                                                                 (case (_grTupRec) of
                                                                                                  { _lhsOgrTupRec ->
                                                                                                  (case (_grVal) of
                                                                                                   { _lhsOgrVal ->
                                                                                                   (case (Nothing) of
                                                                                                    { _lhsOmbFFIApp ->
                                                                                                    (case (Nothing) of
                                                                                                     { _lhsOmbLam ->
                                                                                                     (case (Nothing) of
                                                                                                      { _lhsOmbVar ->
                                                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_ImplsLam_2)) of
                                          { ( sem_CExpr_2) ->
                                          ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }))
              in  sem_CExpr_ImplsLam_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Int_1 :: T_CExpr_1 
                  sem_CExpr_Int_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsInt int_) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_Int_2 :: T_CExpr_2 
                                          sem_CExpr_Int_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case ((if _lhsIdoBox then mkGrBox hsnInt  else id)  (GrVal_LitInt int_)) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (retStrict _lhsIisStrict _grVal) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_Int_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_Int_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Integer_1 :: T_CExpr_1 
                  sem_CExpr_Integer_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_Integer_2 :: T_CExpr_2 
                                          sem_CExpr_Integer_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case ((if _lhsIdoBox then mkGrBox hsnInt  else id)  (GrVal_LitInt $ fromInteger $ integer_)) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_Integer_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_Integer_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case (bind_ ) of
      { ( _bindIbindLamMp,_bindInm,bind_1) ->
          (case (_bindInm) of
           { _argNm ->
           (case (body_ ) of
            { ( _bodyIgathLamMp,_bodyIgrLamArgL,body_1) ->
                (case (_argNm : _bodyIgrLamArgL) of
                 { _lhsOgrLamArgL ->
                 (case ((let sem_CExpr_Lam_1 :: T_CExpr_1 
                             sem_CExpr_Lam_1  =
                                 (\ _lhsIgUniq ->
                                      (case (_lhsIgUniq) of
                                       { _bindOgUniq ->
                                       (case (bind_1 _bindOgUniq ) of
                                        { ( _bindIgUniq,bind_2) ->
                                            (case (_bindIgUniq) of
                                             { _bodyOgUniq ->
                                             (case (body_1 _bodyOgUniq ) of
                                              { ( _bodyIgUniq,_bodyIwhatBelow,body_2) ->
                                                  (case (_bodyIgUniq) of
                                                   { _lhsOgUniq ->
                                                   (case (ExprIsLam) of
                                                    { _whatBelow ->
                                                    (case (_whatBelow) of
                                                     { _lhsOwhatBelow ->
                                                     (case ((let sem_CExpr_Lam_2 :: T_CExpr_2 
                                                                 sem_CExpr_Lam_2  =
                                                                     (\ _lhsIdataGam
                                                                        _lhsIdoBox
                                                                        _lhsIevalCtx
                                                                        _lhsIisLamBody
                                                                        _lhsIisStrict
                                                                        _lhsIisTopApp
                                                                        _lhsIisTopTup
                                                                        _lhsIlamMp
                                                                        _lhsIlev
                                                                        _lhsImodNm
                                                                        _lhsIopts
                                                                        _lhsIwhatAbove ->
                                                                          (case (AppFunKind_NoApp) of
                                                                           { _lhsOappFunKind ->
                                                                           (case (ExprIsLam) of
                                                                            { _whatAbove ->
                                                                            (case (_whatAbove) of
                                                                             { _bodyOwhatAbove ->
                                                                             (case (_lhsIopts) of
                                                                              { _bodyOopts ->
                                                                              (case (_lhsImodNm) of
                                                                               { _bodyOmodNm ->
                                                                               (case (_lhsIlev + 1) of
                                                                                { _lev ->
                                                                                (case (_lev) of
                                                                                 { _bodyOlev ->
                                                                                 (case (True) of
                                                                                  { _isTopTup ->
                                                                                  (case (_isTopTup) of
                                                                                   { _bodyOisTopTup ->
                                                                                   (case (True) of
                                                                                    { _isTopApp ->
                                                                                    (case (_isTopApp) of
                                                                                     { _bodyOisTopApp ->
                                                                                     (case (_lhsIisStrict) of
                                                                                      { _bodyOisStrict ->
                                                                                      (case (_lhsIisLamBody) of
                                                                                       { _bodyOisLamBody ->
                                                                                       (case (_lhsIevalCtx) of
                                                                                        { _bodyOevalCtx ->
                                                                                        (case (True) of
                                                                                         { _doBox ->
                                                                                         (case (_doBox) of
                                                                                          { _bodyOdoBox ->
                                                                                          (case (_lhsIdataGam) of
                                                                                           { _bodyOdataGam ->
                                                                                           (case (Map.delete _argNm _lhsIlamMp) of
                                                                                            { _bodyOlamMp ->
                                                                                            (case (body_2 _bodyOdataGam _bodyOdoBox _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmodNm _bodyOopts _bodyOwhatAbove ) of
                                                                                             { ( _bodyIappFunKind,_bodyIfvS,_bodyIgrAppArgL,_bodyIgrAppFun,_bodyIgrBindL,_bodyIgrExpr,_bodyIgrGlobalL,_bodyIgrLamBody,_bodyIgrLetBody,_bodyIgrTupFldL,_bodyIgrTupRec,_bodyIgrVal,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar) ->
                                                                                                 (case (_argNm `Set.delete` _bodyIfvS) of
                                                                                                  { _fvS ->
                                                                                                  (case (_fvS) of
                                                                                                   { _lhsOfvS ->
                                                                                                   (case ([]) of
                                                                                                    { _lhsOgrAppArgL ->
                                                                                                    (case (GrVal_Empty) of
                                                                                                     { _grVal ->
                                                                                                     (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                      { _lhsOgrAppFun ->
                                                                                                      (case (_lhsIopts) of
                                                                                                       { _bindOopts ->
                                                                                                       (case (_lhsImodNm) of
                                                                                                        { _bindOmodNm ->
                                                                                                        (case (_lev) of
                                                                                                         { _bindOlev ->
                                                                                                         (case (_lhsIlamMp) of
                                                                                                          { _bindOlamMp ->
                                                                                                          (case (_lhsIisStrict) of
                                                                                                           { _bindOisStrict ->
                                                                                                           (case (_lhsIdataGam) of
                                                                                                            { _bindOdataGam ->
                                                                                                            (case (acoreBindcategPlain) of
                                                                                                             { _letBindingsCateg ->
                                                                                                             (case (_letBindingsCateg) of
                                                                                                              { _bindOletBindingsCateg ->
                                                                                                              (case (_lhsIisLamBody) of
                                                                                                               { _bindOisLamBody ->
                                                                                                               (case (False) of
                                                                                                                { _isGlobal ->
                                                                                                                (case (_isGlobal) of
                                                                                                                 { _bindOisGlobal ->
                                                                                                                 (case (_lhsIevalCtx) of
                                                                                                                  { _bindOevalCtx ->
                                                                                                                  (case (bind_2 _bindOdataGam _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOmodNm _bindOopts ) of
                                                                                                                   { ( _bindIfvS,_bindIfvSMp,_bindIgrBindL,_bindIgrGlobalL,_bindInmL) ->
                                                                                                                       (case (_bindIgrBindL ++ _bodyIgrBindL) of
                                                                                                                        { _lhsOgrBindL ->
                                                                                                                        (case (GrExpr_Unit _grVal GrType_None) of
                                                                                                                         { _grExpr ->
                                                                                                                         (case (_grExpr) of
                                                                                                                          { _lhsOgrExpr ->
                                                                                                                          (case (_bindIgrGlobalL ++ _bodyIgrGlobalL) of
                                                                                                                           { _lhsOgrGlobalL ->
                                                                                                                           (case (_bodyIgrLamBody) of
                                                                                                                            { _lhsOgrLamBody ->
                                                                                                                            (case (_grExpr) of
                                                                                                                             { _lhsOgrLetBody ->
                                                                                                                             (case ([]) of
                                                                                                                              { _grTupFldL ->
                                                                                                                              (case (_grTupFldL) of
                                                                                                                               { _lhsOgrTupFldL ->
                                                                                                                               (case (_grVal) of
                                                                                                                                { _grTupRec ->
                                                                                                                                (case (_grTupRec) of
                                                                                                                                 { _lhsOgrTupRec ->
                                                                                                                                 (case (_grVal) of
                                                                                                                                  { _lhsOgrVal ->
                                                                                                                                  (case (Nothing) of
                                                                                                                                   { _lhsOmbFFIApp ->
                                                                                                                                   (case (Just $ maybe [_argNm] (_argNm:) _bodyImbLam) of
                                                                                                                                    { _lhsOmbLam ->
                                                                                                                                    (case (Nothing) of
                                                                                                                                     { _lhsOmbVar ->
                                                                                                                                     ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                             in  sem_CExpr_Lam_2)) of
                                                      { ( sem_CExpr_2) ->
                                                      ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }))
                         in  sem_CExpr_Lam_1)) of
                  { ( sem_CExpr_1) ->
                  ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) }) }) }) })
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (case (body_ ) of
     { ( _bodyIgathLamMp,_bodyIgrLamArgL,body_1) ->
         (case (binds_ ) of
          { ( _bindsIbindLamMp,binds_1) ->
              (case (_bindsIbindLamMp `Map.union` _bodyIgathLamMp) of
               { _lhsOgathLamMp ->
               (case ([]) of
                { _lhsOgrLamArgL ->
                (case ((let sem_CExpr_Let_1 :: T_CExpr_1 
                            sem_CExpr_Let_1  =
                                (\ _lhsIgUniq ->
                                     (case (_lhsIgUniq) of
                                      { _bindsOgUniq ->
                                      (case (binds_1 _bindsOgUniq ) of
                                       { ( _bindsIgUniq,binds_2) ->
                                           (case (_bindsIgUniq) of
                                            { _bodyOgUniq ->
                                            (case (body_1 _bodyOgUniq ) of
                                             { ( _bodyIgUniq,_bodyIwhatBelow,body_2) ->
                                                 (case (_bodyIgUniq) of
                                                  { _lhsOgUniq ->
                                                  (case (ExprIsOther) of
                                                   { _whatBelow ->
                                                   (case (_whatBelow) of
                                                    { _lhsOwhatBelow ->
                                                    (case ((let sem_CExpr_Let_2 :: T_CExpr_2 
                                                                sem_CExpr_Let_2  =
                                                                    (\ _lhsIdataGam
                                                                       _lhsIdoBox
                                                                       _lhsIevalCtx
                                                                       _lhsIisLamBody
                                                                       _lhsIisStrict
                                                                       _lhsIisTopApp
                                                                       _lhsIisTopTup
                                                                       _lhsIlamMp
                                                                       _lhsIlev
                                                                       _lhsImodNm
                                                                       _lhsIopts
                                                                       _lhsIwhatAbove ->
                                                                         (case (AppFunKind_NoApp) of
                                                                          { _lhsOappFunKind ->
                                                                          (case (ExprIsOther) of
                                                                           { _whatAbove ->
                                                                           (case (_whatAbove) of
                                                                            { _bodyOwhatAbove ->
                                                                            (case (_lhsIopts) of
                                                                             { _bodyOopts ->
                                                                             (case (_lhsImodNm) of
                                                                              { _bodyOmodNm ->
                                                                              (case (_lhsIlev) of
                                                                               { _bodyOlev ->
                                                                               (case (_lhsIlamMp) of
                                                                                { _bodyOlamMp ->
                                                                                (case (True) of
                                                                                 { _isTopTup ->
                                                                                 (case (_isTopTup) of
                                                                                  { _bodyOisTopTup ->
                                                                                  (case (True) of
                                                                                   { _isTopApp ->
                                                                                   (case (_isTopApp) of
                                                                                    { _bodyOisTopApp ->
                                                                                    (case (_lhsIisStrict) of
                                                                                     { _bodyOisStrict ->
                                                                                     (case (_lhsIisLamBody) of
                                                                                      { _bodyOisLamBody ->
                                                                                      (case (if categ_ == CBindCateg_Strict
                                                                                             then EvalCtx_Eval
                                                                                             else EvalCtx_None) of
                                                                                       { _evalCtx ->
                                                                                       (case (_evalCtx) of
                                                                                        { _bodyOevalCtx ->
                                                                                        (case (True) of
                                                                                         { _doBox ->
                                                                                         (case (_doBox) of
                                                                                          { _bodyOdoBox ->
                                                                                          (case (_lhsIdataGam) of
                                                                                           { _bodyOdataGam ->
                                                                                           (case (body_2 _bodyOdataGam _bodyOdoBox _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmodNm _bodyOopts _bodyOwhatAbove ) of
                                                                                            { ( _bodyIappFunKind,_bodyIfvS,_bodyIgrAppArgL,_bodyIgrAppFun,_bodyIgrBindL,_bodyIgrExpr,_bodyIgrGlobalL,_bodyIgrLamBody,_bodyIgrLetBody,_bodyIgrTupFldL,_bodyIgrTupRec,_bodyIgrVal,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar) ->
                                                                                                (case (_lhsIopts) of
                                                                                                 { _bindsOopts ->
                                                                                                 (case (_lhsImodNm) of
                                                                                                  { _bindsOmodNm ->
                                                                                                  (case (categ_) of
                                                                                                   { _letBindingsCateg ->
                                                                                                   (case (_letBindingsCateg) of
                                                                                                    { _bindsOletBindingsCateg ->
                                                                                                    (case (_lhsIlamMp) of
                                                                                                     { _bindsOlamMp ->
                                                                                                     (case (_lhsIisLamBody) of
                                                                                                      { _bindsOisLamBody ->
                                                                                                      (case (_lhsIlev == cLevModule) of
                                                                                                       { _isGlobal ->
                                                                                                       (case (_isGlobal) of
                                                                                                        { _bindsOisGlobal ->
                                                                                                        (case (_evalCtx) of
                                                                                                         { _bindsOevalCtx ->
                                                                                                         (case (_lhsIdataGam) of
                                                                                                          { _bindsOdataGam ->
                                                                                                          (case (_isGlobal || categ_ == CBindCateg_Strict) of
                                                                                                           { _bindsOisStrict ->
                                                                                                           (case (_lhsIlev + 1) of
                                                                                                            { _bindsOlev ->
                                                                                                            (case (binds_2 _bindsOdataGam _bindsOevalCtx _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOlamMp _bindsOletBindingsCateg _bindsOlev _bindsOmodNm _bindsOopts ) of
                                                                                                             { ( _bindsIfvS,_bindsIfvSMp,_bindsIgrBindL,_bindsIgrGlobalL,_bindsInmL) ->
                                                                                                                 (case ((_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL) of
                                                                                                                  { _fvS ->
                                                                                                                  (case (_fvS) of
                                                                                                                   { _lhsOfvS ->
                                                                                                                   (case ([]) of
                                                                                                                    { _lhsOgrAppArgL ->
                                                                                                                    (case (GrVal_Empty) of
                                                                                                                     { _grVal ->
                                                                                                                     (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                                      { _lhsOgrAppFun ->
                                                                                                                      (case (if _isGlobal
                                                                                                                             then  (emptyGrExpr
                                                                                                                                   ,case categ_ of
                                                                                                                                      CBindCateg_Rec ->  [GrBind_Rec _bindsIgrBindL] ++ _bodyIgrBindL
                                                                                                                                      _              ->              _bindsIgrBindL  ++ _bodyIgrBindL
                                                                                                                                   )
                                                                                                                             else  (case categ_ of
                                                                                                                                      CBindCateg_Rec ->  mkHoles _bindsIgrBindL
                                                                                                                                                         $ mkVarBindWithUpdates _bindsIgrBindL
                                                                                                                                                         $ _bodyIgrExpr
                                                                                                                                      _              ->  mkVarBinds _bindsIgrBindL _bodyIgrExpr
                                                                                                                                   ,[]
                                                                                                                                   )) of
                                                                                                                       { __tup6 ->
                                                                                                                       (case (__tup6) of
                                                                                                                        { (_,_lhsOgrBindL) ->
                                                                                                                        (case (__tup6) of
                                                                                                                         { (_grExpr,_) ->
                                                                                                                         (case (_grExpr) of
                                                                                                                          { _lhsOgrExpr ->
                                                                                                                          (case (_bindsIgrGlobalL ++ _bodyIgrGlobalL) of
                                                                                                                           { _lhsOgrGlobalL ->
                                                                                                                           (case (_grExpr) of
                                                                                                                            { _lhsOgrLamBody ->
                                                                                                                            (case (_bodyIgrLetBody) of
                                                                                                                             { _lhsOgrLetBody ->
                                                                                                                             (case ([]) of
                                                                                                                              { _grTupFldL ->
                                                                                                                              (case (_grTupFldL) of
                                                                                                                               { _lhsOgrTupFldL ->
                                                                                                                               (case (_grVal) of
                                                                                                                                { _grTupRec ->
                                                                                                                                (case (_grTupRec) of
                                                                                                                                 { _lhsOgrTupRec ->
                                                                                                                                 (case (_grVal) of
                                                                                                                                  { _lhsOgrVal ->
                                                                                                                                  (case (Nothing) of
                                                                                                                                   { _lhsOmbFFIApp ->
                                                                                                                                   (case (Nothing) of
                                                                                                                                    { _lhsOmbLam ->
                                                                                                                                    (case (Nothing) of
                                                                                                                                     { _lhsOmbVar ->
                                                                                                                                     ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                            in  sem_CExpr_Let_2)) of
                                                     { ( sem_CExpr_2) ->
                                                     ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }))
                        in  sem_CExpr_Let_1)) of
                 { ( sem_CExpr_1) ->
                 ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) }) }) })
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_String_1 :: T_CExpr_1 
                  sem_CExpr_String_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_String_2 :: T_CExpr_2 
                                          sem_CExpr_String_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_NoApp) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case ((if _lhsIdoBox then mkGrBox (ehcOptBuiltin _lhsIopts ehbnPackedString) else id)  (GrVal_LitStr str_)) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (GrExpr_Unit _grVal GrType_None) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_String_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_String_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Tup_1 :: T_CExpr_1 
                  sem_CExpr_Tup_1  =
                      (\ _lhsIgUniq ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case (ExprIsOther) of
                             { _whatBelow ->
                             (case (_whatBelow) of
                              { _lhsOwhatBelow ->
                              (case ((let sem_CExpr_Tup_2 :: T_CExpr_2 
                                          sem_CExpr_Tup_2  =
                                              (\ _lhsIdataGam
                                                 _lhsIdoBox
                                                 _lhsIevalCtx
                                                 _lhsIisLamBody
                                                 _lhsIisStrict
                                                 _lhsIisTopApp
                                                 _lhsIisTopTup
                                                 _lhsIlamMp
                                                 _lhsIlev
                                                 _lhsImodNm
                                                 _lhsIopts
                                                 _lhsIwhatAbove ->
                                                   (case (AppFunKind_Tag tag_) of
                                                    { _lhsOappFunKind ->
                                                    (case (Set.empty) of
                                                     { _lhsOfvS ->
                                                     (case ([]) of
                                                      { _lhsOgrAppArgL ->
                                                      (case (ctag (mkGrRecNode []) (\_ l t a ma -> mkGrConNode (mkGrTagAnn a ma) t l []) tag_) of
                                                       { _grVal ->
                                                       (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                        { _lhsOgrAppFun ->
                                                        (case ([]) of
                                                         { _lhsOgrBindL ->
                                                         (case (retStrict _lhsIisStrict _grVal) of
                                                          { _grExpr ->
                                                          (case (_grExpr) of
                                                           { _lhsOgrExpr ->
                                                           (case ([]) of
                                                            { _lhsOgrGlobalL ->
                                                            (case (_grExpr) of
                                                             { _lhsOgrLamBody ->
                                                             (case (_grExpr) of
                                                              { _lhsOgrLetBody ->
                                                              (case ([]) of
                                                               { _grTupFldL ->
                                                               (case (_grTupFldL) of
                                                                { _lhsOgrTupFldL ->
                                                                (case (_grVal) of
                                                                 { _grTupRec ->
                                                                 (case (_grTupRec) of
                                                                  { _lhsOgrTupRec ->
                                                                  (case (_grVal) of
                                                                   { _lhsOgrVal ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbFFIApp ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar ->
                                                                      ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                      in  sem_CExpr_Tup_2)) of
                               { ( sem_CExpr_2) ->
                               ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }))
              in  sem_CExpr_Tup_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_TupDel_1 :: T_CExpr_1 
                  sem_CExpr_TupDel_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup7 ->
                            (case (__tup7) of
                             { (_exprOgUniq,_) ->
                             (case (expr_ ) of
                              { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                  (case (expr_1 _exprOgUniq ) of
                                   { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                       (case (_exprIgUniq) of
                                        { _offsetOgUniq ->
                                        (case (offset_ ) of
                                         { ( _offsetIgathLamMp,_offsetIgrLamArgL,offset_1) ->
                                             (case (offset_1 _offsetOgUniq ) of
                                              { ( _offsetIgUniq,_offsetIwhatBelow,offset_2) ->
                                                  (case (_offsetIgUniq) of
                                                   { _lhsOgUniq ->
                                                   (case (ExprIsOther) of
                                                    { _whatBelow ->
                                                    (case (_whatBelow) of
                                                     { _lhsOwhatBelow ->
                                                     (case ((let sem_CExpr_TupDel_2 :: T_CExpr_2 
                                                                 sem_CExpr_TupDel_2  =
                                                                     (\ _lhsIdataGam
                                                                        _lhsIdoBox
                                                                        _lhsIevalCtx
                                                                        _lhsIisLamBody
                                                                        _lhsIisStrict
                                                                        _lhsIisTopApp
                                                                        _lhsIisTopTup
                                                                        _lhsIlamMp
                                                                        _lhsIlev
                                                                        _lhsImodNm
                                                                        _lhsIopts
                                                                        _lhsIwhatAbove ->
                                                                          (case (AppFunKind_NoApp) of
                                                                           { _lhsOappFunKind ->
                                                                           (case (ExprIsOther) of
                                                                            { _whatAbove ->
                                                                            (case (_whatAbove) of
                                                                             { _offsetOwhatAbove ->
                                                                             (case (_lhsIopts) of
                                                                              { _offsetOopts ->
                                                                              (case (_lhsImodNm) of
                                                                               { _offsetOmodNm ->
                                                                               (case (_lhsIlev) of
                                                                                { _offsetOlev ->
                                                                                (case (_lhsIlamMp) of
                                                                                 { _offsetOlamMp ->
                                                                                 (case (True) of
                                                                                  { _isTopTup ->
                                                                                  (case (_isTopTup) of
                                                                                   { _offsetOisTopTup ->
                                                                                   (case (True) of
                                                                                    { _isTopApp ->
                                                                                    (case (_isTopApp) of
                                                                                     { _offsetOisTopApp ->
                                                                                     (case (_lhsIisStrict) of
                                                                                      { _offsetOisStrict ->
                                                                                      (case (_lhsIisLamBody) of
                                                                                       { _offsetOisLamBody ->
                                                                                       (case (_lhsIevalCtx) of
                                                                                        { _offsetOevalCtx ->
                                                                                        (case (_lhsIdataGam) of
                                                                                         { _offsetOdataGam ->
                                                                                         (case (False) of
                                                                                          { _offsetOdoBox ->
                                                                                          (case (offset_2 _offsetOdataGam _offsetOdoBox _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmodNm _offsetOopts _offsetOwhatAbove ) of
                                                                                           { ( _offsetIappFunKind,_offsetIfvS,_offsetIgrAppArgL,_offsetIgrAppFun,_offsetIgrBindL,_offsetIgrExpr,_offsetIgrGlobalL,_offsetIgrLamBody,_offsetIgrLetBody,_offsetIgrTupFldL,_offsetIgrTupRec,_offsetIgrVal,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar) ->
                                                                                               (case (_whatAbove) of
                                                                                                { _exprOwhatAbove ->
                                                                                                (case (_lhsIopts) of
                                                                                                 { _exprOopts ->
                                                                                                 (case (_lhsImodNm) of
                                                                                                  { _exprOmodNm ->
                                                                                                  (case (_lhsIlev) of
                                                                                                   { _exprOlev ->
                                                                                                   (case (_lhsIlamMp) of
                                                                                                    { _exprOlamMp ->
                                                                                                    (case (_isTopApp) of
                                                                                                     { _exprOisTopApp ->
                                                                                                     (case (_lhsIisStrict) of
                                                                                                      { _exprOisStrict ->
                                                                                                      (case (_lhsIisLamBody) of
                                                                                                       { _exprOisLamBody ->
                                                                                                       (case (_lhsIevalCtx) of
                                                                                                        { _exprOevalCtx ->
                                                                                                        (case (True) of
                                                                                                         { _doBox ->
                                                                                                         (case (_doBox) of
                                                                                                          { _exprOdoBox ->
                                                                                                          (case (_lhsIdataGam) of
                                                                                                           { _exprOdataGam ->
                                                                                                           (case (False) of
                                                                                                            { _exprOisTopTup ->
                                                                                                            (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                                                             { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                                                 (case (_exprIfvS `Set.union` _offsetIfvS) of
                                                                                                                  { _lhsOfvS ->
                                                                                                                  (case ([]) of
                                                                                                                   { _lhsOgrAppArgL ->
                                                                                                                   (case (GrVal_Empty) of
                                                                                                                    { _grVal ->
                                                                                                                    (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                                     { _lhsOgrAppFun ->
                                                                                                                     (case (_exprIgrBindL ++ _offsetIgrBindL) of
                                                                                                                      { _lhsOgrBindL ->
                                                                                                                      (case (__tup7) of
                                                                                                                       { (_,_lUniq) ->
                                                                                                                       (case ((_offsetIgrVal,GrVal_Empty   ,\o _ -> GrAdapt_Del o  ) : _exprIgrTupFldL) of
                                                                                                                        { _grTupFldL ->
                                                                                                                        (case (_exprIgrTupRec) of
                                                                                                                         { _grTupRec ->
                                                                                                                         (case (if _lhsIisTopTup
                                                                                                                                then  mkGrAdapt _lhsImodNm _lUniq _lhsIlamMp _lhsIisStrict (reverse _grTupFldL) (maybe hsnUnknown id (grV2HNm _grTupRec))
                                                                                                                                else  GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                                                          { _grExpr ->
                                                                                                                          (case (_grExpr) of
                                                                                                                           { _lhsOgrExpr ->
                                                                                                                           (case (_exprIgrGlobalL ++ _offsetIgrGlobalL) of
                                                                                                                            { _lhsOgrGlobalL ->
                                                                                                                            (case (_grExpr) of
                                                                                                                             { _lhsOgrLamBody ->
                                                                                                                             (case (_grExpr) of
                                                                                                                              { _lhsOgrLetBody ->
                                                                                                                              (case (_grTupFldL) of
                                                                                                                               { _lhsOgrTupFldL ->
                                                                                                                               (case (_grTupRec) of
                                                                                                                                { _lhsOgrTupRec ->
                                                                                                                                (case (_grVal) of
                                                                                                                                 { _lhsOgrVal ->
                                                                                                                                 (case (Nothing) of
                                                                                                                                  { _lhsOmbFFIApp ->
                                                                                                                                  (case (Nothing) of
                                                                                                                                   { _lhsOmbLam ->
                                                                                                                                   (case (Nothing) of
                                                                                                                                    { _lhsOmbVar ->
                                                                                                                                    ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                             in  sem_CExpr_TupDel_2)) of
                                                      { ( sem_CExpr_2) ->
                                                      ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupDel_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_TupIns_1 :: T_CExpr_1 
                  sem_CExpr_TupIns_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup8 ->
                            (case (__tup8) of
                             { (_exprOgUniq,_) ->
                             (case (expr_ ) of
                              { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                  (case (expr_1 _exprOgUniq ) of
                                   { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                       (case (_exprIgUniq) of
                                        { _offsetOgUniq ->
                                        (case (offset_ ) of
                                         { ( _offsetIgathLamMp,_offsetIgrLamArgL,offset_1) ->
                                             (case (offset_1 _offsetOgUniq ) of
                                              { ( _offsetIgUniq,_offsetIwhatBelow,offset_2) ->
                                                  (case (_offsetIgUniq) of
                                                   { _fldExprOgUniq ->
                                                   (case (fldExpr_ ) of
                                                    { ( _fldExprIgathLamMp,_fldExprIgrLamArgL,fldExpr_1) ->
                                                        (case (fldExpr_1 _fldExprOgUniq ) of
                                                         { ( _fldExprIgUniq,_fldExprIwhatBelow,fldExpr_2) ->
                                                             (case (_fldExprIgUniq) of
                                                              { _lhsOgUniq ->
                                                              (case (ExprIsOther) of
                                                               { _whatBelow ->
                                                               (case (_whatBelow) of
                                                                { _lhsOwhatBelow ->
                                                                (case ((let sem_CExpr_TupIns_2 :: T_CExpr_2 
                                                                            sem_CExpr_TupIns_2  =
                                                                                (\ _lhsIdataGam
                                                                                   _lhsIdoBox
                                                                                   _lhsIevalCtx
                                                                                   _lhsIisLamBody
                                                                                   _lhsIisStrict
                                                                                   _lhsIisTopApp
                                                                                   _lhsIisTopTup
                                                                                   _lhsIlamMp
                                                                                   _lhsIlev
                                                                                   _lhsImodNm
                                                                                   _lhsIopts
                                                                                   _lhsIwhatAbove ->
                                                                                     (case (AppFunKind_NoApp) of
                                                                                      { _lhsOappFunKind ->
                                                                                      (case (ExprIsOther) of
                                                                                       { _whatAbove ->
                                                                                       (case (_whatAbove) of
                                                                                        { _fldExprOwhatAbove ->
                                                                                        (case (_lhsIopts) of
                                                                                         { _fldExprOopts ->
                                                                                         (case (_lhsImodNm) of
                                                                                          { _fldExprOmodNm ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _fldExprOlev ->
                                                                                           (case (_lhsIlamMp) of
                                                                                            { _fldExprOlamMp ->
                                                                                            (case (True) of
                                                                                             { _isTopTup ->
                                                                                             (case (_isTopTup) of
                                                                                              { _fldExprOisTopTup ->
                                                                                              (case (True) of
                                                                                               { _isTopApp ->
                                                                                               (case (_isTopApp) of
                                                                                                { _fldExprOisTopApp ->
                                                                                                (case (_lhsIisStrict) of
                                                                                                 { _fldExprOisStrict ->
                                                                                                 (case (_lhsIisLamBody) of
                                                                                                  { _fldExprOisLamBody ->
                                                                                                  (case (_lhsIevalCtx) of
                                                                                                   { _fldExprOevalCtx ->
                                                                                                   (case (True) of
                                                                                                    { _doBox ->
                                                                                                    (case (_doBox) of
                                                                                                     { _fldExprOdoBox ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _fldExprOdataGam ->
                                                                                                      (case (fldExpr_2 _fldExprOdataGam _fldExprOdoBox _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamMp _fldExprOlev _fldExprOmodNm _fldExprOopts _fldExprOwhatAbove ) of
                                                                                                       { ( _fldExprIappFunKind,_fldExprIfvS,_fldExprIgrAppArgL,_fldExprIgrAppFun,_fldExprIgrBindL,_fldExprIgrExpr,_fldExprIgrGlobalL,_fldExprIgrLamBody,_fldExprIgrLetBody,_fldExprIgrTupFldL,_fldExprIgrTupRec,_fldExprIgrVal,_fldExprImbFFIApp,_fldExprImbLam,_fldExprImbVar) ->
                                                                                                           (case (_whatAbove) of
                                                                                                            { _offsetOwhatAbove ->
                                                                                                            (case (_lhsIopts) of
                                                                                                             { _offsetOopts ->
                                                                                                             (case (_lhsImodNm) of
                                                                                                              { _offsetOmodNm ->
                                                                                                              (case (_lhsIlev) of
                                                                                                               { _offsetOlev ->
                                                                                                               (case (_lhsIlamMp) of
                                                                                                                { _offsetOlamMp ->
                                                                                                                (case (_isTopTup) of
                                                                                                                 { _offsetOisTopTup ->
                                                                                                                 (case (_isTopApp) of
                                                                                                                  { _offsetOisTopApp ->
                                                                                                                  (case (_lhsIisStrict) of
                                                                                                                   { _offsetOisStrict ->
                                                                                                                   (case (_lhsIisLamBody) of
                                                                                                                    { _offsetOisLamBody ->
                                                                                                                    (case (_lhsIevalCtx) of
                                                                                                                     { _offsetOevalCtx ->
                                                                                                                     (case (_lhsIdataGam) of
                                                                                                                      { _offsetOdataGam ->
                                                                                                                      (case (False) of
                                                                                                                       { _offsetOdoBox ->
                                                                                                                       (case (offset_2 _offsetOdataGam _offsetOdoBox _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmodNm _offsetOopts _offsetOwhatAbove ) of
                                                                                                                        { ( _offsetIappFunKind,_offsetIfvS,_offsetIgrAppArgL,_offsetIgrAppFun,_offsetIgrBindL,_offsetIgrExpr,_offsetIgrGlobalL,_offsetIgrLamBody,_offsetIgrLetBody,_offsetIgrTupFldL,_offsetIgrTupRec,_offsetIgrVal,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar) ->
                                                                                                                            (case (_whatAbove) of
                                                                                                                             { _exprOwhatAbove ->
                                                                                                                             (case (_lhsIopts) of
                                                                                                                              { _exprOopts ->
                                                                                                                              (case (_lhsImodNm) of
                                                                                                                               { _exprOmodNm ->
                                                                                                                               (case (_lhsIlev) of
                                                                                                                                { _exprOlev ->
                                                                                                                                (case (_lhsIlamMp) of
                                                                                                                                 { _exprOlamMp ->
                                                                                                                                 (case (_isTopApp) of
                                                                                                                                  { _exprOisTopApp ->
                                                                                                                                  (case (_lhsIisStrict) of
                                                                                                                                   { _exprOisStrict ->
                                                                                                                                   (case (_lhsIisLamBody) of
                                                                                                                                    { _exprOisLamBody ->
                                                                                                                                    (case (_lhsIevalCtx) of
                                                                                                                                     { _exprOevalCtx ->
                                                                                                                                     (case (_doBox) of
                                                                                                                                      { _exprOdoBox ->
                                                                                                                                      (case (_lhsIdataGam) of
                                                                                                                                       { _exprOdataGam ->
                                                                                                                                       (case (False) of
                                                                                                                                        { _exprOisTopTup ->
                                                                                                                                        (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                                                                                         { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                                                                             (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                                                                                                                              { _lhsOfvS ->
                                                                                                                                              (case ([]) of
                                                                                                                                               { _lhsOgrAppArgL ->
                                                                                                                                               (case (GrVal_Empty) of
                                                                                                                                                { _grVal ->
                                                                                                                                                (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                                                                 { _lhsOgrAppFun ->
                                                                                                                                                 (case (_exprIgrBindL ++ _offsetIgrBindL ++ _fldExprIgrBindL) of
                                                                                                                                                  { _lhsOgrBindL ->
                                                                                                                                                  (case (__tup8) of
                                                                                                                                                   { (_,_lUniq) ->
                                                                                                                                                   (case ((_offsetIgrVal,_fldExprIgrVal,\o v -> GrAdapt_Ins o v) : _exprIgrTupFldL) of
                                                                                                                                                    { _grTupFldL ->
                                                                                                                                                    (case (_exprIgrTupRec) of
                                                                                                                                                     { _grTupRec ->
                                                                                                                                                     (case (if _lhsIisTopTup
                                                                                                                                                            then  mkGrAdapt _lhsImodNm _lUniq _lhsIlamMp _lhsIisStrict (reverse _grTupFldL) (maybe hsnUnknown id (grV2HNm _grTupRec))
                                                                                                                                                            else  GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                                                                                      { _grExpr ->
                                                                                                                                                      (case (_grExpr) of
                                                                                                                                                       { _lhsOgrExpr ->
                                                                                                                                                       (case (_exprIgrGlobalL ++ _offsetIgrGlobalL ++ _fldExprIgrGlobalL) of
                                                                                                                                                        { _lhsOgrGlobalL ->
                                                                                                                                                        (case (_grExpr) of
                                                                                                                                                         { _lhsOgrLamBody ->
                                                                                                                                                         (case (_grExpr) of
                                                                                                                                                          { _lhsOgrLetBody ->
                                                                                                                                                          (case (_grTupFldL) of
                                                                                                                                                           { _lhsOgrTupFldL ->
                                                                                                                                                           (case (_grTupRec) of
                                                                                                                                                            { _lhsOgrTupRec ->
                                                                                                                                                            (case (_grVal) of
                                                                                                                                                             { _lhsOgrVal ->
                                                                                                                                                             (case (Nothing) of
                                                                                                                                                              { _lhsOmbFFIApp ->
                                                                                                                                                              (case (Nothing) of
                                                                                                                                                               { _lhsOmbLam ->
                                                                                                                                                               (case (Nothing) of
                                                                                                                                                                { _lhsOmbVar ->
                                                                                                                                                                ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                        in  sem_CExpr_TupIns_2)) of
                                                                 { ( sem_CExpr_2) ->
                                                                 ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupIns_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_TupUpd_1 :: T_CExpr_1 
                  sem_CExpr_TupUpd_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup9 ->
                            (case (__tup9) of
                             { (_exprOgUniq,_) ->
                             (case (expr_ ) of
                              { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
                                  (case (expr_1 _exprOgUniq ) of
                                   { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                       (case (_exprIgUniq) of
                                        { _offsetOgUniq ->
                                        (case (offset_ ) of
                                         { ( _offsetIgathLamMp,_offsetIgrLamArgL,offset_1) ->
                                             (case (offset_1 _offsetOgUniq ) of
                                              { ( _offsetIgUniq,_offsetIwhatBelow,offset_2) ->
                                                  (case (_offsetIgUniq) of
                                                   { _fldExprOgUniq ->
                                                   (case (fldExpr_ ) of
                                                    { ( _fldExprIgathLamMp,_fldExprIgrLamArgL,fldExpr_1) ->
                                                        (case (fldExpr_1 _fldExprOgUniq ) of
                                                         { ( _fldExprIgUniq,_fldExprIwhatBelow,fldExpr_2) ->
                                                             (case (_fldExprIgUniq) of
                                                              { _lhsOgUniq ->
                                                              (case (ExprIsOther) of
                                                               { _whatBelow ->
                                                               (case (_whatBelow) of
                                                                { _lhsOwhatBelow ->
                                                                (case ((let sem_CExpr_TupUpd_2 :: T_CExpr_2 
                                                                            sem_CExpr_TupUpd_2  =
                                                                                (\ _lhsIdataGam
                                                                                   _lhsIdoBox
                                                                                   _lhsIevalCtx
                                                                                   _lhsIisLamBody
                                                                                   _lhsIisStrict
                                                                                   _lhsIisTopApp
                                                                                   _lhsIisTopTup
                                                                                   _lhsIlamMp
                                                                                   _lhsIlev
                                                                                   _lhsImodNm
                                                                                   _lhsIopts
                                                                                   _lhsIwhatAbove ->
                                                                                     (case (AppFunKind_NoApp) of
                                                                                      { _lhsOappFunKind ->
                                                                                      (case (ExprIsOther) of
                                                                                       { _whatAbove ->
                                                                                       (case (_whatAbove) of
                                                                                        { _fldExprOwhatAbove ->
                                                                                        (case (_lhsIopts) of
                                                                                         { _fldExprOopts ->
                                                                                         (case (_lhsImodNm) of
                                                                                          { _fldExprOmodNm ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _fldExprOlev ->
                                                                                           (case (_lhsIlamMp) of
                                                                                            { _fldExprOlamMp ->
                                                                                            (case (True) of
                                                                                             { _isTopTup ->
                                                                                             (case (_isTopTup) of
                                                                                              { _fldExprOisTopTup ->
                                                                                              (case (True) of
                                                                                               { _isTopApp ->
                                                                                               (case (_isTopApp) of
                                                                                                { _fldExprOisTopApp ->
                                                                                                (case (_lhsIisStrict) of
                                                                                                 { _fldExprOisStrict ->
                                                                                                 (case (_lhsIisLamBody) of
                                                                                                  { _fldExprOisLamBody ->
                                                                                                  (case (_lhsIevalCtx) of
                                                                                                   { _fldExprOevalCtx ->
                                                                                                   (case (True) of
                                                                                                    { _doBox ->
                                                                                                    (case (_doBox) of
                                                                                                     { _fldExprOdoBox ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _fldExprOdataGam ->
                                                                                                      (case (fldExpr_2 _fldExprOdataGam _fldExprOdoBox _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamMp _fldExprOlev _fldExprOmodNm _fldExprOopts _fldExprOwhatAbove ) of
                                                                                                       { ( _fldExprIappFunKind,_fldExprIfvS,_fldExprIgrAppArgL,_fldExprIgrAppFun,_fldExprIgrBindL,_fldExprIgrExpr,_fldExprIgrGlobalL,_fldExprIgrLamBody,_fldExprIgrLetBody,_fldExprIgrTupFldL,_fldExprIgrTupRec,_fldExprIgrVal,_fldExprImbFFIApp,_fldExprImbLam,_fldExprImbVar) ->
                                                                                                           (case (_whatAbove) of
                                                                                                            { _offsetOwhatAbove ->
                                                                                                            (case (_lhsIopts) of
                                                                                                             { _offsetOopts ->
                                                                                                             (case (_lhsImodNm) of
                                                                                                              { _offsetOmodNm ->
                                                                                                              (case (_lhsIlev) of
                                                                                                               { _offsetOlev ->
                                                                                                               (case (_lhsIlamMp) of
                                                                                                                { _offsetOlamMp ->
                                                                                                                (case (_isTopTup) of
                                                                                                                 { _offsetOisTopTup ->
                                                                                                                 (case (_isTopApp) of
                                                                                                                  { _offsetOisTopApp ->
                                                                                                                  (case (_lhsIisStrict) of
                                                                                                                   { _offsetOisStrict ->
                                                                                                                   (case (_lhsIisLamBody) of
                                                                                                                    { _offsetOisLamBody ->
                                                                                                                    (case (_lhsIevalCtx) of
                                                                                                                     { _offsetOevalCtx ->
                                                                                                                     (case (_lhsIdataGam) of
                                                                                                                      { _offsetOdataGam ->
                                                                                                                      (case (False) of
                                                                                                                       { _offsetOdoBox ->
                                                                                                                       (case (offset_2 _offsetOdataGam _offsetOdoBox _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmodNm _offsetOopts _offsetOwhatAbove ) of
                                                                                                                        { ( _offsetIappFunKind,_offsetIfvS,_offsetIgrAppArgL,_offsetIgrAppFun,_offsetIgrBindL,_offsetIgrExpr,_offsetIgrGlobalL,_offsetIgrLamBody,_offsetIgrLetBody,_offsetIgrTupFldL,_offsetIgrTupRec,_offsetIgrVal,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar) ->
                                                                                                                            (case (_whatAbove) of
                                                                                                                             { _exprOwhatAbove ->
                                                                                                                             (case (_lhsIopts) of
                                                                                                                              { _exprOopts ->
                                                                                                                              (case (_lhsImodNm) of
                                                                                                                               { _exprOmodNm ->
                                                                                                                               (case (_lhsIlev) of
                                                                                                                                { _exprOlev ->
                                                                                                                                (case (_lhsIlamMp) of
                                                                                                                                 { _exprOlamMp ->
                                                                                                                                 (case (_isTopApp) of
                                                                                                                                  { _exprOisTopApp ->
                                                                                                                                  (case (_lhsIisStrict) of
                                                                                                                                   { _exprOisStrict ->
                                                                                                                                   (case (_lhsIisLamBody) of
                                                                                                                                    { _exprOisLamBody ->
                                                                                                                                    (case (_lhsIevalCtx) of
                                                                                                                                     { _exprOevalCtx ->
                                                                                                                                     (case (_doBox) of
                                                                                                                                      { _exprOdoBox ->
                                                                                                                                      (case (_lhsIdataGam) of
                                                                                                                                       { _exprOdataGam ->
                                                                                                                                       (case (False) of
                                                                                                                                        { _exprOisTopTup ->
                                                                                                                                        (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                                                                                                         { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                                                                                                             (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                                                                                                                              { _lhsOfvS ->
                                                                                                                                              (case ([]) of
                                                                                                                                               { _lhsOgrAppArgL ->
                                                                                                                                               (case (GrVal_Empty) of
                                                                                                                                                { _grVal ->
                                                                                                                                                (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                                                                                                                 { _lhsOgrAppFun ->
                                                                                                                                                 (case (_exprIgrBindL ++ _offsetIgrBindL ++ _fldExprIgrBindL) of
                                                                                                                                                  { _lhsOgrBindL ->
                                                                                                                                                  (case (__tup9) of
                                                                                                                                                   { (_,_lUniq) ->
                                                                                                                                                   (case ((_offsetIgrVal,_fldExprIgrVal,\o v -> GrAdapt_Upd o v) : _exprIgrTupFldL) of
                                                                                                                                                    { _grTupFldL ->
                                                                                                                                                    (case (_exprIgrTupRec) of
                                                                                                                                                     { _grTupRec ->
                                                                                                                                                     (case (if _lhsIisTopTup
                                                                                                                                                            then  mkGrAdapt _lhsImodNm _lUniq _lhsIlamMp _lhsIisStrict (reverse _grTupFldL) (maybe hsnUnknown id (grV2HNm _grTupRec))
                                                                                                                                                            else  GrExpr_Unit GrVal_Empty GrType_None) of
                                                                                                                                                      { _grExpr ->
                                                                                                                                                      (case (_grExpr) of
                                                                                                                                                       { _lhsOgrExpr ->
                                                                                                                                                       (case (_exprIgrGlobalL ++ _offsetIgrGlobalL ++ _fldExprIgrGlobalL) of
                                                                                                                                                        { _lhsOgrGlobalL ->
                                                                                                                                                        (case (_grExpr) of
                                                                                                                                                         { _lhsOgrLamBody ->
                                                                                                                                                         (case (_grExpr) of
                                                                                                                                                          { _lhsOgrLetBody ->
                                                                                                                                                          (case (_grTupFldL) of
                                                                                                                                                           { _lhsOgrTupFldL ->
                                                                                                                                                           (case (_grTupRec) of
                                                                                                                                                            { _lhsOgrTupRec ->
                                                                                                                                                            (case (_grVal) of
                                                                                                                                                             { _lhsOgrVal ->
                                                                                                                                                             (case (Nothing) of
                                                                                                                                                              { _lhsOmbFFIApp ->
                                                                                                                                                              (case (Nothing) of
                                                                                                                                                               { _lhsOmbLam ->
                                                                                                                                                               (case (Nothing) of
                                                                                                                                                                { _lhsOmbVar ->
                                                                                                                                                                ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                        in  sem_CExpr_TupUpd_2)) of
                                                                 { ( sem_CExpr_2) ->
                                                                 ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupUpd_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (case (Map.empty) of
     { _lhsOgathLamMp ->
     (case ([]) of
      { _lhsOgrLamArgL ->
      (case ((let sem_CExpr_Var_1 :: T_CExpr_1 
                  sem_CExpr_Var_1  =
                      (\ _lhsIgUniq ->
                           (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                            { __tup10 ->
                            (case (__tup10) of
                             { (_lhsOgUniq,_) ->
                             (case (acbrefNm ref_) of
                              { _nm ->
                              (case (ExprIsVar _nm) of
                               { _whatBelow ->
                               (case (_whatBelow) of
                                { _lhsOwhatBelow ->
                                (case ((let sem_CExpr_Var_2 :: T_CExpr_2 
                                            sem_CExpr_Var_2  =
                                                (\ _lhsIdataGam
                                                   _lhsIdoBox
                                                   _lhsIevalCtx
                                                   _lhsIisLamBody
                                                   _lhsIisStrict
                                                   _lhsIisTopApp
                                                   _lhsIisTopTup
                                                   _lhsIlamMp
                                                   _lhsIlev
                                                   _lhsImodNm
                                                   _lhsIopts
                                                   _lhsIwhatAbove ->
                                                     (case (AppFunKind_Fun ref_) of
                                                      { _lhsOappFunKind ->
                                                      (case (Set.singleton _nm) of
                                                       { _lhsOfvS ->
                                                       (case ([]) of
                                                        { _lhsOgrAppArgL ->
                                                        (case (mkHNm ref_) of
                                                         { _nmAsp ->
                                                         (case (case lamMpLookupLam _nm _lhsIlamMp of
                                                                    j@(Just arity) | _lhsIisTopApp
                                                                        -> j
                                                                    _   -> Nothing) of
                                                          { _mbLam ->
                                                          (case (maybe (GrVal_Var _nmAsp) (\a -> mkNdPApp _nmAsp a []) _mbLam) of
                                                           { _grVal ->
                                                           (case (maybe hsnUnknown id . grV2HNm $ _grVal) of
                                                            { _lhsOgrAppFun ->
                                                            (case ([]) of
                                                             { _lhsOgrBindL ->
                                                             (case (__tup10) of
                                                              { (_,_lUniq) ->
                                                              (case (mbMkStrict _lhsImodNm _lUniq _lhsIisStrict _grVal) of
                                                               { _grExpr ->
                                                               (case (_grExpr) of
                                                                { _lhsOgrExpr ->
                                                                (case ([]) of
                                                                 { _lhsOgrGlobalL ->
                                                                 (case (_grExpr) of
                                                                  { _lhsOgrLamBody ->
                                                                  (case (_grExpr) of
                                                                   { _lhsOgrLetBody ->
                                                                   (case ([]) of
                                                                    { _grTupFldL ->
                                                                    (case (_grTupFldL) of
                                                                     { _lhsOgrTupFldL ->
                                                                     (case (_grVal) of
                                                                      { _grTupRec ->
                                                                      (case (_grTupRec) of
                                                                       { _lhsOgrTupRec ->
                                                                       (case (_grVal) of
                                                                        { _lhsOgrVal ->
                                                                        (case (Nothing) of
                                                                         { _lhsOmbFFIApp ->
                                                                         (case (Nothing) of
                                                                          { _lhsOmbLam ->
                                                                          (case (Just _nm) of
                                                                           { _mbVar ->
                                                                           (case (_mbVar) of
                                                                            { _lhsOmbVar ->
                                                                            ( _lhsOappFunKind,_lhsOfvS,_lhsOgrAppArgL,_lhsOgrAppFun,_lhsOgrBindL,_lhsOgrExpr,_lhsOgrGlobalL,_lhsOgrLamBody,_lhsOgrLetBody,_lhsOgrTupFldL,_lhsOgrTupRec,_lhsOgrVal,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                        in  sem_CExpr_Var_2)) of
                                 { ( sem_CExpr_2) ->
                                 ( _lhsOgUniq,_lhsOwhatBelow,sem_CExpr_2) }) }) }) }) }) }))
              in  sem_CExpr_Var_1)) of
       { ( sem_CExpr_1) ->
       ( _lhsOgathLamMp,_lhsOgrLamArgL,sem_CExpr_1) }) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attribute:
         fvS                  : FvS
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
      alternative Debug:
         child info           : {String}
      alternative Ty:
         child ty             : {Ty}
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
type T_CExprAnn  = UID ->
                   ( UID,T_CExprAnn_1 )
type T_CExprAnn_1  = DataGam ->
                     LamMp ->
                     Int ->
                     HsName ->
                     EHCOpts ->
                     ( FvS)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CExprAnn_Coe_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Coe_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                ( _lhsOfvS) }))
                  in  sem_CExprAnn_Coe_1)) of
           { ( sem_CExprAnn_1) ->
           ( _lhsOgUniq,sem_CExprAnn_1) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CExprAnn_Debug_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Debug_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                ( _lhsOfvS) }))
                  in  sem_CExprAnn_Debug_1)) of
           { ( sem_CExprAnn_1) ->
           ( _lhsOgUniq,sem_CExprAnn_1) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CExprAnn_Ty_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Ty_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                ( _lhsOfvS) }))
                  in  sem_CExprAnn_Ty_1)) of
           { ( sem_CExprAnn_1) ->
           ( _lhsOgUniq,sem_CExprAnn_1) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         isApply0             : Bool
   alternatives:
      alternative Apply0:
      alternative Function0:
      alternative Function1:
      alternative Plain:
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
type T_CMetaBind  = DataGam ->
                    LamMp ->
                    Int ->
                    HsName ->
                    EHCOpts ->
                    ( FvS,Bool)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (True) of
           { _lhsOisApply0 ->
           ( _lhsOfvS,_lhsOisApply0) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (False) of
           { _lhsOisApply0 ->
           ( _lhsOfvS,_lhsOisApply0) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (False) of
           { _lhsOisApply0 ->
           ( _lhsOfvS,_lhsOisApply0) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (False) of
           { _lhsOisApply0 ->
           ( _lhsOfvS,_lhsOisApply0) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grBindAnn            : GrBindAnn
   alternatives:
      alternative Dict:
      alternative DictClass:
         child tracks         : {[Track]}
      alternative DictInstance:
         child tracks         : {[Track]}
      alternative Track:
         child track          : {Track}
      alternative Val:
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
type T_CMetaVal  = DataGam ->
                   LamMp ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   ( FvS,GrBindAnn)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (GrBindAnnNormal) of
           { _lhsOgrBindAnn ->
           ( _lhsOfvS,_lhsOgrBindAnn) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (GrBindAnnClass tracks_) of
           { _lhsOgrBindAnn ->
           ( _lhsOfvS,_lhsOgrBindAnn) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (GrBindAnnInstance tracks_) of
           { _lhsOgrBindAnn ->
           ( _lhsOfvS,_lhsOgrBindAnn) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (GrBindAnnNormal) of
           { _lhsOgrBindAnn ->
           ( _lhsOfvS,_lhsOgrBindAnn) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (GrBindAnnNormal) of
           { _lhsOgrBindAnn ->
           ( _lhsOfvS,_lhsOgrBindAnn) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grBindAnn            : GrBindAnn
         isApply0             : Bool
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = DataGam ->
                 LamMp ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 ( FvS,GrBindAnn,Bool)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIdataGam
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _x2Oopts ->
          (case (_lhsImodNm) of
           { _x2OmodNm ->
           (case (_lhsIlev) of
            { _x2Olev ->
            (case (_lhsIlamMp) of
             { _x2OlamMp ->
             (case (_lhsIdataGam) of
              { _x2OdataGam ->
              (case (x2_ _x2OdataGam _x2OlamMp _x2Olev _x2OmodNm _x2Oopts ) of
               { ( _x2IfvS,_x2IgrBindAnn) ->
                   (case (_lhsIopts) of
                    { _x1Oopts ->
                    (case (_lhsImodNm) of
                     { _x1OmodNm ->
                     (case (_lhsIlev) of
                      { _x1Olev ->
                      (case (_lhsIlamMp) of
                       { _x1OlamMp ->
                       (case (_lhsIdataGam) of
                        { _x1OdataGam ->
                        (case (x1_ _x1OdataGam _x1OlamMp _x1Olev _x1OmodNm _x1Oopts ) of
                         { ( _x1IfvS,_x1IisApply0) ->
                             (case (_x1IfvS `Set.union` _x2IfvS) of
                              { _lhsOfvS ->
                              (case (_x2IgrBindAnn) of
                               { _lhsOgrBindAnn ->
                               (case (_x1IisApply0) of
                                { _lhsOisApply0 ->
                                ( _lhsOfvS,_lhsOgrBindAnn,_lhsOisApply0) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathLamMp            : LamMp
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         fvS                  : FvS
         grMod                : GrModule
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 1:
            local whatAbove   : {WhatExpr}
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ( LamMp,T_CModule_1 )
type T_CModule_1  = DataGam ->
                    UID ->
                    LamMp ->
                    Int ->
                    EHCOpts ->
                    ( FvS,UID,GrModule)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (case (expr_ ) of
     { ( _exprIgathLamMp,_exprIgrLamArgL,expr_1) ->
         (case (_exprIgathLamMp) of
          { _lhsOgathLamMp ->
          (case ((let sem_CModule_Mod_1 :: T_CModule_1 
                      sem_CModule_Mod_1  =
                          (\ _lhsIdataGam
                             _lhsIgUniq
                             _lhsIlamMp
                             _lhsIlev
                             _lhsIopts ->
                               (case (_lhsIgUniq) of
                                { _exprOgUniq ->
                                (case (expr_1 _exprOgUniq ) of
                                 { ( _exprIgUniq,_exprIwhatBelow,expr_2) ->
                                     (case (ExprIsOther) of
                                      { _whatAbove ->
                                      (case (_whatAbove) of
                                       { _exprOwhatAbove ->
                                       (case (_lhsIopts) of
                                        { _exprOopts ->
                                        (case (_lhsIlev) of
                                         { _exprOlev ->
                                         (case (_lhsIlamMp) of
                                          { _exprOlamMp ->
                                          (case (_lhsIdataGam) of
                                           { _exprOdataGam ->
                                           (case (EvalCtx_Eval) of
                                            { _exprOevalCtx ->
                                            (case (False) of
                                             { _exprOisLamBody ->
                                             (case (True) of
                                              { _exprOisStrict ->
                                              (case (True) of
                                               { _exprOisTopTup ->
                                               (case (True) of
                                                { _exprOisTopApp ->
                                                (case (True) of
                                                 { _exprOdoBox ->
                                                 (case (moduleNm_) of
                                                  { _exprOmodNm ->
                                                  (case (expr_2 _exprOdataGam _exprOdoBox _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmodNm _exprOopts _exprOwhatAbove ) of
                                                   { ( _exprIappFunKind,_exprIfvS,_exprIgrAppArgL,_exprIgrAppFun,_exprIgrBindL,_exprIgrExpr,_exprIgrGlobalL,_exprIgrLamBody,_exprIgrLetBody,_exprIgrTupFldL,_exprIgrTupRec,_exprIgrVal,_exprImbFFIApp,_exprImbLam,_exprImbVar) ->
                                                       (case (_exprIfvS) of
                                                        { _lhsOfvS ->
                                                        (case (_exprIgUniq) of
                                                         { _lhsOgUniq ->
                                                         (case (GrModule_Mod moduleNm_
                                                                             _exprIgrGlobalL
                                                                             _exprIgrBindL
                                                                             (            primNmList2GrTagMap (grBuiltinTyNmL _lhsIopts)
                                                                             `Map.union`  cTagsMp2GrTagMap ctagsMp_
                                                                             )) of
                                                          { _lhsOgrMod ->
                                                          ( _lhsOfvS,_lhsOgUniq,_lhsOgrMod) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_CModule_Mod_1)) of
           { ( sem_CModule_1) ->
           ( _lhsOgathLamMp,sem_CModule_1) }) }) })
-- CPat --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fldNmL               : [HsName]
         fvS                  : FvS
         grPat                : GrPatAlt
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
      alternative Char:
         child char           : {Char}
         visit 0:
            local _tup11      : {(UID,UID)}
         visit 1:
            local lUniq       : {UID}
            intra _tup11      : {(UID,UID)}
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local _tup13      : {(UID,UID)}
         visit 1:
            local lUniq       : {UID}
            local _tup12      : {(GrPatAlt,GrExpr -> GrExpr)}
            intra _tup13      : {(UID,UID)}
      alternative Int:
         child int            : {Int}
         visit 0:
            local _tup14      : {(UID,UID)}
         visit 1:
            local lUniq       : {UID}
            intra _tup14      : {(UID,UID)}
      alternative Var:
         child pnm            : {HsName}
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
type T_CPat  = UID ->
               ( UID,T_CPat_1 )
type T_CPat_1  = DataGam ->
                 LamMp ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 ( ([HsName]),FvS,GrPatAlt,(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CPat_BoolExpr_1 :: T_CPat_1 
                      sem_CPat_BoolExpr_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case ([]) of
                                { _lhsOfldNmL ->
                                (case (Set.empty) of
                                 { _lhsOfvS ->
                                 (case (GrPatAlt_Otherwise) of
                                  { _lhsOgrPat ->
                                  (case (id) of
                                   { _lhsOgrWrapCase ->
                                   (case (idGrWrapCaseSel) of
                                    { _lhsOgrWrapCaseSel ->
                                    (case ([]) of
                                     { _lhsOnmL ->
                                     ( _lhsOfldNmL,_lhsOfvS,_lhsOgrPat,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }))
                  in  sem_CPat_BoolExpr_1)) of
           { ( sem_CPat_1) ->
           ( _lhsOgUniq,sem_CPat_1) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIgUniq ->
         (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
          { __tup11 ->
          (case (__tup11) of
           { (_lhsOgUniq,_) ->
           (case ((let sem_CPat_Char_1 :: T_CPat_1 
                       sem_CPat_Char_1  =
                           (\ _lhsIdataGam
                              _lhsIlamMp
                              _lhsIlev
                              _lhsImodNm
                              _lhsIopts ->
                                (case ([]) of
                                 { _lhsOfldNmL ->
                                 (case (Set.empty) of
                                  { _lhsOfvS ->
                                  (case (GrPatAlt_LitInt (ord char_)) of
                                   { _lhsOgrPat ->
                                   (case (id) of
                                    { _lhsOgrWrapCase ->
                                    (case (__tup11) of
                                     { (_,_lUniq) ->
                                     (case (let n = uidQualHNm _lhsImodNm _lUniq
                                            in  \v -> (GrVal_Var n,GrExpr_Seq (GrExpr_Unit v GrType_None) (mkGrUnbox hsnInt n))) of
                                      { _lhsOgrWrapCaseSel ->
                                      (case ([]) of
                                       { _lhsOnmL ->
                                       ( _lhsOfldNmL,_lhsOfvS,_lhsOgrPat,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }))
                   in  sem_CPat_Char_1)) of
            { ( sem_CPat_1) ->
            ( _lhsOgUniq,sem_CPat_1) }) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIgUniq ->
         (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
          { __tup13 ->
          (case (__tup13) of
           { (_restOgUniq,_) ->
           (case (rest_ _restOgUniq ) of
            { ( _restIgUniq,rest_1) ->
                (case (_restIgUniq) of
                 { _bindsOgUniq ->
                 (case (binds_ _bindsOgUniq ) of
                  { ( _bindsIgUniq,binds_1) ->
                      (case (_bindsIgUniq) of
                       { _lhsOgUniq ->
                       (case ((let sem_CPat_Con_1 :: T_CPat_1 
                                   sem_CPat_Con_1  =
                                       (\ _lhsIdataGam
                                          _lhsIlamMp
                                          _lhsIlev
                                          _lhsImodNm
                                          _lhsIopts ->
                                            (case (_lhsIopts) of
                                             { _bindsOopts ->
                                             (case (_lhsImodNm) of
                                              { _bindsOmodNm ->
                                              (case (_lhsIlev) of
                                               { _bindsOlev ->
                                               (case (_lhsIlamMp) of
                                                { _bindsOlamMp ->
                                                (case (_lhsIdataGam) of
                                                 { _bindsOdataGam ->
                                                 (case (binds_1 _bindsOdataGam _bindsOlamMp _bindsOlev _bindsOmodNm _bindsOopts ) of
                                                  { ( _bindsIfldNmL,_bindsIfvS,_bindsIgrTupFldL,_bindsIgrWrapCase,_bindsIgrWrapCaseSel,_bindsInmL) ->
                                                      (case (_bindsIfldNmL) of
                                                       { _lhsOfldNmL ->
                                                       (case (_lhsIopts) of
                                                        { _restOopts ->
                                                        (case (_lhsImodNm) of
                                                         { _restOmodNm ->
                                                         (case (_lhsIlev) of
                                                          { _restOlev ->
                                                          (case (_lhsIlamMp) of
                                                           { _restOlamMp ->
                                                           (case (_lhsIdataGam) of
                                                            { _restOdataGam ->
                                                            (case (rest_1 _restOdataGam _restOlamMp _restOlev _restOmodNm _restOopts ) of
                                                             { ( _restIfvS,_restIgrWrapCase,_restIgrWrapCaseSel,_restInmL,_restIself) ->
                                                                 (case (_restIfvS `Set.union` _bindsIfvS) of
                                                                  { _lhsOfvS ->
                                                                  (case (__tup13) of
                                                                   { (_,_lUniq) ->
                                                                   (case (case _restIself of
                                                                             CPatRest_Empty
                                                                               ->  (ctag mkGrPatRecNode (\_ l t a ma -> mkGrPatConNode (mkGrTagAnn a ma) t l) tag_ _bindsInmL,id)
                                                                             CPatRest_Var r
                                                                               ->  let  (oL,mkSL) = unzip _bindsIgrTupFldL
                                                                                        (oL',wrO) = unboxArgL _lhsImodNm _lUniq hsnInt oL
                                                                                   in   (ctag mkGrPatRecSplit (\_ l t a ma -> mkGrPatConSplit (mkGrTagAnn a ma) t l) tag_ r . zipWith ($) mkSL $ oL',wrO)) of
                                                                    { __tup12 ->
                                                                    (case (__tup12) of
                                                                     { (_lhsOgrPat,_) ->
                                                                     (case (__tup12) of
                                                                      { (_,_lhsOgrWrapCase) ->
                                                                      (case (_restIgrWrapCaseSel `const` _bindsIgrWrapCaseSel) of
                                                                       { _lhsOgrWrapCaseSel ->
                                                                       (case (_restInmL ++ _bindsInmL) of
                                                                        { _lhsOnmL ->
                                                                        ( _lhsOfldNmL,_lhsOfvS,_lhsOgrPat,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                               in  sem_CPat_Con_1)) of
                        { ( sem_CPat_1) ->
                        ( _lhsOgUniq,sem_CPat_1) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIgUniq ->
         (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
          { __tup14 ->
          (case (__tup14) of
           { (_lhsOgUniq,_) ->
           (case ((let sem_CPat_Int_1 :: T_CPat_1 
                       sem_CPat_Int_1  =
                           (\ _lhsIdataGam
                              _lhsIlamMp
                              _lhsIlev
                              _lhsImodNm
                              _lhsIopts ->
                                (case ([]) of
                                 { _lhsOfldNmL ->
                                 (case (Set.empty) of
                                  { _lhsOfvS ->
                                  (case (GrPatAlt_LitInt int_) of
                                   { _lhsOgrPat ->
                                   (case (id) of
                                    { _lhsOgrWrapCase ->
                                    (case (__tup14) of
                                     { (_,_lUniq) ->
                                     (case (let n = uidQualHNm _lhsImodNm _lUniq
                                            in  \v -> (GrVal_Var n,GrExpr_Seq (GrExpr_Unit v GrType_None) (mkGrUnbox hsnInt n))) of
                                      { _lhsOgrWrapCaseSel ->
                                      (case ([]) of
                                       { _lhsOnmL ->
                                       ( _lhsOfldNmL,_lhsOfvS,_lhsOgrPat,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }))
                   in  sem_CPat_Int_1)) of
            { ( sem_CPat_1) ->
            ( _lhsOgUniq,sem_CPat_1) }) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CPat_Var_1 :: T_CPat_1 
                      sem_CPat_Var_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case ([]) of
                                { _lhsOfldNmL ->
                                (case (Set.empty) of
                                 { _lhsOfvS ->
                                 (case (GrPatAlt_Otherwise) of
                                  { _lhsOgrPat ->
                                  (case (id) of
                                   { _lhsOgrWrapCase ->
                                   (case (idGrWrapCaseSel) of
                                    { _lhsOgrWrapCaseSel ->
                                    (case ([pnm_]) of
                                     { _lhsOnmL ->
                                     ( _lhsOfldNmL,_lhsOfvS,_lhsOgrPat,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }))
                  in  sem_CPat_Var_1)) of
           { ( sem_CPat_1) ->
           ( _lhsOgUniq,sem_CPat_1) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fldNmL               : [HsName]
         fvS                  : FvS
         grTupFldL            : [(GrVal,GrVal->GrSplit)]
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 1:
            local fldNm       : _
            local whatAbove   : {WhatExpr}
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = UID ->
                  ( UID,T_CPatFld_1 )
type T_CPatFld_1  = DataGam ->
                    LamMp ->
                    Int ->
                    HsName ->
                    EHCOpts ->
                    ( ([HsName]),FvS,([(GrVal,GrVal->GrSplit)]),(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _offsetOgUniq ->
          (case (offset_ ) of
           { ( _offsetIgathLamMp,_offsetIgrLamArgL,offset_1) ->
               (case (offset_1 _offsetOgUniq ) of
                { ( _offsetIgUniq,_offsetIwhatBelow,offset_2) ->
                    (case (_offsetIgUniq) of
                     { _bindOgUniq ->
                     (case (bind_ ) of
                      { ( _bindIbindLamMp,_bindInm,bind_1) ->
                          (case (bind_1 _bindOgUniq ) of
                           { ( _bindIgUniq,bind_2) ->
                               (case (_bindIgUniq) of
                                { _fldAnnsOgUniq ->
                                (case (fldAnns_ _fldAnnsOgUniq ) of
                                 { ( _fldAnnsIgUniq,fldAnns_1) ->
                                     (case (_fldAnnsIgUniq) of
                                      { _lhsOgUniq ->
                                      (case ((let sem_CPatFld_Fld_1 :: T_CPatFld_1 
                                                  sem_CPatFld_Fld_1  =
                                                      (\ _lhsIdataGam
                                                         _lhsIlamMp
                                                         _lhsIlev
                                                         _lhsImodNm
                                                         _lhsIopts ->
                                                           (case (_bindInm) of
                                                            { _fldNm ->
                                                            (case ([_fldNm]) of
                                                             { _lhsOfldNmL ->
                                                             (case (_lhsIopts) of
                                                              { _fldAnnsOopts ->
                                                              (case (_lhsImodNm) of
                                                               { _fldAnnsOmodNm ->
                                                               (case (_lhsIlev) of
                                                                { _fldAnnsOlev ->
                                                                (case (_lhsIlamMp) of
                                                                 { _fldAnnsOlamMp ->
                                                                 (case (_lhsIdataGam) of
                                                                  { _fldAnnsOdataGam ->
                                                                  (case (fldAnns_1 _fldAnnsOdataGam _fldAnnsOlamMp _fldAnnsOlev _fldAnnsOmodNm _fldAnnsOopts ) of
                                                                   { ( _fldAnnsIfvS,_fldAnnsIgrTupFldL,_fldAnnsIgrWrapCase,_fldAnnsIgrWrapCaseSel,_fldAnnsInmL) ->
                                                                       (case (_lhsIopts) of
                                                                        { _bindOopts ->
                                                                        (case (_lhsImodNm) of
                                                                         { _bindOmodNm ->
                                                                         (case (_lhsIlev) of
                                                                          { _bindOlev ->
                                                                          (case (_lhsIlamMp) of
                                                                           { _bindOlamMp ->
                                                                           (case (_lhsIdataGam) of
                                                                            { _bindOdataGam ->
                                                                            (case (EvalCtx_None) of
                                                                             { _bindOevalCtx ->
                                                                             (case (False) of
                                                                              { _bindOisLamBody ->
                                                                              (case (False) of
                                                                               { _bindOisStrict ->
                                                                               (case (acoreBindcategPlain) of
                                                                                { _bindOletBindingsCateg ->
                                                                                (case (False) of
                                                                                 { _bindOisGlobal ->
                                                                                 (case (bind_2 _bindOdataGam _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOmodNm _bindOopts ) of
                                                                                  { ( _bindIfvS,_bindIfvSMp,_bindIgrBindL,_bindIgrGlobalL,_bindInmL) ->
                                                                                      (case (ExprIsOther) of
                                                                                       { _whatAbove ->
                                                                                       (case (_whatAbove) of
                                                                                        { _offsetOwhatAbove ->
                                                                                        (case (_lhsIopts) of
                                                                                         { _offsetOopts ->
                                                                                         (case (_lhsImodNm) of
                                                                                          { _offsetOmodNm ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _offsetOlev ->
                                                                                           (case (_lhsIlamMp) of
                                                                                            { _offsetOlamMp ->
                                                                                            (case (_lhsIdataGam) of
                                                                                             { _offsetOdataGam ->
                                                                                             (case (EvalCtx_Eval) of
                                                                                              { _offsetOevalCtx ->
                                                                                              (case (False) of
                                                                                               { _offsetOisLamBody ->
                                                                                               (case (True) of
                                                                                                { _offsetOisStrict ->
                                                                                                (case (True) of
                                                                                                 { _offsetOisTopTup ->
                                                                                                 (case (True) of
                                                                                                  { _offsetOisTopApp ->
                                                                                                  (case (True) of
                                                                                                   { _offsetOdoBox ->
                                                                                                   (case (offset_2 _offsetOdataGam _offsetOdoBox _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmodNm _offsetOopts _offsetOwhatAbove ) of
                                                                                                    { ( _offsetIappFunKind,_offsetIfvS,_offsetIgrAppArgL,_offsetIgrAppFun,_offsetIgrBindL,_offsetIgrExpr,_offsetIgrGlobalL,_offsetIgrLamBody,_offsetIgrLetBody,_offsetIgrTupFldL,_offsetIgrTupRec,_offsetIgrVal,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar) ->
                                                                                                        (case (_offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS) of
                                                                                                         { _lhsOfvS ->
                                                                                                         (case ([(_offsetIgrVal,\o -> GrSplit_Sel _fldNm o)]) of
                                                                                                          { _lhsOgrTupFldL ->
                                                                                                          (case (_fldAnnsIgrWrapCase) of
                                                                                                           { _lhsOgrWrapCase ->
                                                                                                           (case (_fldAnnsIgrWrapCaseSel) of
                                                                                                            { _lhsOgrWrapCaseSel ->
                                                                                                            (case ([_fldNm]) of
                                                                                                             { _lhsOnmL ->
                                                                                                             ( _lhsOfldNmL,_lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                              in  sem_CPatFld_Fld_1)) of
                                       { ( sem_CPatFld_1) ->
                                       ( _lhsOgUniq,sem_CPatFld_1) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fldNmL               : [HsName]
         fvS                  : FvS
         grTupFldL            : [(GrVal,GrVal->GrSplit)]
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
      alternative Nil:
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = UID ->
                   ( UID,T_CPatFldL_1 )
type T_CPatFldL_1  = DataGam ->
                     LamMp ->
                     Int ->
                     HsName ->
                     EHCOpts ->
                     ( ([HsName]),FvS,([(GrVal,GrVal->GrSplit)]),(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (hd_ _hdOgUniq ) of
           { ( _hdIgUniq,hd_1) ->
               (case (_hdIgUniq) of
                { _tlOgUniq ->
                (case (tl_ _tlOgUniq ) of
                 { ( _tlIgUniq,tl_1) ->
                     (case (_tlIgUniq) of
                      { _lhsOgUniq ->
                      (case ((let sem_CPatFldL_Cons_1 :: T_CPatFldL_1 
                                  sem_CPatFldL_Cons_1  =
                                      (\ _lhsIdataGam
                                         _lhsIlamMp
                                         _lhsIlev
                                         _lhsImodNm
                                         _lhsIopts ->
                                           (case (_lhsIopts) of
                                            { _tlOopts ->
                                            (case (_lhsImodNm) of
                                             { _tlOmodNm ->
                                             (case (_lhsIlev) of
                                              { _tlOlev ->
                                              (case (_lhsIlamMp) of
                                               { _tlOlamMp ->
                                               (case (_lhsIdataGam) of
                                                { _tlOdataGam ->
                                                (case (tl_1 _tlOdataGam _tlOlamMp _tlOlev _tlOmodNm _tlOopts ) of
                                                 { ( _tlIfldNmL,_tlIfvS,_tlIgrTupFldL,_tlIgrWrapCase,_tlIgrWrapCaseSel,_tlInmL) ->
                                                     (case (_lhsIopts) of
                                                      { _hdOopts ->
                                                      (case (_lhsImodNm) of
                                                       { _hdOmodNm ->
                                                       (case (_lhsIlev) of
                                                        { _hdOlev ->
                                                        (case (_lhsIlamMp) of
                                                         { _hdOlamMp ->
                                                         (case (_lhsIdataGam) of
                                                          { _hdOdataGam ->
                                                          (case (hd_1 _hdOdataGam _hdOlamMp _hdOlev _hdOmodNm _hdOopts ) of
                                                           { ( _hdIfldNmL,_hdIfvS,_hdIgrTupFldL,_hdIgrWrapCase,_hdIgrWrapCaseSel,_hdInmL) ->
                                                               (case (_hdIfldNmL ++ _tlIfldNmL) of
                                                                { _lhsOfldNmL ->
                                                                (case (_hdIfvS `Set.union` _tlIfvS) of
                                                                 { _lhsOfvS ->
                                                                 (case (_hdIgrTupFldL ++ _tlIgrTupFldL) of
                                                                  { _lhsOgrTupFldL ->
                                                                  (case (_hdIgrWrapCase `const` _tlIgrWrapCase) of
                                                                   { _lhsOgrWrapCase ->
                                                                   (case (_hdIgrWrapCaseSel `const` _tlIgrWrapCaseSel) of
                                                                    { _lhsOgrWrapCaseSel ->
                                                                    (case (_hdInmL ++ _tlInmL) of
                                                                     { _lhsOnmL ->
                                                                     ( _lhsOfldNmL,_lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                              in  sem_CPatFldL_Cons_1)) of
                       { ( sem_CPatFldL_1) ->
                       ( _lhsOgUniq,sem_CPatFldL_1) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CPatFldL_Nil_1 :: T_CPatFldL_1 
                      sem_CPatFldL_Nil_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case ([]) of
                                { _lhsOfldNmL ->
                                (case (Set.empty) of
                                 { _lhsOfvS ->
                                 (case ([]) of
                                  { _lhsOgrTupFldL ->
                                  (case (id) of
                                   { _lhsOgrWrapCase ->
                                   (case (idGrWrapCaseSel) of
                                    { _lhsOgrWrapCaseSel ->
                                    (case ([]) of
                                     { _lhsOnmL ->
                                     ( _lhsOfldNmL,_lhsOfvS,_lhsOgrTupFldL,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL) }) }) }) }) }) }))
                  in  sem_CPatFldL_Nil_1)) of
           { ( sem_CPatFldL_1) ->
           ( _lhsOgUniq,sem_CPatFldL_1) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      inherited attributes:
         dataGam              : DataGam
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fvS                  : FvS
         grWrapCase           : GrExpr -> GrExpr
         grWrapCaseSel        : GrVal -> (GrVal,GrExpr->GrExpr)
         nmL                  : [HsName]
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 1:
            local self        : _
      alternative Var:
         child nm             : {HsName}
         visit 1:
            local self        : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = UID ->
                   ( UID,T_CPatRest_1 )
type T_CPatRest_1  = DataGam ->
                     LamMp ->
                     Int ->
                     HsName ->
                     EHCOpts ->
                     ( FvS,(GrExpr -> GrExpr),(GrVal -> (GrVal,GrExpr->GrExpr)),([HsName]),CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CPatRest_Empty_1 :: T_CPatRest_1 
                      sem_CPatRest_Empty_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                (case (id) of
                                 { _lhsOgrWrapCase ->
                                 (case (idGrWrapCaseSel) of
                                  { _lhsOgrWrapCaseSel ->
                                  (case ([]) of
                                   { _lhsOnmL ->
                                   (case (CPatRest_Empty) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsOfvS,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL,_lhsOself) }) }) }) }) }) }))
                  in  sem_CPatRest_Empty_1)) of
           { ( sem_CPatRest_1) ->
           ( _lhsOgUniq,sem_CPatRest_1) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIgUniq ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ((let sem_CPatRest_Var_1 :: T_CPatRest_1 
                      sem_CPatRest_Var_1  =
                          (\ _lhsIdataGam
                             _lhsIlamMp
                             _lhsIlev
                             _lhsImodNm
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _lhsOfvS ->
                                (case (id) of
                                 { _lhsOgrWrapCase ->
                                 (case (idGrWrapCaseSel) of
                                  { _lhsOgrWrapCaseSel ->
                                  (case ([nm_]) of
                                   { _lhsOnmL ->
                                   (case (CPatRest_Var nm_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsOfvS,_lhsOgrWrapCase,_lhsOgrWrapCaseSel,_lhsOnmL,_lhsOself) }) }) }) }) }) }))
                  in  sem_CPatRest_Var_1)) of
           { ( sem_CPatRest_1) ->
           ( _lhsOgUniq,sem_CPatRest_1) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         gUniq                : UID
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         gathLamMp            : LamMp
         grMod                : GrModule
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local howMergeLamInfo : _
            local gathLamMp   : _
            local howUnionGathLamInfo : _
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = DataGam ->
                    UID ->
                    LamMp ->
                    EHCOpts ->
                    ( LamMp,GrModule)
data Inh_CodeAGItf  = Inh_CodeAGItf {dataGam_Inh_CodeAGItf :: !(DataGam),gUniq_Inh_CodeAGItf :: !(UID),lamMp_Inh_CodeAGItf :: !(LamMp),opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {gathLamMp_Syn_CodeAGItf :: !(LamMp),grMod_Syn_CodeAGItf :: !(GrModule)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIdataGam _lhsIgUniq _lhsIlamMp _lhsIopts )  =
    (let ( _lhsOgathLamMp,_lhsOgrMod) = sem _lhsIdataGam _lhsIgUniq _lhsIlamMp _lhsIopts 
     in  (Syn_CodeAGItf _lhsOgathLamMp _lhsOgrMod ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIdataGam
       _lhsIgUniq
       _lhsIlamMp
       _lhsIopts ->
         (case ((\(LamInfo {laminfoArity=a}) i -> i {laminfoArity=a})) of
          { _howMergeLamInfo ->
          (case (module_ ) of
           { ( _moduleIgathLamMp,module_1) ->
               (case (lamMpMergeInto _howMergeLamInfo const _moduleIgathLamMp _lhsIlamMp) of
                { _gathLamMp ->
                (case (_gathLamMp) of
                 { _lhsOgathLamMp ->
                 (case (_lhsIopts) of
                  { _moduleOopts ->
                  (case (_lhsIgUniq) of
                   { _moduleOgUniq ->
                   (case (_lhsIdataGam) of
                    { _moduleOdataGam ->
                    (case (Map.union _gathLamMp) of
                     { _howUnionGathLamInfo ->
                     (case (_howUnionGathLamInfo _lhsIlamMp) of
                      { _moduleOlamMp ->
                      (case (cLevModule) of
                       { _moduleOlev ->
                       (case (module_1 _moduleOdataGam _moduleOgUniq _moduleOlamMp _moduleOlev _moduleOopts ) of
                        { ( _moduleIfvS,_moduleIgUniq,_moduleIgrMod) ->
                            (case (_moduleIgrMod) of
                             { _lhsOgrMod ->
                             ( _lhsOgathLamMp,_lhsOgrMod) }) }) }) }) }) }) }) }) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         doBox                : Bool
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         modNm                : HsName
         opts                 : EHCOpts
      chained attribute:
         gUniq                : UID
      synthesized attribute:
         fvS                  : FvS
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
      alternative Nothing:
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = DataGam ->
                  Bool ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  LamMp ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  ( FvS,UID)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIdataGam
       _lhsIdoBox
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (just_ ) of
          { ( _justIgathLamMp,_justIgrLamArgL,just_1) ->
              (case (_lhsIgUniq) of
               { _justOgUniq ->
               (case (just_1 _justOgUniq ) of
                { ( _justIgUniq,_justIwhatBelow,just_2) ->
                    (case (ExprIsOther) of
                     { _whatAbove ->
                     (case (_whatAbove) of
                      { _justOwhatAbove ->
                      (case (_lhsIopts) of
                       { _justOopts ->
                       (case (_lhsImodNm) of
                        { _justOmodNm ->
                        (case (_lhsIlev) of
                         { _justOlev ->
                         (case (_lhsIlamMp) of
                          { _justOlamMp ->
                          (case (_lhsIisStrict) of
                           { _justOisStrict ->
                           (case (_lhsIisLamBody) of
                            { _justOisLamBody ->
                            (case (_lhsIevalCtx) of
                             { _justOevalCtx ->
                             (case (_lhsIdoBox) of
                              { _justOdoBox ->
                              (case (_lhsIdataGam) of
                               { _justOdataGam ->
                               (case (True) of
                                { _justOisTopTup ->
                                (case (True) of
                                 { _justOisTopApp ->
                                 (case (just_2 _justOdataGam _justOdoBox _justOevalCtx _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlamMp _justOlev _justOmodNm _justOopts _justOwhatAbove ) of
                                  { ( _justIappFunKind,_justIfvS,_justIgrAppArgL,_justIgrAppFun,_justIgrBindL,_justIgrExpr,_justIgrGlobalL,_justIgrLamBody,_justIgrLetBody,_justIgrTupFldL,_justIgrTupRec,_justIgrVal,_justImbFFIApp,_justImbLam,_justImbVar) ->
                                      (case (_justIfvS) of
                                       { _lhsOfvS ->
                                       (case (_justIgUniq) of
                                        { _lhsOgUniq ->
                                        ( _lhsOfvS,_lhsOgUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIdataGam
       _lhsIdoBox
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImodNm
       _lhsIopts ->
         (case (Set.empty) of
          { _lhsOfvS ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           ( _lhsOfvS,_lhsOgUniq) }) }))