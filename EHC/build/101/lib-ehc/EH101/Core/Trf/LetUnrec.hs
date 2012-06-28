

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/LetUnrec.ag)
module EH101.Core.Trf.LetUnrec(cmodTrfLetUnrec) where

import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import EH101.Base.Debug
import EH.Util.Utils (scc)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set











cmodTrfLetUnrec :: CModule -> CModule
cmodTrfLetUnrec cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod)) Inh_CodeAGItf
     in   cTrf_Syn_CodeAGItf t



type UseMp = Map.Map HsName [HsName]

useMpComb :: UseMp -> UseMp -> UseMp
useMpComb = Map.unionWith (++)



type BindMp = Map.Map HsName (CExpr,CMetas)

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local lev         : _
            local cTrf        : _
            local fvS         : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = ([HsName]) ->
               Int ->
               ( CAlt ,FvS,UseMp)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _exprOinNmL | _exprOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _patOinNmL | _patOinNmL `seq` (True) ->
           (case (_lhsIlev + 1) of
            { _lev | _lev `seq` (True) ->
            (case (_lev) of
             { _exprOlev | _exprOlev `seq` (True) ->
             (case (expr_ _exprOinNmL _exprOlev ) of
              { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                  (case (_lev) of
                   { _patOlev | _patOlev `seq` (True) ->
                   (case (pat_ _patOinNmL _patOlev ) of
                    { ( _patIcTrf,_patIfldNmL,_patIfvS,_patInmL,_patIuseMp) | True ->
                        (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (_exprIfvS `Set.difference` Set.fromList _patInmL) of
                           { _fvS | _fvS `seq` (True) ->
                           (case (_fvS) of
                            { _lhsOfvS | _lhsOfvS `seq` (True) ->
                            (case (_patIuseMp `useMpComb` _exprIuseMp) of
                             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = ([HsName]) ->
                Int ->
                ( CAltL ,FvS,UseMp)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _tlOinNmL | _tlOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _hdOinNmL | _hdOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (tl_ _tlOinNmL _tlOlev ) of
             { ( _tlIcTrf,_tlIfvS,_tlIuseMp) | True ->
                 (case (_lhsIlev) of
                  { _hdOlev | _hdOlev `seq` (True) ->
                  (case (hd_ _hdOinNmL _hdOlev ) of
                   { ( _hdIcTrf,_hdIfvS,_hdIuseMp) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_hdIfvS `Set.union` _tlIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_hdIuseMp `useMpComb` _tlIuseMp) of
                           { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         isGlobal             : Bool
         lev                  : Int
      synthesized attributes:
         bindMp               : BindMp
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nm                   : HsName
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = ([HsName]) ->
                Bool ->
                Int ->
                ( BindMp,CBind ,FvS,FvSMp,HsName,([HsName]),UseMp)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bindAspectsOinNmL | _bindAspectsOinNmL `seq` (True) ->
          (case (nm_) of
           { _bindAspectsOnm | _bindAspectsOnm `seq` (True) ->
           (case (_lhsIlev) of
            { _bindAspectsOlev | _bindAspectsOlev `seq` (True) ->
            (case (_lhsIisGlobal) of
             { _bindAspectsOisGlobal | _bindAspectsOisGlobal `seq` (True) ->
             (case (bindAspects_ _bindAspectsOinNmL _bindAspectsOisGlobal _bindAspectsOlev _bindAspectsOnm ) of
              { ( _bindAspectsIbindMp,_bindAspectsIcTrf,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsInmL,_bindAspectsIuseMp) | True ->
                  (case (_bindAspectsIbindMp) of
                   { _lhsObindMp | _lhsObindMp `seq` (True) ->
                   (case (CBind_Bind nm_ _bindAspectsIcTrf) of
                    { _cTrf | _cTrf `seq` (True) ->
                    (case (_cTrf) of
                     { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                     (case (_bindAspectsIfvS) of
                      { _lhsOfvS | _lhsOfvS `seq` (True) ->
                      (case (Map.singleton nm_ _bindAspectsIfvS) of
                       { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                       (case (nm_) of
                        { _lhsOnm | _lhsOnm `seq` (True) ->
                        (case ([nm_]) of
                         { _lhsOnmL | _lhsOnmL `seq` (True) ->
                         (case (_bindAspectsIuseMp) of
                          { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                          ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnm,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = ([HsName]) ->
                   Int ->
                   ( CBindAnn ,FvS,([HsName]),UseMp)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CBindAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             (case (Map.empty) of
              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = ([HsName]) ->
                    Int ->
                    ( CBindAnnL ,FvS,([HsName]),UseMp)
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (_lhsIinNmL) of
           { _tlOinNmL | _tlOinNmL `seq` (True) ->
           (case (tl_ _tlOinNmL _tlOlev ) of
            { ( _tlIcTrf,_tlIfvS,_tlInmL,_tlIuseMp) | True ->
                (case (_lhsIlev) of
                 { _hdOlev | _hdOlev `seq` (True) ->
                 (case (_lhsIinNmL) of
                  { _hdOinNmL | _hdOinNmL `seq` (True) ->
                  (case (hd_ _hdOinNmL _hdOlev ) of
                   { ( _hdIcTrf,_hdIfvS,_hdInmL,_hdIuseMp) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_hdIfvS `Set.union` _tlIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_hdInmL ++ _tlInmL) of
                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                           (case (_hdIuseMp `useMpComb` _tlIuseMp) of
                            { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                            ( _lhsOcTrf,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             (case (Map.empty) of
              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         isGlobal             : Bool
         lev                  : Int
      synthesized attributes:
         bindMp               : BindMp
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ([HsName]) ->
                 Bool ->
                 Int ->
                 ( BindMp,CBindL ,FvS,FvSMp,([HsName]),UseMp)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _tlOinNmL | _tlOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _hdOinNmL | _hdOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (_lhsIisGlobal) of
             { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
             (case (tl_ _tlOinNmL _tlOisGlobal _tlOlev ) of
              { ( _tlIbindMp,_tlIcTrf,_tlIfvS,_tlIfvSMp,_tlInmL,_tlIuseMp) | True ->
                  (case (_lhsIlev) of
                   { _hdOlev | _hdOlev `seq` (True) ->
                   (case (_lhsIisGlobal) of
                    { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                    (case (hd_ _hdOinNmL _hdOisGlobal _hdOlev ) of
                     { ( _hdIbindMp,_hdIcTrf,_hdIfvS,_hdIfvSMp,_hdInm,_hdInmL,_hdIuseMp) | True ->
                         (case (_hdIbindMp `Map.union` _tlIbindMp) of
                          { _lhsObindMp | _lhsObindMp `seq` (True) ->
                          (case ((:) _hdIcTrf _tlIcTrf) of
                           { _cTrf | _cTrf `seq` (True) ->
                           (case (_cTrf) of
                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                            (case (_hdIfvS `Set.union` _tlIfvS) of
                             { _lhsOfvS | _lhsOfvS `seq` (True) ->
                             (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                              (case (_hdInmL ++ _tlInmL) of
                               { _lhsOnmL | _lhsOnmL `seq` (True) ->
                               (case (_hdIuseMp `useMpComb` _tlIuseMp) of
                                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case ([]) of
           { _cTrf | _cTrf `seq` (True) ->
           (case (_cTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case (Map.empty) of
                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
      synthesized attributes:
         bindMp               : BindMp
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local cTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
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
type T_CBound  = ([HsName]) ->
                 Bool ->
                 Int ->
                 HsName ->
                 ( BindMp,CBound ,FvS,FvSMp,([HsName]),UseMp)
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsInm : _lhsIinNmL) of
          { _exprOinNmL | _exprOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _exprOlev | _exprOlev `seq` (True) ->
           (case (expr_ _exprOinNmL _exprOlev ) of
            { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                (case (_lhsIlev) of
                 { _bindMetaOlev | _bindMetaOlev `seq` (True) ->
                 (case (_lhsIinNmL) of
                  { _bindMetaOinNmL | _bindMetaOinNmL `seq` (True) ->
                  (case (bindMeta_ _bindMetaOinNmL _bindMetaOlev ) of
                   { ( _bindMetaIcTrf,_bindMetaIfvS,_bindMetaIuseMp) | True ->
                       (case (_lhsInm `Map.singleton` (_exprIcTrf,_bindMetaIcTrf)) of
                        { _lhsObindMp | _lhsObindMp `seq` (True) ->
                        (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (_bindMetaIfvS `Set.union` _exprIfvS) of
                           { _lhsOfvS | _lhsOfvS `seq` (True) ->
                           (case (Map.empty) of
                            { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                            (case ([]) of
                             { _lhsOnmL | _lhsOnmL `seq` (True) ->
                             (case ((_lhsInm `Map.singleton` []) `useMpComb` _exprIuseMp) of
                              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                              ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case (_lhsIinNmL) of
           { _exprOinNmL | _exprOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _exprOlev | _exprOlev `seq` (True) ->
            (case (expr_ _exprOinNmL _exprOlev ) of
             { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                 (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case (_cTrf) of
                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                   (case (_exprIfvS) of
                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                    (case (Map.empty) of
                     { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                     (case ([]) of
                      { _lhsOnmL | _lhsOnmL `seq` (True) ->
                      (case (_exprIuseMp) of
                       { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                       ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case (_lhsIlev) of
           { _cmetasOlev | _cmetasOlev `seq` (True) ->
           (case (_lhsIinNmL) of
            { _cmetasOinNmL | _cmetasOinNmL `seq` (True) ->
            (case (cmetas_ _cmetasOinNmL _cmetasOlev ) of
             { ( _cmetasIcTrf,_cmetasIfvS,_cmetasIuseMp) | True ->
                 (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case (_cTrf) of
                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                   (case (_cmetasIfvS) of
                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                    (case (Map.empty) of
                     { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                     (case ([]) of
                      { _lhsOnmL | _lhsOnmL `seq` (True) ->
                      (case (_cmetasIuseMp) of
                       { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                       ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case (CBound_RelevTy aspectKeyS_ relevTy_) of
           { _cTrf | _cTrf `seq` (True) ->
           (case (_cTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case (Map.empty) of
                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case (CBound_Ty aspectKeyS_ ty_) of
           { _cTrf | _cTrf `seq` (True) ->
           (case (_cTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case (Map.empty) of
                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case (_lhsIinNmL) of
           { _exprOinNmL | _exprOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _exprOlev | _exprOlev `seq` (True) ->
            (case (expr_ _exprOinNmL _exprOlev ) of
             { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                 (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case (_cTrf) of
                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                   (case (_exprIfvS) of
                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                    (case (Map.empty) of
                     { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                     (case ([]) of
                      { _lhsOnmL | _lhsOnmL `seq` (True) ->
                      (case (_exprIuseMp) of
                       { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                       ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
      synthesized attributes:
         bindMp               : BindMp
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = ([HsName]) ->
                  Bool ->
                  Int ->
                  HsName ->
                  ( BindMp,CBoundL ,FvS,FvSMp,([HsName]),UseMp)
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsInm) of
          { _tlOnm | _tlOnm `seq` (True) ->
          (case (_lhsIinNmL) of
           { _tlOinNmL | _tlOinNmL `seq` (True) ->
           (case (_lhsInm) of
            { _hdOnm | _hdOnm `seq` (True) ->
            (case (_lhsIinNmL) of
             { _hdOinNmL | _hdOinNmL `seq` (True) ->
             (case (_lhsIlev) of
              { _tlOlev | _tlOlev `seq` (True) ->
              (case (_lhsIisGlobal) of
               { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
               (case (tl_ _tlOinNmL _tlOisGlobal _tlOlev _tlOnm ) of
                { ( _tlIbindMp,_tlIcTrf,_tlIfvS,_tlIfvSMp,_tlInmL,_tlIuseMp) | True ->
                    (case (_lhsIlev) of
                     { _hdOlev | _hdOlev `seq` (True) ->
                     (case (_lhsIisGlobal) of
                      { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                      (case (hd_ _hdOinNmL _hdOisGlobal _hdOlev _hdOnm ) of
                       { ( _hdIbindMp,_hdIcTrf,_hdIfvS,_hdIfvSMp,_hdInmL,_hdIuseMp) | True ->
                           (case (_hdIbindMp `Map.union` _tlIbindMp) of
                            { _lhsObindMp | _lhsObindMp `seq` (True) ->
                            (case ((:) _hdIcTrf _tlIcTrf) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (_hdIfvS `Set.union` _tlIfvS) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                                { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                                (case (_hdInmL ++ _tlInmL) of
                                 { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                 (case (_hdIuseMp `useMpComb` _tlIuseMp) of
                                  { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                  ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIinNmL
       _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (Map.empty) of
          { _lhsObindMp | _lhsObindMp `seq` (True) ->
          (case ([]) of
           { _cTrf | _cTrf `seq` (True) ->
           (case (_cTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case (Map.empty) of
                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                ( _lhsObindMp,_lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local isGlobal    : _
            local cTrf        : _
            local fvS         : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local lev         : _
            local isGlobal    : _
            local cTrf        : _
            local argNm       : _
            local fvS         : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local isGlobal    : _
            local _tup1       : _
            local cTrf        : _
            local fvS         : _
            local remUseMp    : _
      alternative String:
         child str            : {String}
         visit 0:
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local cTrf        : _
            local nm          : {HsName}
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
type T_CExpr  = ([HsName]) ->
                Int ->
                ( CExpr ,FvS,UseMp)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _exprOinNmL | _exprOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _exprOlev | _exprOlev `seq` (True) ->
           (case (expr_ _exprOinNmL _exprOlev ) of
            { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                (case (_lhsIlev) of
                 { _annOlev | _annOlev `seq` (True) ->
                 (case (_lhsIinNmL) of
                  { _annOinNmL | _annOinNmL `seq` (True) ->
                  (case (ann_ _annOinNmL _annOlev ) of
                   { ( _annIcTrf,_annIfvS,_annIuseMp) | True ->
                       (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_annIfvS `Set.union` _exprIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_annIuseMp `useMpComb` _exprIuseMp) of
                           { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _argOinNmL | _argOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _funcOinNmL | _funcOinNmL `seq` (True) ->
           (case (hsnUnknown) of
            { _argOnm | _argOnm `seq` (True) ->
            (case (_lhsIlev) of
             { _argOlev | _argOlev `seq` (True) ->
             (case (False) of
              { _isGlobal | _isGlobal `seq` (True) ->
              (case (_isGlobal) of
               { _argOisGlobal | _argOisGlobal `seq` (True) ->
               (case (arg_ _argOinNmL _argOisGlobal _argOlev _argOnm ) of
                { ( _argIbindMp,_argIcTrf,_argIfvS,_argIfvSMp,_argInmL,_argIuseMp) | True ->
                    (case (_lhsIlev) of
                     { _funcOlev | _funcOlev `seq` (True) ->
                     (case (func_ _funcOinNmL _funcOlev ) of
                      { ( _funcIcTrf,_funcIfvS,_funcIuseMp) | True ->
                          (case (CExpr_App _funcIcTrf _argIcTrf) of
                           { _cTrf | _cTrf `seq` (True) ->
                           (case (_cTrf) of
                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                            (case (_funcIfvS `Set.union` _argIfvS) of
                             { _fvS | _fvS `seq` (True) ->
                             (case (_fvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              (case (_funcIuseMp `useMpComb` _argIuseMp) of
                               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                               ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _dfltOinNmL | _dfltOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _altsOinNmL | _altsOinNmL `seq` (True) ->
           (case (_lhsIinNmL) of
            { _exprOinNmL | _exprOinNmL `seq` (True) ->
            (case (_lhsIlev) of
             { _dfltOlev | _dfltOlev `seq` (True) ->
             (case (dflt_ _dfltOinNmL _dfltOlev ) of
              { ( _dfltIcTrf,_dfltIfvS,_dfltIuseMp) | True ->
                  (case (_lhsIlev) of
                   { _altsOlev | _altsOlev `seq` (True) ->
                   (case (alts_ _altsOinNmL _altsOlev ) of
                    { ( _altsIcTrf,_altsIfvS,_altsIuseMp) | True ->
                        (case (_lhsIlev) of
                         { _exprOlev | _exprOlev `seq` (True) ->
                         (case (expr_ _exprOinNmL _exprOlev ) of
                          { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                              (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (_exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS) of
                                 { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                 (case (_exprIuseMp `useMpComb` _altsIuseMp `useMpComb` _dfltIuseMp) of
                                  { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _errorExprOinNmL | _errorExprOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _errorExprOlev | _errorExprOlev `seq` (True) ->
           (case (errorExpr_ _errorExprOinNmL _errorExprOlev ) of
            { ( _errorExprIcTrf,_errorExprIfvS,_errorExprIuseMp) | True ->
                (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_errorExprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_errorExprIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_CoeArg) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Hole uid_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bodyOinNmL | _bodyOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _bodyOlev | _bodyOlev `seq` (True) ->
           (case (body_ _bodyOinNmL _bodyOlev ) of
            { ( _bodyIcTrf,_bodyIfvS,_bodyIuseMp) | True ->
                (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_bodyIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_bodyIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _funcOinNmL | _funcOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _funcOlev | _funcOlev `seq` (True) ->
           (case (func_ _funcOinNmL _funcOlev ) of
            { ( _funcIcTrf,_funcIfvS,_funcIuseMp) | True ->
                (case (CExpr_ImplsApp _funcIcTrf uid_) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_funcIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_funcIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bodyOinNmL | _bodyOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _bodyOlev | _bodyOlev `seq` (True) ->
           (case (body_ _bodyOinNmL _bodyOlev ) of
            { ( _bodyIcTrf,_bodyIfvS,_bodyIuseMp) | True ->
                (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_bodyIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_bodyIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Integer integer_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bodyOinNmL | _bodyOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _bindOinNmL | _bindOinNmL `seq` (True) ->
           (case (_lhsIlev + 1) of
            { _lev | _lev `seq` (True) ->
            (case (_lev) of
             { _bodyOlev | _bodyOlev `seq` (True) ->
             (case (body_ _bodyOinNmL _bodyOlev ) of
              { ( _bodyIcTrf,_bodyIfvS,_bodyIuseMp) | True ->
                  (case (_lev) of
                   { _bindOlev | _bindOlev `seq` (True) ->
                   (case (False) of
                    { _isGlobal | _isGlobal `seq` (True) ->
                    (case (_isGlobal) of
                     { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                     (case (bind_ _bindOinNmL _bindOisGlobal _bindOlev ) of
                      { ( _bindIbindMp,_bindIcTrf,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL,_bindIuseMp) | True ->
                          (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                           { _cTrf | _cTrf `seq` (True) ->
                           (case (_cTrf) of
                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                            (case (_bindInm) of
                             { _argNm | _argNm `seq` (True) ->
                             (case (_argNm `Set.delete` _bodyIfvS) of
                              { _fvS | _fvS `seq` (True) ->
                              (case (_fvS) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (_bindIuseMp `useMpComb` _bodyIuseMp) of
                                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bodyOinNmL | _bodyOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _bindsOinNmL | _bindsOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _bodyOlev | _bodyOlev `seq` (True) ->
            (case (body_ _bodyOinNmL _bodyOlev ) of
             { ( _bodyIcTrf,_bodyIfvS,_bodyIuseMp) | True ->
                 (case (_lhsIlev) of
                  { _bindsOlev | _bindsOlev `seq` (True) ->
                  (case (_lhsIlev == cLevModule) of
                   { _isGlobal | _isGlobal `seq` (True) ->
                   (case (_isGlobal) of
                    { _bindsOisGlobal | _bindsOisGlobal `seq` (True) ->
                    (case (binds_ _bindsOinNmL _bindsOisGlobal _bindsOlev ) of
                     { ( _bindsIbindMp,_bindsIcTrf,_bindsIfvS,_bindsIfvSMp,_bindsInmL,_bindsIuseMp) | True ->
                         (case (if categ_ == CBindCateg_Rec
                                then  let  h = Map.filterWithKey (\k e -> k `Map.member` _bindsIbindMp) _bindsIuseMp
                                           o = scc . Map.toList $ h
                                           mk c ns b = acoreLet c [ acoreBind1MetasTy n m Ty_Any e | n <- ns, (e,m) <- maybeToList (Map.lookup n _bindsIbindMp) ] b
                                           t = foldr (\bs b
                                                         -> case bs of
                                                               [n] | n `elem` Map.findWithDefault [] n h
                                                                                -> mk CBindCateg_Rec bs b
                                                                   | otherwise  -> mk CBindCateg_Plain bs b
                                                               _                -> mk CBindCateg_Rec bs b
                                                     )
                                                     _bodyIcTrf o
                                      in   (_bindsIuseMp `Map.difference` h,t)
                                else  (_bindsIuseMp,CExpr_Let categ_ _bindsIcTrf _bodyIcTrf)) of
                          { __tup1 | __tup1 `seq` (True) ->
                          (case (__tup1) of
                           { (_,_cTrf) | _cTrf `seq` (True) ->
                           (case (_cTrf) of
                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                            (case ((_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL) of
                             { _fvS | _fvS `seq` (True) ->
                             (case (_fvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              (case (__tup1) of
                               { (_remUseMp,_) | _remUseMp `seq` (True) ->
                               (case (_remUseMp `useMpComb` _bodyIuseMp) of
                                { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_String str_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Tup tag_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _offsetOinNmL | _offsetOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _exprOinNmL | _exprOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _offsetOlev | _offsetOlev `seq` (True) ->
            (case (offset_ _offsetOinNmL _offsetOlev ) of
             { ( _offsetIcTrf,_offsetIfvS,_offsetIuseMp) | True ->
                 (case (_lhsIlev) of
                  { _exprOlev | _exprOlev `seq` (True) ->
                  (case (expr_ _exprOinNmL _exprOlev ) of
                   { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                       (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_exprIfvS `Set.union` _offsetIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_exprIuseMp `useMpComb` _offsetIuseMp) of
                           { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _fldExprOinNmL | _fldExprOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _offsetOinNmL | _offsetOinNmL `seq` (True) ->
           (case (_lhsIinNmL) of
            { _exprOinNmL | _exprOinNmL `seq` (True) ->
            (case (_lhsIlev) of
             { _fldExprOlev | _fldExprOlev `seq` (True) ->
             (case (fldExpr_ _fldExprOinNmL _fldExprOlev ) of
              { ( _fldExprIcTrf,_fldExprIfvS,_fldExprIuseMp) | True ->
                  (case (_lhsIlev) of
                   { _offsetOlev | _offsetOlev `seq` (True) ->
                   (case (offset_ _offsetOinNmL _offsetOlev ) of
                    { ( _offsetIcTrf,_offsetIfvS,_offsetIuseMp) | True ->
                        (case (_lhsIlev) of
                         { _exprOlev | _exprOlev `seq` (True) ->
                         (case (expr_ _exprOinNmL _exprOlev ) of
                          { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                              (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                 { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                 (case (_exprIuseMp `useMpComb` _offsetIuseMp `useMpComb` _fldExprIuseMp) of
                                  { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _fldExprOinNmL | _fldExprOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _offsetOinNmL | _offsetOinNmL `seq` (True) ->
           (case (_lhsIinNmL) of
            { _exprOinNmL | _exprOinNmL `seq` (True) ->
            (case (_lhsIlev) of
             { _fldExprOlev | _fldExprOlev `seq` (True) ->
             (case (fldExpr_ _fldExprOinNmL _fldExprOlev ) of
              { ( _fldExprIcTrf,_fldExprIfvS,_fldExprIuseMp) | True ->
                  (case (_lhsIlev) of
                   { _offsetOlev | _offsetOlev `seq` (True) ->
                   (case (offset_ _offsetOinNmL _offsetOlev ) of
                    { ( _offsetIcTrf,_offsetIfvS,_offsetIuseMp) | True ->
                        (case (_lhsIlev) of
                         { _exprOlev | _exprOlev `seq` (True) ->
                         (case (expr_ _exprOinNmL _exprOlev ) of
                          { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                              (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                 { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                 (case (_exprIuseMp `useMpComb` _offsetIuseMp `useMpComb` _fldExprIuseMp) of
                                  { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExpr_Var ref_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (acbrefNm ref_) of
            { _nm | _nm `seq` (True) ->
            (case (Set.singleton _nm) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.fromList (zip _lhsIinNmL (repeat [_nm]))) of
              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
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
type T_CExprAnn  = ([HsName]) ->
                   Int ->
                   ( CExprAnn ,FvS,UseMp)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExprAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExprAnn_Debug info_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CExprAnn_Ty ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Apply0:
         visit 0:
            local cTrf        : _
      alternative Function0:
         visit 0:
            local cTrf        : _
      alternative Function1:
         visit 0:
            local cTrf        : _
      alternative Plain:
         visit 0:
            local cTrf        : _
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
type T_CMetaBind  = ([HsName]) ->
                    Int ->
                    ( CMetaBind ,FvS,UseMp)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaBind_Apply0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaBind_Function0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaBind_Function1) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaBind_Plain) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Dict:
         visit 0:
            local cTrf        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local cTrf        : _
      alternative Val:
         visit 0:
            local cTrf        : _
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
type T_CMetaVal  = ([HsName]) ->
                   Int ->
                   ( CMetaVal ,FvS,UseMp)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaVal_Dict) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaVal_DictClass tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaVal_Track track_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CMetaVal_Val) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = ([HsName]) ->
                 Int ->
                 ( CMetas ,FvS,UseMp)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIlev) of
          { _x2Olev | _x2Olev `seq` (True) ->
          (case (_lhsIinNmL) of
           { _x2OinNmL | _x2OinNmL `seq` (True) ->
           (case (x2_ _x2OinNmL _x2Olev ) of
            { ( _x2IcTrf,_x2IfvS,_x2IuseMp) | True ->
                (case (_lhsIlev) of
                 { _x1Olev | _x1Olev `seq` (True) ->
                 (case (_lhsIinNmL) of
                  { _x1OinNmL | _x1OinNmL `seq` (True) ->
                  (case (x1_ _x1OinNmL _x1Olev ) of
                   { ( _x1IcTrf,_x1IfvS,_x1IuseMp) | True ->
                       (case ((_x1IcTrf,_x2IcTrf)) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_x1IfvS `Set.union` _x2IfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_x1IuseMp `useMpComb` _x2IuseMp) of
                           { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ([HsName]) ->
                  Int ->
                  ( CModule ,FvS,UseMp)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _exprOinNmL | _exprOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _exprOlev | _exprOlev `seq` (True) ->
           (case (expr_ _exprOinNmL _exprOlev ) of
            { ( _exprIcTrf,_exprIfvS,_exprIuseMp) | True ->
                (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_exprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_exprIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local cTrf        : _
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
type T_CPat  = ([HsName]) ->
               Int ->
               ( CPat ,([HsName]),FvS,([HsName]),UseMp)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPat_BoolExpr cexpr_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              (case (Map.empty) of
               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
               ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPat_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              (case (Map.empty) of
               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
               ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bindsOinNmL | _bindsOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _bindsOlev | _bindsOlev `seq` (True) ->
           (case (binds_ _bindsOinNmL _bindsOlev ) of
            { ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsInmL,_bindsIuseMp) | True ->
                (case (_lhsIlev) of
                 { _restOlev | _restOlev `seq` (True) ->
                 (case (_lhsIinNmL) of
                  { _restOinNmL | _restOinNmL `seq` (True) ->
                  (case (rest_ _restOinNmL _restOlev ) of
                   { ( _restIcTrf,_restIfvS,_restInmL,_restIuseMp) | True ->
                       (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_bindsIfldNmL) of
                          { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                          (case (_restIfvS `Set.union` _bindsIfvS) of
                           { _lhsOfvS | _lhsOfvS `seq` (True) ->
                           (case (_restInmL ++ _bindsInmL) of
                            { _lhsOnmL | _lhsOnmL `seq` (True) ->
                            (case (_restIuseMp `useMpComb` _bindsIuseMp) of
                             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                             ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPat_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              (case (Map.empty) of
               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
               ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPat_Var pnm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([pnm_]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              (case (Map.empty) of
               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
               ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local cTrf        : _
            local fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = ([HsName]) ->
                  Int ->
                  ( CPatFld ,([HsName]),FvS,([HsName]),UseMp)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _bindOinNmL | _bindOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _offsetOinNmL | _offsetOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _fldAnnsOlev | _fldAnnsOlev `seq` (True) ->
            (case (_lhsIinNmL) of
             { _fldAnnsOinNmL | _fldAnnsOinNmL `seq` (True) ->
             (case (fldAnns_ _fldAnnsOinNmL _fldAnnsOlev ) of
              { ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsInmL,_fldAnnsIuseMp) | True ->
                  (case (_lhsIlev) of
                   { _bindOlev | _bindOlev `seq` (True) ->
                   (case (False) of
                    { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                    (case (bind_ _bindOinNmL _bindOisGlobal _bindOlev ) of
                     { ( _bindIbindMp,_bindIcTrf,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL,_bindIuseMp) | True ->
                         (case (_lhsIlev) of
                          { _offsetOlev | _offsetOlev `seq` (True) ->
                          (case (offset_ _offsetOinNmL _offsetOlev ) of
                           { ( _offsetIcTrf,_offsetIfvS,_offsetIuseMp) | True ->
                               (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case (_bindInm) of
                                  { _fldNm | _fldNm `seq` (True) ->
                                  (case ([_fldNm]) of
                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                   (case (_offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS) of
                                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                    (case ([_fldNm]) of
                                     { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                     (case (_offsetIuseMp `useMpComb` _bindIuseMp `useMpComb` _fldAnnsIuseMp) of
                                      { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                                      ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = ([HsName]) ->
                   Int ->
                   ( CPatFldL ,([HsName]),FvS,([HsName]),UseMp)
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _tlOinNmL | _tlOinNmL `seq` (True) ->
          (case (_lhsIinNmL) of
           { _hdOinNmL | _hdOinNmL `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (tl_ _tlOinNmL _tlOlev ) of
             { ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlInmL,_tlIuseMp) | True ->
                 (case (_lhsIlev) of
                  { _hdOlev | _hdOlev `seq` (True) ->
                  (case (hd_ _hdOinNmL _hdOlev ) of
                   { ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdInmL,_hdIuseMp) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_hdIfldNmL ++ _tlIfldNmL) of
                          { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                          (case (_hdIfvS `Set.union` _tlIfvS) of
                           { _lhsOfvS | _lhsOfvS `seq` (True) ->
                           (case (_hdInmL ++ _tlInmL) of
                            { _lhsOnmL | _lhsOnmL `seq` (True) ->
                            (case (_hdIuseMp `useMpComb` _tlIuseMp) of
                             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                             ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              (case (Map.empty) of
               { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
               ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
         useMp                : UseMp
   alternatives:
      alternative Empty:
         visit 0:
            local cTrf        : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = ([HsName]) ->
                   Int ->
                   ( CPatRest ,FvS,([HsName]),UseMp)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPatRest_Empty) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             (case (Map.empty) of
              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (CPatRest_Var nm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([nm_]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             (case (Map.empty) of
              { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOnmL,_lhsOuseMp) }) }) }) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : CModule 
   alternatives:
      alternative AGItf:
         child module         : CModule 
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf )  =
    (let ( _lhsOcTrf) | True = sem 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (case ([]) of
     { _moduleOinNmL | _moduleOinNmL `seq` (True) ->
     (case (cLevModule) of
      { _moduleOlev | _moduleOlev `seq` (True) ->
      (case (module_ _moduleOinNmL _moduleOlev ) of
       { ( _moduleIcTrf,_moduleIfvS,_moduleIuseMp) | True ->
           (case (_moduleIcTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            ( _lhsOcTrf) }) }) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inNmL                : [HsName]
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         useMp                : UseMp
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local cTrf        : _
      alternative Nothing:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = ([HsName]) ->
                  Int ->
                  ( MbCExpr ,FvS,UseMp)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (_lhsIinNmL) of
          { _justOinNmL | _justOinNmL `seq` (True) ->
          (case (_lhsIlev) of
           { _justOlev | _justOlev `seq` (True) ->
           (case (just_ _justOinNmL _justOlev ) of
            { ( _justIcTrf,_justIfvS,_justIuseMp) | True ->
                (case (Just _justIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_justIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_justIuseMp) of
                    { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIinNmL
       _lhsIlev ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOuseMp | _lhsOuseMp `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOuseMp) }) }) }) }))