

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/LetDefBeforeUse.ag)
module EH101.Core.Trf.LetDefBeforeUse(cmodTrfLetDefBeforeUse) where

import Data.Maybe
import EH101.Base.Common
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Base.Debug
import EH101.Base.Builtin
import EH.Util.Utils (scc)













cmodTrfLetDefBeforeUse :: CModule -> CModule
cmodTrfLetDefBeforeUse cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod)) Inh_CodeAGItf
     in   cTrf_Syn_CodeAGItf t



-- | A definition just administers the code (required for transformation) and what it uses (to reorder).
data Def
  = Def
      { defCateg        :: CBindCateg       -- the category of the binding
      , defCore         :: CBind            -- the actual code
      , defFvS          :: FvS              -- its used (free) vars
      }

emptyDef :: Def
emptyDef = Def CBindCateg_Rec (CBind_Bind hsnUnknown []) Set.empty

type DefMp = Map.Map HsName Def

defMpComb :: DefMp -> DefMp -> DefMp
defMpComb = Map.unionWith (\d1 d2 -> d1 {defFvS = defFvS d1 `Set.union` defFvS d2})

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CAlt  = Int ->
               ( CAlt ,DefMp,FvS)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIlev ->
         (case (_lhsIlev + 1) of
          { _lev | _lev `seq` (True) ->
          (case (_lev) of
           { _exprOlev | _exprOlev `seq` (True) ->
           (case (expr_ _exprOlev ) of
            { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
                (case (_lev) of
                 { _patOlev | _patOlev `seq` (True) ->
                 (case (pat_ _patOlev ) of
                  { ( _patIcTrf,_patIdefMp,_patIfldNmL,_patIfvS,_patInmL) | True ->
                      (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                       { _cTrf | _cTrf `seq` (True) ->
                       (case (_cTrf) of
                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                        (case (_patIdefMp `defMpComb` _exprIdefMp) of
                         { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                         (case (_exprIfvS `Set.difference` Set.fromList _patInmL) of
                          { _fvS | _fvS `seq` (True) ->
                          (case (_fvS) of
                           { _lhsOfvS | _lhsOfvS `seq` (True) ->
                           ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CAltL  = Int ->
                ( CAltL ,DefMp,FvS)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (tl_ _tlOlev ) of
           { ( _tlIcTrf,_tlIdefMp,_tlIfvS) | True ->
               (case (_lhsIlev) of
                { _hdOlev | _hdOlev `seq` (True) ->
                (case (hd_ _hdOlev ) of
                 { ( _hdIcTrf,_hdIdefMp,_hdIfvS) | True ->
                     (case ((:) _hdIcTrf _tlIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_hdIdefMp `defMpComb` _tlIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_hdIfvS `Set.union` _tlIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isGlobal             : Bool
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         fvSMp                : FvSMp
         nm                   : HsName
         nmL                  : [HsName]
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
type T_CBind  = Bool ->
                Int ->
                ( CBind ,DefMp,FvS,FvSMp,HsName,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIisGlobal
       _lhsIlev ->
         (case (_lhsIlev) of
          { _bindAspectsOlev | _bindAspectsOlev `seq` (True) ->
          (case (_lhsIisGlobal) of
           { _bindAspectsOisGlobal | _bindAspectsOisGlobal `seq` (True) ->
           (case (nm_) of
            { _bindAspectsOnm | _bindAspectsOnm `seq` (True) ->
            (case (bindAspects_ _bindAspectsOisGlobal _bindAspectsOlev _bindAspectsOnm ) of
             { ( _bindAspectsIcTrf,_bindAspectsIdefMp,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsInmL) | True ->
                 (case (CBind_Bind nm_ _bindAspectsIcTrf) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case (_cTrf) of
                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                   (case (Map.singleton nm_ (emptyDef {defCore = _cTrf, defFvS = _bindAspectsIfvS})) of
                    { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                    (case (_bindAspectsIfvS) of
                     { _lhsOfvS | _lhsOfvS `seq` (True) ->
                     (case (Map.singleton nm_ _bindAspectsIfvS) of
                      { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                      (case (nm_) of
                       { _lhsOnm | _lhsOnm `seq` (True) ->
                       (case ([nm_]) of
                        { _lhsOnmL | _lhsOnmL `seq` (True) ->
                        ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnm,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CBindAnn  = Int ->
                   ( CBindAnn ,DefMp,FvS,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlev ->
         (case (CBindAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CBindAnnL  = Int ->
                    ( CBindAnnL ,DefMp,FvS,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (tl_ _tlOlev ) of
           { ( _tlIcTrf,_tlIdefMp,_tlIfvS,_tlInmL) | True ->
               (case (_lhsIlev) of
                { _hdOlev | _hdOlev `seq` (True) ->
                (case (hd_ _hdOlev ) of
                 { ( _hdIcTrf,_hdIdefMp,_hdIfvS,_hdInmL) | True ->
                     (case ((:) _hdIcTrf _tlIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_hdIdefMp `defMpComb` _tlIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_hdIfvS `Set.union` _tlIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (_hdInmL ++ _tlInmL) of
                          { _lhsOnmL | _lhsOnmL `seq` (True) ->
                          ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isGlobal             : Bool
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
type T_CBindL  = Bool ->
                 Int ->
                 ( CBindL ,DefMp,FvS,FvSMp,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIisGlobal
       _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (_lhsIisGlobal) of
           { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
           (case (tl_ _tlOisGlobal _tlOlev ) of
            { ( _tlIcTrf,_tlIdefMp,_tlIfvS,_tlIfvSMp,_tlInmL) | True ->
                (case (_lhsIlev) of
                 { _hdOlev | _hdOlev `seq` (True) ->
                 (case (_lhsIisGlobal) of
                  { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                  (case (hd_ _hdOisGlobal _hdOlev ) of
                   { ( _hdIcTrf,_hdIdefMp,_hdIfvS,_hdIfvSMp,_hdInm,_hdInmL) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_hdIdefMp `defMpComb` _tlIdefMp) of
                          { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                          (case (_hdIfvS `Set.union` _tlIfvS) of
                           { _lhsOfvS | _lhsOfvS `seq` (True) ->
                           (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                            { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                            (case (_hdInmL ++ _tlInmL) of
                             { _lhsOnmL | _lhsOnmL `seq` (True) ->
                             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIisGlobal
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
type T_CBound  = Bool ->
                 Int ->
                 HsName ->
                 ( CBound ,DefMp,FvS,FvSMp,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsIlev) of
          { _exprOlev | _exprOlev `seq` (True) ->
          (case (expr_ _exprOlev ) of
           { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
               (case (_lhsIlev) of
                { _bindMetaOlev | _bindMetaOlev `seq` (True) ->
                (case (bindMeta_ _bindMetaOlev ) of
                 { ( _bindMetaIcTrf,_bindMetaIdefMp,_bindMetaIfvS) | True ->
                     (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_bindMetaIdefMp `defMpComb` _exprIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_bindMetaIfvS `Set.union` _exprIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (Map.empty) of
                          { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                          (case ([]) of
                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                           ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsIlev) of
          { _exprOlev | _exprOlev `seq` (True) ->
          (case (expr_ _exprOlev ) of
           { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
               (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (_exprIdefMp) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_exprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (Map.empty) of
                    { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                    (case ([]) of
                     { _lhsOnmL | _lhsOnmL `seq` (True) ->
                     ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsIlev) of
          { _cmetasOlev | _cmetasOlev `seq` (True) ->
          (case (cmetas_ _cmetasOlev ) of
           { ( _cmetasIcTrf,_cmetasIdefMp,_cmetasIfvS) | True ->
               (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (_cmetasIdefMp) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_cmetasIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (Map.empty) of
                    { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                    (case ([]) of
                     { _lhsOnmL | _lhsOnmL `seq` (True) ->
                     ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (CBound_RelevTy aspectKeyS_ relevTy_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (CBound_Ty aspectKeyS_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsIlev) of
          { _exprOlev | _exprOlev `seq` (True) ->
          (case (expr_ _exprOlev ) of
           { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
               (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (_exprIdefMp) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_exprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (Map.empty) of
                    { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                    (case ([]) of
                     { _lhsOnmL | _lhsOnmL `seq` (True) ->
                     ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
type T_CBoundL  = Bool ->
                  Int ->
                  HsName ->
                  ( CBoundL ,DefMp,FvS,FvSMp,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case (_lhsInm) of
          { _tlOnm | _tlOnm `seq` (True) ->
          (case (_lhsIlev) of
           { _tlOlev | _tlOlev `seq` (True) ->
           (case (_lhsIisGlobal) of
            { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
            (case (tl_ _tlOisGlobal _tlOlev _tlOnm ) of
             { ( _tlIcTrf,_tlIdefMp,_tlIfvS,_tlIfvSMp,_tlInmL) | True ->
                 (case (_lhsInm) of
                  { _hdOnm | _hdOnm `seq` (True) ->
                  (case (_lhsIlev) of
                   { _hdOlev | _hdOlev `seq` (True) ->
                   (case (_lhsIisGlobal) of
                    { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                    (case (hd_ _hdOisGlobal _hdOlev _hdOnm ) of
                     { ( _hdIcTrf,_hdIdefMp,_hdIfvS,_hdIfvSMp,_hdInmL) | True ->
                         (case ((:) _hdIcTrf _tlIcTrf) of
                          { _cTrf | _cTrf `seq` (True) ->
                          (case (_cTrf) of
                           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                           (case (_hdIdefMp `defMpComb` _tlIdefMp) of
                            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                            (case (_hdIfvS `Set.union` _tlIfvS) of
                             { _lhsOfvS | _lhsOfvS `seq` (True) ->
                             (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                              (case (_hdInmL ++ _tlInmL) of
                               { _lhsOnmL | _lhsOnmL `seq` (True) ->
                               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIisGlobal
       _lhsIlev
       _lhsInm ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (Map.empty) of
              { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         letBody              : CExpr 
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
            local cTrf        : _
            local fvS         : _
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
type T_CExpr  = Int ->
                ( CExpr ,DefMp,FvS,CExpr )
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _exprOlev | _exprOlev `seq` (True) ->
          (case (expr_ _exprOlev ) of
           { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
               (case (_lhsIlev) of
                { _annOlev | _annOlev `seq` (True) ->
                (case (ann_ _annOlev ) of
                 { ( _annIcTrf,_annIdefMp,_annIfvS) | True ->
                     (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_annIdefMp `defMpComb` _exprIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_annIfvS `Set.union` _exprIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOletBody | _lhsOletBody `seq` (True) ->
                          ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _argOlev | _argOlev `seq` (True) ->
          (case (False) of
           { _isGlobal | _isGlobal `seq` (True) ->
           (case (_isGlobal) of
            { _argOisGlobal | _argOisGlobal `seq` (True) ->
            (case (hsnUnknown) of
             { _argOnm | _argOnm `seq` (True) ->
             (case (arg_ _argOisGlobal _argOlev _argOnm ) of
              { ( _argIcTrf,_argIdefMp,_argIfvS,_argIfvSMp,_argInmL) | True ->
                  (case (_lhsIlev) of
                   { _funcOlev | _funcOlev `seq` (True) ->
                   (case (func_ _funcOlev ) of
                    { ( _funcIcTrf,_funcIdefMp,_funcIfvS,_funcIletBody) | True ->
                        (case (CExpr_App _funcIcTrf _argIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (Map.empty) of
                           { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                           (case (_funcIfvS `Set.union` _argIfvS) of
                            { _fvS | _fvS `seq` (True) ->
                            (case (_fvS) of
                             { _lhsOfvS | _lhsOfvS `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOletBody | _lhsOletBody `seq` (True) ->
                              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _dfltOlev | _dfltOlev `seq` (True) ->
          (case (dflt_ _dfltOlev ) of
           { ( _dfltIcTrf,_dfltIdefMp,_dfltIfvS,_dfltIletBody) | True ->
               (case (_lhsIlev) of
                { _altsOlev | _altsOlev `seq` (True) ->
                (case (alts_ _altsOlev ) of
                 { ( _altsIcTrf,_altsIdefMp,_altsIfvS) | True ->
                     (case (_lhsIlev) of
                      { _exprOlev | _exprOlev `seq` (True) ->
                      (case (expr_ _exprOlev ) of
                       { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
                           (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (Map.empty) of
                              { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                              (case (_exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOletBody | _lhsOletBody `seq` (True) ->
                                ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _errorExprOlev | _errorExprOlev `seq` (True) ->
          (case (errorExpr_ _errorExprOlev ) of
           { ( _errorExprIcTrf,_errorExprIdefMp,_errorExprIfvS,_errorExprIletBody) | True ->
               (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (Map.empty) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_errorExprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOletBody | _lhsOletBody `seq` (True) ->
                    ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIlev ->
         (case (CExpr_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIlev ->
         (case (CExpr_CoeArg) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIlev ->
         (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIlev ->
         (case (CExpr_Hole uid_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _bodyOlev | _bodyOlev `seq` (True) ->
          (case (body_ _bodyOlev ) of
           { ( _bodyIcTrf,_bodyIdefMp,_bodyIfvS,_bodyIletBody) | True ->
               (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (Map.empty) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_bodyIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOletBody | _lhsOletBody `seq` (True) ->
                    ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _funcOlev | _funcOlev `seq` (True) ->
          (case (func_ _funcOlev ) of
           { ( _funcIcTrf,_funcIdefMp,_funcIfvS,_funcIletBody) | True ->
               (case (CExpr_ImplsApp _funcIcTrf uid_) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (Map.empty) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_funcIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOletBody | _lhsOletBody `seq` (True) ->
                    ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _bodyOlev | _bodyOlev `seq` (True) ->
          (case (body_ _bodyOlev ) of
           { ( _bodyIcTrf,_bodyIdefMp,_bodyIfvS,_bodyIletBody) | True ->
               (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (Map.empty) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_bodyIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOletBody | _lhsOletBody `seq` (True) ->
                    ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIlev ->
         (case (CExpr_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIlev ->
         (case (CExpr_Integer integer_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIlev ->
         (case (_lhsIlev + 1) of
          { _lev | _lev `seq` (True) ->
          (case (_lev) of
           { _bodyOlev | _bodyOlev `seq` (True) ->
           (case (body_ _bodyOlev ) of
            { ( _bodyIcTrf,_bodyIdefMp,_bodyIfvS,_bodyIletBody) | True ->
                (case (_lev) of
                 { _bindOlev | _bindOlev `seq` (True) ->
                 (case (False) of
                  { _isGlobal | _isGlobal `seq` (True) ->
                  (case (_isGlobal) of
                   { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                   (case (bind_ _bindOisGlobal _bindOlev ) of
                    { ( _bindIcTrf,_bindIdefMp,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL) | True ->
                        (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (Map.empty) of
                           { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                           (case (_bindInm) of
                            { _argNm | _argNm `seq` (True) ->
                            (case (_argNm `Set.delete` _bodyIfvS) of
                             { _fvS | _fvS `seq` (True) ->
                             (case (_fvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOletBody | _lhsOletBody `seq` (True) ->
                               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _bodyOlev | _bodyOlev `seq` (True) ->
          (case (body_ _bodyOlev ) of
           { ( _bodyIcTrf,_bodyIdefMp,_bodyIfvS,_bodyIletBody) | True ->
               (case (_lhsIlev == cLevModule) of
                { _isGlobal | _isGlobal `seq` (True) ->
                (case (_isGlobal) of
                 { _bindsOisGlobal | _bindsOisGlobal `seq` (True) ->
                 (case (_lhsIlev + 1) of
                  { _bindsOlev | _bindsOlev `seq` (True) ->
                  (case (binds_ _bindsOisGlobal _bindsOlev ) of
                   { ( _bindsIcTrf,_bindsIdefMp,_bindsIfvS,_bindsIfvSMp,_bindsInmL) | True ->
                       (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (if _isGlobal
                                then Map.map (\d -> d {defCateg = categ_}) _bindsIdefMp `defMpComb` _bodyIdefMp
                                else Map.empty) of
                          { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                          (case ((_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL) of
                           { _fvS | _fvS `seq` (True) ->
                           (case (_fvS) of
                            { _lhsOfvS | _lhsOfvS `seq` (True) ->
                            (case (_bodyIletBody) of
                             { _lhsOletBody | _lhsOletBody `seq` (True) ->
                             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIlev ->
         (case (CExpr_String str_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIlev ->
         (case (CExpr_Tup tag_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case (_cTrf) of
              { _lhsOletBody | _lhsOletBody `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _offsetOlev | _offsetOlev `seq` (True) ->
          (case (offset_ _offsetOlev ) of
           { ( _offsetIcTrf,_offsetIdefMp,_offsetIfvS,_offsetIletBody) | True ->
               (case (_lhsIlev) of
                { _exprOlev | _exprOlev `seq` (True) ->
                (case (expr_ _exprOlev ) of
                 { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
                     (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (Map.empty) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_exprIfvS `Set.union` _offsetIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOletBody | _lhsOletBody `seq` (True) ->
                          ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _fldExprOlev | _fldExprOlev `seq` (True) ->
          (case (fldExpr_ _fldExprOlev ) of
           { ( _fldExprIcTrf,_fldExprIdefMp,_fldExprIfvS,_fldExprIletBody) | True ->
               (case (_lhsIlev) of
                { _offsetOlev | _offsetOlev `seq` (True) ->
                (case (offset_ _offsetOlev ) of
                 { ( _offsetIcTrf,_offsetIdefMp,_offsetIfvS,_offsetIletBody) | True ->
                     (case (_lhsIlev) of
                      { _exprOlev | _exprOlev `seq` (True) ->
                      (case (expr_ _exprOlev ) of
                       { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
                           (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (Map.empty) of
                              { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                              (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOletBody | _lhsOletBody `seq` (True) ->
                                ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _fldExprOlev | _fldExprOlev `seq` (True) ->
          (case (fldExpr_ _fldExprOlev ) of
           { ( _fldExprIcTrf,_fldExprIdefMp,_fldExprIfvS,_fldExprIletBody) | True ->
               (case (_lhsIlev) of
                { _offsetOlev | _offsetOlev `seq` (True) ->
                (case (offset_ _offsetOlev ) of
                 { ( _offsetIcTrf,_offsetIdefMp,_offsetIfvS,_offsetIletBody) | True ->
                     (case (_lhsIlev) of
                      { _exprOlev | _exprOlev `seq` (True) ->
                      (case (expr_ _exprOlev ) of
                       { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
                           (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (Map.empty) of
                              { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                              (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOletBody | _lhsOletBody `seq` (True) ->
                                ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIlev ->
         (case (CExpr_Var ref_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (acbrefNm ref_) of
             { _nm | _nm `seq` (True) ->
             (case (Set.singleton _nm) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case (_cTrf) of
               { _lhsOletBody | _lhsOletBody `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOletBody) }) }) }) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CExprAnn  = Int ->
                   ( CExprAnn ,DefMp,FvS)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Debug info_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Ty ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CMetaBind  = Int ->
                    ( CMetaBind ,DefMp,FvS)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlev ->
         (case (CMetaBind_Apply0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlev ->
         (case (CMetaBind_Function0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlev ->
         (case (CMetaBind_Function1) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlev ->
         (case (CMetaBind_Plain) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CMetaVal  = Int ->
                   ( CMetaVal ,DefMp,FvS)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlev ->
         (case (CMetaVal_Dict) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlev ->
         (case (CMetaVal_DictClass tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlev ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlev ->
         (case (CMetaVal_Track track_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlev ->
         (case (CMetaVal_Val) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_CMetas  = Int ->
                 ( CMetas ,DefMp,FvS)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _x2Olev | _x2Olev `seq` (True) ->
          (case (x2_ _x2Olev ) of
           { ( _x2IcTrf,_x2IdefMp,_x2IfvS) | True ->
               (case (_lhsIlev) of
                { _x1Olev | _x1Olev `seq` (True) ->
                (case (x1_ _x1Olev ) of
                 { ( _x1IcTrf,_x1IdefMp,_x1IfvS) | True ->
                     (case ((_x1IcTrf,_x2IcTrf)) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_x1IdefMp `defMpComb` _x2IdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_x1IfvS `Set.union` _x2IfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local sccDefs     : _
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = Int ->
                  ( CModule ,FvS)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _exprOlev | _exprOlev `seq` (True) ->
          (case (expr_ _exprOlev ) of
           { ( _exprIcTrf,_exprIdefMp,_exprIfvS,_exprIletBody) | True ->
               (case (map (catMaybes . map (\n -> Map.lookup n _exprIdefMp))
                      $ scc [ (n, Set.toList $ defFvS d) | (n,d) <- Map.toList _exprIdefMp ]) of
                { _sccDefs | _sccDefs `seq` (True) ->
                (case (let mk ds@(_:_:_) e = acoreLetRec              (map defCore ds) e
                           mk    [d]     e = acoreLet    (defCateg d) [defCore d]      e
                       in  CModule_Mod
                             moduleNm_
                             (foldr mk _exprIletBody _sccDefs)
                             ctagsMp_) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_exprIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CPat  = Int ->
               ( CPat ,DefMp,([HsName]),FvS,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlev ->
         (case (CPat_BoolExpr cexpr_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case ([]) of
             { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
             (case (Set.empty) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlev ->
         (case (CPat_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case ([]) of
             { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
             (case (Set.empty) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _bindsOlev | _bindsOlev `seq` (True) ->
          (case (binds_ _bindsOlev ) of
           { ( _bindsIcTrf,_bindsIdefMp,_bindsIfldNmL,_bindsIfvS,_bindsInmL) | True ->
               (case (_lhsIlev) of
                { _restOlev | _restOlev `seq` (True) ->
                (case (rest_ _restOlev ) of
                 { ( _restIcTrf,_restIdefMp,_restIfvS,_restInmL) | True ->
                     (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_restIdefMp `defMpComb` _bindsIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_bindsIfldNmL) of
                         { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                         (case (_restIfvS `Set.union` _bindsIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_restInmL ++ _bindsInmL) of
                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                           ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlev ->
         (case (CPat_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case ([]) of
             { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
             (case (Set.empty) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlev ->
         (case (CPat_Var pnm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case ([]) of
             { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
             (case (Set.empty) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case ([pnm_]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CPatFld  = Int ->
                  ( CPatFld ,DefMp,([HsName]),FvS,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _fldAnnsOlev | _fldAnnsOlev `seq` (True) ->
          (case (fldAnns_ _fldAnnsOlev ) of
           { ( _fldAnnsIcTrf,_fldAnnsIdefMp,_fldAnnsIfvS,_fldAnnsInmL) | True ->
               (case (_lhsIlev) of
                { _bindOlev | _bindOlev `seq` (True) ->
                (case (False) of
                 { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                 (case (bind_ _bindOisGlobal _bindOlev ) of
                  { ( _bindIcTrf,_bindIdefMp,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL) | True ->
                      (case (_lhsIlev) of
                       { _offsetOlev | _offsetOlev `seq` (True) ->
                       (case (offset_ _offsetOlev ) of
                        { ( _offsetIcTrf,_offsetIdefMp,_offsetIfvS,_offsetIletBody) | True ->
                            (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (_offsetIdefMp `defMpComb` _bindIdefMp `defMpComb` _fldAnnsIdefMp) of
                               { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                               (case (_bindInm) of
                                { _fldNm | _fldNm `seq` (True) ->
                                (case ([_fldNm]) of
                                 { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                 (case (_offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS) of
                                  { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                  (case ([_fldNm]) of
                                   { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                   ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CPatFldL  = Int ->
                   ( CPatFldL ,DefMp,([HsName]),FvS,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (tl_ _tlOlev ) of
           { ( _tlIcTrf,_tlIdefMp,_tlIfldNmL,_tlIfvS,_tlInmL) | True ->
               (case (_lhsIlev) of
                { _hdOlev | _hdOlev `seq` (True) ->
                (case (hd_ _hdOlev ) of
                 { ( _hdIcTrf,_hdIdefMp,_hdIfldNmL,_hdIfvS,_hdInmL) | True ->
                     (case ((:) _hdIcTrf _tlIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_hdIdefMp `defMpComb` _tlIdefMp) of
                        { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                        (case (_hdIfldNmL ++ _tlIfldNmL) of
                         { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                         (case (_hdIfvS `Set.union` _tlIfvS) of
                          { _lhsOfvS | _lhsOfvS `seq` (True) ->
                          (case (_hdInmL ++ _tlInmL) of
                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                           ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case ([]) of
             { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
             (case (Set.empty) of
              { _lhsOfvS | _lhsOfvS `seq` (True) ->
              (case ([]) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               ( _lhsOcTrf,_lhsOdefMp,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
         nmL                  : [HsName]
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
type T_CPatRest  = Int ->
                   ( CPatRest ,DefMp,FvS,([HsName]))
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlev ->
         (case (CPatRest_Empty) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlev ->
         (case (CPatRest_Var nm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([nm_]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
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
    (case (cLevModule) of
     { _moduleOlev | _moduleOlev `seq` (True) ->
     (case (module_ _moduleOlev ) of
      { ( _moduleIcTrf,_moduleIfvS) | True ->
          (case (_moduleIcTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         defMp                : DefMp
         fvS                  : FvS
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
type T_MbCExpr  = Int ->
                  ( MbCExpr ,DefMp,FvS)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _justOlev | _justOlev `seq` (True) ->
          (case (just_ _justOlev ) of
           { ( _justIcTrf,_justIdefMp,_justIfvS,_justIletBody) | True ->
               (case (Just _justIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (_justIdefMp) of
                  { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
                  (case (_justIfvS) of
                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                   ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIlev ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOdefMp | _lhsOdefMp `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             ( _lhsOcTrf,_lhsOdefMp,_lhsOfvS) }) }) }) }))