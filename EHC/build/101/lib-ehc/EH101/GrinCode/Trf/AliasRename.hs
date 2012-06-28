

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Trf/AliasRename.ag)
module EH101.GrinCode.Trf.AliasRename(grAliasRename) where

import qualified Data.Map as Map
import Data.Maybe
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.GrinCode.Common
import EH101.GrinCode
import qualified EH101.Config as Cfg











grAliasRename :: Maybe (HsName -> HsName) -> NmAliasMp -> GrExpr -> GrExpr
grAliasRename mbMkNewNm m e
  = trf_Syn_GrExpr t
  where t = wrap_GrExpr (sem_GrExpr e)
            $ Inh_GrExpr
                { nmAliasMp_Inh_GrExpr = m
                , mkNewNm_Inh_GrExpr = maybe id id mbMkNewNm
                }

-- GrAGItf -----------------------------------------------------
{-
   alternatives:
      alternative AGItf:
         child module         : GrModule 
-}
-- cata
sem_GrAGItf :: GrAGItf  ->
               T_GrAGItf 
sem_GrAGItf (GrAGItf_AGItf _module )  =
    (sem_GrAGItf_AGItf (sem_GrModule _module ) )
-- semantic domain
type T_GrAGItf  = ( )
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    ( )
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Del:
         child off            : GrVal 
         visit 0:
            local trf         : _
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
         visit 0:
            local trf         : _
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAdapt :: GrAdapt  ->
               T_GrAdapt 
sem_GrAdapt (GrAdapt_Del _off )  =
    (sem_GrAdapt_Del (sem_GrVal _off ) )
sem_GrAdapt (GrAdapt_Ins _off _val )  =
    (sem_GrAdapt_Ins (sem_GrVal _off ) (sem_GrVal _val ) )
sem_GrAdapt (GrAdapt_Upd _off _val )  =
    (sem_GrAdapt_Upd (sem_GrVal _off ) (sem_GrVal _val ) )
-- semantic domain
type T_GrAdapt  = (HsName -> HsName) ->
                  NmAliasMp ->
                  ( GrAdapt )
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _offOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _offOmkNewNm ->
           (case (off_ _offOmkNewNm _offOnmAliasMp ) of
            { ( _offInmAlias,_offItrf) ->
                (case (GrAdapt_Del _offItrf) of
                 { _trf ->
                 (case (_trf) of
                  { _lhsOtrf ->
                  ( _lhsOtrf) }) }) }) }) }))
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _valOnmAliasMp ->
          (case (_lhsInmAliasMp) of
           { _offOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _valOmkNewNm ->
            (case (val_ _valOmkNewNm _valOnmAliasMp ) of
             { ( _valInmAlias,_valItrf) ->
                 (case (_lhsImkNewNm) of
                  { _offOmkNewNm ->
                  (case (off_ _offOmkNewNm _offOnmAliasMp ) of
                   { ( _offInmAlias,_offItrf) ->
                       (case (GrAdapt_Ins _offItrf _valItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _valOnmAliasMp ->
          (case (_lhsInmAliasMp) of
           { _offOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _valOmkNewNm ->
            (case (val_ _valOmkNewNm _valOnmAliasMp ) of
             { ( _valInmAlias,_valItrf) ->
                 (case (_lhsImkNewNm) of
                  { _offOmkNewNm ->
                  (case (off_ _offOmkNewNm _offOnmAliasMp ) of
                   { ( _offInmAlias,_offItrf) ->
                       (case (GrAdapt_Upd _offItrf _valItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = (HsName -> HsName) ->
                   NmAliasMp ->
                   ( GrAdaptL )
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _tlOnmAliasMp ->
          (case (_lhsInmAliasMp) of
           { _hdOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _tlOmkNewNm ->
            (case (tl_ _tlOmkNewNm _tlOnmAliasMp ) of
             { ( _tlItrf) ->
                 (case (_lhsImkNewNm) of
                  { _hdOmkNewNm ->
                  (case (hd_ _hdOmkNewNm _hdOnmAliasMp ) of
                   { ( _hdItrf) ->
                       (case ((:) _hdItrf _tlItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
         visit 0:
            local patAliasMp  : _
            local nmAliasMp   : _
            local trf         : _
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = (HsName -> HsName) ->
                NmAliasMp ->
                ( GrAlt )
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (pat_ ) of
          { ( _patIintroNmL,pat_1) ->
              (case (mkNmAliasMp [(n,_lhsImkNewNm n) | n <- _patIintroNmL]) of
               { _patAliasMp ->
               (case (_patAliasMp `Map.union` _lhsInmAliasMp) of
                { _nmAliasMp ->
                (case (_nmAliasMp) of
                 { _exprOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _exprOmkNewNm ->
                  (case (_nmAliasMp) of
                   { _patOnmAliasMp ->
                   (case (expr_ _exprOmkNewNm _exprOnmAliasMp ) of
                    { ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) ->
                        (case (_lhsImkNewNm) of
                         { _patOmkNewNm ->
                         (case (pat_1 _patOmkNewNm _patOnmAliasMp ) of
                          { ( _patInmAlias,_patItrf) ->
                              (case (GrAlt_Alt ann_ _patItrf _exprItrf) of
                               { _trf ->
                               (case (_trf) of
                                { _lhsOtrf ->
                                ( _lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( GrAltL )
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _tlOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _tlOmkNewNm ->
           (case (_lhsInmAliasMp) of
            { _hdOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _hdOmkNewNm ->
             (case (tl_ _tlOmkNewNm _tlOnmAliasMp ) of
              { ( _tlItrf) ->
                  (case (hd_ _hdOmkNewNm _hdOnmAliasMp ) of
                   { ( _hdItrf) ->
                       (case ((:) _hdItrf _tlItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrBind ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mkNewNm              : HsName -> HsName
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Arity:
         child nm             : {HsName}
         child arity          : {Int}
         visit 0:
            local trf         : _
      alternative Bind:
         child nm             : {HsName}
         child annot          : {GrBindAnn}
         child argNmL         : {[HsName]}
         child expr           : GrExpr 
         visit 0:
            local trf         : _
      alternative Rec:
         child bindL          : GrBindL 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrBind :: GrBind  ->
              T_GrBind 
sem_GrBind (GrBind_Arity _nm _arity )  =
    (sem_GrBind_Arity _nm _arity )
sem_GrBind (GrBind_Bind _nm _annot _argNmL _expr )  =
    (sem_GrBind_Bind _nm _annot _argNmL (sem_GrExpr _expr ) )
sem_GrBind (GrBind_Rec _bindL )  =
    (sem_GrBind_Rec (sem_GrBindL _bindL ) )
-- semantic domain
type T_GrBind  = (HsName -> HsName) ->
                 ( GrBind )
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (\ _lhsImkNewNm ->
         (case (GrBind_Arity nm_ arity_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (\ _lhsImkNewNm ->
         (case (_lhsImkNewNm) of
          { _exprOmkNewNm ->
          (case (Map.empty) of
           { _exprOnmAliasMp ->
           (case (expr_ _exprOmkNewNm _exprOnmAliasMp ) of
            { ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) ->
                (case (GrBind_Bind nm_ annot_ argNmL_ _exprItrf) of
                 { _trf ->
                 (case (_trf) of
                  { _lhsOtrf ->
                  ( _lhsOtrf) }) }) }) }) }))
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (\ _lhsImkNewNm ->
         (case (_lhsImkNewNm) of
          { _bindLOmkNewNm ->
          (case (bindL_ _bindLOmkNewNm ) of
           { ( _bindLItrf) ->
               (case (GrBind_Rec _bindLItrf) of
                { _trf ->
                (case (_trf) of
                 { _lhsOtrf ->
                 ( _lhsOtrf) }) }) }) }))
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mkNewNm              : HsName -> HsName
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrBind 
         child tl             : GrBindL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrBindL :: GrBindL  ->
               T_GrBindL 
sem_GrBindL list  =
    (Prelude.foldr sem_GrBindL_Cons sem_GrBindL_Nil (Prelude.map sem_GrBind list) )
-- semantic domain
type T_GrBindL  = (HsName -> HsName) ->
                  ( GrBindL )
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (\ _lhsImkNewNm ->
         (case (_lhsImkNewNm) of
          { _tlOmkNewNm ->
          (case (_lhsImkNewNm) of
           { _hdOmkNewNm ->
           (case (tl_ _tlOmkNewNm ) of
            { ( _tlItrf) ->
                (case (hd_ _hdOmkNewNm ) of
                 { ( _hdItrf) ->
                     (case ((:) _hdItrf _tlItrf) of
                      { _trf ->
                      (case (_trf) of
                       { _lhsOtrf ->
                       ( _lhsOtrf) }) }) }) }) }) }))
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (\ _lhsImkNewNm ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         hasSideEffect        : Bool
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local trf         : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
      alternative Eval:
         child nm             : {HsName}
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
      alternative FetchNode:
         child nm             : {HsName}
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
         visit 0:
            local patAliasMp  : _
            local newAliasMp  : _
            local trf         : _
      alternative Store:
         child val            : GrVal 
         visit 0:
            local trf         : _
      alternative Throw:
         child nm             : {HsName}
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local trf         : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
-}
-- cata
sem_GrExpr :: GrExpr  ->
              T_GrExpr 
sem_GrExpr (GrExpr_App _nm _argL )  =
    (sem_GrExpr_App _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Call _nm _argL )  =
    (sem_GrExpr_Call _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Case _val _altL )  =
    (sem_GrExpr_Case (sem_GrVal _val ) (sem_GrAltL _altL ) )
sem_GrExpr (GrExpr_Catch _body _arg _handler )  =
    (sem_GrExpr_Catch (sem_GrExpr _body ) _arg (sem_GrExpr _handler ) )
sem_GrExpr (GrExpr_Eval _nm )  =
    (sem_GrExpr_Eval _nm )
sem_GrExpr (GrExpr_FFI _callconv _impEnt _ffiAnnot _argL )  =
    (sem_GrExpr_FFI _callconv _impEnt _ffiAnnot (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_FetchField _nm _offset _mbTag )  =
    (sem_GrExpr_FetchField _nm _offset _mbTag )
sem_GrExpr (GrExpr_FetchNode _nm )  =
    (sem_GrExpr_FetchNode _nm )
sem_GrExpr (GrExpr_FetchUpdate _src _dst )  =
    (sem_GrExpr_FetchUpdate _src _dst )
sem_GrExpr (GrExpr_Seq _expr _pat _body )  =
    (sem_GrExpr_Seq (sem_GrExpr _expr ) (sem_GrPatLam _pat ) (sem_GrExpr _body ) )
sem_GrExpr (GrExpr_Store _val )  =
    (sem_GrExpr_Store (sem_GrVal _val ) )
sem_GrExpr (GrExpr_Throw _nm )  =
    (sem_GrExpr_Throw _nm )
sem_GrExpr (GrExpr_Unit _val _type )  =
    (sem_GrExpr_Unit (sem_GrVal _val ) (sem_GrType _type ) )
sem_GrExpr (GrExpr_UpdateUnit _nm _val )  =
    (sem_GrExpr_UpdateUnit _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrExpr  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( Bool,NmAlias,GrExpr )
data Inh_GrExpr  = Inh_GrExpr {mkNewNm_Inh_GrExpr :: !((HsName -> HsName)),nmAliasMp_Inh_GrExpr :: !(NmAliasMp)}
data Syn_GrExpr  = Syn_GrExpr {hasSideEffect_Syn_GrExpr :: !(Bool),nmAlias_Syn_GrExpr :: !(NmAlias),trf_Syn_GrExpr :: !(GrExpr )}
wrap_GrExpr :: T_GrExpr  ->
               Inh_GrExpr  ->
               Syn_GrExpr 
wrap_GrExpr sem (Inh_GrExpr _lhsImkNewNm _lhsInmAliasMp )  =
    (let ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) = sem _lhsImkNewNm _lhsInmAliasMp 
     in  (Syn_GrExpr _lhsOhasSideEffect _lhsOnmAlias _lhsOtrf ))
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _argLOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _argLOmkNewNm ->
             (case (argL_ _argLOmkNewNm _argLOnmAliasMp ) of
              { ( _argLInmAliasL,_argLItrf) ->
                  (case (GrExpr_App (nmAliasRepl _lhsInmAliasMp nm_) _argLItrf) of
                   { _lhsOtrf ->
                   ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _argLOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _argLOmkNewNm ->
             (case (argL_ _argLOmkNewNm _argLOnmAliasMp ) of
              { ( _argLInmAliasL,_argLItrf) ->
                  (case (GrExpr_Call (nmAliasRepl _lhsInmAliasMp nm_) _argLItrf) of
                   { _lhsOtrf ->
                   ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _altLOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _altLOmkNewNm ->
             (case (_lhsInmAliasMp) of
              { _valOnmAliasMp ->
              (case (altL_ _altLOmkNewNm _altLOnmAliasMp ) of
               { ( _altLItrf) ->
                   (case (_lhsImkNewNm) of
                    { _valOmkNewNm ->
                    (case (val_ _valOmkNewNm _valOnmAliasMp ) of
                     { ( _valInmAlias,_valItrf) ->
                         (case (GrExpr_Case _valItrf _altLItrf) of
                          { _trf ->
                          (case (_trf) of
                           { _lhsOtrf ->
                           ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }))
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _handlerOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _handlerOmkNewNm ->
             (case (_lhsInmAliasMp) of
              { _bodyOnmAliasMp ->
              (case (_lhsImkNewNm) of
               { _bodyOmkNewNm ->
               (case (handler_ _handlerOmkNewNm _handlerOnmAliasMp ) of
                { ( _handlerIhasSideEffect,_handlerInmAlias,_handlerItrf) ->
                    (case (body_ _bodyOmkNewNm _bodyOnmAliasMp ) of
                     { ( _bodyIhasSideEffect,_bodyInmAlias,_bodyItrf) ->
                         (case (GrExpr_Catch _bodyItrf (nmAliasRepl _lhsInmAliasMp arg_) _handlerItrf) of
                          { _lhsOtrf ->
                          ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (True) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_Eval nm_) of
           { _lhsOnmAlias ->
           (case (GrExpr_Eval $ nmAliasRepl _lhsInmAliasMp nm_) of
            { _lhsOtrf ->
            ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _argLOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _argLOmkNewNm ->
             (case (argL_ _argLOmkNewNm _argLOnmAliasMp ) of
              { ( _argLInmAliasL,_argLItrf) ->
                  (case (GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf) of
                   { _lhsOtrf ->
                   ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (GrExpr_FetchField (nmAliasRepl _lhsInmAliasMp nm_) offset_ mbTag_) of
            { _lhsOtrf ->
            ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (GrExpr_FetchNode  (nmAliasRepl _lhsInmAliasMp nm_)) of
            { _lhsOtrf ->
            ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (GrExpr_FetchUpdate (nmAliasRepl _lhsInmAliasMp src_) (nmAliasRepl _lhsInmAliasMp dst_)) of
            { _lhsOtrf ->
            ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (_lhsImkNewNm) of
           { _bodyOmkNewNm ->
           (case (pat_ ) of
            { ( _patIintroNmL,pat_1) ->
                (case (mkNmAliasMp [(n,_lhsImkNewNm n) | n <- _patIintroNmL]) of
                 { _patAliasMp ->
                 (case (_patAliasMp `Map.union` _lhsInmAliasMp) of
                  { _newAliasMp ->
                  (case (_newAliasMp) of
                   { _bodyOnmAliasMp ->
                   (case (body_ _bodyOmkNewNm _bodyOnmAliasMp ) of
                    { ( _bodyIhasSideEffect,_bodyInmAlias,_bodyItrf) ->
                        (case (_lhsInmAliasMp) of
                         { _exprOnmAliasMp ->
                         (case (_lhsImkNewNm) of
                          { _exprOmkNewNm ->
                          (case (expr_ _exprOmkNewNm _exprOnmAliasMp ) of
                           { ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) ->
                               (case (case (_bodyInmAlias,_exprIhasSideEffect) of
                                        (NmAlias_Nm n,True) -> NmAlias_NmAfterSideEffect n
                                        _ -> _bodyInmAlias) of
                                { _lhsOnmAlias ->
                                (case (_newAliasMp) of
                                 { _patOnmAliasMp ->
                                 (case (_lhsImkNewNm) of
                                  { _patOmkNewNm ->
                                  (case (pat_1 _patOmkNewNm _patOnmAliasMp ) of
                                   { ( _patInmAlias,_patItrf) ->
                                       (case (GrExpr_Seq _exprItrf _patItrf _bodyItrf) of
                                        { _trf ->
                                        (case (_trf) of
                                         { _lhsOtrf ->
                                         ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (_lhsInmAliasMp) of
           { _valOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _valOmkNewNm ->
            (case (val_ _valOmkNewNm _valOnmAliasMp ) of
             { ( _valInmAlias,_valItrf) ->
                 (case (_valInmAlias) of
                  { _lhsOnmAlias ->
                  (case (GrExpr_Store _valItrf) of
                   { _trf ->
                   (case (_trf) of
                    { _lhsOtrf ->
                    ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }))
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (GrExpr_Throw (nmAliasRepl _lhsInmAliasMp nm_)) of
            { _lhsOtrf ->
            ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (_lhsInmAliasMp) of
           { _valOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _valOmkNewNm ->
            (case (val_ _valOmkNewNm _valOnmAliasMp ) of
             { ( _valInmAlias,_valItrf) ->
                 (case (_valInmAlias) of
                  { _lhsOnmAlias ->
                  (case (_lhsInmAliasMp) of
                   { _typeOnmAliasMp ->
                   (case (_lhsImkNewNm) of
                    { _typeOmkNewNm ->
                    (case (type_ _typeOmkNewNm _typeOnmAliasMp ) of
                     { ( _typeItrf) ->
                         (case (GrExpr_Unit _valItrf _typeItrf) of
                          { _trf ->
                          (case (_trf) of
                           { _lhsOtrf ->
                           ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }))
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (False) of
          { _lhsOhasSideEffect ->
          (case (NmAlias_None) of
           { _lhsOnmAlias ->
           (case (_lhsInmAliasMp) of
            { _valOnmAliasMp ->
            (case (_lhsImkNewNm) of
             { _valOmkNewNm ->
             (case (val_ _valOmkNewNm _valOnmAliasMp ) of
              { ( _valInmAlias,_valItrf) ->
                  (case (GrExpr_UpdateUnit (nmAliasRepl _lhsInmAliasMp nm_) _valItrf) of
                   { _lhsOtrf ->
                   ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
-- GrFFIAnnot --------------------------------------------------
{-
   alternatives:
      alternative IsResEval:
         child isEvaluated    : {Bool}
-}
-- cata
sem_GrFFIAnnot :: GrFFIAnnot  ->
                  T_GrFFIAnnot 
sem_GrFFIAnnot (GrFFIAnnot_IsResEval _isEvaluated )  =
    (sem_GrFFIAnnot_IsResEval _isEvaluated )
-- semantic domain
type T_GrFFIAnnot  = ( )
sem_GrFFIAnnot_IsResEval :: Bool ->
                            T_GrFFIAnnot 
sem_GrFFIAnnot_IsResEval isEvaluated_  =
    ( )
-- GrGlobal ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mkNewNm              : HsName -> HsName
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Global:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrGlobal :: GrGlobal  ->
                T_GrGlobal 
sem_GrGlobal (GrGlobal_Global _nm _val )  =
    (sem_GrGlobal_Global _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrGlobal  = (HsName -> HsName) ->
                   ( GrGlobal )
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (\ _lhsImkNewNm ->
         (case (Map.empty) of
          { _valOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _valOmkNewNm ->
           (case (val_ _valOmkNewNm _valOnmAliasMp ) of
            { ( _valInmAlias,_valItrf) ->
                (case (GrGlobal_Global nm_ _valItrf) of
                 { _trf ->
                 (case (_trf) of
                  { _lhsOtrf ->
                  ( _lhsOtrf) }) }) }) }) }))
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mkNewNm              : HsName -> HsName
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrGlobal 
         child tl             : GrGlobalL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrGlobalL :: GrGlobalL  ->
                 T_GrGlobalL 
sem_GrGlobalL list  =
    (Prelude.foldr sem_GrGlobalL_Cons sem_GrGlobalL_Nil (Prelude.map sem_GrGlobal list) )
-- semantic domain
type T_GrGlobalL  = (HsName -> HsName) ->
                    ( GrGlobalL )
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (\ _lhsImkNewNm ->
         (case (_lhsImkNewNm) of
          { _tlOmkNewNm ->
          (case (tl_ _tlOmkNewNm ) of
           { ( _tlItrf) ->
               (case (_lhsImkNewNm) of
                { _hdOmkNewNm ->
                (case (hd_ _hdOmkNewNm ) of
                 { ( _hdItrf) ->
                     (case ((:) _hdItrf _tlItrf) of
                      { _trf ->
                      (case (_trf) of
                       { _lhsOtrf ->
                       ( _lhsOtrf) }) }) }) }) }) }))
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (\ _lhsImkNewNm ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrModule ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mkNewNm              : HsName -> HsName
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
         visit 0:
            local trf         : _
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = (HsName -> HsName) ->
                   ( GrModule )
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (\ _lhsImkNewNm ->
         (case (_lhsImkNewNm) of
          { _bindLOmkNewNm ->
          (case (bindL_ _bindLOmkNewNm ) of
           { ( _bindLItrf) ->
               (case (_lhsImkNewNm) of
                { _globalLOmkNewNm ->
                (case (globalL_ _globalLOmkNewNm ) of
                 { ( _globalLItrf) ->
                     (case (GrModule_Mod moduleNm_ _globalLItrf _bindLItrf tagsMp_) of
                      { _trf ->
                      (case (_trf) of
                       { _lhsOtrf ->
                       ( _lhsOtrf) }) }) }) }) }) }))
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative LitInt:
         child int            : {Int}
         visit 1:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
      alternative Otherwise:
         visit 1:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 1:
            local trf         : _
-}
-- cata
sem_GrPatAlt :: GrPatAlt  ->
                T_GrPatAlt 
sem_GrPatAlt (GrPatAlt_LitInt _int )  =
    (sem_GrPatAlt_LitInt _int )
sem_GrPatAlt (GrPatAlt_Node _tag _fldL )  =
    (sem_GrPatAlt_Node (sem_GrTag _tag ) _fldL )
sem_GrPatAlt (GrPatAlt_NodeSplit _tag _nm _fldL )  =
    (sem_GrPatAlt_NodeSplit (sem_GrTag _tag ) _nm (sem_GrSplitL _fldL ) )
sem_GrPatAlt (GrPatAlt_Otherwise )  =
    (sem_GrPatAlt_Otherwise )
sem_GrPatAlt (GrPatAlt_Tag _tag )  =
    (sem_GrPatAlt_Tag (sem_GrTag _tag ) )
-- semantic domain
type T_GrPatAlt  = ( ([HsName]),T_GrPatAlt_1 )
type T_GrPatAlt_1  = (HsName -> HsName) ->
                     NmAliasMp ->
                     ( NmAlias,GrPatAlt )
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_LitInt_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_LitInt_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatAlt_LitInt int_) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOnmAlias,_lhsOtrf) }) }) }))
             in  sem_GrPatAlt_LitInt_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (case (fldL_) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Node_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Node_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_Grp hsnUnknown $ map NmAlias_Nm fldL_) of
                           { _lhsOnmAlias ->
                           (case (_lhsInmAliasMp) of
                            { _tagOnmAliasMp ->
                            (case (_lhsImkNewNm) of
                             { _tagOmkNewNm ->
                             (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
                              { ( _tagItrf) ->
                                  (case (GrPatAlt_Node _tagItrf (map (nmAliasRepl _lhsInmAliasMp) fldL_)) of
                                   { _lhsOtrf ->
                                   ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }))
             in  sem_GrPatAlt_Node_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIintroNmL,fldL_1) ->
         (case (nm_ : _fldLIintroNmL) of
          { _lhsOintroNmL ->
          (case ((let sem_GrPatAlt_NodeSplit_1 :: T_GrPatAlt_1 
                      sem_GrPatAlt_NodeSplit_1  =
                          (\ _lhsImkNewNm
                             _lhsInmAliasMp ->
                               (case (NmAlias_None) of
                                { _lhsOnmAlias ->
                                (case (_lhsInmAliasMp) of
                                 { _fldLOnmAliasMp ->
                                 (case (_lhsImkNewNm) of
                                  { _fldLOmkNewNm ->
                                  (case (fldL_1 _fldLOmkNewNm _fldLOnmAliasMp ) of
                                   { ( _fldLItrf) ->
                                       (case (_lhsInmAliasMp) of
                                        { _tagOnmAliasMp ->
                                        (case (_lhsImkNewNm) of
                                         { _tagOmkNewNm ->
                                         (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
                                          { ( _tagItrf) ->
                                              (case (GrPatAlt_NodeSplit _tagItrf (nmAliasRepl _lhsInmAliasMp nm_) _fldLItrf) of
                                               { _lhsOtrf ->
                                               ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }))
                  in  sem_GrPatAlt_NodeSplit_1)) of
           { ( sem_GrPatAlt_1) ->
           ( _lhsOintroNmL,sem_GrPatAlt_1) }) }) })
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Otherwise_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Otherwise_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatAlt_Otherwise) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOnmAlias,_lhsOtrf) }) }) }))
             in  sem_GrPatAlt_Otherwise_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Tag_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Tag_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (_lhsInmAliasMp) of
                            { _tagOnmAliasMp ->
                            (case (_lhsImkNewNm) of
                             { _tagOmkNewNm ->
                             (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
                              { ( _tagItrf) ->
                                  (case (GrPatAlt_Tag _tagItrf) of
                                   { _trf ->
                                   (case (_trf) of
                                    { _lhsOtrf ->
                                    ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
             in  sem_GrPatAlt_Tag_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative Empty:
         visit 1:
            local trf         : _
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative EnumNode:
         child nm             : {HsName}
      alternative OpaqueAnnot:
         child nm             : {HsName}
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrVarL 
         visit 1:
            local trf         : _
-}
-- cata
sem_GrPatLam :: GrPatLam  ->
                T_GrPatLam 
sem_GrPatLam (GrPatLam_BasicAnnot _annot _nm )  =
    (sem_GrPatLam_BasicAnnot _annot _nm )
sem_GrPatLam (GrPatLam_BasicNode _annot _nm )  =
    (sem_GrPatLam_BasicNode _annot _nm )
sem_GrPatLam (GrPatLam_Empty )  =
    (sem_GrPatLam_Empty )
sem_GrPatLam (GrPatLam_EnumAnnot _tycon _nm )  =
    (sem_GrPatLam_EnumAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_EnumNode _nm )  =
    (sem_GrPatLam_EnumNode _nm )
sem_GrPatLam (GrPatLam_OpaqueAnnot _nm )  =
    (sem_GrPatLam_OpaqueAnnot _nm )
sem_GrPatLam (GrPatLam_OpaqueNode _nm )  =
    (sem_GrPatLam_OpaqueNode _nm )
sem_GrPatLam (GrPatLam_PtrAnnot _tycon _nm )  =
    (sem_GrPatLam_PtrAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_PtrNode _nm )  =
    (sem_GrPatLam_PtrNode _nm )
sem_GrPatLam (GrPatLam_Var _nm )  =
    (sem_GrPatLam_Var _nm )
sem_GrPatLam (GrPatLam_VarNode _fldL )  =
    (sem_GrPatLam_VarNode (sem_GrVarL _fldL ) )
-- semantic domain
type T_GrPatLam  = ( ([HsName]),T_GrPatLam_1 )
type T_GrPatLam_1  = (HsName -> HsName) ->
                     NmAliasMp ->
                     ( NmAlias,GrPatLam )
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_BasicAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_BasicAnnot_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (case annot_ of
                                   BasicAnnot_Size bsz _ _ _
                                     | basicSizeIsWord bsz  -> NmAlias_Nm nm_
                                   _                        -> NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_BasicAnnot annot_ (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_BasicAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_BasicNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_BasicNode_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) annot_) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_BasicNode annot_ (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_BasicNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_Empty_1 :: T_GrPatLam_1 
                 sem_GrPatLam_Empty_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_Empty) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOnmAlias,_lhsOtrf) }) }) }))
             in  sem_GrPatLam_Empty_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_EnumAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_EnumAnnot_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_EnumAnnot (nmAliasRepl _lhsInmAliasMp tycon_) (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_EnumAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_EnumNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_EnumNode_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_EnumNode (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_EnumNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_OpaqueAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_OpaqueAnnot_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_OpaqueAnnot (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_OpaqueAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_OpaqueNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_OpaqueNode_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_OpaqueNode (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_OpaqueNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_PtrAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_PtrAnnot_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_PtrAnnot (nmAliasRepl _lhsInmAliasMp tycon_) (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_PtrAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_PtrNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_PtrNode_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_None) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_PtrNode (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_PtrNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_Var_1 :: T_GrPatLam_1 
                 sem_GrPatLam_Var_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (NmAlias_Nm    nm_) of
                           { _lhsOnmAlias ->
                           (case (GrPatLam_Var (nmAliasRepl _lhsInmAliasMp nm_)) of
                            { _lhsOtrf ->
                            ( _lhsOnmAlias,_lhsOtrf) }) }))
             in  sem_GrPatLam_Var_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLIintroNmL,fldL_1) ->
         (case (tail _fldLIintroNmL) of
          { _lhsOintroNmL ->
          (case ((let sem_GrPatLam_VarNode_1 :: T_GrPatLam_1 
                      sem_GrPatLam_VarNode_1  =
                          (\ _lhsImkNewNm
                             _lhsInmAliasMp ->
                               (case (NmAlias_Grp   hsnUnknown $ map NmAlias_Nm (tail _fldLIintroNmL)) of
                                { _lhsOnmAlias ->
                                (case (_lhsInmAliasMp) of
                                 { _fldLOnmAliasMp ->
                                 (case (_lhsImkNewNm) of
                                  { _fldLOmkNewNm ->
                                  (case (fldL_1 _fldLOmkNewNm _fldLOnmAliasMp ) of
                                   { ( _fldLItrf) ->
                                       (case (GrPatLam_VarNode _fldLItrf) of
                                        { _trf ->
                                        (case (_trf) of
                                         { _lhsOtrf ->
                                         ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
                  in  sem_GrPatLam_VarNode_1)) of
           { ( sem_GrPatLam_1) ->
           ( _lhsOintroNmL,sem_GrPatLam_1) }) }) })
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = ( ([HsName]),T_GrSplit_1 )
type T_GrSplit_1  = (HsName -> HsName) ->
                    NmAliasMp ->
                    ( GrSplit )
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrSplit_Sel_1 :: T_GrSplit_1 
                 sem_GrSplit_Sel_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (_lhsInmAliasMp) of
                           { _offOnmAliasMp ->
                           (case (_lhsImkNewNm) of
                            { _offOmkNewNm ->
                            (case (off_ _offOmkNewNm _offOnmAliasMp ) of
                             { ( _offInmAlias,_offItrf) ->
                                 (case (GrSplit_Sel (nmAliasRepl _lhsInmAliasMp nm_) _offItrf) of
                                  { _lhsOtrf ->
                                  ( _lhsOtrf) }) }) }) }))
             in  sem_GrSplit_Sel_1)) of
      { ( sem_GrSplit_1) ->
      ( _lhsOintroNmL,sem_GrSplit_1) }) })
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ( ([HsName]),T_GrSplitL_1 )
type T_GrSplitL_1  = (HsName -> HsName) ->
                     NmAliasMp ->
                     ( GrSplitL )
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIintroNmL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIintroNmL,hd_1) ->
              (case (_hdIintroNmL ++ _tlIintroNmL) of
               { _lhsOintroNmL ->
               (case ((let sem_GrSplitL_Cons_1 :: T_GrSplitL_1 
                           sem_GrSplitL_Cons_1  =
                               (\ _lhsImkNewNm
                                  _lhsInmAliasMp ->
                                    (case (_lhsInmAliasMp) of
                                     { _tlOnmAliasMp ->
                                     (case (_lhsInmAliasMp) of
                                      { _hdOnmAliasMp ->
                                      (case (_lhsImkNewNm) of
                                       { _tlOmkNewNm ->
                                       (case (tl_1 _tlOmkNewNm _tlOnmAliasMp ) of
                                        { ( _tlItrf) ->
                                            (case (_lhsImkNewNm) of
                                             { _hdOmkNewNm ->
                                             (case (hd_1 _hdOmkNewNm _hdOnmAliasMp ) of
                                              { ( _hdItrf) ->
                                                  (case ((:) _hdItrf _tlItrf) of
                                                   { _trf ->
                                                   (case (_trf) of
                                                    { _lhsOtrf ->
                                                    ( _lhsOtrf) }) }) }) }) }) }) }) }))
                       in  sem_GrSplitL_Cons_1)) of
                { ( sem_GrSplitL_1) ->
                ( _lhsOintroNmL,sem_GrSplitL_1) }) }) }) })
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrSplitL_Nil_1 :: T_GrSplitL_1 
                 sem_GrSplitL_Nil_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case ([]) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrSplitL_Nil_1)) of
      { ( sem_GrSplitL_1) ->
      ( _lhsOintroNmL,sem_GrSplitL_1) }) })
-- GrTag -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative App:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Con:
         child grtgAnn        : {GrTagAnn}
         child int            : {Int}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Fun:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Hole:
         visit 0:
            local trf         : _
      alternative PApp:
         child needs          : {Int}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Rec:
         visit 0:
            local trf         : _
      alternative Unboxed:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTag :: GrTag  ->
             T_GrTag 
sem_GrTag (GrTag_App _nm )  =
    (sem_GrTag_App _nm )
sem_GrTag (GrTag_Con _grtgAnn _int _nm )  =
    (sem_GrTag_Con _grtgAnn _int _nm )
sem_GrTag (GrTag_Fun _nm )  =
    (sem_GrTag_Fun _nm )
sem_GrTag (GrTag_Hole )  =
    (sem_GrTag_Hole )
sem_GrTag (GrTag_PApp _needs _nm )  =
    (sem_GrTag_PApp _needs _nm )
sem_GrTag (GrTag_Rec )  =
    (sem_GrTag_Rec )
sem_GrTag (GrTag_Unboxed )  =
    (sem_GrTag_Unboxed )
-- semantic domain
type T_GrTag  = (HsName -> HsName) ->
                NmAliasMp ->
                ( GrTag )
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_App nm_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_Con grtgAnn_ int_ nm_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_Fun nm_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_Hole) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_PApp needs_ nm_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_Rec) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTag_Unboxed) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrTagL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrTag 
         child tl             : GrTagL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTagL :: GrTagL  ->
              T_GrTagL 
sem_GrTagL list  =
    (Prelude.foldr sem_GrTagL_Cons sem_GrTagL_Nil (Prelude.map sem_GrTag list) )
-- semantic domain
type T_GrTagL  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( GrTagL )
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _tlOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _tlOmkNewNm ->
           (case (tl_ _tlOmkNewNm _tlOnmAliasMp ) of
            { ( _tlItrf) ->
                (case (_lhsInmAliasMp) of
                 { _hdOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _hdOmkNewNm ->
                  (case (hd_ _hdOmkNewNm _hdOnmAliasMp ) of
                   { ( _hdItrf) ->
                       (case ((:) _hdItrf _tlItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrType ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Arrow:
         child args           : GrTypeBaseL 
         child res            : GrTypeBase 
         visit 0:
            local trf         : _
      alternative None:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrType :: GrType  ->
              T_GrType 
sem_GrType (GrType_Arrow _args _res )  =
    (sem_GrType_Arrow (sem_GrTypeBaseL _args ) (sem_GrTypeBase _res ) )
sem_GrType (GrType_None )  =
    (sem_GrType_None )
-- semantic domain
type T_GrType  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( GrType )
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _resOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _resOmkNewNm ->
           (case (res_ _resOmkNewNm _resOnmAliasMp ) of
            { ( _resItrf) ->
                (case (_lhsInmAliasMp) of
                 { _argsOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _argsOmkNewNm ->
                  (case (args_ _argsOmkNewNm _argsOnmAliasMp ) of
                   { ( _argsItrf) ->
                       (case (GrType_Arrow _argsItrf _resItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrType_None) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Node:
         visit 0:
            local trf         : _
      alternative Pointer:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTypeBase :: GrTypeBase  ->
                  T_GrTypeBase 
sem_GrTypeBase (GrTypeBase_Node )  =
    (sem_GrTypeBase_Node )
sem_GrTypeBase (GrTypeBase_Pointer )  =
    (sem_GrTypeBase_Pointer )
-- semantic domain
type T_GrTypeBase  = (HsName -> HsName) ->
                     NmAliasMp ->
                     ( GrTypeBase )
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTypeBase_Node) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (GrTypeBase_Pointer) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrTypeBase 
         child tl             : GrTypeBaseL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTypeBaseL :: GrTypeBaseL  ->
                   T_GrTypeBaseL 
sem_GrTypeBaseL list  =
    (Prelude.foldr sem_GrTypeBaseL_Cons sem_GrTypeBaseL_Nil (Prelude.map sem_GrTypeBase list) )
-- semantic domain
type T_GrTypeBaseL  = (HsName -> HsName) ->
                      NmAliasMp ->
                      ( GrTypeBaseL )
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _tlOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _tlOmkNewNm ->
           (case (tl_ _tlOmkNewNm _tlOnmAliasMp ) of
            { ( _tlItrf) ->
                (case (_lhsInmAliasMp) of
                 { _hdOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _hdOmkNewNm ->
                  (case (hd_ _hdOmkNewNm _hdOnmAliasMp ) of
                   { ( _hdItrf) ->
                       (case ((:) _hdItrf _tlItrf) of
                        { _trf ->
                        (case (_trf) of
                         { _lhsOtrf ->
                         ( _lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case ([]) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }))
-- GrVal -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
      alternative Empty:
         visit 0:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
      alternative LitInt:
         child int            : {Int}
         visit 0:
            local trf         : _
      alternative LitStr:
         child str            : {String}
         visit 0:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
         visit 0:
            local trf         : _
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Tag:
         child tag            : GrTag 
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrValL 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrVal :: GrVal  ->
             T_GrVal 
sem_GrVal (GrVal_BasicNode _tag _nm )  =
    (sem_GrVal_BasicNode (sem_GrTag _tag ) _nm )
sem_GrVal (GrVal_Empty )  =
    (sem_GrVal_Empty )
sem_GrVal (GrVal_EnumNode _nm )  =
    (sem_GrVal_EnumNode _nm )
sem_GrVal (GrVal_LitInt _int )  =
    (sem_GrVal_LitInt _int )
sem_GrVal (GrVal_LitStr _str )  =
    (sem_GrVal_LitStr _str )
sem_GrVal (GrVal_Node _tag _fldL )  =
    (sem_GrVal_Node (sem_GrTag _tag ) (sem_GrValL _fldL ) )
sem_GrVal (GrVal_NodeAdapt _nm _fldL )  =
    (sem_GrVal_NodeAdapt _nm (sem_GrAdaptL _fldL ) )
sem_GrVal (GrVal_OpaqueNode _nm )  =
    (sem_GrVal_OpaqueNode _nm )
sem_GrVal (GrVal_PtrNode _nm )  =
    (sem_GrVal_PtrNode _nm )
sem_GrVal (GrVal_Tag _tag )  =
    (sem_GrVal_Tag (sem_GrTag _tag ) )
sem_GrVal (GrVal_Var _nm )  =
    (sem_GrVal_Var _nm )
sem_GrVal (GrVal_VarNode _fldL )  =
    (sem_GrVal_VarNode (sem_GrValL _fldL ) )
-- semantic domain
type T_GrVal  = (HsName -> HsName) ->
                NmAliasMp ->
                ( NmAlias,GrVal )
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) BasicAnnot_Dflt) of
          { _lhsOnmAlias ->
          (case (_lhsInmAliasMp) of
           { _tagOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _tagOmkNewNm ->
            (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
             { ( _tagItrf) ->
                 (case (GrVal_BasicNode _tagItrf (nmAliasRepl _lhsInmAliasMp nm_)) of
                  { _lhsOtrf ->
                  ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }))
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (GrVal_Empty) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (GrVal_EnumNode (nmAliasRepl _lhsInmAliasMp nm_)) of
           { _lhsOtrf ->
           ( _lhsOnmAlias,_lhsOtrf) }) }))
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_Const hsnUnknown (GrVal_LitInt int_)) of
          { _lhsOnmAlias ->
          (case (GrVal_LitInt int_) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_Const hsnUnknown (GrVal_LitStr str_)) of
          { _lhsOnmAlias ->
          (case (GrVal_LitStr str_) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOnmAlias,_lhsOtrf) }) }) }))
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _fldLOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _fldLOmkNewNm ->
           (case (fldL_ _fldLOmkNewNm _fldLOnmAliasMp ) of
            { ( _fldLInmAliasL,_fldLItrf) ->
                (case (_lhsInmAliasMp) of
                 { _tagOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _tagOmkNewNm ->
                  (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
                   { ( _tagItrf) ->
                       (case (case _tagItrf of
                                GrTag_Con _ _ _
                                  -> NmAlias_Grp hsnUnknown _fldLInmAliasL
                                _ -> NmAlias_None) of
                        { _lhsOnmAlias ->
                        (case (GrVal_Node _tagItrf _fldLItrf) of
                         { _trf ->
                         (case (_trf) of
                          { _lhsOtrf ->
                          ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (_lhsInmAliasMp) of
           { _fldLOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _fldLOmkNewNm ->
            (case (fldL_ _fldLOmkNewNm _fldLOnmAliasMp ) of
             { ( _fldLItrf) ->
                 (case (GrVal_NodeAdapt (nmAliasRepl _lhsInmAliasMp nm_) _fldLItrf) of
                  { _lhsOtrf ->
                  ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }))
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (GrVal_OpaqueNode (nmAliasRepl _lhsInmAliasMp nm_)) of
           { _lhsOtrf ->
           ( _lhsOnmAlias,_lhsOtrf) }) }))
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (GrVal_PtrNode (nmAliasRepl _lhsInmAliasMp nm_)) of
           { _lhsOtrf ->
           ( _lhsOnmAlias,_lhsOtrf) }) }))
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (_lhsInmAliasMp) of
           { _tagOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _tagOmkNewNm ->
            (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
             { ( _tagItrf) ->
                 (case (GrVal_Tag _tagItrf) of
                  { _trf ->
                  (case (_trf) of
                   { _lhsOtrf ->
                   ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_Nm nm_) of
          { _lhsOnmAlias ->
          (case (GrVal_Var $ nmAliasRepl _lhsInmAliasMp nm_) of
           { _lhsOtrf ->
           ( _lhsOnmAlias,_lhsOtrf) }) }))
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (NmAlias_None) of
          { _lhsOnmAlias ->
          (case (_lhsInmAliasMp) of
           { _fldLOnmAliasMp ->
           (case (_lhsImkNewNm) of
            { _fldLOmkNewNm ->
            (case (fldL_ _fldLOmkNewNm _fldLOnmAliasMp ) of
             { ( _fldLInmAliasL,_fldLItrf) ->
                 (case (GrVal_VarNode _fldLItrf) of
                  { _trf ->
                  (case (_trf) of
                   { _lhsOtrf ->
                   ( _lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }))
-- GrValL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         nmAliasL             : [NmAlias]
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( ([NmAlias]),GrValL )
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case (_lhsInmAliasMp) of
          { _tlOnmAliasMp ->
          (case (_lhsImkNewNm) of
           { _tlOmkNewNm ->
           (case (tl_ _tlOmkNewNm _tlOnmAliasMp ) of
            { ( _tlInmAliasL,_tlItrf) ->
                (case (_lhsInmAliasMp) of
                 { _hdOnmAliasMp ->
                 (case (_lhsImkNewNm) of
                  { _hdOmkNewNm ->
                  (case (hd_ _hdOmkNewNm _hdOnmAliasMp ) of
                   { ( _hdInmAlias,_hdItrf) ->
                       (case (_hdInmAlias : _tlInmAliasL) of
                        { _lhsOnmAliasL ->
                        (case ((:) _hdItrf _tlItrf) of
                         { _trf ->
                         (case (_trf) of
                          { _lhsOtrf ->
                          ( _lhsOnmAliasL,_lhsOtrf) }) }) }) }) }) }) }) }) }))
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (case ([]) of
          { _lhsOnmAliasL ->
          (case ([]) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOnmAliasL,_lhsOtrf) }) }) }))
-- GrVar -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Ignore:
         visit 1:
            local trf         : _
      alternative KnownTag:
         child tag            : GrTag 
         visit 1:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_GrVar :: GrVar  ->
             T_GrVar 
sem_GrVar (GrVar_Ignore )  =
    (sem_GrVar_Ignore )
sem_GrVar (GrVar_KnownTag _tag )  =
    (sem_GrVar_KnownTag (sem_GrTag _tag ) )
sem_GrVar (GrVar_Var _nm )  =
    (sem_GrVar_Var _nm )
-- semantic domain
type T_GrVar  = ( ([HsName]),T_GrVar_1 )
type T_GrVar_1  = (HsName -> HsName) ->
                  NmAliasMp ->
                  ( GrVar )
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (case ([ ]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_Ignore_1 :: T_GrVar_1 
                 sem_GrVar_Ignore_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (GrVar_Ignore) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrVar_Ignore_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (case ([ error "introNmL known tag" ]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_KnownTag_1 :: T_GrVar_1 
                 sem_GrVar_KnownTag_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (_lhsInmAliasMp) of
                           { _tagOnmAliasMp ->
                           (case (_lhsImkNewNm) of
                            { _tagOmkNewNm ->
                            (case (tag_ _tagOmkNewNm _tagOnmAliasMp ) of
                             { ( _tagItrf) ->
                                 (case (GrVar_KnownTag _tagItrf) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOtrf) }) }) }) }) }))
             in  sem_GrVar_KnownTag_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_Var_1 :: T_GrVar_1 
                 sem_GrVar_Var_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case (GrVar_Var $ nmAliasRepl _lhsInmAliasMp nm_) of
                           { _lhsOtrf ->
                           ( _lhsOtrf) }))
             in  sem_GrVar_Var_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ( ([HsName]),T_GrVarL_1 )
type T_GrVarL_1  = (HsName -> HsName) ->
                   NmAliasMp ->
                   ( GrVarL )
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIintroNmL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIintroNmL,hd_1) ->
              (case (_hdIintroNmL ++ _tlIintroNmL) of
               { _lhsOintroNmL ->
               (case ((let sem_GrVarL_Cons_1 :: T_GrVarL_1 
                           sem_GrVarL_Cons_1  =
                               (\ _lhsImkNewNm
                                  _lhsInmAliasMp ->
                                    (case (_lhsInmAliasMp) of
                                     { _tlOnmAliasMp ->
                                     (case (_lhsInmAliasMp) of
                                      { _hdOnmAliasMp ->
                                      (case (_lhsImkNewNm) of
                                       { _tlOmkNewNm ->
                                       (case (tl_1 _tlOmkNewNm _tlOnmAliasMp ) of
                                        { ( _tlItrf) ->
                                            (case (_lhsImkNewNm) of
                                             { _hdOmkNewNm ->
                                             (case (hd_1 _hdOmkNewNm _hdOnmAliasMp ) of
                                              { ( _hdItrf) ->
                                                  (case ((:) _hdItrf _tlItrf) of
                                                   { _trf ->
                                                   (case (_trf) of
                                                    { _lhsOtrf ->
                                                    ( _lhsOtrf) }) }) }) }) }) }) }) }))
                       in  sem_GrVarL_Cons_1)) of
                { ( sem_GrVarL_1) ->
                ( _lhsOintroNmL,sem_GrVarL_1) }) }) }) })
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVarL_Nil_1 :: T_GrVarL_1 
                 sem_GrVarL_Nil_1  =
                     (\ _lhsImkNewNm
                        _lhsInmAliasMp ->
                          (case ([]) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrVarL_Nil_1)) of
      { ( sem_GrVarL_1) ->
      ( _lhsOintroNmL,sem_GrVarL_1) }) })