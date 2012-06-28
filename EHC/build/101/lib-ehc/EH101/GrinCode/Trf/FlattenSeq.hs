

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Trf/FlattenSeq.ag)
module EH101.GrinCode.Trf.FlattenSeq(grFlattenSeq) where

import qualified Data.Map as Map
import qualified EH.Util.FastSeq as FastSeq
import EH101.Base.Common
import EH101.GrinCode.Common
import EH101.GrinCode









grFlattenSeq :: GrModule -> GrModule
grFlattenSeq grmod
  = trf_Syn_GrAGItf t
  where t = wrap_GrAGItf (sem_GrAGItf $ GrAGItf_AGItf grmod)
            $ Inh_GrAGItf

-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : GrModule 
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
type T_GrAGItf  = ( GrModule )
data Inh_GrAGItf  = Inh_GrAGItf {}
data Syn_GrAGItf  = Syn_GrAGItf {trf_Syn_GrAGItf :: !(GrModule )}
wrap_GrAGItf :: T_GrAGItf  ->
                Inh_GrAGItf  ->
                Syn_GrAGItf 
wrap_GrAGItf sem (Inh_GrAGItf )  =
    (let ( _lhsOtrf) = sem 
     in  (Syn_GrAGItf _lhsOtrf ))
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleItrf) ->
         (case (_moduleItrf) of
          { _lhsOtrf ->
          ( _lhsOtrf) }) })
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
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
type T_GrAdapt  = ( GrAdapt )
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (case (off_ ) of
     { ( _offItrf) ->
         (case (GrAdapt_Del _offItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (case (val_ ) of
     { ( _valItrf) ->
         (case (off_ ) of
          { ( _offItrf) ->
              (case (GrAdapt_Ins _offItrf _valItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (case (val_ ) of
     { ( _valItrf) ->
         (case (off_ ) of
          { ( _offItrf) ->
              (case (GrAdapt_Upd _offItrf _valItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
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
type T_GrAdaptL  = ( GrAdaptL )
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = ( GrAlt )
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (case (expr_ ) of
     { ( _exprItrf,_exprItrfLast,_exprItrfPairs) ->
         (case (pat_ ) of
          { ( _patItrf) ->
              (case (GrAlt_Alt ann_ _patItrf _exprItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
-- GrAltL ------------------------------------------------------
{-
   visit 0:
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
type T_GrAltL  = ( GrAltL )
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrBind ------------------------------------------------------
{-
   visit 0:
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
type T_GrBind  = ( GrBind )
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (case (GrBind_Arity nm_ arity_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (case (expr_ ) of
     { ( _exprItrf,_exprItrfLast,_exprItrfPairs) ->
         (case (GrBind_Bind nm_ annot_ argNmL_ _exprItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (case (bindL_ ) of
     { ( _bindLItrf) ->
         (case (GrBind_Rec _bindLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrBindL -----------------------------------------------------
{-
   visit 0:
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
type T_GrBindL  = ( GrBindL )
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         trf                  : SELF 
         trfLast              : GrExpr 
         trfPairs             : FastSeq.FastSeq (GrExpr,GrPatLam)
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local trf         : _
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local trf         : _
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local trf         : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
         visit 0:
            local trf         : _
      alternative Eval:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local trf         : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
         visit 0:
            local trf         : _
      alternative FetchNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
         visit 0:
            local trf         : _
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
      alternative Store:
         child val            : GrVal 
         visit 0:
            local trf         : _
      alternative Throw:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local trf         : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local trf         : _
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
type T_GrExpr  = ( GrExpr ,GrExpr ,(FastSeq.FastSeq (GrExpr,GrPatLam)))
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (case (argL_ ) of
     { ( _argLItrf) ->
         (case (GrExpr_App nm_ _argLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           (case (_trf) of
            { _lhsOtrfLast ->
            (case (FastSeq.empty) of
             { _lhsOtrfPairs ->
             ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) })
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (case (argL_ ) of
     { ( _argLItrf) ->
         (case (GrExpr_Call nm_ _argLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           (case (_trf) of
            { _lhsOtrfLast ->
            (case (FastSeq.empty) of
             { _lhsOtrfPairs ->
             ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) })
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (case (altL_ ) of
     { ( _altLItrf) ->
         (case (val_ ) of
          { ( _valItrf) ->
              (case (GrExpr_Case _valItrf _altLItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                (case (_trf) of
                 { _lhsOtrfLast ->
                 (case (FastSeq.empty) of
                  { _lhsOtrfPairs ->
                  ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) }) })
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (case (handler_ ) of
     { ( _handlerItrf,_handlerItrfLast,_handlerItrfPairs) ->
         (case (body_ ) of
          { ( _bodyItrf,_bodyItrfLast,_bodyItrfPairs) ->
              (case (GrExpr_Catch _bodyItrf arg_ _handlerItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                (case (_trf) of
                 { _lhsOtrfLast ->
                 (case (FastSeq.empty) of
                  { _lhsOtrfPairs ->
                  ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) }) })
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (case (GrExpr_Eval nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      (case (_trf) of
       { _lhsOtrfLast ->
       (case (FastSeq.empty) of
        { _lhsOtrfPairs ->
        ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) })
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (case (argL_ ) of
     { ( _argLItrf) ->
         (case (GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           (case (_trf) of
            { _lhsOtrfLast ->
            (case (FastSeq.empty) of
             { _lhsOtrfPairs ->
             ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) })
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (case (GrExpr_FetchField nm_ offset_ mbTag_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      (case (_trf) of
       { _lhsOtrfLast ->
       (case (FastSeq.empty) of
        { _lhsOtrfPairs ->
        ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) })
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (case (GrExpr_FetchNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      (case (_trf) of
       { _lhsOtrfLast ->
       (case (FastSeq.empty) of
        { _lhsOtrfPairs ->
        ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) })
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (case (GrExpr_FetchUpdate src_ dst_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      (case (_trf) of
       { _lhsOtrfLast ->
       (case (FastSeq.empty) of
        { _lhsOtrfPairs ->
        ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) })
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (case (body_ ) of
     { ( _bodyItrf,_bodyItrfLast,_bodyItrfPairs) ->
         (case (pat_ ) of
          { ( _patItrf) ->
              (case (expr_ ) of
               { ( _exprItrf,_exprItrfLast,_exprItrfPairs) ->
                   (case (foldr (\(e,p) b -> GrExpr_Seq e p b)
                                (GrExpr_Seq _exprItrfLast _patItrf _bodyItrf)
                                (FastSeq.toList _exprItrfPairs)) of
                    { _lhsOtrf ->
                    (case (_bodyItrfLast) of
                     { _lhsOtrfLast ->
                     (case (_exprItrfPairs
                            FastSeq.:++: FastSeq.singleton (_exprItrfLast,_patItrf)
                            FastSeq.:++: _bodyItrfPairs) of
                      { _lhsOtrfPairs ->
                      ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) }) })
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (case (val_ ) of
     { ( _valItrf) ->
         (case (GrExpr_Store _valItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           (case (_trf) of
            { _lhsOtrfLast ->
            (case (FastSeq.empty) of
             { _lhsOtrfPairs ->
             ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) })
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (case (GrExpr_Throw nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      (case (_trf) of
       { _lhsOtrfLast ->
       (case (FastSeq.empty) of
        { _lhsOtrfPairs ->
        ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) })
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (case (type_ ) of
     { ( _typeItrf) ->
         (case (val_ ) of
          { ( _valItrf) ->
              (case (GrExpr_Unit _valItrf _typeItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                (case (_trf) of
                 { _lhsOtrfLast ->
                 (case (FastSeq.empty) of
                  { _lhsOtrfPairs ->
                  ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) }) })
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (case (val_ ) of
     { ( _valItrf) ->
         (case (GrExpr_UpdateUnit nm_ _valItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           (case (_trf) of
            { _lhsOtrfLast ->
            (case (FastSeq.empty) of
             { _lhsOtrfPairs ->
             ( _lhsOtrf,_lhsOtrfLast,_lhsOtrfPairs) }) }) }) }) })
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
type T_GrGlobal  = ( GrGlobal )
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (case (val_ ) of
     { ( _valItrf) ->
         (case (GrGlobal_Global nm_ _valItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
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
type T_GrGlobalL  = ( GrGlobalL )
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrModule ----------------------------------------------------
{-
   visit 0:
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
type T_GrModule  = ( GrModule )
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (case (bindL_ ) of
     { ( _bindLItrf) ->
         (case (globalL_ ) of
          { ( _globalLItrf) ->
              (case (GrModule_Mod moduleNm_ _globalLItrf _bindLItrf tagsMp_) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative LitInt:
         child int            : {Int}
         visit 0:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
         visit 0:
            local trf         : _
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
         visit 0:
            local trf         : _
      alternative Otherwise:
         visit 0:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 0:
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
type T_GrPatAlt  = ( GrPatAlt )
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (case (GrPatAlt_LitInt int_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (case (tag_ ) of
     { ( _tagItrf) ->
         (case (GrPatAlt_Node _tagItrf fldL_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLItrf) ->
         (case (tag_ ) of
          { ( _tagItrf) ->
              (case (GrPatAlt_NodeSplit _tagItrf nm_ _fldLItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (case (GrPatAlt_Otherwise) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (case (tag_ ) of
     { ( _tagItrf) ->
         (case (GrPatAlt_Tag _tagItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Empty:
         visit 0:
            local trf         : _
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative OpaqueAnnot:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative VarNode:
         child fldL           : GrVarL 
         visit 0:
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
type T_GrPatLam  = ( GrPatLam )
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (case (GrPatLam_BasicAnnot annot_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (case (GrPatLam_BasicNode annot_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (case (GrPatLam_Empty) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (case (GrPatLam_EnumAnnot tycon_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (case (GrPatLam_EnumNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (case (GrPatLam_OpaqueAnnot nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (case (GrPatLam_OpaqueNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (case (GrPatLam_PtrAnnot tycon_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (case (GrPatLam_PtrNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (case (GrPatLam_Var nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLItrf) ->
         (case (GrPatLam_VarNode _fldLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = ( GrSplit )
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (case (off_ ) of
     { ( _offItrf) ->
         (case (GrSplit_Sel nm_ _offItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ( GrSplitL )
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrTag -------------------------------------------------------
{-
   visit 0:
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
type T_GrTag  = ( GrTag )
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (case (GrTag_App nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (case (GrTag_Con grtgAnn_ int_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (case (GrTag_Fun nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (case (GrTag_Hole) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (case (GrTag_PApp needs_ nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (case (GrTag_Rec) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (case (GrTag_Unboxed) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrTagL ------------------------------------------------------
{-
   visit 0:
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
type T_GrTagL  = ( GrTagL )
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrType ------------------------------------------------------
{-
   visit 0:
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
type T_GrType  = ( GrType )
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (case (res_ ) of
     { ( _resItrf) ->
         (case (args_ ) of
          { ( _argsItrf) ->
              (case (GrType_Arrow _argsItrf _resItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (case (GrType_None) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
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
type T_GrTypeBase  = ( GrTypeBase )
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (case (GrTypeBase_Node) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (case (GrTypeBase_Pointer) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
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
type T_GrTypeBaseL  = ( GrTypeBaseL )
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrVal -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Empty:
         visit 0:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
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
         visit 0:
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local trf         : _
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
type T_GrVal  = ( GrVal )
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (case (tag_ ) of
     { ( _tagItrf) ->
         (case (GrVal_BasicNode _tagItrf nm_) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (case (GrVal_Empty) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (case (GrVal_EnumNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (case (GrVal_LitInt int_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (case (GrVal_LitStr str_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLItrf) ->
         (case (tag_ ) of
          { ( _tagItrf) ->
              (case (GrVal_Node _tagItrf _fldLItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLItrf) ->
         (case (GrVal_NodeAdapt nm_ _fldLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (case (GrVal_OpaqueNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (case (GrVal_PtrNode nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (case (tag_ ) of
     { ( _tagItrf) ->
         (case (GrVal_Tag _tagItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (case (GrVal_Var nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLItrf) ->
         (case (GrVal_VarNode _fldLItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
-- GrValL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
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
type T_GrValL  = ( GrValL )
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrVar -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Ignore:
         visit 0:
            local trf         : _
      alternative KnownTag:
         child tag            : GrTag 
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local trf         : _
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
type T_GrVar  = ( GrVar )
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (case (GrVar_Ignore) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (case (tag_ ) of
     { ( _tagItrf) ->
         (case (GrVar_KnownTag _tagItrf) of
          { _trf ->
          (case (_trf) of
           { _lhsOtrf ->
           ( _lhsOtrf) }) }) })
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (case (GrVar_Var nm_) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ( GrVarL )
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItrf) ->
         (case (hd_ ) of
          { ( _hdItrf) ->
              (case ((:) _hdItrf _tlItrf) of
               { _trf ->
               (case (_trf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }) })
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (case ([]) of
     { _trf ->
     (case (_trf) of
      { _lhsOtrf ->
      ( _lhsOtrf) }) })