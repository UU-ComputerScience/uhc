

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/FreeVars.ag)
module EH101.GrinCode.FreeVars(grFreeVars) where

import qualified Data.Map as Map
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.GrinCode.Common
import EH101.GrinCode









grFreeVars :: GrExpr -> FvInfoMp
grFreeVars e
  = gathFviMp_Syn_GrExpr t
  where t = wrap_GrExpr (sem_GrExpr e)
            $ Inh_GrExpr

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
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Del:
         child off            : GrVal 
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
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
type T_GrAdapt  = ( FvInfoMp)
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (case (off_ ) of
     { ( _offIgathFviMp) ->
         (case (_offIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (off_ ) of
          { ( _offIgathFviMp) ->
              (case (_offIgathFviMp `fviMpUnion` _valIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (off_ ) of
          { ( _offIgathFviMp) ->
              (case (_offIgathFviMp `fviMpUnion` _valIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
      alternative Nil:
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = ( FvInfoMp)
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = ( FvInfoMp)
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (case (expr_ ) of
     { ( _exprIgathFviMp) ->
         (case (pat_ ) of
          { ( _patIgathFviMp,_patIintroNmL) ->
              (case (_exprIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
      alternative Nil:
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = ( FvInfoMp)
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrBind ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Arity:
         child nm             : {HsName}
         child arity          : {Int}
      alternative Bind:
         child nm             : {HsName}
         child annot          : {GrBindAnn}
         child argNmL         : {[HsName]}
         child expr           : GrExpr 
      alternative Rec:
         child bindL          : GrBindL 
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
type T_GrBind  = ( FvInfoMp)
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (case (expr_ ) of
     { ( _exprIgathFviMp) ->
         (case (_exprIgathFviMp `fviMpDifference` fviMpFromList argNmL_) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (case (bindL_ ) of
     { ( _bindLIgathFviMp) ->
         (case (_bindLIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrBind 
         child tl             : GrBindL 
      alternative Nil:
-}
-- cata
sem_GrBindL :: GrBindL  ->
               T_GrBindL 
sem_GrBindL list  =
    (Prelude.foldr sem_GrBindL_Cons sem_GrBindL_Nil (Prelude.map sem_GrBind list) )
-- semantic domain
type T_GrBindL  = ( FvInfoMp)
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local gathFviMp   : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
         visit 0:
            local gathFviMp   : _
      alternative Eval:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
         visit 0:
            local gathFviMp   : _
      alternative FetchNode:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
         visit 0:
            local gathFviMp   : _
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
         visit 0:
            local gathBodyFviMp : {FvInfoMp}
            local gathFviMp   : _
      alternative Store:
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
      alternative Throw:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local gathFviMp   : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
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
type T_GrExpr  = ( FvInfoMp)
data Inh_GrExpr  = Inh_GrExpr {}
data Syn_GrExpr  = Syn_GrExpr {gathFviMp_Syn_GrExpr :: !(FvInfoMp)}
wrap_GrExpr :: T_GrExpr  ->
               Inh_GrExpr  ->
               Syn_GrExpr 
wrap_GrExpr sem (Inh_GrExpr )  =
    (let ( _lhsOgathFviMp) = sem 
     in  (Syn_GrExpr _lhsOgathFviMp ))
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (case (argL_ ) of
     { ( _argLIgathFviMp) ->
         (case (fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (case (argL_ ) of
     { ( _argLIgathFviMp) ->
         (case (fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (case (altL_ ) of
     { ( _altLIgathFviMp) ->
         (case (val_ ) of
          { ( _valIgathFviMp) ->
              (case (fviMpUnions [_valIgathFviMp, _altLIgathFviMp]) of
               { _gathFviMp ->
               (case (_gathFviMp) of
                { _lhsOgathFviMp ->
                ( _lhsOgathFviMp) }) }) }) })
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (case (handler_ ) of
     { ( _handlerIgathFviMp) ->
         (case (body_ ) of
          { ( _bodyIgathFviMp) ->
              (case (fviMpUnions [fviMpSingleton arg_, _bodyIgathFviMp, _handlerIgathFviMp]) of
               { _gathFviMp ->
               (case (_gathFviMp) of
                { _lhsOgathFviMp ->
                ( _lhsOgathFviMp) }) }) }) })
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _gathFviMp ->
     (case (_gathFviMp) of
      { _lhsOgathFviMp ->
      ( _lhsOgathFviMp) }) })
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (case (argL_ ) of
     { ( _argLIgathFviMp) ->
         (case (_argLIgathFviMp) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _gathFviMp ->
     (case (_gathFviMp) of
      { _lhsOgathFviMp ->
      ( _lhsOgathFviMp) }) })
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _gathFviMp ->
     (case (_gathFviMp) of
      { _lhsOgathFviMp ->
      ( _lhsOgathFviMp) }) })
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (case (fviMpFromList [src_,dst_]) of
     { _gathFviMp ->
     (case (_gathFviMp) of
      { _lhsOgathFviMp ->
      ( _lhsOgathFviMp) }) })
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (case (body_ ) of
     { ( _bodyIgathFviMp) ->
         (case (pat_ ) of
          { ( _patIgathFviMp,_patIintroNmL) ->
              (case (_bodyIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL) of
               { _gathBodyFviMp ->
               (case (expr_ ) of
                { ( _exprIgathFviMp) ->
                    (case (fviMpUnions [_exprIgathFviMp, _gathBodyFviMp]) of
                     { _gathFviMp ->
                     (case (_gathFviMp) of
                      { _lhsOgathFviMp ->
                      ( _lhsOgathFviMp) }) }) }) }) }) })
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (_valIgathFviMp) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _gathFviMp ->
     (case (_gathFviMp) of
      { _lhsOgathFviMp ->
      ( _lhsOgathFviMp) }) })
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (_valIgathFviMp) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (fviMpUnions [fviMpSingleton nm_, _valIgathFviMp]) of
          { _gathFviMp ->
          (case (_gathFviMp) of
           { _lhsOgathFviMp ->
           ( _lhsOgathFviMp) }) }) })
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
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Global:
         child nm             : {HsName}
         child val            : GrVal 
-}
-- cata
sem_GrGlobal :: GrGlobal  ->
                T_GrGlobal 
sem_GrGlobal (GrGlobal_Global _nm _val )  =
    (sem_GrGlobal_Global _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrGlobal  = ( FvInfoMp)
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp) ->
         (case (_valIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrGlobal 
         child tl             : GrGlobalL 
      alternative Nil:
-}
-- cata
sem_GrGlobalL :: GrGlobalL  ->
                 T_GrGlobalL 
sem_GrGlobalL list  =
    (Prelude.foldr sem_GrGlobalL_Cons sem_GrGlobalL_Nil (Prelude.map sem_GrGlobal list) )
-- semantic domain
type T_GrGlobalL  = ( FvInfoMp)
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrModule ----------------------------------------------------
{-
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = ( )
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    ( )
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative LitInt:
         child int            : {Int}
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
      alternative Otherwise:
      alternative Tag:
         child tag            : GrTag 
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
type T_GrPatAlt  = ( FvInfoMp,([HsName]))
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,_tagItrf) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          (case (fldL_) of
           { _lhsOintroNmL ->
           ( _lhsOgathFviMp,_lhsOintroNmL) }) }) })
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp,_fldLIintroNmL) ->
         (case (tag_ ) of
          { ( _tagIgathFviMp,_tagItrf) ->
              (case (_tagIgathFviMp `fviMpUnion` _fldLIgathFviMp) of
               { _lhsOgathFviMp ->
               (case (nm_ : _fldLIintroNmL) of
                { _lhsOintroNmL ->
                ( _lhsOgathFviMp,_lhsOintroNmL) }) }) }) })
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,_tagItrf) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ([]) of
           { _lhsOintroNmL ->
           ( _lhsOgathFviMp,_lhsOintroNmL) }) }) })
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative Empty:
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
type T_GrPatLam  = ( FvInfoMp,([HsName]))
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp,_fldLIintroNmL) ->
         (case (_fldLIgathFviMp) of
          { _lhsOgathFviMp ->
          (case (tail _fldLIintroNmL) of
           { _lhsOintroNmL ->
           ( _lhsOgathFviMp,_lhsOintroNmL) }) }) })
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
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
type T_GrSplit  = ( FvInfoMp,([HsName]))
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (case (off_ ) of
     { ( _offIgathFviMp) ->
         (case (_offIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ([nm_]) of
           { _lhsOintroNmL ->
           ( _lhsOgathFviMp,_lhsOintroNmL) }) }) })
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
      alternative Nil:
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ( FvInfoMp,([HsName]))
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp,_tlIintroNmL) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,_hdIintroNmL) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               (case (_hdIintroNmL ++ _tlIintroNmL) of
                { _lhsOintroNmL ->
                ( _lhsOgathFviMp,_lhsOintroNmL) }) }) }) })
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
-- GrTag -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
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
type T_GrTag  = ( FvInfoMp,GrTag )
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case (GrTag_App nm_) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case (GrTag_Con grtgAnn_ int_ nm_) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     (case (GrTag_Fun nm_) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case (GrTag_Hole) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     (case (GrTag_PApp needs_ nm_) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case (GrTag_Rec) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case (GrTag_Unboxed) of
      { _trf ->
      (case (_trf) of
       { _lhsOtrf ->
       ( _lhsOgathFviMp,_lhsOtrf) }) }) })
-- GrTagL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrTag 
         child tl             : GrTagL 
      alternative Nil:
-}
-- cata
sem_GrTagL :: GrTagL  ->
              T_GrTagL 
sem_GrTagL list  =
    (Prelude.foldr sem_GrTagL_Cons sem_GrTagL_Nil (Prelude.map sem_GrTag list) )
-- semantic domain
type T_GrTagL  = ( FvInfoMp)
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,_hdItrf) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrType ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Arrow:
         child args           : GrTypeBaseL 
         child res            : GrTypeBase 
      alternative None:
-}
-- cata
sem_GrType :: GrType  ->
              T_GrType 
sem_GrType (GrType_Arrow _args _res )  =
    (sem_GrType_Arrow (sem_GrTypeBaseL _args ) (sem_GrTypeBase _res ) )
sem_GrType (GrType_None )  =
    (sem_GrType_None )
-- semantic domain
type T_GrType  = ( FvInfoMp)
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (case (res_ ) of
     { ( _resIgathFviMp) ->
         (case (args_ ) of
          { ( _argsIgathFviMp) ->
              (case (_argsIgathFviMp `fviMpUnion` _resIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Node:
      alternative Pointer:
-}
-- cata
sem_GrTypeBase :: GrTypeBase  ->
                  T_GrTypeBase 
sem_GrTypeBase (GrTypeBase_Node )  =
    (sem_GrTypeBase_Node )
sem_GrTypeBase (GrTypeBase_Pointer )  =
    (sem_GrTypeBase_Pointer )
-- semantic domain
type T_GrTypeBase  = ( FvInfoMp)
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrTypeBase 
         child tl             : GrTypeBaseL 
      alternative Nil:
-}
-- cata
sem_GrTypeBaseL :: GrTypeBaseL  ->
                   T_GrTypeBaseL 
sem_GrTypeBaseL list  =
    (Prelude.foldr sem_GrTypeBaseL_Cons sem_GrTypeBaseL_Nil (Prelude.map sem_GrTypeBase list) )
-- semantic domain
type T_GrTypeBaseL  = ( FvInfoMp)
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrVal -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
      alternative Empty:
      alternative EnumNode:
         child nm             : {HsName}
      alternative LitInt:
         child int            : {Int}
      alternative LitStr:
         child str            : {String}
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Tag:
         child tag            : GrTag 
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrValL 
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
type T_GrVal  = ( FvInfoMp)
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,_tagItrf) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp) ->
         (case (tag_ ) of
          { ( _tagIgathFviMp,_tagItrf) ->
              (case (_tagIgathFviMp `fviMpUnion` _fldLIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp) ->
         (case (fviMpUnions [fviMpSingleton nm_, _fldLIgathFviMp]) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,_tagItrf) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp) ->
         (case (_fldLIgathFviMp) of
          { _lhsOgathFviMp ->
          ( _lhsOgathFviMp) }) })
-- GrValL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
      alternative Nil:
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = ( FvInfoMp)
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               ( _lhsOgathFviMp) }) }) })
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     ( _lhsOgathFviMp) })
-- GrVar -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Ignore:
      alternative KnownTag:
         child tag            : GrTag 
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
type T_GrVar  = ( FvInfoMp,([HsName]))
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([ ]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,_tagItrf) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ([ error "introNmL known tag" ]) of
           { _lhsOintroNmL ->
           ( _lhsOgathFviMp,_lhsOintroNmL) }) }) })
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([nm_]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
      alternative Nil:
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ( FvInfoMp,([HsName]))
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp,_tlIintroNmL) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,_hdIintroNmL) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               (case (_hdIintroNmL ++ _tlIintroNmL) of
                { _lhsOintroNmL ->
                ( _lhsOgathFviMp,_lhsOintroNmL) }) }) }) })
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOintroNmL ->
      ( _lhsOgathFviMp,_lhsOintroNmL) }) })