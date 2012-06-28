

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Foreign/Pretty.ag)
module EH101.Foreign.Pretty(ppForeignEnt
, ppForeignExpr) where

import EH.Util.Pretty
import EH101.Base.Common
import EH101.Foreign





ppForeignEnt :: ForeignEnt -> PP_Doc
ppForeignEnt ent
  =  let  t =  wrap_ForeignAGItf
                 (sem_ForeignAGItf (ForeignAGItf_AGItf ent))
                 Inh_ForeignAGItf
     in   pp_Syn_ForeignAGItf t

instance PP ForeignEnt where
  pp t = ppForeignEnt t



ppForeignExpr :: String -> ForeignExpr -> PP_Doc
ppForeignExpr nm e
  =  let  t =  wrap_ForeignExprAGItf
                 (sem_ForeignExprAGItf (ForeignExprAGItf_AGItf e))
                 (Inh_ForeignExprAGItf
                   { entNm_Inh_ForeignExprAGItf = nm
                   })
     in   pp_Syn_ForeignExprAGItf t

-- CCall -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Dynamic:
      alternative Id:
         child isStatic       : {Bool}
         child mbInclude      : {Maybe String}
         child asPointer      : {Bool}
         child nm             : {String}
      alternative Wrapper:
-}
-- cata
sem_CCall :: CCall  ->
             T_CCall 
sem_CCall (CCall_Dynamic )  =
    (sem_CCall_Dynamic )
sem_CCall (CCall_Id _isStatic _mbInclude _asPointer _nm )  =
    (sem_CCall_Id _isStatic _mbInclude _asPointer _nm )
sem_CCall (CCall_Wrapper )  =
    (sem_CCall_Wrapper )
-- semantic domain
type T_CCall  = ( PP_Doc)
sem_CCall_Dynamic :: T_CCall 
sem_CCall_Dynamic  =
    (case (pp "dynamic") of
     { _lhsOpp ->
     ( _lhsOpp) })
sem_CCall_Id :: Bool ->
                (Maybe String) ->
                Bool ->
                String ->
                T_CCall 
sem_CCall_Id isStatic_ mbInclude_ asPointer_ nm_  =
    (case ((if isStatic_ then pp "static" else empty)
           >#< (maybe empty pp mbInclude_)
           >#< (if asPointer_ then pp "&" else empty)
           >#< nm_) of
     { _lhsOpp ->
     ( _lhsOpp) })
sem_CCall_Wrapper :: T_CCall 
sem_CCall_Wrapper  =
    (case (pp "wrapper") of
     { _lhsOpp ->
     ( _lhsOpp) })
-- ForeignAGItf ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child ent            : ForeignEnt 
-}
-- cata
sem_ForeignAGItf :: ForeignAGItf  ->
                    T_ForeignAGItf 
sem_ForeignAGItf (ForeignAGItf_AGItf _ent )  =
    (sem_ForeignAGItf_AGItf (sem_ForeignEnt _ent ) )
-- semantic domain
type T_ForeignAGItf  = ( PP_Doc)
data Inh_ForeignAGItf  = Inh_ForeignAGItf {}
data Syn_ForeignAGItf  = Syn_ForeignAGItf {pp_Syn_ForeignAGItf :: !(PP_Doc)}
wrap_ForeignAGItf :: T_ForeignAGItf  ->
                     Inh_ForeignAGItf  ->
                     Syn_ForeignAGItf 
wrap_ForeignAGItf sem (Inh_ForeignAGItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_ForeignAGItf _lhsOpp ))
sem_ForeignAGItf_AGItf :: T_ForeignEnt  ->
                          T_ForeignAGItf 
sem_ForeignAGItf_AGItf ent_  =
    (case (ent_ ) of
     { ( _entIpp) ->
         (case (_entIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- ForeignEnt --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative CCall:
         child ent            : CCall 
      alternative JavaScriptCall:
         child ent            : JavaScriptCall 
      alternative PlainCall:
         child ent            : PlainCall 
      alternative PrimCall:
         child ent            : PrimCall 
-}
-- cata
sem_ForeignEnt :: ForeignEnt  ->
                  T_ForeignEnt 
sem_ForeignEnt (ForeignEnt_CCall _ent )  =
    (sem_ForeignEnt_CCall (sem_CCall _ent ) )
sem_ForeignEnt (ForeignEnt_JavaScriptCall _ent )  =
    (sem_ForeignEnt_JavaScriptCall (sem_JavaScriptCall _ent ) )
sem_ForeignEnt (ForeignEnt_PlainCall _ent )  =
    (sem_ForeignEnt_PlainCall (sem_PlainCall _ent ) )
sem_ForeignEnt (ForeignEnt_PrimCall _ent )  =
    (sem_ForeignEnt_PrimCall (sem_PrimCall _ent ) )
-- semantic domain
type T_ForeignEnt  = ( PP_Doc)
sem_ForeignEnt_CCall :: T_CCall  ->
                        T_ForeignEnt 
sem_ForeignEnt_CCall ent_  =
    (case (ent_ ) of
     { ( _entIpp) ->
         (case (_entIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_ForeignEnt_JavaScriptCall :: T_JavaScriptCall  ->
                                 T_ForeignEnt 
sem_ForeignEnt_JavaScriptCall ent_  =
    (case (ent_ ) of
     { ( _entIpp) ->
         (case (_entIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_ForeignEnt_PlainCall :: T_PlainCall  ->
                            T_ForeignEnt 
sem_ForeignEnt_PlainCall ent_  =
    (case (ent_ ) of
     { ( _entIpp) ->
         (case (_entIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_ForeignEnt_PrimCall :: T_PrimCall  ->
                           T_ForeignEnt 
sem_ForeignEnt_PrimCall ent_  =
    (case (ent_ ) of
     { ( _entIpp) ->
         (case (_entIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- ForeignExpr -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         entNm                : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AllArg:
      alternative Arg:
         child nr             : {Int}
      alternative Call:
         child expr           : ForeignExpr 
      alternative CallArgs:
         child expr           : ForeignExpr 
         child args           : ForeignExprs 
      alternative Empty:
      alternative Ent:
      alternative EntNm:
         child nm             : {String}
      alternative Inx:
         child expr           : ForeignExpr 
         child inx            : ForeignExpr 
      alternative NewObj:
         child expr           : ForeignExpr 
      alternative ObjData:
      alternative Ptr:
         child expr           : ForeignExpr 
      alternative Sel:
         child expr           : ForeignExpr 
         child sel            : ForeignExpr 
      alternative Str:
         child str            : {String}
-}
-- cata
sem_ForeignExpr :: ForeignExpr  ->
                   T_ForeignExpr 
sem_ForeignExpr (ForeignExpr_AllArg )  =
    (sem_ForeignExpr_AllArg )
sem_ForeignExpr (ForeignExpr_Arg _nr )  =
    (sem_ForeignExpr_Arg _nr )
sem_ForeignExpr (ForeignExpr_Call _expr )  =
    (sem_ForeignExpr_Call (sem_ForeignExpr _expr ) )
sem_ForeignExpr (ForeignExpr_CallArgs _expr _args )  =
    (sem_ForeignExpr_CallArgs (sem_ForeignExpr _expr ) (sem_ForeignExprs _args ) )
sem_ForeignExpr (ForeignExpr_Empty )  =
    (sem_ForeignExpr_Empty )
sem_ForeignExpr (ForeignExpr_Ent )  =
    (sem_ForeignExpr_Ent )
sem_ForeignExpr (ForeignExpr_EntNm _nm )  =
    (sem_ForeignExpr_EntNm _nm )
sem_ForeignExpr (ForeignExpr_Inx _expr _inx )  =
    (sem_ForeignExpr_Inx (sem_ForeignExpr _expr ) (sem_ForeignExpr _inx ) )
sem_ForeignExpr (ForeignExpr_NewObj _expr )  =
    (sem_ForeignExpr_NewObj (sem_ForeignExpr _expr ) )
sem_ForeignExpr (ForeignExpr_ObjData )  =
    (sem_ForeignExpr_ObjData )
sem_ForeignExpr (ForeignExpr_Ptr _expr )  =
    (sem_ForeignExpr_Ptr (sem_ForeignExpr _expr ) )
sem_ForeignExpr (ForeignExpr_Sel _expr _sel )  =
    (sem_ForeignExpr_Sel (sem_ForeignExpr _expr ) (sem_ForeignExpr _sel ) )
sem_ForeignExpr (ForeignExpr_Str _str )  =
    (sem_ForeignExpr_Str _str )
-- semantic domain
type T_ForeignExpr  = String ->
                      ( PP_Doc)
sem_ForeignExpr_AllArg :: T_ForeignExpr 
sem_ForeignExpr_AllArg  =
    (\ _lhsIentNm ->
         (case (pp "%*") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_Arg :: Int ->
                       T_ForeignExpr 
sem_ForeignExpr_Arg nr_  =
    (\ _lhsIentNm ->
         (case ("%" >|< nr_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_Call :: T_ForeignExpr  ->
                        T_ForeignExpr 
sem_ForeignExpr_Call expr_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _exprOentNm ->
          (case (expr_ _exprOentNm ) of
           { ( _exprIpp) ->
               (case (_exprIpp >|< ppParens empty) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_ForeignExpr_CallArgs :: T_ForeignExpr  ->
                            T_ForeignExprs  ->
                            T_ForeignExpr 
sem_ForeignExpr_CallArgs expr_ args_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _argsOentNm ->
          (case (_lhsIentNm) of
           { _exprOentNm ->
           (case (args_ _argsOentNm ) of
            { ( _argsIppL) ->
                (case (expr_ _exprOentNm ) of
                 { ( _exprIpp) ->
                     (case (_exprIpp >|< ppParensCommas _argsIppL) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_ForeignExpr_Empty :: T_ForeignExpr 
sem_ForeignExpr_Empty  =
    (\ _lhsIentNm ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_Ent :: T_ForeignExpr 
sem_ForeignExpr_Ent  =
    (\ _lhsIentNm ->
         (case (pp _lhsIentNm) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_EntNm :: String ->
                         T_ForeignExpr 
sem_ForeignExpr_EntNm nm_  =
    (\ _lhsIentNm ->
         (case (pp nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_Inx :: T_ForeignExpr  ->
                       T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Inx expr_ inx_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _inxOentNm ->
          (case (_lhsIentNm) of
           { _exprOentNm ->
           (case (inx_ _inxOentNm ) of
            { ( _inxIpp) ->
                (case (expr_ _exprOentNm ) of
                 { ( _exprIpp) ->
                     (case (_exprIpp >|< "[" >|< _inxIpp >|< "]") of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_ForeignExpr_NewObj :: T_ForeignExpr  ->
                          T_ForeignExpr 
sem_ForeignExpr_NewObj expr_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _exprOentNm ->
          (case (expr_ _exprOentNm ) of
           { ( _exprIpp) ->
               (case (_exprIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_ForeignExpr_ObjData :: T_ForeignExpr 
sem_ForeignExpr_ObjData  =
    (\ _lhsIentNm ->
         (case (pp "{}") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_ForeignExpr_Ptr :: T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Ptr expr_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _exprOentNm ->
          (case (expr_ _exprOentNm ) of
           { ( _exprIpp) ->
               (case ("&" >|< _exprIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_ForeignExpr_Sel :: T_ForeignExpr  ->
                       T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Sel expr_ sel_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _selOentNm ->
          (case (_lhsIentNm) of
           { _exprOentNm ->
           (case (sel_ _selOentNm ) of
            { ( _selIpp) ->
                (case (expr_ _exprOentNm ) of
                 { ( _exprIpp) ->
                     (case (_exprIpp >|< "." >|< _selIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_ForeignExpr_Str :: String ->
                       T_ForeignExpr 
sem_ForeignExpr_Str str_  =
    (\ _lhsIentNm ->
         (case (pp $ show str_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- ForeignExprAGItf --------------------------------------------
{-
   visit 0:
      inherited attribute:
         entNm                : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child expr           : ForeignExpr 
-}
-- cata
sem_ForeignExprAGItf :: ForeignExprAGItf  ->
                        T_ForeignExprAGItf 
sem_ForeignExprAGItf (ForeignExprAGItf_AGItf _expr )  =
    (sem_ForeignExprAGItf_AGItf (sem_ForeignExpr _expr ) )
-- semantic domain
type T_ForeignExprAGItf  = String ->
                           ( PP_Doc)
data Inh_ForeignExprAGItf  = Inh_ForeignExprAGItf {entNm_Inh_ForeignExprAGItf :: !(String)}
data Syn_ForeignExprAGItf  = Syn_ForeignExprAGItf {pp_Syn_ForeignExprAGItf :: !(PP_Doc)}
wrap_ForeignExprAGItf :: T_ForeignExprAGItf  ->
                         Inh_ForeignExprAGItf  ->
                         Syn_ForeignExprAGItf 
wrap_ForeignExprAGItf sem (Inh_ForeignExprAGItf _lhsIentNm )  =
    (let ( _lhsOpp) = sem _lhsIentNm 
     in  (Syn_ForeignExprAGItf _lhsOpp ))
sem_ForeignExprAGItf_AGItf :: T_ForeignExpr  ->
                              T_ForeignExprAGItf 
sem_ForeignExprAGItf_AGItf expr_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _exprOentNm ->
          (case (expr_ _exprOentNm ) of
           { ( _exprIpp) ->
               (case (_exprIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- ForeignExprs ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         entNm                : String
      synthesized attribute:
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : ForeignExpr 
         child tl             : ForeignExprs 
      alternative Nil:
-}
-- cata
sem_ForeignExprs :: ForeignExprs  ->
                    T_ForeignExprs 
sem_ForeignExprs list  =
    (Prelude.foldr sem_ForeignExprs_Cons sem_ForeignExprs_Nil (Prelude.map sem_ForeignExpr list) )
-- semantic domain
type T_ForeignExprs  = String ->
                       ( ([PP_Doc]))
sem_ForeignExprs_Cons :: T_ForeignExpr  ->
                         T_ForeignExprs  ->
                         T_ForeignExprs 
sem_ForeignExprs_Cons hd_ tl_  =
    (\ _lhsIentNm ->
         (case (_lhsIentNm) of
          { _tlOentNm ->
          (case (_lhsIentNm) of
           { _hdOentNm ->
           (case (tl_ _tlOentNm ) of
            { ( _tlIppL) ->
                (case (hd_ _hdOentNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp : _tlIppL) of
                      { _lhsOppL ->
                      ( _lhsOppL) }) }) }) }) }))
sem_ForeignExprs_Nil :: T_ForeignExprs 
sem_ForeignExprs_Nil  =
    (\ _lhsIentNm ->
         (case ([]) of
          { _lhsOppL ->
          ( _lhsOppL) }))
-- JavaScriptCall ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Dynamic:
      alternative Id:
         child nm             : {String}
         child mbForeignExpr  : {Maybe ForeignExpr}
      alternative Wrapper:
-}
-- cata
sem_JavaScriptCall :: JavaScriptCall  ->
                      T_JavaScriptCall 
sem_JavaScriptCall (JavaScriptCall_Dynamic )  =
    (sem_JavaScriptCall_Dynamic )
sem_JavaScriptCall (JavaScriptCall_Id _nm _mbForeignExpr )  =
    (sem_JavaScriptCall_Id _nm _mbForeignExpr )
sem_JavaScriptCall (JavaScriptCall_Wrapper )  =
    (sem_JavaScriptCall_Wrapper )
-- semantic domain
type T_JavaScriptCall  = ( PP_Doc)
sem_JavaScriptCall_Dynamic :: T_JavaScriptCall 
sem_JavaScriptCall_Dynamic  =
    (case (pp "dynamic") of
     { _lhsOpp ->
     ( _lhsOpp) })
sem_JavaScriptCall_Id :: String ->
                         (Maybe ForeignExpr) ->
                         T_JavaScriptCall 
sem_JavaScriptCall_Id nm_ mbForeignExpr_  =
    (case (maybe (pp nm_) (ppForeignExpr nm_) mbForeignExpr_) of
     { _lhsOpp ->
     ( _lhsOpp) })
sem_JavaScriptCall_Wrapper :: T_JavaScriptCall 
sem_JavaScriptCall_Wrapper  =
    (case (pp "wrapper") of
     { _lhsOpp ->
     ( _lhsOpp) })
-- PlainCall ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Id:
         child nm             : {String}
-}
-- cata
sem_PlainCall :: PlainCall  ->
                 T_PlainCall 
sem_PlainCall (PlainCall_Id _nm )  =
    (sem_PlainCall_Id _nm )
-- semantic domain
type T_PlainCall  = ( PP_Doc)
sem_PlainCall_Id :: String ->
                    T_PlainCall 
sem_PlainCall_Id nm_  =
    (case (pp nm_) of
     { _lhsOpp ->
     ( _lhsOpp) })
-- PrimCall ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Id:
         child nm             : {String}
         child mbKnownPrim    : {Maybe KnownPrim}
-}
-- cata
sem_PrimCall :: PrimCall  ->
                T_PrimCall 
sem_PrimCall (PrimCall_Id _nm _mbKnownPrim )  =
    (sem_PrimCall_Id _nm _mbKnownPrim )
-- semantic domain
type T_PrimCall  = ( PP_Doc)
sem_PrimCall_Id :: String ->
                   (Maybe KnownPrim) ->
                   T_PrimCall 
sem_PrimCall_Id nm_ mbKnownPrim_  =
    (case (pp nm_ >#< maybe empty pp mbKnownPrim_) of
     { _lhsOpp ->
     ( _lhsOpp) })