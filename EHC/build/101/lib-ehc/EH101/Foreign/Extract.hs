

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Foreign/Extract.ag)
module EH101.Foreign.Extract(ForeignExtraction (..), forextractMbEnt
, foreignEntExtract) where

import EH101.Base.Common
import EH101.Foreign
import Data.Maybe









-- | all relevant info for all calling conventions is gathered in one place, for backends to choose from
data ForeignExtraction
  = ForeignExtraction_Plain
      { forextractIncludes      :: ![String]            -- ccall: include files
      , forextractEnt           :: !String              -- all: name of function/...
      , forextractMbKnownPrim   :: !(Maybe KnownPrim)   -- prim: known semantics
      , forextractMbThisArgNr   :: !(Maybe Int)         -- javascript: which arg acts as this/receiver of call
      , forextractMbIndexArgNr  :: !(Maybe Int)         -- javascript: combination with indexing an array
      , forextractOptIsStatic   :: !Bool                -- ccall: static
      , forextractOptIsPtr      :: !Bool                -- ccall: pointer
      , forextractForeignExpr   :: !ForeignExpr         -- the AST for building the FFI call in terms of the target language (20101020 AD: soon replaces flags above)
      }
  | ForeignExtraction_Wrapper
  | ForeignExtraction_Dynamic

emptyForeignExtraction = ForeignExtraction_Plain [] "??" Nothing Nothing Nothing False False (ForeignExpr_Call ForeignExpr_Ent)

forextractMbEnt :: ForeignExtraction -> Maybe String
forextractMbEnt (ForeignExtraction_Plain{forextractEnt=e})  = Just e
forextractMbEnt _                                           = Nothing



foreignEntExtract :: ForeignEnt -> ForeignExtraction
foreignEntExtract ty
  =  let  t =  wrap_ForeignAGItf
                 (sem_ForeignAGItf (ForeignAGItf_AGItf ty))
                 Inh_ForeignAGItf
     in   extr_Syn_ForeignAGItf t

-- CCall -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_CCall  = ( ForeignExtraction)
sem_CCall_Dynamic :: T_CCall 
sem_CCall_Dynamic  =
    (case (ForeignExtraction_Dynamic) of
     { _lhsOextr ->
     ( _lhsOextr) })
sem_CCall_Id :: Bool ->
                (Maybe String) ->
                Bool ->
                String ->
                T_CCall 
sem_CCall_Id isStatic_ mbInclude_ asPointer_ nm_  =
    (case (emptyForeignExtraction
               { forextractIncludes        = maybeToList mbInclude_
               , forextractEnt             = nm_
               , forextractOptIsStatic     = isStatic_
               , forextractOptIsPtr        = asPointer_
               }) of
     { _lhsOextr ->
     ( _lhsOextr) })
sem_CCall_Wrapper :: T_CCall 
sem_CCall_Wrapper  =
    (case (ForeignExtraction_Wrapper) of
     { _lhsOextr ->
     ( _lhsOextr) })
-- ForeignAGItf ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_ForeignAGItf  = ( ForeignExtraction)
data Inh_ForeignAGItf  = Inh_ForeignAGItf {}
data Syn_ForeignAGItf  = Syn_ForeignAGItf {extr_Syn_ForeignAGItf :: !(ForeignExtraction)}
wrap_ForeignAGItf :: T_ForeignAGItf  ->
                     Inh_ForeignAGItf  ->
                     Syn_ForeignAGItf 
wrap_ForeignAGItf sem (Inh_ForeignAGItf )  =
    (let ( _lhsOextr) = sem 
     in  (Syn_ForeignAGItf _lhsOextr ))
sem_ForeignAGItf_AGItf :: T_ForeignEnt  ->
                          T_ForeignAGItf 
sem_ForeignAGItf_AGItf ent_  =
    (case (ent_ ) of
     { ( _entIextr) ->
         (case (_entIextr) of
          { _lhsOextr ->
          ( _lhsOextr) }) })
-- ForeignEnt --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_ForeignEnt  = ( ForeignExtraction)
sem_ForeignEnt_CCall :: T_CCall  ->
                        T_ForeignEnt 
sem_ForeignEnt_CCall ent_  =
    (case (ent_ ) of
     { ( _entIextr) ->
         (case (_entIextr) of
          { _lhsOextr ->
          ( _lhsOextr) }) })
sem_ForeignEnt_JavaScriptCall :: T_JavaScriptCall  ->
                                 T_ForeignEnt 
sem_ForeignEnt_JavaScriptCall ent_  =
    (case (ent_ ) of
     { ( _entIextr) ->
         (case (_entIextr) of
          { _lhsOextr ->
          ( _lhsOextr) }) })
sem_ForeignEnt_PlainCall :: T_PlainCall  ->
                            T_ForeignEnt 
sem_ForeignEnt_PlainCall ent_  =
    (case (ent_ ) of
     { ( _entIextr) ->
         (case (_entIextr) of
          { _lhsOextr ->
          ( _lhsOextr) }) })
sem_ForeignEnt_PrimCall :: T_PrimCall  ->
                           T_ForeignEnt 
sem_ForeignEnt_PrimCall ent_  =
    (case (ent_ ) of
     { ( _entIextr) ->
         (case (_entIextr) of
          { _lhsOextr ->
          ( _lhsOextr) }) })
-- ForeignExpr -------------------------------------------------
{-
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
type T_ForeignExpr  = ( )
sem_ForeignExpr_AllArg :: T_ForeignExpr 
sem_ForeignExpr_AllArg  =
    ( )
sem_ForeignExpr_Arg :: Int ->
                       T_ForeignExpr 
sem_ForeignExpr_Arg nr_  =
    ( )
sem_ForeignExpr_Call :: T_ForeignExpr  ->
                        T_ForeignExpr 
sem_ForeignExpr_Call expr_  =
    ( )
sem_ForeignExpr_CallArgs :: T_ForeignExpr  ->
                            T_ForeignExprs  ->
                            T_ForeignExpr 
sem_ForeignExpr_CallArgs expr_ args_  =
    ( )
sem_ForeignExpr_Empty :: T_ForeignExpr 
sem_ForeignExpr_Empty  =
    ( )
sem_ForeignExpr_Ent :: T_ForeignExpr 
sem_ForeignExpr_Ent  =
    ( )
sem_ForeignExpr_EntNm :: String ->
                         T_ForeignExpr 
sem_ForeignExpr_EntNm nm_  =
    ( )
sem_ForeignExpr_Inx :: T_ForeignExpr  ->
                       T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Inx expr_ inx_  =
    ( )
sem_ForeignExpr_NewObj :: T_ForeignExpr  ->
                          T_ForeignExpr 
sem_ForeignExpr_NewObj expr_  =
    ( )
sem_ForeignExpr_ObjData :: T_ForeignExpr 
sem_ForeignExpr_ObjData  =
    ( )
sem_ForeignExpr_Ptr :: T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Ptr expr_  =
    ( )
sem_ForeignExpr_Sel :: T_ForeignExpr  ->
                       T_ForeignExpr  ->
                       T_ForeignExpr 
sem_ForeignExpr_Sel expr_ sel_  =
    ( )
sem_ForeignExpr_Str :: String ->
                       T_ForeignExpr 
sem_ForeignExpr_Str str_  =
    ( )
-- ForeignExprAGItf --------------------------------------------
{-
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
type T_ForeignExprAGItf  = ( )
sem_ForeignExprAGItf_AGItf :: T_ForeignExpr  ->
                              T_ForeignExprAGItf 
sem_ForeignExprAGItf_AGItf expr_  =
    ( )
-- ForeignExprs ------------------------------------------------
{-
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
type T_ForeignExprs  = ( )
sem_ForeignExprs_Cons :: T_ForeignExpr  ->
                         T_ForeignExprs  ->
                         T_ForeignExprs 
sem_ForeignExprs_Cons hd_ tl_  =
    ( )
sem_ForeignExprs_Nil :: T_ForeignExprs 
sem_ForeignExprs_Nil  =
    ( )
-- JavaScriptCall ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_JavaScriptCall  = ( ForeignExtraction)
sem_JavaScriptCall_Dynamic :: T_JavaScriptCall 
sem_JavaScriptCall_Dynamic  =
    (case (ForeignExtraction_Dynamic) of
     { _lhsOextr ->
     ( _lhsOextr) })
sem_JavaScriptCall_Id :: String ->
                         (Maybe ForeignExpr) ->
                         T_JavaScriptCall 
sem_JavaScriptCall_Id nm_ mbForeignExpr_  =
    (case (emptyForeignExtraction
               { forextractEnt             = nm_
               , forextractForeignExpr     = maybe (forextractForeignExpr emptyForeignExtraction) id mbForeignExpr_
               }) of
     { _lhsOextr ->
     ( _lhsOextr) })
sem_JavaScriptCall_Wrapper :: T_JavaScriptCall 
sem_JavaScriptCall_Wrapper  =
    (case (ForeignExtraction_Wrapper) of
     { _lhsOextr ->
     ( _lhsOextr) })
-- PlainCall ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_PlainCall  = ( ForeignExtraction)
sem_PlainCall_Id :: String ->
                    T_PlainCall 
sem_PlainCall_Id nm_  =
    (case (emptyForeignExtraction
               { forextractEnt             = nm_
               }) of
     { _lhsOextr ->
     ( _lhsOextr) })
-- PrimCall ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         extr                 : ForeignExtraction
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
type T_PrimCall  = ( ForeignExtraction)
sem_PrimCall_Id :: String ->
                   (Maybe KnownPrim) ->
                   T_PrimCall 
sem_PrimCall_Id nm_ mbKnownPrim_  =
    (case (emptyForeignExtraction
               { forextractEnt             = nm_
               , forextractMbKnownPrim     = mbKnownPrim_
               }) of
     { _lhsOextr ->
     ( _lhsOextr) })