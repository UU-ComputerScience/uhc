

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag)
module EH101.GrinCode.Trf.AliasElim(grAliasElim) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.GrinCode.Common
import EH101.GrinCode
import qualified EH101.Config as Cfg
import EH101.Foreign.Extract













grAliasElim :: GrModule -> GrModule
grAliasElim grmod
  = trf_Syn_GrAGItf t
  where t = wrap_GrAGItf (sem_GrAGItf $ GrAGItf_AGItf grmod)
            $ Inh_GrAGItf



nmAliasOne :: NmAliasMp -> NmAlias -> NmAlias -> (GrExpr->GrExpr,NmAliasMp)
nmAliasOne mpGlob e p
  = case (e,p) of
      (NmAlias_Nm ne, NmAlias_Nm np)
        -> (id,Map.singleton np $ nmAliasRepl' mpGlob ne)
      (NmAlias_Const _ c, NmAlias_Nm np)
        -> (GrExpr_Seq (GrExpr_Unit c GrType_None) (GrPatLam_Var np),Map.empty)
      _ -> (id,Map.empty)

nmAliasGrp :: NmAliasMp -> [NmAlias] -> [NmAlias] -> (GrExpr->GrExpr,NmAliasMp)
nmAliasGrp mpGlob eAliL pAliL
  = (foldr (.) id mkel, Map.unions mpl)
  where (mkel,mpl) = unzip $ zipWith (nmAliasOne mpGlob) eAliL pAliL

-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         trf                  : GrModule 
   alternatives:
      alternative AGItf:
         child module         : GrModule 
         visit 0:
            local mkNewNm     : _
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
    (let _lhsOtrf :: GrModule 
         _moduleOmkNewNm :: (HsName -> HsName)
         _moduleItrf :: GrModule 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 41, column 25)
         _mkNewNm =
             id
         -- copy rule (up)
         _lhsOtrf =
             _moduleItrf
         -- copy rule (from local)
         _moduleOmkNewNm =
             _mkNewNm
         ( _moduleItrf) =
             module_ _moduleOmkNewNm 
     in  ( _lhsOtrf))
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
         (let _lhsOtrf :: GrAdapt 
              _offOmkNewNm :: (HsName -> HsName)
              _offOnmAliasMp :: NmAliasMp
              _offInmAlias :: NmAlias
              _offItrf :: GrVal 
              -- self rule
              _trf =
                  GrAdapt_Del _offItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _offOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _offOnmAliasMp =
                  _lhsInmAliasMp
              ( _offInmAlias,_offItrf) =
                  off_ _offOmkNewNm _offOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrAdapt 
              _offOmkNewNm :: (HsName -> HsName)
              _offOnmAliasMp :: NmAliasMp
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _offInmAlias :: NmAlias
              _offItrf :: GrVal 
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- self rule
              _trf =
                  GrAdapt_Ins _offItrf _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _offOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _offOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              ( _offInmAlias,_offItrf) =
                  off_ _offOmkNewNm _offOnmAliasMp 
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrAdapt 
              _offOmkNewNm :: (HsName -> HsName)
              _offOnmAliasMp :: NmAliasMp
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _offInmAlias :: NmAlias
              _offItrf :: GrVal 
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- self rule
              _trf =
                  GrAdapt_Upd _offItrf _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _offOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _offOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              ( _offInmAlias,_offItrf) =
                  off_ _offOmkNewNm _offOnmAliasMp 
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrAdaptL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdItrf :: GrAdapt 
              _tlItrf :: GrAdaptL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrAdaptL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrAlt 
              _patOmkNewNm :: (HsName -> HsName)
              _patOnmAliasMp :: NmAliasMp
              _exprOmkNewNm :: (HsName -> HsName)
              _exprOnmAliasMp :: NmAliasMp
              _patIintroNmL :: ([HsName])
              _patInmAlias :: NmAlias
              _patItrf :: GrPatAlt 
              _exprIhasSideEffect :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 13, column 17)
              _patAliasMp =
                  mkNmAliasMp [(n,_lhsImkNewNm n) | n <- _patIintroNmL]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 13, column 17)
              _nmAliasMp =
                  _patAliasMp `Map.union` _lhsInmAliasMp
              -- self rule
              _trf =
                  GrAlt_Alt ann_ _patItrf _exprItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _patOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (from local)
              _patOnmAliasMp =
                  _nmAliasMp
              -- copy rule (down)
              _exprOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (from local)
              _exprOnmAliasMp =
                  _nmAliasMp
              ( _patIintroNmL,_patInmAlias,_patItrf) =
                  pat_ _patOmkNewNm _patOnmAliasMp 
              ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) =
                  expr_ _exprOmkNewNm _exprOnmAliasMp 
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrAltL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdItrf :: GrAlt 
              _tlItrf :: GrAltL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrAltL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrBind 
              -- self rule
              _trf =
                  GrBind_Arity nm_ arity_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (\ _lhsImkNewNm ->
         (let _exprOnmAliasMp :: NmAliasMp
              _lhsOtrf :: GrBind 
              _exprOmkNewNm :: (HsName -> HsName)
              _exprIhasSideEffect :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 4, column 17)
              _exprOnmAliasMp =
                  Map.empty
              -- self rule
              _trf =
                  GrBind_Bind nm_ annot_ argNmL_ _exprItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _exprOmkNewNm =
                  _lhsImkNewNm
              ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) =
                  expr_ _exprOmkNewNm _exprOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (\ _lhsImkNewNm ->
         (let _lhsOtrf :: GrBind 
              _bindLOmkNewNm :: (HsName -> HsName)
              _bindLItrf :: GrBindL 
              -- self rule
              _trf =
                  GrBind_Rec _bindLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _bindLOmkNewNm =
                  _lhsImkNewNm
              ( _bindLItrf) =
                  bindL_ _bindLOmkNewNm 
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrBindL 
              _hdOmkNewNm :: (HsName -> HsName)
              _tlOmkNewNm :: (HsName -> HsName)
              _hdItrf :: GrBind 
              _tlItrf :: GrBindL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              ( _hdItrf) =
                  hd_ _hdOmkNewNm 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm 
          in  ( _lhsOtrf)))
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (\ _lhsImkNewNm ->
         (let _lhsOtrf :: GrBindL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
            local foreignEntInfo : _
            local impEntNm    : _
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
         visit 0:
            local _tup1       : _
            local seqTrf      : _
            local mkExtraSeq  : _
            local newNmAliasMp : _
            local patAliasMp  : _
            local trf         : _
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
type T_GrExpr  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( Bool,NmAlias,GrExpr )
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _argLOmkNewNm :: (HsName -> HsName)
              _argLOnmAliasMp :: NmAliasMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 22, column 17)
              _lhsOtrf =
                  GrExpr_App (nmAliasRepl _lhsInmAliasMp nm_) _argLItrf
              -- self rule
              _trf =
                  GrExpr_App nm_ _argLItrf
              -- copy rule (down)
              _argLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _argLOnmAliasMp =
                  _lhsInmAliasMp
              ( _argLInmAliasL,_argLItrf) =
                  argL_ _argLOmkNewNm _argLOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _argLOmkNewNm :: (HsName -> HsName)
              _argLOnmAliasMp :: NmAliasMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 21, column 17)
              _lhsOtrf =
                  GrExpr_Call (nmAliasRepl _lhsInmAliasMp nm_) _argLItrf
              -- self rule
              _trf =
                  GrExpr_Call nm_ _argLItrf
              -- copy rule (down)
              _argLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _argLOnmAliasMp =
                  _lhsInmAliasMp
              ( _argLInmAliasL,_argLItrf) =
                  argL_ _argLOmkNewNm _argLOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _altLOmkNewNm :: (HsName -> HsName)
              _altLOnmAliasMp :: NmAliasMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              _altLItrf :: GrAltL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- self rule
              _trf =
                  GrExpr_Case _valItrf _altLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _altLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _altLOnmAliasMp =
                  _lhsInmAliasMp
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
              ( _altLItrf) =
                  altL_ _altLOmkNewNm _altLOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _bodyOmkNewNm :: (HsName -> HsName)
              _bodyOnmAliasMp :: NmAliasMp
              _handlerOmkNewNm :: (HsName -> HsName)
              _handlerOnmAliasMp :: NmAliasMp
              _bodyIhasSideEffect :: Bool
              _bodyInmAlias :: NmAlias
              _bodyItrf :: GrExpr 
              _handlerIhasSideEffect :: Bool
              _handlerInmAlias :: NmAlias
              _handlerItrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 30, column 17)
              _lhsOtrf =
                  GrExpr_Catch _bodyItrf (nmAliasRepl _lhsInmAliasMp arg_) _handlerItrf
              -- self rule
              _trf =
                  GrExpr_Catch _bodyItrf arg_ _handlerItrf
              -- copy rule (down)
              _bodyOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _bodyOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _handlerOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _handlerOnmAliasMp =
                  _lhsInmAliasMp
              ( _bodyIhasSideEffect,_bodyInmAlias,_bodyItrf) =
                  body_ _bodyOmkNewNm _bodyOnmAliasMp 
              ( _handlerIhasSideEffect,_handlerInmAlias,_handlerItrf) =
                  handler_ _handlerOmkNewNm _handlerOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 41, column 17)
              _lhsOnmAlias =
                  NmAlias_Eval nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 54, column 25)
              _lhsOhasSideEffect =
                  True
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 24, column 17)
              _lhsOtrf =
                  GrExpr_Eval $ nmAliasRepl _lhsInmAliasMp nm_
              -- self rule
              _trf =
                  GrExpr_Eval nm_
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _argLOmkNewNm :: (HsName -> HsName)
              _argLOnmAliasMp :: NmAliasMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/CommonForGen.ag"(line 2, column 17)
              _foreignEntInfo =
                  foreignEntExtract impEnt_
              -- "build/101/lib-ehc/EH101/GrinCode/CommonForGen.ag"(line 2, column 17)
              _impEntNm =
                  forextractEnt _foreignEntInfo
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 23, column 17)
              _lhsOtrf =
                  GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf
              -- self rule
              _trf =
                  GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf
              -- copy rule (down)
              _argLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _argLOnmAliasMp =
                  _lhsInmAliasMp
              ( _argLInmAliasL,_argLItrf) =
                  argL_ _argLOmkNewNm _argLOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 26, column 17)
              _lhsOtrf =
                  GrExpr_FetchField (nmAliasRepl _lhsInmAliasMp nm_) offset_ mbTag_
              -- self rule
              _trf =
                  GrExpr_FetchField nm_ offset_ mbTag_
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 25, column 17)
              _lhsOtrf =
                  GrExpr_FetchNode  (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrExpr_FetchNode nm_
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 28, column 17)
              _lhsOtrf =
                  GrExpr_FetchUpdate (nmAliasRepl _lhsInmAliasMp src_) (nmAliasRepl _lhsInmAliasMp dst_)
              -- self rule
              _trf =
                  GrExpr_FetchUpdate src_ dst_
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _bodyOnmAliasMp :: NmAliasMp
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _exprOmkNewNm :: (HsName -> HsName)
              _exprOnmAliasMp :: NmAliasMp
              _patOmkNewNm :: (HsName -> HsName)
              _patOnmAliasMp :: NmAliasMp
              _bodyOmkNewNm :: (HsName -> HsName)
              _exprIhasSideEffect :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              _patIintroNmL :: ([HsName])
              _patInmAlias :: NmAlias
              _patItrf :: GrPatLam 
              _bodyIhasSideEffect :: Bool
              _bodyInmAlias :: NmAlias
              _bodyItrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 44, column 17)
              _bodyOnmAliasMp =
                  _newNmAliasMp `Map.union` _lhsInmAliasMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 65, column 33)
              __tup1 =
                  case (_exprInmAlias,_patInmAlias,_bodyInmAlias) of
                    (_,NmAlias_Nm p,NmAlias_Nm b) | p == b
                      -> (_exprItrf,id,Map.empty)
                    (e@(NmAlias_Nm _),p@(NmAlias_Nm _),_)
                      -> (_bodyItrf,mke,mp)
                      where (mke,mp) = nmAliasOne _lhsInmAliasMp e p
                    (NmAlias_Grp _ es,NmAlias_Grp _ ps,_)
                      -> (_bodyItrf,mke,mp)
                      where (mke,mp) = nmAliasGrp _lhsInmAliasMp es ps
                    (e@(NmAlias_Grp _ _),NmAlias_Nm p,_)
                      -> (_trf,id,Map.singleton p (e {naliNm = p}))
                    (NmAlias_Nm e,NmAlias_Grp _ ps,_) | isJust mbAli
                      -> case mbAli of
                           Just (NmAlias_Grp _ es)
                             -> (_bodyItrf,mke,mp)
                             where (mke,mp) = nmAliasGrp _lhsInmAliasMp es ps
                           _ -> (_trf,id,Map.empty)
                      where mbAli = Map.lookup e _lhsInmAliasMp
                    (NmAlias_Nm e,NmAlias_Basic _ p ann,_) | isJust mbAli
                      -> case mbAli of
                           Just (NmAlias_Grp _ [e])
                             -> (_bodyItrf,mke,mp)
                             where (mke,mp) = nmAliasOne _lhsInmAliasMp e p
                           _ -> (_trf,id,Map.empty)
                      where mbAli = Map.lookup e _lhsInmAliasMp
                    _ -> (_trf,id,Map.empty)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 65, column 33)
              (_seqTrf,_,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 65, column 33)
              (_,_mkExtraSeq,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 65, column 33)
              (_,_,_newNmAliasMp) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/AliasElim.ag"(line 98, column 17)
              _lhsOtrf =
                  _mkExtraSeq _seqTrf
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 38, column 17)
              _lhsOnmAlias =
                  case (_bodyInmAlias,_exprIhasSideEffect) of
                    (NmAlias_Nm n,True) -> NmAlias_NmAfterSideEffect n
                    _ -> _bodyInmAlias
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 10, column 17)
              _patAliasMp =
                  mkNmAliasMp [(n,_lhsImkNewNm n) | n <- _patIintroNmL]
              -- self rule
              _trf =
                  GrExpr_Seq _exprItrf _patItrf _bodyItrf
              -- copy rule (down)
              _exprOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _exprOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _patOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _patOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _bodyOmkNewNm =
                  _lhsImkNewNm
              ( _exprIhasSideEffect,_exprInmAlias,_exprItrf) =
                  expr_ _exprOmkNewNm _exprOnmAliasMp 
              ( _patIintroNmL,_patInmAlias,_patItrf) =
                  pat_ _patOmkNewNm _patOnmAliasMp 
              ( _bodyIhasSideEffect,_bodyInmAlias,_bodyItrf) =
                  body_ _bodyOmkNewNm _bodyOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- self rule
              _trf =
                  GrExpr_Store _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOnmAlias =
                  _valInmAlias
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 29, column 17)
              _lhsOtrf =
                  GrExpr_Throw (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrExpr_Throw nm_
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _typeOmkNewNm :: (HsName -> HsName)
              _typeOnmAliasMp :: NmAliasMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              _typeItrf :: GrType 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- self rule
              _trf =
                  GrExpr_Unit _valItrf _typeItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOnmAlias =
                  _valInmAlias
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _typeOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _typeOnmAliasMp =
                  _lhsInmAliasMp
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
              ( _typeItrf) =
                  type_ _typeOmkNewNm _typeOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _lhsOtrf :: GrExpr 
              _valOmkNewNm :: (HsName -> HsName)
              _valOnmAliasMp :: NmAliasMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 27, column 17)
              _lhsOtrf =
                  GrExpr_UpdateUnit (nmAliasRepl _lhsInmAliasMp nm_) _valItrf
              -- self rule
              _trf =
                  GrExpr_UpdateUnit nm_ _valItrf
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _valOnmAliasMp =
                  _lhsInmAliasMp
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
          in  ( _lhsOhasSideEffect,_lhsOnmAlias,_lhsOtrf)))
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
    (let 
     in  ( ))
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
         (let _valOnmAliasMp :: NmAliasMp
              _lhsOtrf :: GrGlobal 
              _valOmkNewNm :: (HsName -> HsName)
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 7, column 17)
              _valOnmAliasMp =
                  Map.empty
              -- self rule
              _trf =
                  GrGlobal_Global nm_ _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _valOmkNewNm =
                  _lhsImkNewNm
              ( _valInmAlias,_valItrf) =
                  val_ _valOmkNewNm _valOnmAliasMp 
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrGlobalL 
              _hdOmkNewNm :: (HsName -> HsName)
              _tlOmkNewNm :: (HsName -> HsName)
              _hdItrf :: GrGlobal 
              _tlItrf :: GrGlobalL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              ( _hdItrf) =
                  hd_ _hdOmkNewNm 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm 
          in  ( _lhsOtrf)))
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (\ _lhsImkNewNm ->
         (let _lhsOtrf :: GrGlobalL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrModule 
              _globalLOmkNewNm :: (HsName -> HsName)
              _bindLOmkNewNm :: (HsName -> HsName)
              _globalLItrf :: GrGlobalL 
              _bindLItrf :: GrBindL 
              -- self rule
              _trf =
                  GrModule_Mod moduleNm_ _globalLItrf _bindLItrf tagsMp_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _globalLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _bindLOmkNewNm =
                  _lhsImkNewNm
              ( _globalLItrf) =
                  globalL_ _globalLOmkNewNm 
              ( _bindLItrf) =
                  bindL_ _bindLOmkNewNm 
          in  ( _lhsOtrf)))
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
         nmAlias              : NmAlias
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
type T_GrPatAlt  = (HsName -> HsName) ->
                   NmAliasMp ->
                   ( ([HsName]),NmAlias,GrPatAlt )
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatAlt 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  GrPatAlt_LitInt int_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatAlt 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 5, column 17)
              _lhsOnmAlias =
                  NmAlias_Grp hsnUnknown $ map NmAlias_Nm fldL_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 4, column 17)
              _lhsOintroNmL =
                  fldL_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 33, column 17)
              _lhsOtrf =
                  GrPatAlt_Node _tagItrf (map (nmAliasRepl _lhsInmAliasMp) fldL_)
              -- self rule
              _trf =
                  GrPatAlt_Node _tagItrf fldL_
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatAlt 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _fldLOmkNewNm :: (HsName -> HsName)
              _fldLOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              _fldLIintroNmL :: ([HsName])
              _fldLItrf :: GrSplitL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 5, column 17)
              _lhsOintroNmL =
                  nm_ : _fldLIintroNmL
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 34, column 17)
              _lhsOtrf =
                  GrPatAlt_NodeSplit _tagItrf (nmAliasRepl _lhsInmAliasMp nm_) _fldLItrf
              -- self rule
              _trf =
                  GrPatAlt_NodeSplit _tagItrf nm_ _fldLItrf
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _fldLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _fldLOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
              ( _fldLIintroNmL,_fldLItrf) =
                  fldL_ _fldLOmkNewNm _fldLOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatAlt 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  GrPatAlt_Otherwise
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatAlt 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  GrPatAlt_Tag _tagItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
         nmAlias              : NmAlias
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
type T_GrPatLam  = (HsName -> HsName) ->
                   NmAliasMp ->
                   ( ([HsName]),NmAlias,GrPatLam )
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 10, column 17)
              _lhsOnmAlias =
                  case annot_ of
                    BasicAnnot_Size bsz _ _ _
                      | basicSizeIsWord bsz  -> NmAlias_Nm nm_
                    _                        -> NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 42, column 17)
              _lhsOtrf =
                  GrPatLam_BasicAnnot annot_ (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_BasicAnnot annot_ nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 15, column 17)
              _lhsOnmAlias =
                  NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) annot_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 38, column 17)
              _lhsOtrf =
                  GrPatLam_BasicNode annot_ (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_BasicNode annot_ nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  GrPatLam_Empty
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 43, column 17)
              _lhsOtrf =
                  GrPatLam_EnumAnnot (nmAliasRepl _lhsInmAliasMp tycon_) (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_EnumAnnot tycon_ nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 39, column 17)
              _lhsOtrf =
                  GrPatLam_EnumNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_EnumNode nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 44, column 17)
              _lhsOtrf =
                  GrPatLam_OpaqueAnnot (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_OpaqueAnnot nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 40, column 17)
              _lhsOtrf =
                  GrPatLam_OpaqueNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_OpaqueNode nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 45, column 17)
              _lhsOtrf =
                  GrPatLam_PtrAnnot (nmAliasRepl _lhsInmAliasMp tycon_) (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_PtrAnnot tycon_ nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 41, column 17)
              _lhsOtrf =
                  GrPatLam_PtrNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_PtrNode nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 9, column 25)
              _lhsOnmAlias =
                  NmAlias_Nm    nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 8, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 37, column 17)
              _lhsOtrf =
                  GrPatLam_Var (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrPatLam_Var nm_
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrPatLam 
              _fldLOmkNewNm :: (HsName -> HsName)
              _fldLOnmAliasMp :: NmAliasMp
              _fldLIintroNmL :: ([HsName])
              _fldLItrf :: GrVarL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 14, column 17)
              _lhsOnmAlias =
                  NmAlias_Grp   hsnUnknown $ map NmAlias_Nm (tail _fldLIintroNmL)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 9, column 17)
              _lhsOintroNmL =
                  tail _fldLIintroNmL
              -- self rule
              _trf =
                  GrPatLam_VarNode _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _fldLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _fldLOnmAliasMp =
                  _lhsInmAliasMp
              ( _fldLIintroNmL,_fldLItrf) =
                  fldL_ _fldLOmkNewNm _fldLOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOnmAlias,_lhsOtrf)))
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
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
type T_GrSplit  = (HsName -> HsName) ->
                  NmAliasMp ->
                  ( ([HsName]),GrSplit )
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrSplit 
              _offOmkNewNm :: (HsName -> HsName)
              _offOnmAliasMp :: NmAliasMp
              _offInmAlias :: NmAlias
              _offItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 19, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 59, column 17)
              _lhsOtrf =
                  GrSplit_Sel (nmAliasRepl _lhsInmAliasMp nm_) _offItrf
              -- self rule
              _trf =
                  GrSplit_Sel nm_ _offItrf
              -- copy rule (down)
              _offOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _offOnmAliasMp =
                  _lhsInmAliasMp
              ( _offInmAlias,_offItrf) =
                  off_ _offOmkNewNm _offOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOtrf)))
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
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
type T_GrSplitL  = (HsName -> HsName) ->
                   NmAliasMp ->
                   ( ([HsName]),GrSplitL )
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrSplitL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdIintroNmL :: ([HsName])
              _hdItrf :: GrSplit 
              _tlIintroNmL :: ([HsName])
              _tlItrf :: GrSplitL 
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  _hdIintroNmL ++ _tlIintroNmL
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdIintroNmL,_hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlIintroNmL,_tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOtrf)))
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrSplitL 
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOtrf)))
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
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_App nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_Con grtgAnn_ int_ nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_Fun nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_Hole
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_PApp needs_ nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_Rec
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTag 
              -- self rule
              _trf =
                  GrTag_Unboxed
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrTagL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdItrf :: GrTag 
              _tlItrf :: GrTagL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTagL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrType 
              _argsOmkNewNm :: (HsName -> HsName)
              _argsOnmAliasMp :: NmAliasMp
              _resOmkNewNm :: (HsName -> HsName)
              _resOnmAliasMp :: NmAliasMp
              _argsItrf :: GrTypeBaseL 
              _resItrf :: GrTypeBase 
              -- self rule
              _trf =
                  GrType_Arrow _argsItrf _resItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _argsOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _argsOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _resOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _resOnmAliasMp =
                  _lhsInmAliasMp
              ( _argsItrf) =
                  args_ _argsOmkNewNm _argsOnmAliasMp 
              ( _resItrf) =
                  res_ _resOmkNewNm _resOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrType 
              -- self rule
              _trf =
                  GrType_None
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrTypeBase 
              -- self rule
              _trf =
                  GrTypeBase_Node
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTypeBase 
              -- self rule
              _trf =
                  GrTypeBase_Pointer
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
         (let _lhsOtrf :: GrTypeBaseL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdItrf :: GrTypeBase 
              _tlItrf :: GrTypeBaseL 
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOtrf)))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOtrf :: GrTypeBaseL 
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOtrf)))
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
type T_GrVal  = (HsName -> HsName) ->
                NmAliasMp ->
                ( NmAlias,GrVal )
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 31, column 17)
              _lhsOnmAlias =
                  NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) BasicAnnot_Dflt
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 52, column 17)
              _lhsOtrf =
                  GrVal_BasicNode _tagItrf (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrVal_BasicNode _tagItrf nm_
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- self rule
              _trf =
                  GrVal_Empty
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 53, column 17)
              _lhsOtrf =
                  GrVal_EnumNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrVal_EnumNode nm_
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 32, column 17)
              _lhsOnmAlias =
                  NmAlias_Const hsnUnknown (GrVal_LitInt int_)
              -- self rule
              _trf =
                  GrVal_LitInt int_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 33, column 17)
              _lhsOnmAlias =
                  NmAlias_Const hsnUnknown (GrVal_LitStr str_)
              -- self rule
              _trf =
                  GrVal_LitStr str_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _fldLOmkNewNm :: (HsName -> HsName)
              _fldLOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              _fldLInmAliasL :: ([NmAlias])
              _fldLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 27, column 17)
              _lhsOnmAlias =
                  case _tagItrf of
                    GrTag_Con _ _ _
                      -> NmAlias_Grp hsnUnknown _fldLInmAliasL
                    _ -> NmAlias_None
              -- self rule
              _trf =
                  GrVal_Node _tagItrf _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _fldLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _fldLOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
              ( _fldLInmAliasL,_fldLItrf) =
                  fldL_ _fldLOmkNewNm _fldLOnmAliasMp 
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              _fldLOmkNewNm :: (HsName -> HsName)
              _fldLOnmAliasMp :: NmAliasMp
              _fldLItrf :: GrAdaptL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 56, column 17)
              _lhsOtrf =
                  GrVal_NodeAdapt (nmAliasRepl _lhsInmAliasMp nm_) _fldLItrf
              -- self rule
              _trf =
                  GrVal_NodeAdapt nm_ _fldLItrf
              -- copy rule (down)
              _fldLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _fldLOnmAliasMp =
                  _lhsInmAliasMp
              ( _fldLItrf) =
                  fldL_ _fldLOmkNewNm _fldLOnmAliasMp 
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 55, column 17)
              _lhsOtrf =
                  GrVal_OpaqueNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrVal_OpaqueNode nm_
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 54, column 21)
              _lhsOtrf =
                  GrVal_PtrNode (nmAliasRepl _lhsInmAliasMp nm_)
              -- self rule
              _trf =
                  GrVal_PtrNode nm_
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- self rule
              _trf =
                  GrVal_Tag _tagItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 26, column 17)
              _lhsOnmAlias =
                  NmAlias_Nm nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 51, column 17)
              _lhsOtrf =
                  GrVal_Var $ nmAliasRepl _lhsInmAliasMp nm_
              -- self rule
              _trf =
                  GrVal_Var nm_
          in  ( _lhsOnmAlias,_lhsOtrf)))
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAlias :: NmAlias
              _lhsOtrf :: GrVal 
              _fldLOmkNewNm :: (HsName -> HsName)
              _fldLOnmAliasMp :: NmAliasMp
              _fldLInmAliasL :: ([NmAlias])
              _fldLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- self rule
              _trf =
                  GrVal_VarNode _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _fldLOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _fldLOnmAliasMp =
                  _lhsInmAliasMp
              ( _fldLInmAliasL,_fldLItrf) =
                  fldL_ _fldLOmkNewNm _fldLOnmAliasMp 
          in  ( _lhsOnmAlias,_lhsOtrf)))
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
         (let _lhsOnmAliasL :: ([NmAlias])
              _lhsOtrf :: GrValL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdInmAlias :: NmAlias
              _hdItrf :: GrVal 
              _tlInmAliasL :: ([NmAlias])
              _tlItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 48, column 17)
              _lhsOnmAliasL =
                  _hdInmAlias : _tlInmAliasL
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdInmAlias,_hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlInmAliasL,_tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOnmAliasL,_lhsOtrf)))
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOnmAliasL :: ([NmAlias])
              _lhsOtrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 49, column 17)
              _lhsOnmAliasL =
                  []
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOnmAliasL,_lhsOtrf)))
-- GrVar -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
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
type T_GrVar  = (HsName -> HsName) ->
                NmAliasMp ->
                ( ([HsName]),GrVar )
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrVar 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 16, column 17)
              _lhsOintroNmL =
                  [ ]
              -- self rule
              _trf =
                  GrVar_Ignore
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOtrf)))
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrVar 
              _tagOmkNewNm :: (HsName -> HsName)
              _tagOnmAliasMp :: NmAliasMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 15, column 17)
              _lhsOintroNmL =
                  [ error "introNmL known tag" ]
              -- self rule
              _trf =
                  GrVar_KnownTag _tagItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _tagOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tagOnmAliasMp =
                  _lhsInmAliasMp
              ( _tagItrf) =
                  tag_ _tagOmkNewNm _tagOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOtrf)))
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrVar 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 14, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonRenameAG.ag"(line 48, column 17)
              _lhsOtrf =
                  GrVar_Var $ nmAliasRepl _lhsInmAliasMp nm_
              -- self rule
              _trf =
                  GrVar_Var nm_
          in  ( _lhsOintroNmL,_lhsOtrf)))
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mkNewNm              : HsName -> HsName
         nmAliasMp            : NmAliasMp
      synthesized attributes:
         introNmL             : [HsName]
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
type T_GrVarL  = (HsName -> HsName) ->
                 NmAliasMp ->
                 ( ([HsName]),GrVarL )
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrVarL 
              _hdOmkNewNm :: (HsName -> HsName)
              _hdOnmAliasMp :: NmAliasMp
              _tlOmkNewNm :: (HsName -> HsName)
              _tlOnmAliasMp :: NmAliasMp
              _hdIintroNmL :: ([HsName])
              _hdItrf :: GrVar 
              _tlIintroNmL :: ([HsName])
              _tlItrf :: GrVarL 
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  _hdIintroNmL ++ _tlIintroNmL
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (down)
              _hdOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _hdOnmAliasMp =
                  _lhsInmAliasMp
              -- copy rule (down)
              _tlOmkNewNm =
                  _lhsImkNewNm
              -- copy rule (down)
              _tlOnmAliasMp =
                  _lhsInmAliasMp
              ( _hdIintroNmL,_hdItrf) =
                  hd_ _hdOmkNewNm _hdOnmAliasMp 
              ( _tlIintroNmL,_tlItrf) =
                  tl_ _tlOmkNewNm _tlOnmAliasMp 
          in  ( _lhsOintroNmL,_lhsOtrf)))
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (\ _lhsImkNewNm
       _lhsInmAliasMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOtrf :: GrVarL 
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOintroNmL,_lhsOtrf)))