%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core seen as Ty, for System F generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}Core.AsTy} import({%{EH}Base.Common},{%{EH}Opts.Base})
%%]

%%[(8 codegen) hs import(qualified {%{EH}Core} as C, qualified {%{EH}Ty} as T)
%%]

%%[(8 codegen) hs import({%{EH}AbstractCore})
%%]

%%[(8 codegen) hs import({%{EH}Core.BindExtract})
%%]

%%[(8 codegen coresysf) hs import({%{EH}Ty.ToSysfTy}) export(module {%{EH}Ty.ToSysfTy})
%%]
 
%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[doesWhat doclatex
This module wraps Core in such a way that it behaves as Ty,
i.e. provides the same API.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Explicit name for Ty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast) hs export(Ty)
-- | The type, represented by a term CExpr
type Ty     		= C.SysfTy			-- base ty

-- | A sequence of parameters (for now just a single type)
type TySeq			= C.SysfTySeq

-- | Binding the bound
type TyBind			= C.SysfTyBind
type TyBound		= C.SysfTyBound
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion interface: only for FFI types, already expanded etc (for now)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(ty2TyCforFFI)
-- | Construct a type for use by AbstractCore, specifically for use by FFI
ty2TyCforFFI :: EHCOpts -> T.Ty -> C.CTy
%%[[(8 coresysf)
ty2TyCforFFI o t = C.mkCTy o t (tyToSysfTyBase t)
%%][8
ty2TyCforFFI o t = C.mkCTy o t                 t
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen coresysf) hs export(mkTySeq,unTySeq)
-- lift & unlift to TySeq, with mkTySeq . unTySeq == id for singleton sequences
mkTySeq :: Ty -> TySeq
mkTySeq = id -- mkExprSeq
{-# INLINE mkTySeq #-}

unTySeq :: TySeq -> Ty
unTySeq = id -- unExprSeq
{-# INLINE unTySeq #-}
%%]

%%[(8 codegen coresysf) hs export(mkTySeq1,unTySeq1)
mkTySeq1 :: Ty -> TySeq
mkTySeq1 = id -- mkExprSeq1
{-# INLINE mkTySeq1 #-}

unTySeq1 :: TySeq -> Ty
unTySeq1 = id -- unExprSeq1
{-# INLINE unTySeq1 #-}
%%]

%%[(8 codegen coresysf) hs export(mkTyThunk)
mkTySeqThunk :: TySeq -> Ty
mkTySeqThunk t = t -- mkTySeqArrow1Ty [] t
{-# INLINE mkTySeqThunk #-}

mkTyThunk :: Ty -> Ty
mkTyThunk t = t -- mkTyArrow1Ty [] t
{-# INLINE mkTyThunk #-}
%%]

%%[(8 codegen coresysf) hs export(tyUnThunkTySeq,tyUnThunkTy)
tyUnThunkTySeq :: Ty -> TySeq
-- tyUnThunkTySeq (Expr_Arrow (Expr_Seq []) r) = r
tyUnThunkTySeq t                            = t -- tyErr "TyCore.tyUnThunkTySeq"
{-# INLINE tyUnThunkTySeq #-}

tyUnThunkTy :: Ty -> Ty
tyUnThunkTy = unTySeq . tyUnThunkTySeq
{-# INLINE tyUnThunkTy #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deconstruction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

??

%%[(8 codegen coresysf) hs export(CSubst)
type CSubstInfo = CSubstInfo' C.CExpr C.CMetaVal C.CBind C.CBound C.CTy
type CSubst     = CSubst'     C.CExpr C.CMetaVal C.CBind C.CBound C.CTy
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type manipulation: deriving type structures, e.g. from signature to actual application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen tycore) hs export(tyStripL1Args)
-- strip L1 argument bindings of an arrow type
tyStripL1Args :: Ty -> Ty
tyStripL1Args t
  = foldr Expr_Arrow r $ filter (\x -> case unSeq x of {ExprSeq1_L0Bind _ _ -> False ; _ -> True}) as
  where (as,r) = appUnArr t
%%]

%%[(8 codegen coresysf) hs
-- convert bind to bound
tyBindToBound
  :: (ACoreBindAspectKeyS -> MetaLev -> CLbl -> Bool)		-- selection
     -> (HsName -> TyBound -> x)							-- when found
     -> (HsName -> TyBound -> x)							-- when not found
     -> TyBind
     -> x
tyBindToBound sel convYes convNo bind@(C.CBind_Bind n bbs)
  | null bs   = convNo  n $ head bbs
  | otherwise = convYes n $ head bs
  where bs = cbindExtract (noBoundSel {selVal = sel}) bind
%%]

%%[(8 codegen coresysf) hs export(tyL0BindToL1Val)
-- convert type level l<n-1> binding to l<n> argument, used to convert from type signature to value application
tyL0BindToL1Val :: MetaLev -> TyBind -> TyBound
tyL0BindToL1Val mlev
  = tyBindToBound
      (\a m l -> a == acbaspkeyDefault && (m-1) == mlev)
      (\n (C.CBound_Val a m l _) -> C.CBound_Val a m l (acoreVar n))
      (\_ b                      -> b                                  )
%%]
  where tosq (ExprSeq1_L0Bind n   _) = ExprSeq1_L1Val (Expr_Var n)
        tosq (ExprSeq1_L1Bind n   _) = ExprSeq1_L2Val (Expr_Var n)
        tosq s                       = s
        to   (Expr_Seq  s) = Expr_Seq  (map tosq s)
        to   (Expr_Seq1 s) = Expr_Seq1 (    tosq s)
        to   t             = t

%%[(8 codegen coresysf) hs export(tyL0BindToL1Bind)
-- convert type level l0 binding to l1 binding, used to convert from type signature to value abstraction
tyL0BindToL1Bind :: TyBind -> TyBind
tyL0BindToL1Bind
  = tyBindToBound
      (\a _ _ -> a == acbaspkeyDefault)
      (\n (C.CBound_Val a m l e) -> C.CBind_Bind n [C.CBound_Val a (m+1) l e])
      (\n b                      -> C.CBind_Bind n [b                       ])
%%]
  = to t
  where tosq (ExprSeq1_L0Bind n   t) = ExprSeq1_L1Bind n t
        tosq (ExprSeq1_L1Bind n   t) = ExprSeq1_L2Bind n t
        tosq s                       = s
        to   (Expr_Seq  s) = Expr_Seq  (map tosq s)
        to   (Expr_Seq1 s) = Expr_Seq1 (    tosq s)
        to   t             = t


