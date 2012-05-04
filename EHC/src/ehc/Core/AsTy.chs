%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core seen as Ty, for System F generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}Core.AsTy} import({%{EH}Base.Common},{%{EH}Opts.Base})
%%]

%%[(8 codegen) hs import({%{EH}Core})
%%]

%%[(8 codegen) hs import({%{EH}AbstractCore})
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
type Ty     		= SysfTy			-- base ty

-- | A sequence of parameters (for now just a single type)
type TySeq			= SysfTySeq
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion to SysfTy & CTy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 hmtyinfer || hmtyast) hs export(Ty)
-- | The type, represented by a term CExpr
type Ty     		= SysfTy			-- base ty

-- | A sequence of parameters (for now just a single type)
type TySeq			= SysfTySeq
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

%%[(8 codegen coresysf) hs export(CSubst)
type CSubstInfo = CSubstInfo' CExpr CMetaVal CBind CBound Ty
type CSubst     = CSubst'     CExpr CMetaVal CBind CBound Ty
%%]

