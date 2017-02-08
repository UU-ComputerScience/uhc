%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared structures for fitsIn and related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in FitsInCommon, but to avoid cycles is placed in this additional file with common
structures for fitsIn and related functions.

%%[(4 hmtyinfer) module {%{EH}Ty.FitsInCommon2} import({%{EH}Base.Common}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty}, {%{EH}Ty.Utils1}, {%{EH}Substitutable})
%%]

%%[(4 hmtyinfer) import({%{EH}Opts}, {%{EH}VarMp})
%%]
%%[(4 hmtyinfer) import({%{EH}Ty.FIEnv}) export(module {%{EH}Ty.FIEnv})
%%]

%%[(4 hmtyinfer) import(qualified Data.Set as Set)
%%]

%%[(8 hmtyinfer) import({%{EH}AbstractCore})
%%]

%%[(50 hmtyinfer) import(UHC.Util.Utils)
%%]

For debug/trace:
%%[(4 hmtyinfer) import(UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace/debug PP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(ppTyWithFI,ppTyWithFIFO)
%%[[4
ppTyWithFI :: FIIn -> Ty -> PP_Doc
%%][8
ppTyWithFI :: (LookupApply VarMp gm, VarUpdatable Ty gm, VarLookupKey gm ~ VarId) => FIIn' gm -> Ty -> PP_Doc
%%]]
ppTyWithFI fi t =  ppTyS (fiVarMpLoc fi |+> fiVarMp fi) t

%%[[4
ppTyWithFIFO :: FIIn -> FIOut -> Ty -> PP_Doc
%%][8
ppTyWithFIFO :: (LookupApply VarMp gm, VarUpdatable Ty gm, VarLookupKey gm ~ VarId) => FIIn' gm -> FIOut -> Ty -> PP_Doc
%%]]
ppTyWithFIFO fi fo t    =  ppTyS (foVarMp fo |+> fiVarMp fi) t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to configuration/input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).FIIn export(FIIn'(..),FIIn)
data FIIn' globvm
  = FIIn
      { fiFIOpts          ::  !FIOpts               -- options to fitsIn
      , fiUniq            ::  !UID                  -- unique thread
      , fiVarMp           ::   globvm               -- global (type) var bindings
      , fiVarMpLoc        ::  !VarMp                    -- locally introduced (type) var bindings
      , fiExpLTvS         ::  !(Set.Set TyVarId)        -- lhs ty vars for which expansion (via VarMp) is inhibited (already done once)
      , fiExpRTvS         ::  !(Set.Set TyVarId)        -- and rhs
      , fiRank            ::  !Int                  -- rank
      , fiMbInstRank      ::  !(Maybe Int)          -- rank where possible deep instantation did start
      , fiTrace           ::  [PP_Doc]              -- ???? 20080110, must be strict otherwise ghc 6.8.1 generates crashing program ????
%%[[8
      , fiCoeCtx		  ::  CoeCtx				-- the coercion context
      , fiEnv             ::  !FIEnv                -- environment (Gam's,...)
%%]]
      }
%%[[50
      deriving (Typeable)
%%]]

type FIIn = FIIn' VarMp
%%]

%%[(4 hmtyinfer).FIn.emptyFI export(emptyFI',emptyFI)
emptyFI' :: gm -> FIIn' gm
emptyFI' m
  = FIIn
      { fiFIOpts          =   strongFIOpts
      , fiUniq            =   uidStart
      , fiVarMp           =   m
      , fiVarMpLoc        =   emptyVarMp
      , fiExpLTvS         =   Set.empty
      , fiExpRTvS         =   Set.empty
      , fiRank            =   1
      , fiMbInstRank      =   Nothing
      , fiTrace           =   []
%%[[8
      , fiCoeCtx		  =   CoeCtx_Allow
      , fiEnv             =   emptyFE
%%]]
      }

-- emptyFI :: forall gm . FIIn' gm
emptyFI = emptyFI' emptyVarMp
%%]

%%[(4 hmtyinfer) export(fiLookupVar',fiLookupTyVarCyc)
-- lookup a tvar subsequently in 2 VarMps
fiLookupVar' :: (v -> m1 -> Maybe x) -> (v -> m2 -> Maybe x) -> v -> m1 -> m2 -> Maybe x
fiLookupVar' lkup1 lkup2 v m1 m2
  = case lkup1 v m1 of
      Nothing -> lkup2 v m2
      j       -> j

-- lookup a tvar in the VarMps of a FIIn
%%[[4
fiLookupTyVarCyc :: FIIn -> TyVarId -> Maybe Ty
%%][8
fiLookupTyVarCyc :: (VarLookup gm, VarLookupKey gm ~ TyVarId, VarLookupVal gm ~ VarMpInfo) => FIIn' gm -> TyVarId -> Maybe Ty
%%]]
fiLookupTyVarCyc  fi v  =  fiLookupVar' varmpTyLookupCyc varmpTyLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)
%%]

%%[(4 hmtyinfer) export(fiLookupReplaceTyCyc)
-- lookup a possible tvar in the VarMps of a FIIn, the result being the replacement if any
%%[[4
fiLookupReplaceTyCyc :: FIIn -> Ty -> Ty
%%][8
fiLookupReplaceTyCyc :: (VarLookup gm, VarLookupKey gm ~ TyVarId, VarLookupVal gm ~ VarMpInfo) => FIIn' gm -> Ty -> Ty
%%]]
fiLookupReplaceTyCyc  fi t  =  maybe t (maybe t id . fiLookupTyVarCyc fi) $ tyMbVar t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extension of FIIn' used as env for ty betared, canonicalization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(TyBetaRedEnv(..), emptyTyBetaRedEnv, emptyTyBetaRedEnv')
data TyBetaRedEnv gm
  = TyBetaRedEnv
      { tbredFI		:: FIIn' gm
      }

emptyTyBetaRedEnv' fe = TyBetaRedEnv ((emptyFI {fiEnv = fe}) :: FIIn)
emptyTyBetaRedEnv = TyBetaRedEnv emptyFI
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Allowing binding of tvar?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fiAllowTyVarBind)
-- Pre: is a tyvar
fiAllowTyVarBind :: FIIn' gm -> Ty -> Bool
fiAllowTyVarBind fi (Ty_Var v f)   =  f `elem` fioBindCategs (fiFIOpts fi) -- f == TyVarCateg_Plain
%%[[9
                                      && not (v `Set.member` fioDontBind (fiFIOpts fi))
%%]]
fiAllowTyVarBind fi _              =  False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rank
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fiInitInstRank,fiRankEqInstRank,fiUpdRankByPolarity)
fiInitInstRank :: FIIn' gm -> FIIn' gm
fiInitInstRank fi = maybe (fi {fiMbInstRank = Just (fiRank fi)}) (const fi) (fiMbInstRank fi)

fiRankEqInstRank :: FIIn' gm -> Bool
fiRankEqInstRank fi = maybe True (== fiRank fi) (fiMbInstRank fi)

fiUpdRankByPolarity :: Polarity -> FIIn' gm -> FIIn' gm
fiUpdRankByPolarity pol fi = if polIsContravariant pol then fi {fiRank = fiRank fi + 1} else fi
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bind type var
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fiPlusVarMp,fiSetVarMp,fiBindTyVar)
fiPlusVarMp :: VarMp -> FIIn' gm -> FIIn' gm
fiPlusVarMp c fi = fi {fiVarMpLoc = c |+> fiVarMpLoc fi}

fiSetVarMp :: VarMp -> FIIn' gm -> FIIn' gm
fiSetVarMp  c fi = fi {fiVarMpLoc = c}

fiBindTyVar :: TyVarId -> Ty -> FIIn' gm -> FIIn' gm
fiBindTyVar v t = fiPlusVarMp (v `varmpTyUnit` t)
%%]

%%[(9 hmtyinfer) export(fiBindImplsVar)
fiBindImplsVar :: ImplsVarId -> Impls -> FIIn' gm -> FIIn' gm
fiBindImplsVar v i = fiPlusVarMp (v `varmpImplsUnit` i)
%%]

