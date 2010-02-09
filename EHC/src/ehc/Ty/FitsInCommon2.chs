%%[0
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

%%[(4 hmtyinfer) import({%{EH}Base.Opts}, {%{EH}VarMp})
%%]
%%[(4 hmtyinfer) import({%{EH}Ty.FIEnv}) export(module {%{EH}Ty.FIEnv})
%%]

%%[(4 hmtyinfer) import(qualified Data.Set as Set)
%%]

For debug/trace:
%%[(4 hmtyinfer) import(EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace/debug PP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(ppTyWithFI,ppTyWithFIFO)
ppTyWithFI :: FIIn -> Ty -> PP_Doc
ppTyWithFI fi t =  ppTyS (fiVarMpLoc fi |=> fiVarMp fi) t

ppTyWithFIFO :: FIIn -> FIOut -> Ty -> PP_Doc
ppTyWithFIFO fi fo t    =  ppTyS (foVarMp fo |=> fiVarMp fi) t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to configuration/input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).FIIn export(FIIn(..))
data FIIn   =  FIIn     {  fiFIOpts          ::  !FIOpts				-- options to fitsIn
                        ,  fiUniq            ::  !UID					-- unique thread
                        ,  fiVarMp           ::  !VarMp					-- global (type) var bindings
                        ,  fiVarMpLoc        ::  !VarMp					-- locally introduced (type) var bindings
                        ,  fiExpLTvS         ::  !(Set.Set TyVarId)		-- lhs ty vars for which expansion (via VarMp) is inhibited (already done once)
                        ,  fiExpRTvS         ::  !(Set.Set TyVarId)		-- and rhs
                        ,  fiRank            ::  !Int					-- rank
                        ,  fiMbInstRank      ::  !(Maybe Int)			-- rank where possible deep instantation did start
%%[[8
                        ,  fiEnv             ::  !FIEnv					-- environment (Gam's,...)
%%]]
                        ,  fiTrace           ::  [PP_Doc]       -- ???? 20080110, must be strict otherwise ghc 6.8.1 generates crashing program ????
                        }
%%]

%%[(4 hmtyinfer).FIn.emptyFI export(emptyFI)
emptyFI     =  FIIn     {  fiFIOpts          =   strongFIOpts
                        ,  fiUniq            =   uidStart
                        ,  fiVarMp           =   emptyVarMp
                        ,  fiVarMpLoc        =   emptyVarMp
                        ,  fiExpLTvS         =   Set.empty
                        ,  fiExpRTvS         =   Set.empty
                        ,  fiRank            =   1
                        ,  fiMbInstRank      =   Nothing
%%[[8
                        ,  fiEnv             =   emptyFE
%%]]
                        ,  fiTrace           =   []
                        }
%%]

%%[(4 hmtyinfer) export(fiLookupVar',fiLookupTyVarCyc)
fiLookupVar' :: (v -> VarMp -> Maybe x) -> v -> VarMp -> VarMp -> Maybe x
fiLookupVar' lkup v m1 m2
  = case lkup v m1 of
      Nothing -> lkup v m2
      j       -> j

fiLookupTyVarCyc :: FIIn -> TyVarId -> Maybe Ty
fiLookupTyVarCyc  fi v  =  fiLookupVar' varmpTyLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)
%%]
fiLookupTyVarCyc :: FIIn -> TyVarId -> Maybe Ty
fiLookupTyVarCyc  fi v | fiVarIsExpanded v fi =  Nothing
                       | otherwise            =  fiLookupVar' varmpTyLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rank
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fiInitInstRank,fiRankEqInstRank,fiUpdRankByPolarity)
fiInitInstRank :: FIIn -> FIIn
fiInitInstRank fi = maybe (fi {fiMbInstRank = Just (fiRank fi)}) (const fi) (fiMbInstRank fi)

fiRankEqInstRank :: FIIn -> Bool
fiRankEqInstRank fi = maybe True (== fiRank fi) (fiMbInstRank fi)

fiUpdRankByPolarity :: Polarity -> FIIn -> FIIn
fiUpdRankByPolarity pol fi = if polIsContravariant pol then fi {fiRank = fiRank fi + 1} else fi
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bind type var
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fiPlusVarMp,fiSetVarMp,fiBindTyVar)
fiPlusVarMp :: VarMp -> FIIn -> FIIn
fiPlusVarMp c fi = fi {fiVarMpLoc = c |+> fiVarMpLoc fi}

fiSetVarMp :: VarMp -> FIIn -> FIIn
fiSetVarMp  c fi = fi {fiVarMpLoc = c}

fiBindTyVar :: TyVarId -> Ty -> FIIn -> FIIn
fiBindTyVar v t = fiPlusVarMp (v `varmpTyUnit` t)
%%]

