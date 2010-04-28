%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Beta reduce types
\begin{itemize}
\item Type lambdas \verb|Ty_Lam| are applied to their arguments.
\item Type constants \verb|Ty_Con| are expanded to their definition.
\item (Polarity inferencing only) Special cases are treated: negation of negation, in particular.
\end{itemize}

All is paremeterized with lookup for type constants.
The reduction is limited by the expansion cutoff limit indicated by option @ehcOptTyBetaRedCutOffAt@.
%%]

%%[(11 hmtyinfer) module {%{EH}Ty.Trf.BetaReduce} import({%{EH}Base.Builtin}, {%{EH}Base.Common}, {%{EH}Base.Opts}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty.FitsInCommon2}, {%{EH}Ty}, {%{EH}Gam.Full}, {%{EH}Substitutable}, {%{EH}VarMp})
%%]

%%[(11 hmtyinfer) import(Data.Maybe)
%%]

For debug/trace:
%%[(11 hmtyinfer) import(EH.Util.Pretty)
%%]
%%[(11 hmtyinfer) import({%{EH}Base.Debug})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(11 hmtyinfer) export(TyBetaRedOut,mkDfltTyBetaRedOut,TyBetaRedOut')
type TyBetaRedOut' x = (x,VarMp)
type TyBetaRedOut    = TyBetaRedOut' Ty

mkDfltTyBetaRedOut :: x -> TyBetaRedOut' x
mkDfltTyBetaRedOut x = (x,emptyVarMp)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(11 hmtyinfer) export(TyBetaRedLkup,betaRedTyLookup)
type TyBetaRedLkup = FIIn -> HsName -> Maybe Ty

betaRedTyLookup :: TyBetaRedLkup
betaRedTyLookup fi nm = fmap tgiTy $ tyGamLookup nm $ feTyGam $ fiEnv fi
%%]

%%[(11 hmtyinfer)
tyBetaRed1 :: FIIn -> TyBetaRedLkup -> Ty -> Maybe (Ty,[PP_Doc])
tyBetaRed1 fi lkup tp
  = eval (fiLookupReplaceTyCyc fi fun) args
  where (fun,args) = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) tp
        eval lam@(Ty_Lam fa b) args
          | lamLen <= argLen
              = mkres (mkApp (subst |=> lamBody : drop lamLen args))
          | otherwise
              = Nothing
          where (lamArgs,lamBody) = tyLamArgsRes lam
                lamLen = length lamArgs
                argLen = length args
                subst  = assocTyLToVarMp (zip lamArgs args)
%%[[17
        -- normalization for polarity types
        -- * removes double negations
        -- * removes negation on 'basic' polarities
        eval (Ty_Con nm) [arg]
          | nm == hsnPolNegation
              = case fiLookupReplaceTyCyc fi fun' of
                  Ty_Con nm
                    | nm == hsnPolNegation   -> mkres (head args')
                    | otherwise              -> mkres (polOpp arg)
                  _ -> Nothing
              where
                (fun',args') = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) arg
%%]]
        eval (Ty_Con nm) aa
              = case lkup fi nm of
                  Just ty -> case ty of
                               Ty_Con nm' | nm == nm' -> Nothing
                               f                      -> mkres (mkApp (f:aa))
                  Nothing -> Nothing
        eval _ _ = Nothing
        mkres t  = Just (t,[trfitIn "tylam" ("from:" >#< ppTyWithFI fi tp >-< "to  :" >#< ppTyWithFI fi t)])
%%]

%%[(11 hmtyinfer) export(tyBetaRed)
tyBetaRed :: FIIn -> TyBetaRedLkup -> Ty -> [(Ty,[PP_Doc])]
tyBetaRed fi lkup ty
  = case tyBetaRed1 fi lkup ty of
      Just tf@(ty,_) -> tf : tyBetaRed fi lkup ty
      _              -> []
%%]

Reduce fully (upto expansion limit) an outer layer of type synonyms,
expanding the inner layer with redSub, only if the outer layer has been
replaced.
Additional substitutions found are assumed to be non-contradictory, so threading is not done.

%%[(11 hmtyinfer) export(tyBetaRedFullMb)
tyBetaRedFullMb :: FIIn -> TyBetaRedLkup -> (Ty -> Maybe TyBetaRedOut) -> Ty -> Maybe TyBetaRedOut
tyBetaRedFullMb fi lkup redSub ty
  = fmap reda $ choose ty $ redl ty
  where env = fiEnv fi
        lim     = ehcOptTyBetaRedCutOffAt $ feEHCOpts env
        redl ty = take lim $ map fst $ tyBetaRed fi lkup ty
        reda ty = if null (catMaybes as')
                  then mkDfltTyBetaRedOut ty
                  else let (as'',ms) = unzip $ zipWith (\t mt -> maybe (mkDfltTyBetaRedOut t) id mt) as as'
                       in  (mk f as'', varmpUnions ms)
                where (f,as,mk) = tyDecomposeMk ty
                      as' = map redSub as
        choose a [] = Nothing
        choose a as = Just (last as)

%%]

