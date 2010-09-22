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

%%[(11 hmtyinfer) module {%{EH}Ty.Trf.BetaReduce} import({%{EH}Base.Builtin}, {%{EH}Base.Common}, {%{EH}Opts}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty.FitsInCommon2}, {%{EH}Ty}, {%{EH}Gam.Full}, {%{EH}Substitutable}, {%{EH}VarMp})
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
%%% Beta reduction extra info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(11 hmtyinfer) export(TyBetaRedExtra(..), emptyTyBetaRedExtra)
-- | expansion lookahead info
type TyBetaRedLookAheadExpansion
  = ( Ty				-- type function
    , [Ty]				-- arguments
    , Maybe Ty			-- function in ty looked up as if it were to be used for expansion
    )

-- | extra result info from betared
data TyBetaRedExtra
  = TyBetaRedExtra
      { tybetaredextraTracePPL		:: [PP_Doc]
      , tybetaredextraExpandedTo	:: Maybe TyBetaRedLookAheadExpansion	-- 1 expansion step lookahead type function + args
      }

emptyTyBetaRedExtra :: TyBetaRedExtra
emptyTyBetaRedExtra = TyBetaRedExtra [] Nothing
%%]

20100922 AD; Note: it is somewhat a mystery why this is not symmetric but IOBase will fail compilation.
So, for now, it therefore is somewhat a hack...

%%[(11 hmtyinfer) export(betaRedIsOkFitsinCombi)
-- | check for a valid combi using lookahead info of next expansion.
--   Basically prevent synonyms and lambdas from being bound, but forced to be expanded
betaRedIsOkFitsinCombi :: (Ty -> Bool) -> TyBetaRedExtra -> TyBetaRedExtra -> Bool
betaRedIsOkFitsinCombi isBoundable
                       (TyBetaRedExtra {tybetaredextraExpandedTo = Just (fl,al,_    )})		-- a tvar
                       (TyBetaRedExtra {tybetaredextraExpandedTo = Just (fr,ar,mbExp)})		-- cannot be bound/matched against non expanded synonym/lambda
                       | isBoundable fl && not (null ar || null al) && (tyIsLam fr || isJust mbExp)
                       = False
{-
betaRedIsOkFitsinCombi isBoundable
                       (TyBetaRedExtra {tybetaredextraExpandedTo = Just (fl,al,mbExp)})
                       (TyBetaRedExtra {tybetaredextraExpandedTo = Just (fr,ar,_    )})
                       | isBoundable fr && not (null ar || null al) && (tyIsLam fl || isJust mbExp)
                       = False
-}
betaRedIsOkFitsinCombi _ _ _
                       = True
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
-- | lookup the type used as if in function position of type application
betaRedTyFunLookup :: FIIn -> TyBetaRedLkup -> Ty -> Maybe Ty
betaRedTyFunLookup fi lkup funTy
  = do nm <- tyMbCon funTy
       t' <- lkup fi nm
       case t' of
          Ty_Con nm' | nm == nm' -> Nothing
          f                      -> Just f
%%]

%%[(11 hmtyinfer)
-- | get the lookahead for reduction
betaRedTyLookAhead :: FIIn -> TyBetaRedLkup -> Ty -> TyBetaRedLookAheadExpansion
betaRedTyLookAhead fi lkup ty
  = unpack ty
  where unpack t = (f', as, betaRedTyFunLookup fi lkup f')
          where (f,as) = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) t
                f' = tyUnAnn $ fiLookupReplaceTyCyc fi f
%%]

%%[(11 hmtyinfer)
-- | one expansion step of type level beta reduction
tyBetaRed1 :: FIIn -> TyBetaRedLkup -> Either Ty TyBetaRedLookAheadExpansion -> Maybe (Ty,TyBetaRedExtra)
tyBetaRed1 fi lkup tyOrFunAndArgs
  = eval (either (betaRedTyLookAhead fi lkup) id tyOrFunAndArgs)
  where -- lambda expression: take body and substitute arguments
        eval (lam@(Ty_Lam fa b), args, _)
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
        eval (Ty_Con nm, [arg], _)
          | nm == hsnPolNegation
              = case tyUnAnn $ fiLookupReplaceTyCyc fi fun' of
                  Ty_Con nm
                    | nm == hsnPolNegation   -> mkres (head args')
                    | otherwise              -> mkres (polOpp arg)
                  _ -> Nothing
              where
                (fun',args') = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) arg
%%]]

        -- looked up in the environment
        eval (_, args, Just funExp)
              = mkres $ mkApp $ funExp : args

        -- no expansion possible
        eval _ = Nothing

        -- utils
        mkres t  = Just ( t
                        , emptyTyBetaRedExtra
                            { tybetaredextraExpandedTo = Just $ betaRedTyLookAhead fi lkup t
                            , tybetaredextraTracePPL   = [trfitIn "tylam" ("from:" >#< ppTyWithFI fi (pack tyOrFunAndArgs) >-< "to  :" >#< ppTyWithFI fi t)]
                            }
                        )
        pack = either id (\(f,as,_) -> mkApp (f:as))
%%]

%%[(11 hmtyinfer) export(tyBetaRed,tyBetaRedAndInit)
tyBetaRed' :: FIIn -> TyBetaRedLkup -> Either Ty TyBetaRedLookAheadExpansion -> [(Ty,TyBetaRedExtra)]
tyBetaRed' fi lkup tyOrFunArgs
  = case tyBetaRed1 fi lkup tyOrFunArgs of
      Just tf@(ty,e) -> tf : tyBetaRed' fi lkup (maybe (Left ty) Right $ tybetaredextraExpandedTo e)
      _              -> []

tyBetaRed :: FIIn -> TyBetaRedLkup -> Ty -> [(Ty,TyBetaRedExtra)]
tyBetaRed fi lkup ty = tyBetaRed' fi lkup (Left ty)

tyBetaRedAndInit :: FIIn -> TyBetaRedLkup -> Ty -> [(Ty,TyBetaRedExtra)]
tyBetaRedAndInit fi lkup ty
  = (ty, emptyTyBetaRedExtra {tybetaredextraExpandedTo = Just l}) : tyBetaRed' fi lkup (Right l)
  where l = betaRedTyLookAhead fi lkup ty
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

Just expand, recursively, without intermediate external expanding.

%%[(11 hmtyinfer) export(tyBetaRedFull)
tyBetaRedFull :: FIIn -> VarMp -> Ty -> Ty
tyBetaRedFull fi varmp ty
  = maybe ty fst $ sub ty
  where fi' = fi {fiVarMp = varmp}
        sub = \t -> tyBetaRedFullMb fi' betaRedTyLookup sub $ varmp |=> t
%%]

