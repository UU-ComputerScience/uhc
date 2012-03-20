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

%%[(11 hmtyinfer) export(TyBetaRedOut,mkDfltTyBetaRedOut,TyBetaRedOut'(..))
data TyBetaRedOut' x
  = TyBetaRedOut
      { tbroutRes			:: x
      , tbroutVarMp			:: VarMp
      , tbroutTracePPL		:: [PP_Doc]
      , tbroutExpandedTo	:: Maybe TyBetaRedLookAheadExpansion	-- 1 expansion step lookahead type function + args
      }

type TyBetaRedOut    = TyBetaRedOut' Ty

mkDfltTyBetaRedOut :: x -> TyBetaRedOut' x
mkDfltTyBetaRedOut = emptyTyBetaRedOut'
%%]

%%[(11 hmtyinfer) export(emptyTyBetaRedOut',emptyTyBetaRedOut)
emptyTyBetaRedOut' :: x -> TyBetaRedOut' x
emptyTyBetaRedOut' x = TyBetaRedOut x emptyVarMp [] Nothing

emptyTyBetaRedOut :: TyBetaRedOut' Ty
emptyTyBetaRedOut = emptyTyBetaRedOut' Ty_Any
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction extra info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(11 hmtyinfer)
-- | expansion lookahead info
type TyBetaRedLookAheadExpansion
  = ( Ty							-- type function
    , [Ty]							-- arguments
    , Maybe TyBetaRedOut			-- function in ty looked up as if it were to be used for expansion
    )

%%]

20100922 AD; Note: it is somewhat a mystery why this is not symmetric but IOBase will fail compilation.
So, for now, it therefore is somewhat a hack...

%%[(11 hmtyinfer) export(betaRedIsOkFitsinCombi)
-- | check for a valid combi using lookahead info of next expansion.
--   Basically prevent synonyms and lambdas from being bound, but forced to be expanded
betaRedIsOkFitsinCombi :: (Ty -> Bool) -> TyBetaRedOut -> TyBetaRedOut -> Bool
betaRedIsOkFitsinCombi isBoundable
                       (TyBetaRedOut {tbroutExpandedTo = Just (fl,al,_    )})		-- a tvar
                       (TyBetaRedOut {tbroutExpandedTo = Just (fr,ar,mbExp)})		-- cannot be bound/matched against non expanded synonym/lambda
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
type TyBetaRedLkup gm = TyBetaRedEnv gm -> HsName -> Maybe TyBetaRedOut

betaRedTyLookup :: TyBetaRedLkup gm
betaRedTyLookup fi nm = fmap (mkDfltTyBetaRedOut . tgiTy) $ tyGamLookup nm $ feTyGam $ fiEnv $ tbredFI fi
%%]

%%[(11 hmtyinfer)
-- | lookup the type used as if in function position of type application
betaRedTyFunLookup :: TyBetaRedEnv gm -> TyBetaRedLkup gm -> Ty -> Maybe TyBetaRedOut
betaRedTyFunLookup fi lkup funTy
  = do nm <- tyMbCon funTy
       t' <- lkup fi nm
       case tbroutRes t' of
          Ty_Con nm' | nm == nm' -> Nothing
          f                      -> Just (mkDfltTyBetaRedOut f)
%%]

%%[(11 hmtyinfer)
-- | get the lookahead for reduction
betaRedTyLookAhead :: VarLookup gm TyVarId VarMpInfo => TyBetaRedEnv gm -> TyBetaRedLkup gm -> Ty -> TyBetaRedLookAheadExpansion
betaRedTyLookAhead renv lkup ty
  = unpack ty
  where unpack t = (f', as, betaRedTyFunLookup renv lkup f')
          where (f,as) = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) t
                f' = tyUnAnn $ fiLookupReplaceTyCyc fi f
                fi = tbredFI renv
%%]

%%[(11 hmtyinfer)
-- | one expansion step of type level beta reduction
tyBetaRed1
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> TyBetaRedLkup gm -> Either Ty TyBetaRedLookAheadExpansion
     -> Maybe TyBetaRedOut
tyBetaRed1 renv lkup tyOrFunAndArgs
  = eval (either (betaRedTyLookAhead renv lkup) id tyOrFunAndArgs)
  where -- lambda expression: take body and substitute arguments
        eval (lam@(Ty_Lam fa b), args, _)
          | lamLen <= argLen
              = mkres (mkApp (subst `varUpd` lamBody : drop lamLen args))
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
              = mkres $ mkApp $ tbroutRes funExp : args

        -- no expansion possible
        eval _ = Nothing

        -- utils
        mkres t  = Just ( (emptyTyBetaRedOut' t)
                            { tbroutExpandedTo = Just $ betaRedTyLookAhead renv lkup t
                            , tbroutTracePPL   = [trfitIn "tylam" ("from:" >#< ppTyWithFI fi (pack tyOrFunAndArgs) >-< "to  :" >#< ppTyWithFI fi t)]
                            }
                        )
        pack = either id (\(f,as,_) -> mkApp (f:as))
        fi = tbredFI renv
%%]

%%[(11 hmtyinfer) export(tyBetaRed,tyBetaRedAndInit)
tyBetaRed'
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> TyBetaRedLkup gm -> Either Ty TyBetaRedLookAheadExpansion
     -> [TyBetaRedOut]
tyBetaRed' renv lkup tyOrFunArgs
  = case tyBetaRed1 renv lkup tyOrFunArgs of
      Just re -> re : tyBetaRed' renv lkup (maybe (Left (tbroutRes re)) Right $ tbroutExpandedTo re)
      _       -> []

tyBetaRed
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> TyBetaRedLkup gm -> Ty
     -> [TyBetaRedOut]
tyBetaRed renv lkup ty = tyBetaRed' renv lkup (Left ty)

tyBetaRedAndInit
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> TyBetaRedLkup gm -> Ty
     -> [TyBetaRedOut]
tyBetaRedAndInit renv lkup ty
  = ((emptyTyBetaRedOut' ty) {tbroutExpandedTo = Just l}) : tyBetaRed' renv lkup (Right l)
  where l = betaRedTyLookAhead renv lkup ty
%%]

Reduce fully (upto expansion limit) an outer layer of type synonyms,
expanding the inner layer with redSub, only if the outer layer has been
replaced.
Additional substitutions found are assumed to be non-contradictory, so threading is not done.

%%[(11 hmtyinfer) export(tyBetaRedFullMb)
tyBetaRedFullMb
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> TyBetaRedLkup gm -> (Ty -> Maybe TyBetaRedOut) -> Ty
     -> Maybe TyBetaRedOut
tyBetaRedFullMb renv lkup redSub ty
  = fmap reda $ choose ty $ redl ty
  where env = fiEnv $ tbredFI renv
        lim     = ehcOptTyBetaRedCutOffAt $ feEHCOpts env
        redl ty = take lim $ tyBetaRed renv lkup ty
        reda re = if null (catMaybes as')
                  then mkDfltTyBetaRedOut ty
                  else let as'' = zipWith (\t mt -> maybe (mkDfltTyBetaRedOut t) id mt) as as'
                       in   emptyTyBetaRedOut {tbroutRes = mk f $ map tbroutRes as'', tbroutVarMp = varmpUnions $ map tbroutVarMp as''}
                where (f,as,mk) = tyDecomposeMk ty
                      as' = map redSub as
                      ty  = tbroutRes re
        choose a [] = Nothing
        choose a as = Just (last as)
%%]

Just expand, recursively, without intermediate external expanding.

%%[(11 hmtyinfer) export(tyBetaRedFull)
tyBetaRedFull
  :: (VarLookup gm TyVarId VarMpInfo, VarLookupCmb VarMp gm)
     => TyBetaRedEnv gm -> VarMp -> Ty
     -> Ty
tyBetaRedFull renv varmp ty
  = maybe ty tbroutRes $ sub ty
  where fi  = tbredFI renv
        fi' = fi {fiVarMp = varmp}
        sub = \t -> tyBetaRedFullMb (renv {tbredFI = fi'}) betaRedTyLookup sub $ varmp `varUpd` t
%%]

