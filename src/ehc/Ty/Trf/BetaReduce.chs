%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11 module {%{EH}Ty.Trf.BetaReduce} import({%{EH}Base.Builtin}, {%{EH}Base.Common}, {%{EH}Base.Opts}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty.FitsInCommon2}, {%{EH}Ty}, {%{EH}Gam}, {%{EH}Substitutable}, {%{EH}VarMp})
%%]

%%[11 import(Data.Maybe)
%%]

For debug/trace:
%%[11 import(EH.Util.Pretty)
%%]
%%[11 import({%{EH}Base.Debug})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11 export(tyBetaRed1)
tyBetaRed1 :: FIIn -> Ty -> Maybe (Ty,[PP_Doc])
tyBetaRed1 fi tp
  = eval fun args
  where (fun,args) = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) tp
        eval lam@(Ty_Lam fa b) args
          | lamLen <= argLen
              = mkres (mkApp (subst |=> lamBody : drop lamLen args))
          | otherwise
              = Nothing
          where (lamArgs,lamBody) = tyLamArgsRes lam
                lamLen = length lamArgs
                argLen = length args
                subst  = assocLToVarMp (zip lamArgs args)
%%[[17
        -- normalization for polarity types
        -- * removes double negations
        -- * removes negation on 'basic' polarities
        eval (Ty_Con nm) [arg]
          | nm == hsnPolNegation
              = case fun' of
                  Ty_Con nm
                    | nm == hsnPolNegation   -> mkres (head args')
                    | otherwise              -> mkres (polOpp arg)
                  _ -> Nothing
              where
                (fun',args') = tyAppFunArgsWithLkup (fiLookupTyVarCyc fi) arg
%%]]
        eval (Ty_Con nm) aa
              = case tyGamLookup nm tyGam of
                  Just tgi -> case tgiTy tgi of
                                Ty_Con nm' | nm == nm' -> Nothing
                                f                      -> mkres (mkApp (f:aa))
                  Nothing  -> Nothing
        eval _ _ = Nothing
        mkres t  = Just (t,[trfitIn "tylam" ("from:" >#< ppTyWithFI fi tp >-< "to  :" >#< ppTyWithFI fi t)])
        tyGam    = feTyGam $ fiEnv fi
%%]

%%[11 export(tyBetaRed)
tyBetaRed :: FIIn -> Ty -> [(Ty,[PP_Doc])]
tyBetaRed fi ty
  = case tyBetaRed1 fi ty of
      Just tf@(ty,_) -> tf : tyBetaRed fi ty
      _              -> []
%%]

Reduce fully (upto expansion limit) an outer layer of type synonyms,
expanding the inner layer with redSub, only if the outer layer has been
replaced.

%%[11 export(tyBetaRedFullMb)
tyBetaRedFullMb :: FIIn -> (Ty -> Maybe Ty) -> Ty -> Maybe Ty
tyBetaRedFullMb fi redSub ty
  = fmap reda $ choose ty $ redl ty
  where env = fiEnv fi
        lim     = ehcOptTyBetaRedCutOffAt $ feEHCOpts env
        redl ty = take lim $ map fst $ tyBetaRed fi ty
        reda ty = if null (catMaybes as') then ty else mk f (zipWith (\t mt -> maybe t id mt) as as')
                where (f,as,mk) = tyDecomposeMk ty
                      as' = map redSub as
        choose a [] = Nothing
        choose a as = Just (last as)

%%]

