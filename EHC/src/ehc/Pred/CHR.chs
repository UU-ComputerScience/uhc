%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional Pred admin for Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[9 module {%{EH}Pred.CHR} import({%{EH}CHR},{%{EH}CHR.Constraint})
%%]

%%[9 import(qualified Data.Set as Set,Data.Maybe)
%%]

%%[9 import(EH.Util.AGraph)
%%]

%%[9 import({%{EH}Ty},{%{EH}Cnstr},{%{EH}Ty.FitsIn},{%{EH}Ty.TrieKey})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRMatchable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance CHRMatchable FIIn Pred Cnstr where
  chrMatchTo fi pr1 pr2
    = do { (_,subst) <- fitPredInPred fi pr1 pr2
         ; return subst
         }

instance CHRMatchable FIIn PredOcc Cnstr where
  chrMatchTo fi po1 po2
    = do { subst <- chrMatchTo fi (poPr po1) (poPr po2)
         ; return subst
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Guard, CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Guard(..))
data Guard
  = HasCommonScope	PredOcc PredOcc PredOcc					-- have common scope?
  | IsParentScope	PredOcc PredOcc     					-- is parent scope?
%%]

%%[9
instance CHRCheckable Guard Cnstr where
  chrCheck (HasCommonScope poDst po1 po2)
    = case (poScope poDst,poScope po1,poScope po2) of
        (PredScope_Var vDst, sc1, sc2)
          -> do { scDst <- pscpCommon sc1 sc2
                ; return $ vDst `cnstrScopeUnit` scDst
                }
        _ -> Nothing
  chrCheck (IsParentScope poDst po1)
    = case (poScope poDst,poScope po1) of
        (PredScope_Var vDst, sc1)
          -> do { scDst <- pscpParent sc1
                ; return $ vDst `cnstrScopeUnit` scDst
                }
        _ -> Nothing
%%]

