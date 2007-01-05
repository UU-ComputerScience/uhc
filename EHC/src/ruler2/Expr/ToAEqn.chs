-------------------------------------------------------------------------
-- (A)Eqn as Expr
-------------------------------------------------------------------------

%%[1 hs module (Expr.ToAEqn)
%%]

%%[1 hs export (exprMbAEqnRest, mkExprEqn, exprCheckAEqnForm)
%%]

%%[1 hs import (qualified Data.Set as Set, qualified Data.Map as Map, Common, Expr.Expr)
%%]

%%[1 hs import (Expr.NmS, ARule.ARule, Gam, FmGam)
%%]

-------------------------------------------------------------------------
-- Construct Expr of the form later to be dissected into AEqn
-------------------------------------------------------------------------

%%[1 hs
mkExprEqn :: Expr -> Expr -> Expr
mkExprEqn l r = Expr_AppTop (Expr_Op (nmEql) (Expr_Var (nmEql)) l r)
%%]

-------------------------------------------------------------------------
-- Expr dissected as AEqn (as constructed by mkExprEqn)
-------------------------------------------------------------------------

%%[1 hs
exprCheckAEqnForm :: Expr -> Maybe (Map.Map Expr Expr)
exprCheckAEqnForm e
  = eqn (exprStrip StripBasic e)
  where eqn (Expr_Op n _ l r) | n == nmEql       = Just (lr l r `Map.union` tup l r)
        eqn e                                    = Nothing
        tup (Expr_Op ln _ l1 l2)
            (Expr_Op rn _ r1 r2)  | ln == nmComma && rn == nmComma
                                                 = lr l1 r1 `Map.union` tup l1 r1 `Map.union` tup l2 r2
        tup l r                                  = Map.empty
        lr  l r                                  = Map.fromList $ concat $ [ [(l,r),(r,l)] | l <- ls, r <- rs ]
                                                 where (_,_,ls) = exprStrip' StripBasic l
                                                       (_,_,rs) = exprStrip' StripBasic r
%%]

-------------------------------------------------------------------------
-- Expr as AEqn
-------------------------------------------------------------------------

%%[1 hs
exprMbAEqnRest :: Expr -> Maybe (AEqn,[Expr],FmGam Expr)
exprMbAEqnRest expr
  = eE (exprStrip StripBasic expr)
  where eE (Expr_Op n _ d s) | n == nmEql
          = do (d',ed,gd) <- dE (exprStrip StripBasic d) False
               (s'      ) <- sE s
               return (AEqn_Eqn d' s', ed, gd)
        eE e                                        = Nothing
        dE (Expr_AVar n)        _                   = return (AEqnDest_One n,[],emptyGam)
        dE (Expr_Retain (Expr_AVar (ANm_Loc n p))) _= return (AEqnDest_One (ANm_Loc n (AtRetain:p)),[],emptyGam)
        dE (Expr_Var n)         _                   = vE [] n
        dE (Expr_Retain (Expr_Var n)) _             = vE [AtRetain] n
        dE (Expr_StrAsIs s)     _                   = nE [] s
        dE (Expr_Retain (Expr_StrAsIs s)) _         = nE [AtRetain] s
        dE (Expr_Retain e)      p                   = dE e p
        dE (Expr_SelTop e)      p                   = dE e p
        dE e@(Expr_Op n _ _ _)  p | n == nmComma    = tE e
        dE (Expr_Sel e (Just s)) _
          = do ne <- dsE e
               ns <- dsE s
               return (AEqnDest_One (ANm_Node ne ns),[],emptyGam)
        dE _                    False               = Nothing
        dE e                    True                = tE e
        dsE (Expr_AppTop e)                         = dsE e
        dsE (Expr_Paren e)                          = dsE e
        dsE (Expr_AVar (ANm_Fld n))                 = return n
        dsE (Expr_Var n)                            = return n
        dsE _                                       = Nothing
        tE e@(Expr_Op n _ _ _) | n == nmComma
          = do (dL,eo,go) <- oE e
               return (AEqnDest_Many dL,eo,go)
        tE e
          = return (AEqnDest_One n,[mkExprEqn (Expr_AppTop e) (Expr_AVar n)],emptyGam)
          where n = flip ANm_Loc [] . Nm . nmShowAG . foldr nmApd nmWild . take 2 . Set.toList $ exprNmS e
        oE (Expr_Op n _ e1 e2) | n == nmComma
          = do (e1',ed,gd) <- dE (exprStrip StripBasic e1) True
               (e2',eo,go) <- oE e2
               return (e1' : e2', ed ++ eo, gd `gu` go)
        oE e
          = do (e',ee,ge) <- dE (exprStrip StripBasic e) True
               return ([e'],ee,ge)
        sE e                                        = return (AExpr_Expr e)
        vE props n
          = if n == nmWild
            then return (AEqnDest_One ANm_Wild,[],emptyGam)
            else let a = ANm_Loc (Nm (nmShowAG n)) props
                 in  return (AEqnDest_One a,[],n `gs` Expr_AVar a)
        nE props n
          = fmap (\n -> (AEqnDest_One n,[],emptyGam)) m
          where m = case n of
                      ('@':'l':'o':'c':'.':nm)  -> Just (ANm_Loc  (Nm nm) props)
                      (    'l':'o':'c':'.':nm)  -> Just (ANm_Loc  (Nm nm) props)
                      ('@':'l':'h':'s':'.':nm)  -> Just (ANm_Lhs  (Nm nm) [])
                      (    'l':'h':'s':'.':nm)  -> Just (ANm_Lhs  (Nm nm) [])
                      s | s == strUnd           -> Just (ANm_Wild)
                        | otherwise             -> Nothing
        gu = fmGamUnion
        gs n e = fmGamFromList' FmAG [(n,e)]

%%]
