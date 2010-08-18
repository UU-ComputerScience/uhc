%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AnaDomain utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}AnaDomain.Utils} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) hs import({%{EH}AnaDomain},{%{EH}AnaDomain.Trf.Subst},{%{EH}AnaDomain.Ftv})
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set, Data.Array, Data.Maybe)
%%]

%%[(8 codegen) hs import(EH.Util.Utils)
%%]

%%[(8 codegen) hs import(Control.Monad.State hiding (join), Control.Applicative)
%%]

-- debug only
%%[(8 codegen) hs import({%{EH}Base.Debug},EH.Util.Pretty,{%{EH}AnaDomain.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quantification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(relevtyQuant)
relevtyQuant
  :: RVarMp -> RelevQualS -> RelevTy
     -> ( RelevTy
        , RVarMp	-- extra subst
        -- , [UID]		-- quantified over
        -- , [UID]		-- remaining
        )
relevtyQuant m qs t
  = case funTy of
      t'@(RelevTy_Fun _ _ as r)
         -> ( RelevTy_Fun quantOver qs'' as r, solveVarMp
            -- , quantOver, Set.toList $ ftvSet (solveVarMp |=> qs) `Set.difference` ftvTQ
            )
         where ftvT = -- (\x -> tr "relevtyQuant.ftvT" (t' >#< show x) x) $
                      ftvSet t'
               (qs',solveVarMp) = assSolve ftvT $ Set.map (m |=>) qs
               qs'' = Set.toList qs'
               ftvTQ = ftvT `Set.union` ftvSet qs''
               quantOver = Set.toList ftvTQ
      t' -> ( t', emptyVarMp
            -- , [], []
            )
  where funTy = case m |=> t of
                  t'@(RelevTy_Fun _ _ _ _) -> t'
                  t'                       -> RelevTy_Fun [] [] [] t'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
instance Substitutable RelevTy UID RVarMp where
  (|=>) = relevtyAppVarLookup
  ftvSet = relevtyFtv
%%]

%%[(8 codegen) hs
instance Substitutable RelevQual UID RVarMp where
  (|=>) = relevqualAppVarLookup
  ftvSet = relevqualFtv
%%]

%%[(8 codegen) hs
instance Substitutable RelevCoe UID RVarMp where
  (|=>) = relevcoeAppVarLookup
  ftvSet = relevcoeFtv
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat.solving doclatex
Solving takes a set of RelevQual and tries to get rid of as many as possible.
The remainder will become part of a signature.
The goal is to minimize the nr of variables,
exploiting:
\begin{itemize}
\item the fact that variables not visible (called reducable variables) in the final signature may/must be removed,
\item and can be freely chosen to accomplish this.
\end{itemize}
The solver uses the following heuristics:
\begin{itemize}
\item reduce 2 quals into 1 when they share reducable variables
\item reduce 1
\end{itemize}
Both possibly yielding a substitution.
%%]

%%[(8 codegen) hs
type WL = Map.Map UID (Set.Set Int)

data ASS
  = ASS
      { assWl		:: !WL
      , assWlRedo	::  WL
      , assQm		:: !(Map.Map Int RelevQual)
      , assVm		:: !RVarMp
      , assProgress	:: !Bool
      , assSolved	:: !(Set.Set Int)
      , assReduced	:: !(Set.Set Int)
      }

assSolve :: UIDS -> RelevQualS -> (RelevQualS,RVarMp)
assSolve bound qualS
  = maybe (qualS, emptyVarMp)
          (\ass -> ( Set.map (qual $ assQm ass) (assReduced ass)
                   -- Set.fromList [ q | q <- Map.elems $ assQm ass, ftvSet q `Set.isSubsetOf` bound ]
                   , assVm ass
          )        )
    -- $ tr "assSolve" (pp (show bound))
    $ solve (ASS wlInit Map.empty initQualM emptyVarMp False Set.empty Set.empty)
  where -- solver
        solve ass@(ASS {assWl=wl, assWlRedo=wlRedo, assQm=qualM})
          | check_sz && not (Set.null isReduced)
            = -- tr "solve.assReduced" (show isReduced >-< show (Set.map (qual qualM) isReduced)) $
              solve (ass {assReduced = Set.union isReduced $ assReduced ass, assProgress = True, assWl = Map.insert v isToSolve wl'})
          | check_sz_vUnbound && isJust mb2
            = -- tr ("solve: wlSz > 0 && isJust mb2") (show wl >-< show wl' >-< show (assQm ass) >-< show (assQm $ fromJust mb2)) $
              solve ((fromJust mb2) {assProgress = True})
          -- | check_sz_vUnbound && isJust mb1
          --   = -- tr ("solve: wlSz > 0 && isJust mb1") (show wl >-< show wl' >-< show (assQm ass) >-< show (assQm $ fromJust mb1)) $
          --     solve ((fromJust mb1) {assProgress = True})
          | check_sz
            = -- tr ("solve: wlSz > 0(" ++ show wlSz ++ ")") (Map.size wl' >#< show wl') $
              solve (ass {assWl=wl', assWlRedo= Map.insert v is $ assWlRedo ass})
          | assProgress ass && Map.size wlRedo > 0
            = -- tr ("solve: assProgress ass && Map.size wlRedo > 0") (Map.size wl >#< show wl) $
              solve (ass {assWl= wlRedo, assWlRedo= Map.empty, assProgress = False})
          | not (Set.null $ assSolved ass)
            = Just ass
          | otherwise
            = -- tr ("solve: term") (show wl >-< show wlRedo) $
              Nothing
          where wlSz = Map.size wl
                ((v,is), wl') = -- (\x@((v,is), wl') -> tr "Map.deleteFindMin" (v >#< pp (show is)) x) $ 
                                Map.deleteFindMin wl
                (isReduced,isToSolve) = Set.partition (\i -> ftvSet (qual qualM i) `Set.isSubsetOf` bound) is
                is'  = (Set.toList is,[])
                ass' = ass {assWl=wl'}
                
                -- the real solving work attempts
                mb2  = solve2 v is' Nothing ass'
                mb1  = solve1 v is' ass'
                
                -- checks, to be shared in guards
                check_sz = wlSz > 0
                check_sz_vUnbound = check_sz && Set.notMember v bound
        -- solve ass@(ASS {assWl=wl, assWlRedo=wlRedo}) = tr "solve: term" (pp (show wl)) Nothing

        -- solve pairs
        solve2 v (i1:i2:is',isSeen) todo ass@(ASS {assWl=wl, assQm=qualM})
          | isJust mb
            = updAss wl' qualM' s solved ass
          | otherwise 
            = solve2 v (i1:is',i2:isSeen) (maybe (Just (i2:is')) Just todo) ass
          where mb@(~(Just (i',s,solved))) = solveQ2 qualM v i1 i2
                (qualM',wl') = updQualWl s wl qualM $ [i'] ++ is' ++ isSeen
        solve2 v _ (Just (todo@(_:_:_))) ass = solve2 v (todo,[]) Nothing ass
        solve2 _ _ _ _ = Nothing

        -- solve singletons
        solve1 v (is@(i1:is'),isSeen) ass@(ASS {assWl=wl, assQm=qualM})
          | isJust mb
            = updAss wl' qualM' s solved ass
          | otherwise 
            = solve1 v (is',i1:isSeen) ass
          where q1 = qual qualM i1
                mb@(~(Just (s,solved))) = solveQ1 qualM v i1
                (qualM',wl') = updQualWl s wl qualM $ is ++ isSeen
        solve1 _ _ _ = Nothing

        -- access qual via index
        qual   m q     = panicJust "amsSolve.qual" $ Map.lookup q m
        
        -- update qualM
        updQualWl s wl qm is = (qm', wl')
          where qm'                  = foldr (\i m -> Map.update (\q -> Just $ s |=> q) i m) qm is
                wl' | varmpIsEmpty s = wl
                    | otherwise      = wlAddL qm' is wl

        -- update ass when something is solved
        updAss wl qm s slv ass = -- tr "amsSolve.updAss" (s >-< assVm ass) $
                                 Just (ass {assWl=wl, assQm=qm, assVm= s `varmpPlus` assVm ass, assSolved = Set.union slv $ assSolved ass})
        
        -- worklist: initial + remainder
        wlAdd  m q  wl = foldr (\v wl -> add v q wl) wl (ftv (qual m q))
                       where add v q wl = Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) v wl
        wlAddL m qs wl = foldr (wlAdd m) wl qs

        -- get worklist to solve
        wlInit = wlAddL initQualM (Map.keys initQualM) Map.empty

        -- solve a qual pair
        solveQ2 m var i1 i2
          = -- () $
            case -- (\xy@(x,y) -> tr "solveQ2.arg" (i1 >#< x >-< i2 >#< y) xy) $
                 (qual m i1, qual m i2) of
              -- transitivity
              (RelevQual_SubEval al1 (AnaEval_Var r1), RelevQual_SubEval (AnaEval_Var l2) ar2)
                  | r1 == var && l2 == var -> Just (i1, bindAnaEval var ar2, Set.singleton i2)
              -- and reverse
              (RelevQual_SubEval (AnaEval_Var l1) ar1, RelevQual_SubEval al2 (AnaEval_Var r2))
                  | r2 == var && l1 == var -> Just (i2, bindAnaEval var ar1, Set.singleton i1)
              {-
              -- transitivity for alternative (only 1 direction here!)
              (RelevQual_SubEval al1 (AnaEval_Var r1), RelevQual_Alt (RelevQual_SubEval (AnaEval_Var l2) ar2) _ _ _)
                  | r1 == var && l2 == var -> Just (i2, bindAnaEval var ar2, Set.singleton i1)
              -}
              (_,_)
                  -> Nothing

        -- solve a singleton
        solveQ1 m var i1
          = (\x -> tr "solveQ1.res" (maybe (pp "-") (\(_,s) -> pp $ show s) x) x) $
            case -- (\x -> tr "solveQ1.arg" (i1 >#< x) x) $
                 (qual m i1) of
              -- arbitrariness
              (RelevQual_SubEval al1 (AnaEval_Var r1))
                  | r1 == var -> Just (bindAnaEval var al1, Set.singleton i1)
              -- and reverse
              (RelevQual_SubEval (AnaEval_Var l1) ar1)
                  | l1 == var -> Just (bindAnaEval var ar1, Set.singleton i1)
              _               -> Nothing

        -- initial qual map
        initQualM = -- (\v -> tr "initQualM" (pp $ show v) v) $
                    Map.fromList $ zip [(1::Int) ..] (Set.toList qualS)

        -- bind, with simple occur check
        bindAnaEval v1 a2@(AnaEval_Var v2) | v1 == v2 = emptyVarMp
        bindAnaEval v1 a2                             = rvarmpEvalUnit v1 a2
%%]





