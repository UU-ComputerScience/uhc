%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AnaDomain utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}AnaDomain.Utils} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) hs import({%{EH}AnaDomain},{%{EH}AnaDomain.Trf.Subst},{%{EH}AnaDomain.Ftv})
%%]

%%[(8 codegen) hs import({%{EH}Gam.DataGam})
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set, Data.Array, Data.Maybe, Data.List)
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

%%[(8 codegen) hs export(RelevTyQuantHow(..))
-- | configuring quantification, for debugging
data RelevTyQuantHow
  = RelevTyQuantHow_Solve				-- solve
  | RelevTyQuantHow_RemoveAmbig			-- constraints over vars not in type are removed
  | RelevTyQuantHow_VarDefaultToTop		-- vars left over, and in type, are defaulted to top
  | RelevTyQuantHow_Quant				-- quant
  | RelevTyQuantHow_Rec					-- for recursive use
  deriving Eq
%%]

%%[(8 codegen) hs export(relevtyQuant)
relevtyQuant
  :: [RelevTyQuantHow]
     -> RVarMp -> RelevQualS -> RelevTy
     -> ( RelevTy
        , RVarMp    	-- extra subst
        , RelevQualS	-- removed quals
        )
relevtyQuant how m qs t
  = case funTy of
      t'@(RelevTy_Fun _ _ _ as r) | RelevTyQuantHow_Rec `elem` how
         -> ( RelevTy_Fun RQuant_Rec (ftv t') [] as r
            , emptyVarMp
            , Set.empty
            )
      t'@(RelevTy_Fun _ _ _ as r)
         -> ( RelevTy_Fun RQuant_Forall quantOver qs4 as' r'
            , solveVarMp
            , qs3rem
            )
         where ftvT = ftvSet t'
               (qs2,solveVarMp)
                   | RelevTyQuantHow_Solve `elem` how = assSolve ftvT qsm
                   | otherwise                        = (qsm, emptyVarMp)
                   where qsm = Set.map (m |=>) qs
               as' = solveVarMp |=> as
               r'  = solveVarMp |=> r
               ftvT' = ftvSet as' `Set.union` ftvSet r'
               (qs3,qs3rem)
                   | RelevTyQuantHow_RemoveAmbig `elem` how = relevQualRemoveAmbig ftvT' qsm
                   | otherwise                              = (qsm, Set.empty)
                   where qsm = Set.map (solveVarMp |=>) qs2
               qs4 = Set.toList qs3
               ftvTQ = ftvT' `Set.union` ftvSet qs4
               quantOver
                   | RelevTyQuantHow_Quant `elem` how = Set.toList ftvTQ
                   | otherwise                        = []
      t' -> ( t'
            , emptyVarMp
            , Set.empty
            )
  where funTy = case m |=> t of
                  t'@(RelevTy_Fun _ _ _ _ _) -> t'
                  t'                         -> RelevTy_Fun RQuant_None [] [] [] t'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Removal of constraints which relate only to internal variables (free vars), and alternatives (which depend on constructor alternative)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(relevQualRemoveAmbig)
-- | Remove unresolvable constraints, and those referring to free vars
relevQualRemoveAmbig :: UIDS -> RelevQualS -> (RelevQualS,RelevQualS)
relevQualRemoveAmbig bound qualS
  = Set.partition ok qualS
  where -- ok (RelevQual_Alt _ _ _ _ _ _) = False
        ok q                           = ftvSet q `Set.isSubsetOf` bound
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
instance Substitutable RelevTy UID RVarMp where
  (|=>) = relevtyAppVarLookup
  ftvSet = relevTyFtv
%%]

%%[(8 codegen) hs
instance Substitutable RelevQual UID RVarMp where
  (|=>) = relevqualAppVarLookup
  ftvSet = relevQualFtv
%%]

%%[(8 codegen) hs
instance Substitutable RelevCoe UID RVarMp where
  (|=>) = relevcoeAppVarLookup
  ftvSet = relevCoeFtv
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

%%[(8 codegen) hs export(assSolve)
type WL = Map.Map UID (Set.Set Int)

data ASS
  = ASS
      { assWl       :: !WL
      , assWlRedo   :: !WL
      , assQm       :: !(Map.Map Int RelevQual)
      , assVm       :: !RVarMp
      , assProgress :: !Bool
      , assSolved   :: !(Set.Set Int)
      , assReduced  :: !(Set.Set Int)
      , assWlSize   :: !Int
      , assWlPos    :: !Int
      }

assSolve :: UIDS -> RelevQualS -> (RelevQualS,RVarMp)
assSolve bound qualS
  = -- maybe (qualS, emptyVarMp)
          (\ass -> ( Set.map (qual $ assQm ass) $ (Set.unions $ Map.elems (assWl ass)) `Set.difference` assSolved ass
                   , assVm ass
          )        )
    -- $ tr "assSolve" (pp (show bound))
    $ solve (ASS wlInit Map.empty initQualM emptyVarMp False Set.empty Set.empty (Map.size wlInit) 0)
  where -- solver
        solve ass@(ASS {assWl=wl, assQm=qualM, assWlPos=wlPos, assWlSize=wlSize})
          | check_pos {- _vUnbound -} && isJust mbN
            = -- tr ("solve: isJust mbN") (v >#< show is >-< "slv :" >#< show (assSolved ass) >-< "slvN:" >#< show (assSolved $ fromJust mbN) >-< "wl :" >#< show wl >-< "wlRedo:" >#< show (assWlRedo $ fromJust mbN) >-< show (assQm ass) >-< show (assQm $ fromJust mbN)) $
              solve ((fromJust mbN) {assProgress = True, assWlPos = wlPos + 1})
          | check_pos
            = -- tr ("solve: wlSz > 0(" ++ show wlSz ++ ")") (v >#< show is0 >#< show is >-< show wl) $
              solve (ass {assWlPos = wlPos + 1})
          | assProgress ass
            = -- tr ("solve: assProgress ass && Map.size wlRedo > 0") (Map.size wl >#< show wl >-< Map.size wlRedo >#< show wlRedo) $
              let wl' = Map.filter (\s -> not (Set.null s)) wl
              in  solve (ass {assProgress = False, assWlPos=0, assWl = wl', assWlSize = Map.size wl'})
          | otherwise
            = -- tr ("solve: done") ("assVm:" >#< assVm ass) $
              ass
          where (v,is0) = Map.elemAt (assWlPos ass) wl
                is = is0 `Set.difference` assSolved ass
                (isReduced,isToSolve) = splitRedSlv1 qualM is
                
                -- the real solving work attempts
                mbN  = solveN v is  ass
                
                -- checks, to be shared in guards
                check_pos = wlPos < wlSize
                
        -- split into those which cannot be further solved, and the rest
        splitRedSlv1 m = Set.partition (\i -> ftvSet (qual m i) `Set.isSubsetOf` bound)

        -- solve combination of all quals related to single AnaEval var
        solveN v is ass@(ASS {assWl=wl, assQm=qualM})
          = do (q,s,slv,_) <- solveQN1 (l) v (r)
               let (qualM2,isNw) = qmAddL q qualM
                   slvS          = Set.fromList slv
                   touchedBySubs = (Set.fold (\v is -> is `Set.union` Map.findWithDefault Set.empty v wl) Set.empty $ Set.delete v $ varmpKeysSet s) `Set.difference` assSolved ass
                   (qualM3,wl') = updQualWl slvS s wl qualM2 $ Set.toList $ Set.unions [isNw, is, touchedBySubs] `Set.difference` slvS -- filter (\i -> Set.notMember i slvS) $ isNw ++ isL
               updAss wl' qualM3 s slvS ass
          where isL = Set.toList is
                (l, r, o) = split ([],[],[]) [ (qual qualM i, i) | i <- isL ]
                split (l,r,o) ((   RelevQual_SubEval a1 (AnaEval_Var v2), i) : qs) | v2 == v  = split ((i,a1):l,        r,   o) qs
                split (l,r,o) ((   RelevQual_SubEval (AnaEval_Var v1) a2, i) : qs) | v1 == v  = split (       l, (i,a2):r,   o) qs
                split (l,r,o) ((   _                                    , i) : qs)            = split (       l,        r, i:o) qs
                split res     _                                                               = res

        -- access qual via index
        qual   m q     = panicJust "amsSolve.qual" $ Map.lookup q m
        
        -- update qualM
        updQualWl slvS s wl qm is = (qm', wl')
          where qm' = foldr (\i m -> Map.update (\q -> Just $ s |=> q) i m) qm is
                wl' = wlAddL qm' is wl

        -- update ass when something is solved
        updAss wl qm s slv ass = Just (ass {assWl=wl, assQm=qm, assVm= s `varmpPlus` assVm ass, assSolved = Set.union slv $ assSolved ass})
        
        -- worklist: initial + remainder; TBD: variable-less quals need to be checked for being tautology
        wlAdd  m q  wl = foldr (add q) wl (ftv (qual m q))
                       where add q v wl = Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) v wl
        wlAddL m qs wl = foldr (wlAdd m) wl qs

        -- get worklist to solve
        wlInit = wlAddL initQualM (Map.keys initQualM) Map.empty

        -- solve a combi of quals with sharing of middle AnaEval var
        solveQN1 (als) var (ars)
          = -- (\x -> tr "solveQN1.res" (ppCommas als >-< var >|< ppParens varIsBound >-< ppCommas ars >-< "=>" >#< maybe (pp "-") (\(q,s,slv,reason) -> (reason ++ ":") >#< "qual:" >#< q >#< "subs:" >#< s >-< "slv:" >#< show slv) x) x) $
            case (als,ars) of
              {-
              -}
              -- transitive, no alternatives
              (ll1, rr1)
                | canDo && isJust mbJn
                -> return
                     ( (if null ll1 || null rr1 then [] else [RelevQual_SubEval jn mt])
                       ++ varConstraint
                     , amsLocalVarMp ams2
                     , (slv $ als ++ ars)
                     , "transitivity 1"
                     )
                | canDo     -- weaker transitivity where meet is distributed over the left sides
                -> return
                     ( [ RelevQual_SubEval a mt | a <- alsq ]
                       ++ varConstraint
                     , amsLocalVarMp ams1
                     , (slv $ als ++ ars)
                     , "transitivity 2"
                     )
                where canDo = varIsFree && not (null ll1) && not (null rr1) && isJust mbMt
                      mbMt@(~(Just (mt,ams1))) = if null rr1 then Just (top,ams0) else r1 ams0 meet arsq
                      mbJn@(~(Just (jn,ams2))) = if null ll1 then Just (bot,ams1) else r1 ams1 join alsq
                      varConstraint
                        = if null rr1 then [] else [RelevQual_SubEval (AnaEval_Var var) mt]
              -- symmetry
              {-
              -}
              ((_:_), (_:_))
                | not (null lr)
                -> case lr of
                     ((l,v,r):_)
                       -> return ( [], bind, [l,r]
                                 , "symmetry"
                                 )
                       where bind | varIsFree                  = bindAnaEval var (AnaEval_Var v  )
                                  | not $ v `Set.member` bound = bindAnaEval v   (AnaEval_Var var)
                                  | otherwise                  = emptyVarMp
                where mbV l = [ (fromJust mbv,i) | (i,a) <- l, let mbv = isVar a, isJust mbv ]
                      l = mbV als
                      r = mbV ars
                      lr = [ (il, v, fromJust mbr) | (v,il) <- l, let mbr = lookup v r, isJust mbr ]
              -- reflexivity
              {-
              -}
              ((_:_), _)
                | not (null l)
                -> case l of
                     (i:_)
                       -> return ( [], emptyVarMp, [i]
                                 , "reflexivity"
                                 )
                where l = [ i | (i,a) <- als, maybe False (==var) (isVar a) ]
              -- a free var which is not left constrained anymore, simplifies to the right side
              ([], (_:_:_))
                | isJust mbMt
                -> return ( [RelevQual_SubEval (AnaEval_Var var) mt], amsLocalVarMp ams1, slv ars
                          , "right meet"
                          )
                where mbMt@(~(Just (mt,ams1))) = r1 ams0 meet arsq
              {-
              ([], (_:_))
                | varIsFree
                -> do (mt,ams1) <- r1 ams0 meet arsq
                      return ( [], bindAnaEval var mt `varmpPlus` amsLocalVarMp ams1, slv ars
                             , "right meet"
                             )
              -}
              {-
              -- a free var not right constrained anymore, is just forgotten, which is pessimistic because we cannot infer 'more info', i.e. down in the lattice
              ((_:_), [])
                | varIsFree
                -> return ( [], emptyVarMp, slv als, "left forget" )
              -}
              {-
                -> do (jn,ams1) <- r1 ams0 join alsq
                      return ( [], bindAnaEval var jn `varmpPlus` amsLocalVarMp ams1, slv als
                             , "left join"
                             )
              -}
              -- overlaps with transitivity, fired only when we can conclude var <= bot anyway
              {-
              -}
              ([], (_:_))
                | not (null b)
                -> return ( [], bindAnaEval var a2, [i]
                          , "right bot"
                          )
                where (b,_) = partition (\(_,a) -> isBot a) ars
                      ~(i,a2) = head b
              ((_:_), [])
                | not (null b)
                -> return ( [], bindAnaEval var a2, [i]
                          , "left top"
                          )
                where (b,_) = partition (\(_,a) -> isTop a) als
                      ~(i,a2) = head b
              -- top at right is non-information, throw it away
              (_, (_:_))
                | not (null t)
                -> return ( [], emptyVarMp, slv t
                          , "right top"
                          )
                where (t,_) = partition (\(_,a) -> isTop a) ars
              -- bot at left is non-information, throw it away
              ((_:_), _)
                | not (null b)
                -> return ( [], emptyVarMp, slv b
                          , "left bot"
                          )
                where (b,_) = partition (\(_,a) -> isBot a) als
              _ -> Nothing
          where alsq  = map snd als
                arsq  = map snd ars
                slv a = map fst a
                ams0 = emptyAnaMatchState {amsBoundS = bound}
                r0 s act one l   = r1 s act (one:l)
                r1 s act (hd:tl) = foldM (\(x,s') y -> amsRunMb s' $ act x y) (hd,s) tl
                varIsBound = var `Set.member` bound
                varIsFree  = not varIsBound

        -- initial qual map
        qmAddL qs m = (m `Map.union` m', Map.keysSet m')
                    where m' = Map.fromList $ zip [Map.size m ..] qs
        initQualM = -- (\v -> tr "initQualM" (pp $ show v) v) $
                    fst $ qmAddL (Set.toList qualS) Map.empty

        -- bind, with simple occur check
        bindAnaEval v1 a2@(AnaEval_Var v2) | v1 == v2 = emptyVarMp
        bindAnaEval v1 a2                             = rvarmpEvalUnit v1 a2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Datatype/tuple related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(relevTyArgsFromCTag)
-- | relev ty for tuple/constructor. isJust mbResTy <=> not isInPat
relevTyArgsFromCTag :: Bool -> CTag -> Maybe RelevTy -> Int -> DataGam -> UID -> Maybe ([RelevTy],RelevQualS)
relevTyArgsFromCTag isInPat ct mbResTy arity dataGam u
  = case ct of
      CTagRec
        -> Just (map fresh $ mkNewLevUIDL arity u, Set.empty)
      _ -> case dataGamTagLookup ct dataGam of
             Just (_,dti)
               -> Just (as, Set.fromList qa)
               where (as, qa) = relevTyArgs frC constrain u (map ((==Strictness_Strict) . dcfaiStrictness) $ dtiConFldAnnL dti) res
                     (res, frC, constrain)
                              = case mbResTy of
                                  Just t
                                    -> (t  , fr, \(RelevTy_Ana x) (RelevTy_Ana y) -> [RelevQual_SubEval x y])
                                    where fr _ = fresh
                                  _ -> (top, fr, \_               _               -> []                     )
                                    where fr e | e         = freshStrict
                                               | otherwise = freshLazy
             _ -> Nothing

%%]


%%[(8 codegen) hs export(relevTyArgs)
relevTyArgs :: (Bool -> UID -> RelevTy) -> (RelevTy -> RelevTy -> [RelevQual]) -> UID -> [Bool] -> RelevTy -> ([RelevTy],[RelevQual])
relevTyArgs fresh constrain u relevForRes res
  = (as, concat qa)
  where (as,qa) = unzip $ zipWith mk relevForRes $ mkNewLevUIDL (length relevForRes) u
                where mk e u | e         = (t,constrain t res)
                             | otherwise = (t,[]             )
                        where t = fresh e u
%%]



