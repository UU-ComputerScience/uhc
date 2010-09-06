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

%%[(8 codegen) hs export(relevtyQuant)
relevtyQuant
  :: Bool
     -> RVarMp -> RelevQualS -> RelevTy
     -> ( RelevTy
        , RVarMp    -- extra subst
        -- , [UID]      -- quantified over
        -- , [UID]      -- remaining
        )
relevtyQuant doSolve m qs t
  = case funTy of
      t'@(RelevTy_Fun _ _ as r)
         -> ( RelevTy_Fun quantOver qs'' as' r', solveVarMp
            -- , quantOver, Set.toList $ ftvSet (solveVarMp |=> qs) `Set.difference` ftvTQ
            )
         where ftvT = ftvSet t'
               (qs',solveVarMp) | doSolve   = assSolve ftvT qsm
                                | otherwise = (qsm, emptyVarMp)
                                where qsm = Set.map (m |=>) qs
               qs'' = Set.toList qs'
               as' = solveVarMp |=> as
               r'  = solveVarMp |=> r
               ftvTQ = ftvSet as' `Set.union` ftvSet r' `Set.union` ftvSet qs''
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

%%[(8 codegen) hs
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
      , assWlSize	:: !Int
      , assWlPos	:: !Int
      }

assSolve :: UIDS -> RelevQualS -> (RelevQualS,RVarMp)
assSolve bound qualS
  = -- maybe (qualS, emptyVarMp)
          (\ass -> ( Set.map (qual $ assQm ass) $ (Set.unions $ Map.elems (assWl ass)) `Set.difference` assSolved ass
                     {-
                     Set.map 
                             (assReduced ass
                              `Set.union` (Set.unions $ Map.elems $ assWlRedo ass)
                             )
                     -}
                   -- Set.fromList [ q | q <- Map.elems $ assQm ass, ftvSet q `Set.isSubsetOf` bound ]
                   , assVm ass
          )        )
    $ tr "assSolve" (pp (show bound))
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
          {-
          | check_sz && not (Set.null isReduced)
            = -- tr "solve.assReduced" (show isReduced >-< show (Set.map (qual qualM) isReduced)) $
              solve (ass { assReduced = Set.union isReduced $ assReduced ass, assProgress = True
                         , assWl = if Set.null isToSolve then wl' else Map.insert v isToSolve wl'
                         })
          | not ((Set.null $ assSolved ass) && (Set.null $ assReduced ass))
            = tr ("solve: exit") (show wl >-< show wlRedo) $
              Just ass
          -}
          | otherwise
            = -- tr ("solve: term") (show wl >-< show wlRedo) $
              ass
          where (v,is0) = Map.elemAt (assWlPos ass) wl
                -- ((v,is0), wl') = Map.deleteFindMin wl
                is = is0 `Set.difference` assSolved ass
                (isReduced,isToSolve) = splitRedSlv1 qualM is
                -- is'  = (Set.toList is,[])
                -- ass' = ass {assWl=wl'}
                
                -- the real solving work attempts
                mbN  = solveN v is  ass
                
                -- checks, to be shared in guards
                check_pos = wlPos < wlSize
                -- check_sz = wlSz > 0
                -- check_sz_vUnbound = check_sz && Set.notMember v bound
                
        -- solve ass@(ASS {assWl=wl, assWlRedo=wlRedo}) = tr "solve: term" (pp (show wl)) Nothing

        -- split into those which cannot be further solved, and the rest
        splitRedSlv1 m = Set.partition (\i -> ftvSet (qual m i) `Set.isSubsetOf` bound)
        -- splitRedSlv2 m = Set.filter (\i -> Set.null $ ftvSet (qual m i) `Set.intersection` bound)

        -- solve combination of all quals related to single AnaEval var
        solveN v is ass@(ASS {assWl=wl, assQm=qualM})
          = do (q,s,slv,_) <- solveQN1 (l,al) v (r,ar)
               let (qualM2,isNw) = qmAddL q qualM
                   slvS          = Set.fromList slv
                   (qualM3,wl') = updQualWl slvS s wl qualM2 $ filter (\i -> Set.notMember i slvS) $ isNw ++ isL
               updAss wl' qualM3 s slvS ass
          where isL = Set.toList is
                (l, al, r, ar, o) = split ([],[],[],[],[]) [ (qual qualM i, i) | i <- isL ]
                split (l,al,r,ar,o) ((   RelevQual_SubEval a1 (AnaEval_Var v2), i) : qs) | v2 == v  = split ((i,a1):l,          al,        r,          ar,   o) qs
                split (l,al,r,ar,o) ((q@(RelevQual_Alt (RelevQual_SubEval a1 (AnaEval_Var v2)) _ _ _)
                                                                              , i) : qs) | v2 == v  = split (       l, (q,i,a1):al,        r,          ar,   o) qs
                split (l,al,r,ar,o) ((   RelevQual_SubEval (AnaEval_Var v1) a2, i) : qs) | v1 == v  = split (       l,          al, (i,a2):r,          ar,   o) qs
                split (l,al,r,ar,o) ((q@(RelevQual_Alt (RelevQual_SubEval (AnaEval_Var v1) a2) _ _ _)
                                                                              , i) : qs) | v1 == v  = split (       l,          al,        r, (q,i,a2):ar,   o) qs
                split (l,al,r,ar,o) ((   _                                    , i) : qs)            = split (       l,          al,        r,          ar, i:o) qs
                split res           _                                                               = res

        -- access qual via index
        qual   m q     = panicJust "amsSolve.qual" $ Map.lookup q m
        
        -- update qualM
        updQualWl slvS s wl qm is = (qm', wl')
          where qm'                  = foldr (\i m -> Map.update (\q -> Just $ s |=> q) i m) qm is
                wl'                  = wlAddL qm' is wl
                {-
                wl' | varmpIsEmpty s = wl
                    | otherwise      = wlAddL qm' is wl
                -}

        -- update ass when something is solved
        updAss wl qm s slv ass = Just (ass {assWl=wl, assQm=qm, assVm= s `varmpPlus` assVm ass, assSolved = Set.union slv $ assSolved ass})
        
        -- worklist: initial + remainder; TBD: variable-less quals need to be checked for being tautology
        wlAdd  m q  wl = foldr (add q) wl (ftv (qual m q))
                       where add q v wl = Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) v wl
        wlAddL m qs wl = foldr (wlAdd m) wl qs

        -- get worklist to solve
        wlInit = wlAddL initQualM (Map.keys initQualM) Map.empty

        -- solve a combi of quals with sharing of middle AnaEval var
        solveQN1 (als,aals) var (ars,aars)
          = (\x -> tr "solveQN1.res" (ppCommas als >-< ppCommas (map (\(x,y,z) -> ppParensCommas [pp x,pp y,pp z]) aals) >-< var >|< ppParens varIsBound >-< ppCommas ars >-< ppCommas (map (\(x,y,z) -> ppParensCommas [pp x,pp y,pp z]) aars) >-< "=>" >#< maybe (pp "-") (\(q,s,slv,reason) -> (reason ++ ":") >#< "qual:" >#< q >#< "subs:" >#< s >-< "slv:" >#< show slv) x) x) $
            case (als,aals,ars,aars) of
              -- transitive
              (ll1, ll2, rr1, rr2)
                | varIsFree && not (null ll1 && null ll2) && not (null rr1 && null rr2)
                -> do (jn,ams1) <- if null ll1
                                   then return (bot,ams0)
                                   else r1 ams0 join alsq
                      (mt,ams2) <- if null rr1
                                   then return (top,ams0)
                                   else r1 ams1 meet arsq
                      return ( (if null ll1 || null rr1 then [] else [RelevQual_SubEval jn mt])
                               ++ [ RelevQual_Alt (RelevQual_SubEval a  mt) x y z | (RelevQual_Alt _ x y z, _, a) <- aals ]
                               ++ [ RelevQual_Alt (RelevQual_SubEval jn a ) x y z | (RelevQual_Alt _ x y z, _, a) <- aars ]
                             , -- (if null ll1 then emptyVarMp else bindAnaEval var jn)
                               (if null rr1 then emptyVarMp else bindAnaEval var mt)
                               `varmpPlus` amsLocalVarMp ams2
                             , (slv $ als ++ ars) ++ [i | (_,i,_) <- aals] ++ [i | (_,i,_) <- aars]
                             , "transitivity"
                             )
              -- alternative removal
              {-
              -}
              (_, l@((RelevQual_Alt _ id _ mx, _, a) : _), _, _) 
                | Set.isSubsetOf all thisnrs
                -> return
                     ( [RelevQual_SubEval a (AnaEval_Var var)]
                     , emptyVarMp
                     , [i | (_,i,_) <- thisid]
                     , "alternative"
                     )
                where all = Set.fromList [1 .. mx]
                      thisid = filter (\(RelevQual_Alt _ id' _ _, _, a') -> id == id' && a == a') l
                      thisnrs = Set.fromList [ nr | (RelevQual_Alt _ _ nr _, _, _) <- thisid ]
              -- symmetry
              ((_:_), _, (_:_), _)
                | varIsFree && not (null lr)
                -> case lr of
                     ((l,v,r):_)
                       -> return ( [], bindAnaEval var (AnaEval_Var v), [l,r]
                                 , "symmetry"
                                 )
                where mbV l = [ (fromJust mbv,i) | (i,a) <- l, let mbv = isVar a, isJust mbv ]
                      l = mbV als
                      r = mbV ars
                      lr = [ (il, v, fromJust mbr) | (v,il) <- l, let mbr = lookup v r, isJust mbr ]
              -- reflexivity
              {-
              -}
              ((_:_), _, _, _)
                | not (null l)
                -> case l of
                     (i:_)
                       -> return ( [], emptyVarMp, [i]
                                 , "reflexivity"
                                 )
                where l = [ i | (i,a) <- als, maybe False (==var) (isVar a) ]
              --
              {-
              ((_:_), _, [], _)
                | varIsFree
                -> do (jn,ams1) <- r1 ams0 join alsq
                      return ( [], bindAnaEval var jn `varmpPlus` amsLocalVarMp ams1, slv als
                             , "left join"
                             )
              ([], _, (_:_), _)
                | varIsFree
                -> do (mt,ams1) <- r1 ams0 meet arsq
                      return ( [], bindAnaEval var mt `varmpPlus` amsLocalVarMp ams1, slv ars
                             , "right meet"
                             )
              -}
              -- overlaps with transitivity, fired only when we can conclude var <= bot anyway
              ([], _, (_:_), _)
                | not (null b)
                -> return ( [], bindAnaEval var a2, [i]
                          , "right bot"
                          )
                where (b,_) = partition (\(_,a) -> isBot a) ars
                      ~(i,a2) = head b
              (_, _, (_:_), _)
                | not (null t)
                -> return ( [], emptyVarMp, map fst t
                          , "right top"
                          )
                where (t,_) = partition (\(_,a) -> isTop a) ars
              ((_:_), _, _, _)
                | not (null b)
                -> return ( [], emptyVarMp, map fst b
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
        qmAddL qs m = (m `Map.union` m', Map.keys m')
                    where m' = Map.fromList $ zip [Map.size m ..] qs
        initQualM = (\v -> tr "initQualM" (pp $ show v) v) $
                    fst $ qmAddL (Set.toList qualS) Map.empty

        -- bind, with simple occur check
        bindAnaEval v1 a2@(AnaEval_Var v2) | v1 == v2 = emptyVarMp
        bindAnaEval v1 a2                             = rvarmpEvalUnit v1 a2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Datatype related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(relevTyArgsFromCTag)
relevTyArgsFromCTag :: CTag -> Int -> DataGam -> UID -> Maybe [RelevTy]
relevTyArgsFromCTag CTagRec arity _ u
  = Just $ map freshLazy $ mkNewLevUIDL arity u
relevTyArgsFromCTag ct _ dataGam u
  = case dataGamTagLookup ct dataGam of
      Just (_,dti)
        -> Just $
           zipWith freshFromAnn
                   (mkNewLevUIDL (ctagArity ct) u)
                   (map dcfaiStrictness $ dtiConFldAnnL dti)
      _ -> Nothing
%%]





